"""
Generate infostop reference outputs for the R regression tests.
"""
import json
from dataclasses import asdict, dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from infostop import cpputils
from infostop.models import Infostop


@dataclass
class Params:
    r1: float
    r2: float
    min_staying_time: float
    max_time_between: float
    min_size: int
    min_spacial_resolution: float = 0.0
    weighted: bool = False
    weight_exponent: float = 1.0
    label_singleton: bool = True


PARAMS: dict[str, Params] = {
    "01": Params(r1=50.0, r2=200.0, min_staying_time=1.0, max_time_between=1e9, min_size=2),
    "02": Params(r1=20.0, r2=300.0, min_staying_time=1.0, max_time_between=1e9, min_size=10),
    "03": Params(r1=100.0, r2=50.0, min_staying_time=1.0, max_time_between=30 * 60.0,
                 min_size=2, weighted=True, weight_exponent=1.0),
}


def to_seconds(times: pd.Series) -> np.ndarray:
    """Return `times` as float seconds, parsing datetime strings if needed.

    infostop only uses time *differences*, so the epoch origin is irrelevant.
    """
    numeric = pd.to_numeric(times, errors="coerce")
    if numeric.notna().all():
        return numeric.to_numpy(dtype=float)
    return pd.to_datetime(times).astype("int64").to_numpy() / 1e9


def load(path: Path) -> tuple[list[np.ndarray], list[np.ndarray], list[str]]:
    """Load an input CSV into per-track ``(N, 3)`` ``[c0, c1, time]`` arrays.

    Returns the per-track coordinate arrays, the original row indices of each
    track (so per-row outputs can be scattered back), and the two coordinate
    column names.
    """
    df = pd.read_csv(path)
    coord_cols = list(df.columns[:2])
    coords = df[coord_cols].to_numpy(dtype=float)
    time = to_seconds(df["time"])
    ids = df["id"].to_numpy()

    tracks: list[np.ndarray] = []
    indices: list[np.ndarray] = []
    for uid in dict.fromkeys(ids):            # order-preserving unique
        idx = np.flatnonzero(ids == uid)
        indices.append(idx)
        tracks.append(np.column_stack([coords[idx], time[idx]]))
    return tracks, indices, coord_cols


def identify_stops(
    tracks: list[np.ndarray],
    indices: list[np.ndarray],
    n_rows: int,
    p: Params,
    metric: str,
) -> tuple[np.ndarray, np.ndarray]:
    stop_id = np.full(n_rows, -1, dtype=np.int64)
    centroids: list = []
    offset = 0
    for coords_u, idx in zip(tracks, indices):
        stop_events, event_map = cpputils.get_stationary_events(
            coords_u, p.r1, int(p.min_size),
            float(p.min_staying_time), float(p.max_time_between), metric,
        )
        event_map = np.asarray(event_map, dtype=np.int64)
        n_ev = len(stop_events)
        # event_map indexes into stop_events; values outside [0, n_ev) are the
        # outlier sentinel and map back to -1.
        keep = (event_map >= 0) & (event_map < n_ev)
        stop_id[idx] = np.where(keep, event_map + offset, -1)
        centroids.extend(stop_events)
        offset += n_ev
    stop_centers = np.asarray(centroids, dtype=float).reshape(-1, 2)
    return stop_id, stop_centers


def identify_sites(
    tracks: list[np.ndarray],
    indices: list[np.ndarray],
    n_rows: int,
    p: Params,
    metric: str,
) -> tuple[np.ndarray, dict]:
    model = Infostop(
        r1=p.r1, r2=p.r2, label_singleton=p.label_singleton,
        min_staying_time=p.min_staying_time, max_time_between=p.max_time_between,
        min_size=int(p.min_size), min_spacial_resolution=p.min_spacial_resolution,
        distance_metric=metric, weighted=p.weighted,
        weight_exponent=p.weight_exponent, verbose=False,
    )
    data = tracks if len(tracks) > 1 else tracks[0]
    site_id = np.full(n_rows, -1, dtype=np.int64)
    site_centers: dict = {}
    result = model.fit_predict(data)
    labels = result if isinstance(result, list) else [result]
    for labels_u, idx in zip(labels, indices):
        site_id[idx] = np.asarray(labels_u, dtype=np.int64)
    site_centers = model.compute_label_medians()
    return site_id, site_centers


def run(
    csv_file: Path,
    out_dir: Path
) -> None:
    stem = csv_file.stem                      # e.g. "lon-lat_01"
    variant, key = stem.rsplit("_", 1)        # ("lon-lat", "01")
    metric = "haversine" if variant == "lon-lat" else "euclidean"
    p = PARAMS[key]

    tracks, indices, coord_cols = load(csv_file)
    n_rows = sum(len(i) for i in indices)
    c0, c1 = coord_cols

    stop_id, event_cen = identify_stops(tracks, indices, n_rows, p, metric)
    if event_cen.shape[0] == 0:
        raise "No centers found!"
    site_id, site_centers = identify_sites(tracks, indices, n_rows, p, metric)

    out = out_dir / stem
    pd.DataFrame({"stop_id": stop_id}).to_csv(f"{out}_stops.csv", index=False)
    pd.DataFrame({c0: event_cen[:, 0], c1: event_cen[:, 1]}).to_csv(f"{out}_stop_centers.csv", index=False)
    pd.DataFrame({"site_id": site_id}).to_csv(f"{out}_sites.csv", index=False)
    if site_centers:
        loc = pd.DataFrame(
            [{"site_id": int(k), c0: v[0], c1: v[1]}
             for k, v in sorted(site_centers.items())]
        )
    else:
        loc = pd.DataFrame(columns=["site_id", c0, c1])
    loc.to_csv(f"{out}_site_centers.csv", index=False)

    params = {**asdict(p), "distance_metric": metric}
    Path(f"{out}_params.json").write_text(json.dumps(params, indent=2) + "\n")


def main() -> None:
    try:
        script_dir = Path(__file__).resolve().parent
    except:
        script_dir = Path(".").resolve()
    data_dir = script_dir.parent / "data"
    reference_dir = script_dir.parent / "reference"
    reference_dir.mkdir(parents=True, exist_ok=True)
    
    csv_files = sorted(data_dir.glob("*.csv"))
    # csv_file = csv_files[0]
    for csv_file in csv_files:
        run(csv_file, out_dir=reference_dir)
    print("done.")


if __name__ == "__main__":
    main()
