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
    p: Params,
    metric: str,
) -> tuple[np.ndarray, np.ndarray]:
    stop_events = []
    event_maps = []
    col_names = ["x", "y"] if metric == "euclidean" else ["lat", "lon"]
    for k, coords_u in enumerate(tracks):
        stop_events_i, event_map_i = cpputils.get_stationary_events(
            coords_u, p.r1, int(p.min_size),
            float(p.min_staying_time), float(p.max_time_between), metric,
        )
        stop_events_i = pd.DataFrame(stop_events_i)
        stop_events_i.columns = col_names
        stop_events_i["id"] = f"track_{k + 1}"
        stop_events.append(stop_events_i)
        event_map_i = pd.DataFrame({"id": f"track_{k + 1}", "stop_id": event_map_i})
        event_maps.append(event_map_i)
    stop_events = pd.concat(stop_events)
    event_maps = pd.concat(event_maps)
    return (stop_events, event_maps)


def identify_sites(
    tracks: list[np.ndarray],
    p: Params,
    metric: str,
) -> tuple[np.ndarray, dict]:
    col_names = ["x", "y"] if metric == "euclidean" else ["lat", "lon"]
    model = Infostop(
        r1=p.r1, r2=p.r2, label_singleton=p.label_singleton,
        min_staying_time=p.min_staying_time, max_time_between=p.max_time_between,
        min_size=int(p.min_size), min_spacial_resolution=p.min_spacial_resolution,
        distance_metric=metric, weighted=p.weighted,
        weight_exponent=p.weight_exponent, verbose=False,
    )
    pred = model.fit_predict(tracks)
    site_ids = []
    for k, labels in enumerate(pred):
        df = pd.DataFrame({"id": f"track_{k + 1}", "site_id": labels.tolist()})
        site_ids.append(df)
    site_ids = pd.concat(site_ids)

    site_centers = pd.DataFrame(model.compute_label_medians().values())
    site_centers.columns = col_names

    return site_ids, site_centers


def run(
    csv_file: Path,
    out_dir: Path
) -> None:
    stem = csv_file.stem                      # e.g. "lon-lat_01"
    variant, key = stem.rsplit("_", 1)        # ("lon-lat", "01")
    metric = "haversine" if variant == "lon-lat" else "euclidean"
    p = PARAMS[key]

    tracks, indices, coord_cols = load(csv_file)

    stop_centers, stop_ids = identify_stops(tracks, p, metric)
    if stop_centers.shape[0] == 0:
        raise "No centers found!"
    site_ids, site_centers = identify_sites(tracks, p, metric)

    out = out_dir / stem
    stop_ids.to_csv(f"{out}_stops.csv", index=False)
    stop_centers.to_csv(f"{out}_stop_centers.csv", index=False)

    site_ids.to_csv(f"{out}_sites.csv", index=False)
    site_centers.to_csv(f"{out}_site_centers.csv", index=False)

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
