# Python reference generator

Generates the reference outputs used by the R-side regression tests. The Python `infostop` package is run on a set of fixed datasets, and the inputs and expected outputs are written as CSVs plus JSON params. The R tests read only those CSVs.

## Contents

- `generate_data.R` - builds the input datasets and writes them to `../data/`.
- `generate_reference.py` - reads the datasets, runs stage 1 (`cpputils.get_stationary_events`) and stage 2 (`Infostop.fit_predict`), and writes the output CSVs and params to `../reference/`.
- `pyproject.toml` - project metadata and dependencies (infostop, numpy, pandas).
- `uv.lock` - pinned dependency lockfile.

## Usage

Generate input data (requires R and `sf`):

```bash
Rscript generate_data.R
```

Generate reference outputs (requires Python 3.12 and `uv`):

```bash
uv sync
uv run python generate_reference.py
```
