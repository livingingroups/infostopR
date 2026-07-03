# Python reference generator

Generates the reference outputs used by the R-side regression tests. The Python `infostop` package is run on a set of fixed
datasets, and the inputs and expected outputs are written as CSVs plus
a JSON manifest. The R tests read only those CSVs.

## Contents

- `generate.py` - builds the datasets, runs stage 1
  (`cpputils.get_stationary_events`) and stage 2
  (`Infostop.fit_predict`), and writes the CSVs and manifest.
- `pyproject.toml` - project metadata and dependencies (infostop,
  numpy, pandas).
- `uv.lock` - pinned dependency lockfile.

## Usage

Requires Python 3.12 and `uv`.

```bash
uv sync
uv run python generate.py
```

Outputs are written to `../data/` (inputs, params) and `../expected/`
(expected outputs, manifest).
