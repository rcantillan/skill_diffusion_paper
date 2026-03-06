# Project structure

## R/
Contains all analysis scripts.

### Key shared files:
- `R/utils.R` — Canonical `extract_coefs()` function.
- `R/99_paths_local.R.example` — Template for local path configuration. Copy to `R/99_paths_local.R` (gitignored) and set machine-specific paths.
- `R/00_setup_comun.R` — Session setup: loads data, sources `utils.R`, derives wage variables, and draws the 50% source sample.

### R/main/
Main-text analyses and figures (01–04).

### R/SI/
Supplementary Information analyses. Each script runs in a clean R session.

## output/
Generated outputs (gitignored except `.gitkeep`).

## images/
LaTeX-facing figure folder (flat). Keeps `\includegraphics{images/...}` paths working.

## data/
Local-only raw/derived data; small metadata tracked. See `docs/DATA_PROVENANCE.md`.

## docs/
- `DATA_PROVENANCE.md` — Data sources, construction steps, and checksums.
- `PROJECT_STRUCTURE.md` — This file.
- `REPRODUCIBILITY_CHECKLIST.md` — Pre-submission checklist.

## Makefile
Build automation: `make check`, `make si_from_cache`, `make sync`, `make session_info`.

## .gitignore
Excludes: R session artifacts, generated output directories, large data directories, local configuration (`R/99_paths_local.R`), OS artifacts, and LaTeX build artifacts.