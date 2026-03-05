# Project structure

## R/
Contains the analysis scripts.

Key shared files:
- `R/utils.R` — Canonical `extract_coefs()` function, sourced by `00_setup_comun.R` and `07_rca_threshold_sensitivity_FULL.R`.
- `R/99_paths_local.R.example` — Template for local path configuration. Copy to `R/99_paths_local.R` (gitignored) and set machine-specific paths.
- `R/00_setup_comun.R` — Session setup: loads data via `99_paths_local.R`, sources `utils.R`, derives wage variables, and draws the 50% source sample.

## scripts/
Thin wrappers that:
- run figure/table generation from cached outputs (no model re-estimation)
- sync outputs into `./images/` so LaTeX compiles without path edits

## output_SI_identity/
Canonical SI output folder (scripts write here).
- Track: figs + tables
- Do not track: large cached model objects (rds/models) unless needed

## output_rca_threshold/
RCA threshold sensitivity outputs (gitignored).

## images/
LaTeX-facing figure folder.
- `images/` (flat): keeps current `\includegraphics{images/...}` paths working
- `images/main/` and `images/si/`: organized mirrors

## paper/
Reference PDFs of the latest compiled main + SI.

## data/
Local-only raw/derived data; small metadata tracked. See `docs/DATA_PROVENANCE.md`.

## docs/
- `DATA_PROVENANCE.md` — Data sources, construction steps, and checksums.
- `PROJECT_STRUCTURE.md` — This file.
- `REPRODUCIBILITY_CHECKLIST.md` — Pre-submission checklist.

## .gitignore
Excludes: R session artifacts (`.Rhistory`, `.RData`, `.Rproj.user/`), generated output directories (`output_*/`, `subperiod_results/`), large data directories (`datos_eventos_*/`), local configuration (`R/99_paths_local.R`), OS artifacts, and LaTeX build artifacts.

