# Project structure

## R/
Contains all analysis scripts.

### Key shared files:
- `R/utils.R` — Canonical `extract_coefs()` function.
- `R/99_paths_local_TEMPLATE.R` — Template for local path configuration. Copy to `R/99_paths_local.R` (gitignored) and set machine-specific paths.
- `R/00_setup_comun.R` — Session setup: loads data, sources `utils.R`, derives wage variables, and draws the 50% source sample. **Note:** `output_dir` defaults to `output_SI_identity/`. Main-text scripts in `R/main/` manage their own output paths under `output/output_main/`.

### R/main/
Main-text analyses and figures (01–04).

### R/SI/
Supplementary Information analyses. Each script runs in a clean R session.
Scripts are organized by test:
- `01A/01B` — Baseline Panels A & B
- `02A/02B` — Test (i): Threshold placebo
- `03A/03B` — Test (ii): Domain-label permutation
- `04A/04B` — Test (iii): Within-stratum permutation
- `05` — Consolidation: figures and tables from cached results
- `06`–`10`, `S8`, `fig_S2`, `fig_S3_S4` — Additional robustness analyses and figures

## output/
Generated outputs (gitignored except `.gitkeep`).
- `output_main/` — Main-text outputs (descriptives, figs, flow_networks, logs, models, tables)
- `output_SI_identity/` — SI outputs (figs, logs, models, rds, tables)
- `output_rca_threshold/` — RCA sensitivity outputs (figs, rds, tables)
- `subperiod_results/` — Temporal stability outputs

## images/
LaTeX-facing figure folder (flat). Keeps `\includegraphics{images/...}` paths working.

## data/
Local-only raw/derived data; small metadata tracked. See `docs/DATA_PROVENANCE.md`.
- `data/raw/` — Public source files (O*NET, BLS; not tracked by git)
- `data/derived/` — Intermediate derived files (not tracked)
- `data/metadata/` — Checksums and version identifiers

## docs/
- `DATA_PROVENANCE.md` — Data sources, construction steps, and checksums.
- `PROJECT_STRUCTURE.md` — This file.
- `REPRODUCIBILITY_CHECKLIST.md` — Pre-submission checklist.

## Makefile
Build automation: `make check`, `make si_from_cache`, `make sync`, `make session_info`.

## renv.lock
Lockfile for R package versions. Restore with `renv::restore()`.

## .gitignore
Excludes: R session artifacts, generated output directories, large data directories, local configuration (`R/99_paths_local.R`), OS artifacts, and LaTeX build artifacts.