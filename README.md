# Asymmetric Directional Diffusion of Skill Requirements Reproduces Occupational Stratification

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R ≥ 4.3](https://img.shields.io/badge/R-%E2%89%A54.3-blue.svg)](https://cran.r-project.org/)
[![Under Review](https://img.shields.io/badge/Status-Under%20Review%20%40%20Science%20Advances-green.svg)]()

## Overview

This repository contains the full analysis code for the paper "Asymmetric directional diffusion of skill requirements reproduces occupational stratification" by Roberto Cantillan and Mauricio Bucca (targeting *Science Advances*). The study applies the Asymmetric Trajectory Channeling (ATC) framework to a triadic dataset of 17.3 million skill-diffusion opportunities constructed from O\*NET (2015–2024) and BLS Occupational Employment and Wage Statistics (May 2015), covering 873 occupations and 161 skills. The core finding is that cognitive skills diffuse preferentially upward across the wage hierarchy while physical skills diffuse downward, reproducing and reinforcing occupational stratification.

The empirical strategy estimates a complementary log-log model with high-dimensional fixed effects, decomposing the net effect of wage distance on skill adoption into an upward component ($\beta$↑) and a downward component ($\beta$↓), interacted with skill domain (Cognitive / Physical). Three pre-registered robustness tests — a threshold placebo, a domain-label permutation, and a within-stratum permutation — confirm that the asymmetry is not an artifact of the binarization threshold, domain labeling, or confounding by wage quintile.

## Table of Contents

1. [Directory structure](#directory-structure)
2. [Requirements](#requirements)
3. [Data](#data)
4. [Quick start](#quick-start)
5. [Analysis pipeline](#analysis-pipeline)
6. [Script reference](#script-reference)
7. [Citation](#citation)
8. [License and contact](#license-and-contact)

---

## Directory structure

```
skill_diffusion_paper/
├── R/                          # Analysis scripts (numbered pipeline)
│   ├── utils.R                 # Shared utility: extract_coefs()
│   ├── 99_paths_local.R.example  # Template for local path config
│   ├── 00_setup_comun.R        # Session setup (sources utils.R, loads data)
│   ├── 01A/01B_baseline_*.R    # Baseline ATC models (Panels A and B)
│   ├── 02-04[A/B]_test_*.R     # Robustness tests i-iii
│   ├── 05_tablas_y_figuras.R   # All SI figures (S5, S6, S7) and tables
│   ├── 06_*.R                  # Flow networks and subsample stability
│   ├── 07_rca_threshold_sensitivity_FULL.R  # RCA threshold sensitivity
│   ├── 08_nestedness.R         # Nestedness contributions (c_s)
│   ├── 09_*.R                  # Structural models and sub-period analysis
│   ├── 10_nestedness_NHB.R     # Alternative nestedness (disparity filter)
│   ├── 00_confusion.R          # Domain x nestedness confounding diagnostic
│   ├── fig_S2_cs_distribution.R
│   └── fig_S3_S4_skill_profiles.R
├── scripts/                    # Thin wrappers for cache-based rendering
├── paper/                      # Reference PDFs (main + SI)
├── data/                       # Small metadata; large data excluded
├── docs/                       # Data provenance, project structure, checklist
├── images/                     # LaTeX-facing figure folder
├── output_SI_identity/         # Canonical SI outputs (gitignored if empty)
└── output_rca_threshold/       # RCA sensitivity outputs (gitignored)
```

---

## Requirements

- **R >= 4.3**
- Key packages: `data.table`, `fixest`, `vegan`, `ggplot2`, `patchwork`, `future`, `future.apply`, `progressr`, `scales`
- **>= 32 GB RAM** recommended for full estimation (17.3M-row dataset)
- Operating system: Linux or macOS (Windows untested)

Install dependencies:

```r
install.packages(c("data.table", "fixest", "vegan", "ggplot2",
                   "patchwork", "future", "future.apply", "progressr", "scales"))
```

---

## Data

### Sources

| Dataset | Description | Access |
|---------|-------------|--------|
| O\*NET 2015–2024 | Occupational skill importance ratings (continuous) | [onetonline.org](https://www.onetonline.org/help/onet/download) |
| BLS OEWS May 2015 | Occupational mean wages | [bls.gov/oes](https://www.bls.gov/oes/2015/may/oes_nat.htm) |

### Derived data

The analysis consumes two derived datasets that are **not committed to this repository** due to size:

| File | Description | Size |
|------|-------------|------|
| `dt_con_cs_nestedness.rds` | Triadic diffusion dataset (17.3M rows, 873 occupations x 161 skills) with nestedness contributions (c_s) | ~2–4 GB |
| `datos_eventos_v13_MULTITHR/all_events_v13_multithr.rds` | Multi-threshold version for RCA sensitivity (Section S2.4) | ~2–4 GB |

These files should be deposited in a public repository (e.g., Zenodo, Harvard Dataverse, OSF) and cited by DOI in the published paper. See `docs/DATA_PROVENANCE.md` for construction details.

### Local path configuration

Copy the template and edit for your machine:

```bash
cp R/99_paths_local.R.example R/99_paths_local.R
# Then edit R/99_paths_local.R with the actual paths on your system
```

`R/99_paths_local.R` is gitignored and will never be committed.

---

## Quick start

### Path A — From cached SI outputs (no re-estimation)

If the cached RDS objects are available in `output_SI_identity/rds/`:

```bash
Rscript scripts/01_render_SI_from_cache.R
Rscript scripts/02_sync_figures_to_images.R
```

Expected cached files:
- `output_SI_identity/rds/baseline_coefs.rds`
- `output_SI_identity/rds/test_i_threshold.rds`
- `output_SI_identity/rds/test_ii_domain_perm.rds`
- `output_SI_identity/rds/test_iii_stratum_perm.rds`

### Path B — Full re-estimation

1. Place (or symlink) the triadic dataset at the path specified in `R/99_paths_local.R`.
2. Run the estimation pipeline in order (see [Analysis pipeline](#analysis-pipeline) below).
3. Run `Rscript scripts/02_sync_figures_to_images.R` to sync figures for LaTeX.

---

## Analysis pipeline

The numbered scripts in `R/` form a linear pipeline. Each script sources `R/00_setup_comun.R` to load the data and shared utilities.

| Stage | Scripts | Description |
|-------|---------|-------------|
| **1. Data construction** | `00_build_events_v13_multithr.R` | Builds the multi-threshold dyadic dataset |
| **2. Baseline models** | `01A_baseline_panelA.R`, `01B_baseline_panelB.R` | Baseline ATC estimation (Panels A and B) |
| **3. Robustness tests** | `02–04[A/B]_test_*.R` | Tests i (threshold placebo), ii (domain permutation), iii (within-stratum permutation) |
| **4. Figures and tables** | `05_tablas_y_figuras.R` | Generates SI Figs. S5, S6, S7 and all main tables |
| **5. Additional diagnostics** | `06_flow_networks.R`, `06_subsample_stability.R` | Flow network visualization and subsample stability |
| **6. RCA sensitivity** | `07_rca_threshold_sensitivity_FULL.R` | Re-estimates ATC across RCA thresholds {0.90, 1.00, 1.10, 1.25} (Fig. S1) |
| **7. Nestedness** | `08_nestedness.R`, `09_structural_models.R`, `10_nestedness_NHB.R` | Computes c_s contributions; structural models; robustness via disparity filter |

---

## Script reference

| Script | Purpose | Key outputs |
|--------|---------|-------------|
| `R/utils.R` | Canonical `extract_coefs()` function | (sourced by other scripts) |
| `R/00_setup_comun.R` | Session setup: loads data, derives wage variables, samples sources | `dt_sample` in memory |
| `R/01A_baseline_panelA.R` | Baseline model, Panel A (FE: source + skill) | `output_SI_identity/rds/baseline_coefs.rds` |
| `R/01B_baseline_panelB.R` | Baseline model, Panel B (FE: target + skill) | (appended to above) |
| `R/02–04[A/B]_test_*.R` | Robustness tests i–iii | `test_i_threshold.rds`, `test_ii_domain_perm.rds`, `test_iii_stratum_perm.rds` |
| `R/05_tablas_y_figuras.R` | All SI figures (S5–S7) and tables | `output_SI_identity/figs/`, `output_SI_identity/tables/` |
| `R/07_rca_threshold_sensitivity_FULL.R` | RCA threshold sensitivity ({0.90, 1.00, 1.10, 1.25}) | `output_rca_threshold/` |
| `R/08_nestedness.R` | Skill nestedness contributions (c_s) via column permutation (R=500) | `dt_con_cs_nestedness.rds` |
| `R/09_subperiod_analysis_table_S2.R` | Sub-period analysis (2015–2018, 2019–2021, 2022–2024) | `subperiod_results/` |
| `R/fig_S2_cs_distribution.R` | SI Fig. S2: density of c_s by domain | `fig_s2_cs_distribution.png` |
| `R/fig_S3_S4_skill_profiles.R` | SI Figs. S3–S4: skill-level wage-quintile shift profiles | `fig_s3_*.png`, `fig_s4_*.png` |
| `R/00_confusion.R` | Diagnostic: domain x nestedness confounding check | (console output) |
| `R/10_nestedness_NHB.R` | Alternative nestedness via disparity-filter binarization | (robustness check) |
| `R/fig_SI_test_ii_FINAL.R` | **DEPRECATED** — superseded by `05_tablas_y_figuras.R` | — |

---

## Citation

```bibtex
@article{cantillan_bucca_2026,
  author  = {Cantillan, Roberto and Bucca, Mauricio},
  title   = {Asymmetric directional diffusion of skill requirements reproduces
             occupational stratification},
  journal = {Science Advances},
  year    = {2026},
  note    = {Under review}
}
```

---

## License and contact

This repository is released under the [MIT License](LICENSE).

Maintainer: Roberto Cantillan (Pontificia Universidad Católica de Chile) — in collaboration with Mauricio Bucca.
