# Replication Package: Asymmetric Directional Diffusion of Skill Requirements Reproduces Occupational Stratification

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R ≥ 4.3](https://img.shields.io/badge/R-%E2%89%A54.3-blue.svg)](https://cran.r-project.org/)
[![arXiv](https://img.shields.io/badge/arXiv-preprint-b31b1b.svg)](https://arxiv.org/abs/2602.21369)

**Authors:** Roberto Cantillan and Mauricio Bucca  
**Affiliation:** Department of Sociology, Pontificia Universidad Católica de Chile

---

## Overview

This repository provides the data, code, and materials needed to replicate all analyses, figures, and tables in:

> Cantillan, R. and Bucca, M. (2026). "Asymmetric directional diffusion of skill requirements reproduces occupational stratification." *Working paper.* arXiv: [2602.21369](https://arxiv.org/abs/2602.21369)

The study applies the Asymmetric Trajectory Channeling (ATC) framework to a triadic dataset of 17.3 million skill-diffusion opportunities constructed from O\*NET (2015–2024) and BLS Occupational Employment and Wage Statistics (May 2015), covering 873 occupations and 161 skills.

---

## Repository structure

```
├── R/
│   ├── 00_setup_comun.R              # Session setup: loads data, derives variables, draws sample
│   ├── 00_build_dyads.R              # Construct triadic dataset (v12)
│   ├── 00_build_events_v13_multithr.R # Multi-threshold dataset (v13, for RCA sensitivity)
│   ├── utils.R                       # Shared helper functions (extract_coefs)
│   ├── 99_paths_local_TEMPLATE.R     # Template for local path configuration
│   │
│   ├── main/                         # Main-text analyses and figures
│   │   ├── 01_descriptives_flow_networks.R
│   │   ├── 02_nestedness.R
│   │   ├── 03_structural_models.R
│   │   └── 04_structural_models_nestedness.R
│   │
│   └── SI/                           # Supplementary Information analyses
│       ├── 01A_baseline_panelA.R     # Baseline Panel A (source + skill FE)
│       ├── 01B_baseline_panelB.R     # Baseline Panel B (target + skill FE)
│       ├── 02A_test_i_panelA.R       # Threshold placebo, Panel A
│       ├── 02B_test_i_panelB.R       # Threshold placebo, Panel B
│       ├── 03A_test_ii_panelA.R      # Domain-label permutation, Panel A
│       ├── 03B_test_ii_panelB.R      # Domain-label permutation, Panel B
│       ├── 04A_test_iii_panelA.R     # Within-stratum permutation, Panel A
│       ├── 04B_test_iii_panelB.R     # Within-stratum permutation, Panel B
│       ├── 05_tablas_y_figuras.R     # SI figures and tables (Tests i–iii)
│       ├── 06_subsample_stability.R  # Subsample stability (Table S3)
│       ├── 07_rca_threshold_sensitivity_FULL.R # RCA threshold sensitivity
│       ├── 08_nestedness.R           # Nestedness computation (NODF, c_s)
│       ├── 09_subperiod_analysis_table_S2.R # Temporal stability (Table S2)
│       ├── 09b_print_table_S2.R      # Print Table S2 from cached results
│       ├── 10_nestedness_NHB.R       # Alternative binarization robustness
│       ├── S8_physical_HN_archetype.R # Physical_HN as separate archetype
│       ├── fig_S2_cs_distribution.R  # Distribution of c_s by domain (Fig. S2)
│       └── fig_S3_S4_skill_profiles.R # Skill-level directional profiles
│
├── data/
│   ├── README.md                     # Data layout and archival instructions
│   ├── raw/                          # Public raw data (not tracked)
│   ├── derived/                      # Derived datasets (not tracked)
│   └── metadata/                     # Checksums and version info
│
├── output/                           # Generated outputs (gitignored except .gitkeep)
│   ├── output_main/                  # Main-text figures, tables, models
│   │   ├── descriptives/
│   │   ├── figs/
│   │   ├── flow_networks/
│   │   ├── logs/
│   │   ├── models/
│   │   └── tables/
│   ├── output_SI_identity/           # SI figures, tables, and cached .rds
│   │   ├── figs/
│   │   ├── logs/
│   │   ├── models/
│   │   ├── rds/
│   │   └── tables/
│   ├── output_rca_threshold/         # RCA sensitivity outputs
│   │   ├── figs/
│   │   ├── rds/
│   │   └── tables/
│   └── subperiod_results/            # Temporal stability outputs
│
├── images/                           # LaTeX-facing figure folder
│   └── .gitkeep
│
├── docs/
│   ├── DATA_PROVENANCE.md            # Data sources and construction steps
│   ├── PROJECT_STRUCTURE.md          # Directory layout documentation
│   └── REPRODUCIBILITY_CHECKLIST.md  # Pre-submission checklist
│
├── Makefile                          # Build automation (check, si_from_cache, sync)
├── LICENSE                           # MIT License
├── CITATION.cff                      # Citation metadata
├── renv.lock                         # R package versions (reproducibility)
└── README.md                         # This file
```

---

## Data

### Public sources

| Dataset | Version | Access |
|---------|---------|--------|
| O\*NET Database | Releases 15.1, 23.1, 24.1, 26.1, 27.1, 29.2 (2010–2025) | [onetcenter.org](https://www.onetcenter.org/db_releases.html) |
| BLS OEWS | May 2015 national estimates | [bls.gov](https://www.bls.gov/oes/2015/may/oes_nat.htm) |
| BLS OEWS | May 2024 national estimates | [bls.gov](https://www.bls.gov/oes/2024/may/oes_nat.htm) |

**Note:** The main analysis uses O\*NET releases 15.1 through 29.2 covering 2015–2024. Releases 15.1 (2010) is retained for out-of-sample validation only. See `docs/DATA_PROVENANCE.md` for exact version identifiers and construction details.

### Derived data

The triadic diffusion dataset (`dt_con_cs_nestedness.rds`, ~17.3M rows, ~2–4 GB) will be archived at:

> **[Zenodo/Dataverse/OSF]** — DOI: `[TO BE INSERTED UPON ACCEPTANCE]`

Place the downloaded file in the repository root (or configure the path in `R/99_paths_local.R`). See `docs/DATA_PROVENANCE.md` for construction details.

---

## Requirements

- **R ≥ 4.3**
- **≥ 32 GB RAM** (full estimation on 17.3M-row dataset)
- Linux or macOS recommended (Windows untested)

### R packages

Package versions are pinned in `renv.lock`. To restore the exact environment:

```r
# install.packages("renv")
renv::restore()
```

Alternatively, install manually:

```r
install.packages(c(
  "data.table", "fixest", "vegan",
  "ggplot2", "patchwork", "scales",
  "future", "future.apply", "progressr"
))
```

---

## Reproducing the results

### Quick start (from cached outputs)

If you have the cached `.rds` objects in `output_SI_identity/rds/`:

```bash
make si_from_cache   # Regenerate SI figures and tables
make sync            # Copy figures to images/ for LaTeX
```

### Full pipeline

The analysis scripts are designed to run one model per R session (due to memory constraints with ~17M rows). See `R/00_setup_comun.R` for the shared session setup.

**Data construction:**
```bash
Rscript R/00_build_dyads.R               # Build triadic dataset (v12)
Rscript R/00_build_events_v13_multithr.R  # Build multi-threshold dataset (v13)
```

**Main analyses** (in `R/main/`): run interactively after loading data.

**SI analyses** (in `R/SI/`): run each script in a clean R session. Order:
1. `01A` → `01B` (baseline Panels A & B)
2. `02A` → `02B` (threshold placebo, Test i)
3. `03A` → `03B` (domain-label permutation, Test ii)
4. `04A` → `04B` (within-stratum permutation, Test iii)
5. `05_tablas_y_figuras.R` (generate all SI figures from cached results)
6. `06` → `07` → `08` → `09` (subsample stability, RCA sensitivity, nestedness, sub-periods)
7. `10`, `S8`, `fig_S2`, `fig_S3_S4` (additional robustness and figures)

---

## Correspondence between scripts and paper

### Main text

| Figure/Table | Description | Script |
|--------------|-------------|--------|
| Figure 1 | Directional skill flow networks between wage quintiles | `R/main/01_descriptives_flow_networks.R` |
| Figure 2 | ATC coefficients by skill domain | `R/main/03_structural_models.R` |
| Figure 3 | Nestedness amplifies ATC | `R/main/04_structural_models_nestedness.R` |

### Supplementary Information

| Figure/Table | Description | Script |
|--------------|-------------|--------|
| Fig. S1 | RCA threshold sensitivity | `R/SI/07_rca_threshold_sensitivity_FULL.R` |
| Fig. S2 | Distribution of c_s by domain | `R/SI/fig_S2_cs_distribution.R` |
| Figs. S3–S4 | Skill-level directional profiles | `R/SI/fig_S3_S4_skill_profiles.R` |
| Figs. S5–S7 | Robustness visualizations (Tests i–iii) | `R/SI/05_tablas_y_figuras.R` |
| Table S1 | Full robustness battery (Tests i–iii) | `R/SI/05_tablas_y_figuras.R` |
| Table S2 | Temporal stability (sub-periods) | `R/SI/09_subperiod_analysis_table_S2.R` |
| Table S3 | Subsample stability | `R/SI/06_subsample_stability.R` |
| Table S4 | Physical_HN as separate archetype | `R/SI/S8_physical_HN_archetype.R` |
| Table S5 | Alternative nestedness binarization | `R/SI/10_nestedness_NHB.R` |

**Test-level scripts** (estimation only — results consumed by `05_tablas_y_figuras.R`):

| Test | Description | Scripts |
|------|-------------|---------|
| Baseline | Reference model, Panels A & B | `01A_baseline_panelA.R`, `01B_baseline_panelB.R` |
| Test i | Threshold placebo | `02A_test_i_panelA.R`, `02B_test_i_panelB.R` |
| Test ii | Domain-label permutation | `03A_test_ii_panelA.R`, `03B_test_ii_panelB.R` |
| Test iii | Within-stratum permutation | `04A_test_iii_panelA.R`, `04B_test_iii_panelB.R` |

---

## Citation

```bibtex
@article{cantillan_bucca_2026,
  author  = {Cantillan, Roberto and Bucca, Mauricio},
  title   = {Asymmetric directional diffusion of skill requirements
             reproduces occupational stratification},
  year    = {2026},
  note    = {Working paper}
}
```

---

## License

This project is licensed under the [MIT License](LICENSE).

---

## Contact

Roberto Cantillan — Pontificia Universidad Católica de Chile  
Mauricio Bucca — Pontificia Universidad Católica de Chile