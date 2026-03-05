# Replication Package: Asymmetric Directional Diffusion of Skill Requirements Reproduces Occupational Stratification

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R ≥ 4.3](https://img.shields.io/badge/R-%E2%89%A54.3-blue.svg)](https://cran.r-project.org/)

**Authors:** Roberto Cantillan and Mauricio Bucca  
**Affiliation:** Department of Sociology, Pontificia Universidad Católica de Chile

---

## Overview

This repository provides the data, code, and materials needed to replicate all analyses, figures, and tables in:

> Cantillan, R. and Bucca, M. (2026). "Asymmetric directional diffusion of skill requirements reproduces occupational stratification." *Working paper.*

The study applies the Asymmetric Trajectory Channeling (ATC) framework to a triadic dataset of 17.3 million skill-diffusion opportunities constructed from O\*NET (2015–2024) and BLS Occupational Employment and Wage Statistics (May 2015), covering 873 occupations and 161 skills.

---

## Repository structure

```
├── code/                           # All analysis scripts (run in order)
│   ├── 00_setup.R                  # Packages, paths, shared functions
│   ├── 01_build_data.R             # Construct triadic dataset
│   ├── 02_baseline_models.R        # ATC baseline estimation (Panels A & B)
│   ├── 03_robustness_tests.R       # Robustness battery (Tests i–v)
│   ├── 04_nestedness.R             # NODF and c_s contributions
│   ├── 05_structural_models.R      # Structural distance models
│   ├── 06_subperiod_analysis.R     # Temporal stability (Table S2)
│   ├── 07_figures_main.R           # All main-text figures (Figs. 1–4)
│   ├── 08_figures_si.R             # All SI figures (Figs. S1–S7)
│   ├── 09_tables.R                 # All tables (main + SI: Tables S1–S4)
│   └── utils.R                     # Helper functions (extract_coefs, etc.)
│
├── data/
│   ├── raw/                        # Public source data (or download instructions)
│   │   └── README.md               # O*NET and BLS access URLs and versions
│   └── derived/                    # Derived analytical dataset
│       └── README.md               # DOI, checksums, construction notes
│
├── output/
│   ├── figures/
│   │   ├── main/                   # Figs. 1–4 (main paper)
│   │   └── si/                     # Figs. S1–S7 (Supplementary Information)
│   ├── tables/
│   │   ├── main/                   # Main-text tables
│   │   └── si/                     # Tables S1–S4 (Supplementary Information)
│   └── rds/                        # Cached intermediate objects
│
├── paper/
│   ├── main.tex                    # Main manuscript source
│   ├── si.tex                      # Supplementary Information source
│   └── references.bib              # Bibliography
│
├── docs/
│   ├── DATA_PROVENANCE.md          # Full data provenance documentation
│   ├── SESSION_INFO.txt            # R session info from final run
│   └── CODEBOOK.md                 # Variable descriptions for derived data
│
├── Makefile                        # Build automation
├── LICENSE                         # MIT License
├── CITATION.cff                    # Citation metadata
└── README.md                       # This file
```

---

## Data

### Public sources

| Dataset | Version | Access |
|---------|---------|--------|
| O\*NET Database | Releases 20.3–29.2 (2015–2024) | [onetonline.org/help/onet/download](https://www.onetonline.org/help/onet/download) |
| BLS OEWS | May 2015 national estimates | [bls.gov/oes/2015/may/oes_nat.htm](https://www.bls.gov/oes/2015/may/oes_nat.htm) |

### Derived data

The triadic diffusion dataset (`dt_con_cs_nestedness.rds`, ~17.3M rows, ~2–4 GB) is archived at:

> **[Zenodo/Dataverse/OSF]** — DOI: `[TO BE INSERTED]`

Place the downloaded file in `data/derived/` before running the pipeline. See `data/derived/README.md` for SHA-256 checksums and construction details.

---

## Requirements

- **R ≥ 4.3**
- **≥ 32 GB RAM** (full estimation on 17.3M-row dataset)
- Linux or macOS recommended (Windows untested)

### R packages

```r
install.packages(c(
  "data.table", "fixest", "vegan",
  "ggplot2", "patchwork", "scales",
  "future", "future.apply", "progressr"
))
```

---

## Reproducing the results

### Option A: Full pipeline

Run all scripts in numbered order:

```bash
# From the repository root:
Rscript code/00_setup.R
Rscript code/01_build_data.R
Rscript code/02_baseline_models.R
Rscript code/03_robustness_tests.R
Rscript code/04_nestedness.R
Rscript code/05_structural_models.R
Rscript code/06_subperiod_analysis.R
Rscript code/07_figures_main.R
Rscript code/08_figures_si.R
Rscript code/09_tables.R
```

Or using Make:

```bash
make all
```

### Option B: From cached outputs (no re-estimation)

If you have the cached `.rds` objects in `output/rds/`:

```bash
Rscript code/07_figures_main.R
Rscript code/08_figures_si.R
Rscript code/09_tables.R
```

---

## Script descriptions

| Script | Description | Outputs |
|--------|-------------|---------|
| `00_setup.R` | Loads packages, sets paths, sources `utils.R` | — |
| `01_build_data.R` | Constructs the triadic risk set from O\*NET and BLS | `data/derived/dt_con_cs_nestedness.rds` |
| `02_baseline_models.R` | Estimates ATC gravity model, Panels A and B | `output/rds/baseline_coefs.rds` |
| `03_robustness_tests.R` | Runs Tests i–v: threshold placebo, domain permutation, within-stratum permutation, RCA sensitivity, subsample stability | `output/rds/test_*.rds` |
| `04_nestedness.R` | Computes NODF and skill-level $c_s$ contributions | `output/rds/nestedness_cs.rds` |
| `05_structural_models.R` | Estimates models with alternative distance measures | `output/rds/structural_*.rds` |
| `06_subperiod_analysis.R` | Re-estimates ATC over three sub-periods (2015–18, 2019–21, 2022–24) | `output/rds/subperiod_*.rds`, `output/tables/si/table_S2.tex` |
| `07_figures_main.R` | Generates Figures 1–4 (main text) | `output/figures/main/fig_01–04.*` |
| `08_figures_si.R` | Generates Figures S1–S7 (SI) | `output/figures/si/fig_S1–S7.*` |
| `09_tables.R` | Generates Tables S1–S4 (SI) | `output/tables/si/table_S1–S4.tex` |
| `utils.R` | Shared helper functions (`extract_coefs()`, plotting themes) | (sourced by other scripts) |

---

## Correspondence between scripts and paper

### Main text

| Figure/Table | Description | Script |
|--------------|-------------|--------|
| Figure 1 | Raw adoption rates by distance and wage gap | `07_figures_main.R` |
| Figure 2 | Directional skill flows between wage quintiles | `07_figures_main.R` |
| Figure 3 | ATC coefficients by skill domain | `07_figures_main.R` |
| Figure 4 | Nestedness amplifies ATC | `07_figures_main.R` |
| Table 1 | Domain–nestedness typology | (in manuscript) |

### Supplementary Information

| Figure/Table | Description | Script |
|--------------|-------------|--------|
| Fig. S1 | RCA threshold sensitivity | `08_figures_si.R` |
| Fig. S2 | Distribution of $c_s$ by domain | `08_figures_si.R` |
| Figs. S3–S4 | Skill-level directional profiles | `08_figures_si.R` |
| Figs. S5–S7 | Robustness visualizations (Tests i–iii) | `08_figures_si.R` |
| Table S1 | Full robustness battery | `09_tables.R` |
| Table S2 | Temporal stability | `09_tables.R` |
| Table S3 | Subsample stability | `09_tables.R` |
| Table S4 | Physical\_HN as separate archetype | `09_tables.R` |

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