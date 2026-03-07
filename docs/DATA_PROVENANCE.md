# Data provenance

## O*NET

- **Source:** O*NET Database (U.S. Department of Labor / O*NET Resource Center)
- **URL:** <https://www.onetcenter.org/db_releases.html>
- **Releases used:**

| Folder name  | O*NET version | Release date    | Usage               |
|-------------|---------------|-----------------|----------------------|
| `db_15_1`   | 15.1          | November 2010   | Out-of-sample only   |
| `db_23_1`   | 23.1          | November 2018   | Main analysis (2015–2024 window) |
| `db_24_1`   | 24.1          | November 2019   | Main analysis        |
| `db_26_1`   | 26.1          | November 2021   | Main analysis        |
| `db_27_1`   | 27.1          | November 2022   | Main analysis        |
| `db_29_2`   | 29.2          | February 2025   | Main analysis (endline) |

- **Skill content tables used:** `Skills.txt`, `Abilities.txt`, `Knowledge.txt`, `Work Activities.txt`, `Work Styles.txt`
- **Occupation metadata:** `Occupation Data.txt`
- **Extraction rule:** All occupations "actively updated" within each release year (see SI for details)

## Wages

- **Source:** BLS Occupational Employment and Wage Statistics (OEWS)
- **Baseline wages:** May 2015 national estimates (`national_M2015_dl.xlsx`)
  - URL: <https://www.bls.gov/oes/2015/may/oes_nat.htm>
- **Endline wages:** May 2024 national estimates (`national_M2024_dl.xlsx`)
  - URL: <https://www.bls.gov/oes/2024/may/oes_nat.htm>
- **Crosswalk:** `crosswalk/` folder — 2018 SOC crosswalk to O*NET-SOC occupations

## Derived dataset

- **File:** `dt_con_cs_nestedness.rds` (triadic risk set + covariates)
- **Row count:** ~17.3 million
- **Stored:** External archive (Zenodo/Dataverse/OSF)
- **DOI:** [TO BE INSERTED UPON ACCEPTANCE]
- **SHA256:** [TO BE COMPUTED: `sha256sum dt_con_cs_nestedness.rds`]

## Earlier version

An earlier version of this paper is available as an arXiv preprint:

- **arXiv:** [2602.21369](https://arxiv.org/abs/2602.21369)