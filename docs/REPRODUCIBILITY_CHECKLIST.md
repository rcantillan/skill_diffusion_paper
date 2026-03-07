# Reproducibility checklist

## Environment
- [ ] `renv.lock` included and tested on a clean machine
- [ ] `docs/sessionInfo.txt` captured on the machine used for final figures (run `make session_info`)

## Data provenance
- [ ] Exact O*NET release versions documented in `docs/DATA_PROVENANCE.md`
  - 15.1 (2010), 23.1 (2018), 24.1 (2019), 26.1 (2021), 27.1 (2022), 29.2 (2025)
- [ ] BLS OEWS May 2015 and May 2024 URLs recorded
- [ ] Crosswalk version documented
- [ ] External archive DOI for `dt_con_cs_nestedness.rds` (Zenodo/Dataverse/OSF)
- [ ] SHA256 checksum of `dt_con_cs_nestedness.rds` recorded in `data/metadata/`

## Reproducibility
- [ ] `make check` passes (repo skeleton consistent)
- [ ] `make si_from_cache` regenerates SI figs/tables from cached outputs
- [ ] `make sync` makes LaTeX compilation path-stable
- [ ] Full pipeline tested: `00_build_dyads.R` → main scripts → SI scripts
- [ ] All `[TO BE INSERTED]` placeholders filled before submission