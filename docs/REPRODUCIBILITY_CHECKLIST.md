# Reproducibility checklist

- [ ] `renv.lock` (or container spec) included and tested
- [ ] Exact O*NET and OEWS versions documented (DATA_PROVENANCE)
- [ ] External archive DOI for the large derived dataset
- [ ] Checksums recorded
- [ ] `make si_from_cache` regenerates SI figs/tables from cached outputs
- [ ] `make sync` makes LaTeX compilation path-stable
- [ ] `docs/sessionInfo.txt` captured on the machine used for final figures