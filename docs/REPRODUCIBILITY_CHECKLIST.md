# Reproducibility checklist (Science Advances packaging)

- [ ] `renv.lock` (or container spec) included and tested
- [ ] Exact O*NET and OEWS versions documented (DATA_PROVENANCE)
- [ ] External archive DOI for the large derived dataset
- [ ] Checksums recorded
- [ ] `scripts/01_render_SI_from_cache.R` regenerates SI figs/tables from cached outputs
- [ ] `scripts/02_sync_figures_to_images.R` makes LaTeX compilation path-stable
- [ ] `docs/sessionInfo.txt` captured on the machine used for final figures
