.PHONY: check si_from_cache sync

check:
	Rscript scripts/03_project_check.R

si_from_cache:
	Rscript scripts/01_render_SI_from_cache.R

sync:
	Rscript scripts/02_sync_figures_to_images.R
