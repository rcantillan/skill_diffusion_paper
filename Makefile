.PHONY: check si_from_cache sync session_info

check:
	@Rscript -e 'stopifnot(dir.exists("R/main"), dir.exists("R/SI"), dir.exists("output")); message("OK: repo skeleton consistent.")'

si_from_cache:
	Rscript R/SI/05_tablas_y_figuras.R

sync:
	@echo "Sync figures from output/ to images/ for LaTeX"
	Rscript -e 'fs <- list.files("output", pattern = "\\.(png|pdf)$$", recursive = TRUE, full.names = TRUE); file.copy(fs, file.path("images", basename(fs)), overwrite = TRUE); message(length(fs), " files synced.")'

session_info:
	Rscript -e 'writeLines(capture.output(sessionInfo()), "docs/sessionInfo.txt")'
