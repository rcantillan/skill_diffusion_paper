# Sync final figures into ./images/ so LaTeX compiles without path edits.

from_si  <- file.path("output_SI_identity", "figs")
from_main <- file.path("output_main", "figs")

if(!dir.exists("images")) dir.create("images", recursive = TRUE)
if(!dir.exists(file.path("images","si"))) dir.create(file.path("images","si"), recursive = TRUE)
if(!dir.exists(file.path("images","main"))) dir.create(file.path("images","main"), recursive = TRUE)

copy_pngs <- function(src_dir, dst_dir){
  if(!dir.exists(src_dir)) return(invisible(FALSE))
  f <- list.files(src_dir, pattern = "\\.(png|pdf)$", full.names = TRUE, ignore.case = TRUE)
  if(length(f) == 0L) return(invisible(FALSE))
  file.copy(f, dst_dir, overwrite = TRUE)
  invisible(TRUE)
}

# Copy into organized subfolders
copy_pngs(from_si,   file.path("images","si"))
copy_pngs(from_main, file.path("images","main"))

# Also copy into flat ./images/ (keeps your current \includegraphics{images/...} paths working)
copy_pngs(from_si,   "images")
copy_pngs(from_main, "images")

message("Synced figures to ./images (flat) and ./images/{main,si}.")
