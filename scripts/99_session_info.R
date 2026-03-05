# Capture session info for reproducibility.
writeLines(capture.output(sessionInfo()), con = file.path("docs","sessionInfo.txt"))
message("Wrote docs/sessionInfo.txt")
