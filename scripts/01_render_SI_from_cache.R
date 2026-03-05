# Render SI figures/tables *without re-estimating*.
# Assumes cached objects exist in output_SI_identity/rds/.

required <- c(
  "baseline_coefs.rds",
  "test_i_threshold.rds",
  "test_ii_domain_perm.rds",
  "test_iii_stratum_perm.rds"
)
missing <- required[!file.exists(file.path("output_SI_identity","rds", required))]
if(length(missing) > 0L){
  stop(
    "Missing cached RDS in output_SI_identity/rds/: ", paste(missing, collapse=", "), "\n",
    "Copy your existing cached outputs into output_SI_identity/rds/ and re-run."
  )
}

# This script writes tables (.csv) and figures (.png/.pdf) into output_SI_identity/
source(file.path("R","05_tablas_y_figuras.R"))

message("Done. SI artifacts written to output_SI_identity/{figs,tables}.")
