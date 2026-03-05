# Lightweight checks: directory layout + presence of key inputs.

stopifnot(dir.exists("R"), dir.exists("scripts"), dir.exists("output_SI_identity"))

# Key large input expected by estimation scripts
if(!file.exists("dt_con_cs_nestedness.rds")){
  message("NOTE: dt_con_cs_nestedness.rds not found at repo root.\n",
          "      This is expected if you keep large data outside git.\n",
          "      Place a symlink or copy at ./dt_con_cs_nestedness.rds when you need to run estimation.")
}

message("OK: repo skeleton looks consistent.")
