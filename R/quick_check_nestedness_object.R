# =============================================================================
# quick_check_nestedness_object.R
# Sanity check: inspect dt_con_cs_nestedness.rds
# =============================================================================

library(data.table)

dt_nest <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt_nest)

print(names(dt_nest))
print(head(dt_nest, 3))
