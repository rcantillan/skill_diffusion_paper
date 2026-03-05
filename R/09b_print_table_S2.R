# =============================================================================
# 09b_print_table_S2.R
# Convenience script: print Table S2 coefficients from subperiod_results
# =============================================================================

library(data.table)

results <- readRDS("subperiod_results/table_S2_all_panels.rds")
setDT(results)

cat("\n=== TABLE S2 — Panel A ===\n")
print(results[panel == "Panel_A", .(
  period, archetype, direction,
  estimate  = round(estimate,  3),
  std.error = round(std.error, 3),
  conf.low  = round(conf.low,  3),
  conf.high = round(conf.high, 3),
  p.value   = round(p.value,   4)
)][order(period, archetype, direction)])

cat("\n=== TABLE S2 — Panel B ===\n")
print(results[panel == "Panel_B", .(
  period, archetype, direction,
  estimate  = round(estimate,  3),
  std.error = round(std.error, 3),
  conf.low  = round(conf.low,  3),
  conf.high = round(conf.high, 3),
  p.value   = round(p.value,   4)
)][order(period, archetype, direction)])
