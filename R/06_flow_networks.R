# ==============================================================================
# FLOW NETWORKS COMPLETOS - BASADOS EN TASAS DE ADOPCIÓN
# ==============================================================================

library(ggraph)
library(igraph)
library(dplyr)
library(tidyr)
library(tibble)
library(patchwork)
library(stringr)
library(ggplot2)

# ==============================================================================
# CONFIGURACIÓN GLOBAL
# ==============================================================================

verts <- tibble(name = paste0("Q", 1:5), x = 1:5, y = 0)
pal_direction <- c(Down = "#B2182B", Up = "#2166AC")

# Flecha triangular definida
arrow_sharp <- arrow(
  angle = 10,
  length = unit(7, "mm"),
  type = "closed"
)

gc()

# ==============================================================================
# CALCULAR TASAS POR EDGE
# ==============================================================================

rate_by_edge <- dt %>%
  as.data.frame() %>%
  filter(! is.na(s_wage), !is.na(t_wage)) %>%
  mutate(
    wage_q_src = suppressWarnings(ggplot2::cut_number(s_wage, n = 5, labels = paste0("Q", 1:5))),
    wage_q_dest = suppressWarnings(ggplot2::cut_number(t_wage, n = 5, labels = paste0("Q", 1:5))),
    q_src = as.integer(str_remove(wage_q_src, "Q")),
    q_dst = as.integer(str_remove(wage_q_dest, "Q")),
    direction = case_when(
      q_dst > q_src ~ "Up",
      q_dst < q_src ~ "Down",
      TRUE ~ "Lateral"
    )
  ) %>%
  filter(direction != "Lateral") %>%
  group_by(domain, wage_q_src, wage_q_dest, direction) %>%
  summarise(
    n_opp = n(),
    n_adopt = sum(diffusion == 1),
    rate = mean(diffusion == 1),
    .groups = "drop"
  )
gc()

# Normalizar tasas
rate_by_edge <- rate_by_edge %>%
  group_by(domain) %>%
  mutate(rate_norm = rate / max(rate)) %>%
  ungroup()

# ==============================================================================
# FIGURA 1: FLOW NETWORKS AGREGADOS (COGNITIVE VS PHYSICAL)
# ==============================================================================

message(">>> Generando Flow Networks Agregados <<<")

# COGNITIVE
flows_cog_rate <- rate_by_edge %>%
  filter(domain == "Cognitive") %>%
  arrange(desc(rate)) %>%
  slice_head(n = 8)

g_cog <- graph_from_data_frame(
  d = transmute(flows_cog_rate, from = wage_q_src, to = wage_q_dest, 
                weight = rate_norm, direction = direction),
  vertices = verts, directed = TRUE
)

p_cog_rate <- ggraph(g_cog, layout = "linear") +
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
    strength = 0.3,
    arrow = arrow_sharp,
    end_cap = circle(9, "mm"),
    linejoin = "mitre",
    lineend = "butt",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(1, 10), guide = "none") +
  scale_edge_alpha(range = c(0.5, 1), guide = "none") +
  scale_edge_colour_manual(values = pal_direction, name = "Direction") +
  geom_node_point(size = 18, color = "black", fill = "white", shape = 21, stroke = 1.5) +
  geom_node_text(aes(label = name), vjust = 0.4, size = 7, color = "black", fontface = "bold") +
  coord_cartesian(clip = "off") +
  labs(
    title = "A. Cognitive Skills",
    subtitle = "Upward adoption rate: 20. 5% | Downward: 14.9%",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#2166AC", face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# PHYSICAL
flows_phy_rate <- rate_by_edge %>%
  filter(domain == "Physical") %>%
  arrange(desc(rate)) %>%
  slice_head(n = 8)

g_phy <- graph_from_data_frame(
  d = transmute(flows_phy_rate, from = wage_q_src, to = wage_q_dest, 
                weight = rate_norm, direction = direction),
  vertices = verts, directed = TRUE
)

p_phy_rate <- ggraph(g_phy, layout = "linear") +
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
    strength = 0.3,
    arrow = arrow_sharp,
    end_cap = circle(9, "mm"),
    linejoin = "mitre",
    lineend = "butt",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(1, 10), guide = "none") +
  scale_edge_alpha(range = c(0.5, 1), guide = "none") +
  scale_edge_colour_manual(values = pal_direction, name = "Direction") +
  geom_node_point(size = 18, color = "black", fill = "white", shape = 21, stroke = 1.5) +
  geom_node_text(aes(label = name), vjust = 0.4, size = 7, color = "black", fontface = "bold") +
  coord_cartesian(clip = "off") +
  labs(
    title = "B. Physical Skills",
    subtitle = "Downward adoption rate: 19.8% | Upward: 10.4%",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#B2182B", face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Leyenda
p_legend <- ggplot() +
  annotate("segment", x = 0.5, xend = 1.2, y = 0.5, yend = 0.5,
           color = "#2166AC", linewidth = 4,
           arrow = arrow(angle = 10, length = unit(7, "mm"), type = "closed")) +
  annotate("text", x = 1.4, y = 0.5, label = "Upward", 
           color = "#2166AC", size = 8, fontface = "bold", hjust = 0) +
  annotate("segment", x = 3, xend = 3.7, y = 0.5, yend = 0.5,
           color = "#B2182B", linewidth = 4,
           arrow = arrow(angle = 10, length = unit(7, "mm"), type = "closed")) +
  annotate("text", x = 3.9, y = 0.5, label = "Downward", 
           color = "#B2182B", size = 8, fontface = "bold", hjust = 0) +
  xlim(0, 5.5) +
  ylim(0, 1) +
  theme_void()

# Combinar
p_combined_rate <- (p_cog_rate | p_phy_rate) / p_legend +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    #title = "Asymmetric Trajectory Channeling: Adoption Rates by Direction",
    subtitle = "Edge thickness proportional to adoption rate | Arrows indicate diffusion direction",
    caption = "Nodes = wage quintiles (Q1 low → Q5 high) | Physical skills face 2x upward friction",
    theme = theme(
      plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5, color = "grey30"),
      plot.caption = element_text(size = 16, hjust = 0.5, color = "grey50")
    )
  )

print(p_combined_rate)

#ggsave(file.path(output_data_dir, "flow_net_aggregated. png"),
#       p_combined_rate, width = 18, height = 10, dpi = 300)





# ==============================================================================
# RC28: EMPIRICAL FLOW NETWORKS - FINAL LAYOUT (FIXED CAPTION)
# ==============================================================================

library(ggraph)
library(igraph)
library(data.table)
library(dplyr)
library(stringr)
library(patchwork)
library(ggplot2)

# 1. PROCESAMIENTO DE DATOS (17.3M UNIVERSO)
# ------------------------------------------------------------------------------
q_breaks <- quantile(dt$s_wage, probs = seq(0, 1, 0.2), na.rm = TRUE)

dt[, `:=`(
  wage_q_src = cut(s_wage, breaks = q_breaks, labels = paste0("Q", 1:5), include.lowest = TRUE),
  wage_q_dest = cut(t_wage, breaks = q_breaks, labels = paste0("Q", 1:5), include.lowest = TRUE)
)]

dt[, direction := fcase(
  as.integer(wage_q_dest) > as.integer(wage_q_src), "Up",
  as.integer(wage_q_dest) < as.integer(wage_q_src), "Down",
  default = "Lateral"
)]

# 2. CÁLCULO DE ESTADÍSTICAS EMPÍRICAS
# ------------------------------------------------------------------------------
stats_empiricas <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE)
), by = .(domain, direction)]

text_cog <- sprintf("Upward adoption rate: %.1f%% | Downward: %.1f%%",
                    stats_empiricas[domain == "Cognitive" & direction == "Up", rate] * 100,
                    stats_empiricas[domain == "Cognitive" & direction == "Down", rate] * 100)

text_phy <- sprintf("Downward adoption rate: %.1f%% | Upward: %.1f%%",
                    stats_empiricas[domain == "Physical" & direction == "Down", rate] * 100,
                    stats_empiricas[domain == "Physical" & direction == "Up", rate] * 100)

# 3. DEFINICIÓN DE LEYENDA
# ------------------------------------------------------------------------------
p_legend_rc28 <- ggplot() +
  annotate("segment", x = 1, xend = 1.8, y = 0.5, yend = 0.5, color = "#2166AC", 
           linewidth = 4, arrow = arrow(angle = 10, length = unit(6, "mm"), type = "closed")) +
  annotate("text", x = 2, y = 0.5, label = "Upward diffusion", color = "#2166AC", 
           size = 8, fontface = "bold", hjust = 0, family = "Inconsolata") +
  annotate("segment", x = 4.5, xend = 5.3, y = 0.5, yend = 0.5, color = "#B2182B", 
           linewidth = 4, arrow = arrow(angle = 10, length = unit(6, "mm"), type = "closed")) +
  annotate("text", x = 5.5, y = 0.5, label = "Downward diffusion", color = "#B2182B", 
           size = 8, fontface = "bold", hjust = 0, family = "Inconsolata") +
  xlim(0.5, 8.5) + ylim(0.4, 0.6) + theme_void()

# 4. FUNCIÓN PARA PANELES
# ------------------------------------------------------------------------------
rate_by_edge <- dt[direction != "Lateral", .(rate = mean(diffusion, na.rm = TRUE)), 
                   by = .(domain, wage_q_src, wage_q_dest, direction)]
rate_by_edge[, rate_norm := rate / max(rate), by = domain]

make_flow_panel <- function(dom, subtitle_str) {
  edges <- rate_by_edge[domain == dom][order(-rate)][1:8]
  g <- graph_from_data_frame(edges[, .(from = wage_q_src, to = wage_q_dest, weight = rate_norm, direction)], 
                             vertices = tibble(name = paste0("Q", 1:5), x = 1:5, y = 0), directed = TRUE)
  
  ggraph(g, layout = "linear") +
    geom_edge_arc(aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
                  strength = 0.35, arrow = arrow(angle = 10, length = unit(6, "mm"), type = "closed"), 
                  end_cap = circle(10, "mm"), show.legend = FALSE) +
    scale_edge_width(range = c(2, 16)) +
    scale_edge_alpha(range = c(0.4, 1)) +
    scale_edge_colour_manual(values = c(Down = "#B2182B", Up = "#2166AC")) +
    geom_node_point(size = 24, color = "black", fill = "white", shape = 21, stroke = 2.5) +
    geom_node_text(aes(label = name), vjust = 0.4, size = 9, fontface = "bold", family = "Inconsolata") +
    labs(title = paste(dom, "Skills"), subtitle = subtitle_str) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 34, hjust = 0.5, family = "Inconsolata"),
      plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold", family = "Inconsolata",
                                   color = ifelse(dom == "Cognitive", "#2166AC", "#B2182B")),
      plot.margin = margin(30, 10, 30, 10)
    )
}

# 5. ENSAMBLAJE FINAL CON CAPTION FORMATEADO
# ------------------------------------------------------------------------------
# Redactamos y envolvemos el texto para que no se salga de la imagen
base_explanation <- "Note: Adoption is defined as the transition where skill k reaches an RCA ≥ 1 in occupation j at t1, conditional on j not being specialized at t0 and source i being specialized at t0. Rates represent the empirical mean probability across 17.3M transitions in the O*NET rolling panel (2015-2024)."

# Aplicamos str_wrap para ajustar el ancho (width=110 suele ir bien para este tamaño)
caption_wrapped <- str_wrap(base_explanation, width = 110)

p_final <- (make_flow_panel("Cognitive", text_cog) | make_flow_panel("Physical", text_phy)) / p_legend_rc28 +
  plot_layout(heights = c(10, 1.2)) +
  plot_annotation(
<<<<<<< HEAD
    title = "",
    caption = caption_wrapped,
    theme = theme(
      plot.title = element_text(face = "bold", size = 40, hjust = 0.5, family = "Inconsolata", margin = margin(t=10, b=15)),
      plot.caption = element_text(size = 18, hjust = 0.5, family = "Inconsolata", color = "grey30", 
                                  lineheight = 1.1, margin = margin(t=20, b=10)),
      plot.caption.position = "plot" # Asegura que el caption se alinee con todo el gráfico
    )
=======
    title = "Cognitive Skills: Top Skills — Adoption Rate Flows",
    subtitle = "Edge thickness proportional to adoption rate | Arrows show diffusion direction",
    caption = "Nodes: wage quintiles (Q1 low … Q5 high)"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 12)
>>>>>>> 51a2e716fd89c23cc0dc687d5cf080c5f593d769
  )

# Exportar con dimensiones suficientes para que no se vea apretado
# ggsave("flow_network_rc28.png", p_final, width = 20, height = 12, dpi = 300)
print(p_final)




<<<<<<< HEAD

# ==============================================================================
# RC28: INDIVIDUAL SKILL DRIFTS - 6 COGNITIVE UP / 6 PHYSICAL DOWN
# ==============================================================================

library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(igraph)
library(patchwork)

# 1. IDENTIFICACIÓN EMPÍRICA DE TOP SKILLS (17.3M)
# ------------------------------------------------------------------------------
# Calculamos tasas para seleccionar los mejores ejemplos de asimetría
skill_stats <- dt[direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE),
  n_total = .N
), by = .(skill_name, domain, direction)]

# Seleccionamos las 6 con mayor éxito en su dirección "teórica"
# Filtro: al menos 5000 oportunidades para asegurar robustez en RC28
top_6_cog_climb <- skill_stats[domain == "Cognitive" & direction == "Up" & n_total > 5000][
  order(-rate)][1:6, skill_name]

# CORRECCIÓN DE VARIABLE: top_6_phy_down
top_6_phy_down  <- skill_stats[domain == "Physical" & direction == "Down" & n_total > 5000][
  order(-rate)][1:6, skill_name]

# 2. PREPARACIÓN DE EDGES PARA PANELES
# ------------------------------------------------------------------------------
selected_skills_edges <- dt[skill_name %in% c(top_6_cog_climb, top_6_phy_down) & direction != "Lateral", .(
  rate = mean(diffusion, na.rm = TRUE)
), by = .(skill_name, domain, wage_q_src, wage_q_dest, direction)]

# Normalización por skill para máxima claridad visual en la grilla
selected_skills_edges[, rate_norm := rate / max(rate), by = skill_name]

# 3. FUNCIÓN DE RENDERIZADO CON AUTO-WRAP Y TAMAÑO MEJORADO
# ------------------------------------------------------------------------------
render_rc28_individual <- function(s_name) {
  skill_data <- selected_skills_edges[skill_name == s_name]
  
  g <- graph_from_data_frame(
    d = skill_data[, .(from = wage_q_src, to = wage_q_dest, weight = rate_norm, direction)],
    vertices = tibble(name = paste0("Q", 1:5), x = 1:5, y = 0), directed = TRUE
=======
p_phy_skills <- wrap_plots(plist_phy, ncol = 3) +
  plot_annotation(
    title = "Physical Skills: Top Skills — Adoption Rate Flows",
    subtitle = "Edge thickness proportional to adoption rate | Arrows show diffusion direction",
    caption = "Nodes: wage quintiles (Q1 low … Q5 high)"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 12)
>>>>>>> 51a2e716fd89c23cc0dc687d5cf080c5f593d769
  )
  
  ggraph(g, layout = "linear") +
    geom_edge_arc(aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
                  strength = 0.3, arrow = arrow(angle = 10, length = unit(4, "mm"), type = "closed"),
                  end_cap = circle(5, "mm"), show.legend = FALSE) +
    scale_edge_width(range = c(0.8, 6)) +
    scale_edge_alpha(range = c(0.3, 1)) +
    scale_edge_colour_manual(values = c(Up = "#2166AC", Down = "#B2182B")) +
    geom_node_point(size = 12, color = "black", fill = "white", shape = 21, stroke = 1.2) +
    geom_node_text(aes(label = name), vjust = 0.4, size = 4, fontface = "bold", family = "Inconsolata") +
    
    # Título con auto-wrap (caracteres max 30) y tamaño aumentado
    labs(title = str_wrap(s_name, width = 30)) + 
    
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "Inconsolata", 
                                lineheight = 0.9, margin = margin(b = 10)),
      plot.margin = margin(10, 5, 10, 5)
    )
}

# 4. GENERACIÓN DE LAS GRILLAS (PANELES 3x2)
# ------------------------------------------------------------------------------
p_list_cog <- lapply(top_6_cog_climb, render_rc28_individual)
p_list_phy <- lapply(top_6_phy_down, render_rc28_individual)

# Texto de base para los plots (Caption)
caption_text <- str_wrap("Methodological Note: Adoption rates calculated as the empirical mean probability of a skill crossing the RCA threshold (≥1) within the O*NET rolling panel (2015-2024). Individual scales normalized by maximum observed rate per skill for cross-panel legibility.", width = 120)

grid_cog <- wrap_plots(p_list_cog, ncol = 3) + 
  plot_annotation(title = "Cognitive Mobility: Top Skills by Upward Success Rate",
                  caption = caption_text,
                  theme = theme(plot.title = element_text(size = 24, face = "bold", family = "Inconsolata", color = "#2166AC"),
                                plot.caption = element_text(size = 13, family = "Inconsolata", color = "black", hjust = 0.5, margin = margin(t=15))))

grid_phy <- wrap_plots(p_list_phy, ncol = 3) + 
  plot_annotation(title = "Physical Mobility: Top Skills by Downward Propagation Rate",
                  caption = caption_text,
                  theme = theme(plot.title = element_text(size = 24, face = "bold", family = "Inconsolata", color = "#B2182B"),
                                plot.caption = element_text(size = 13, family = "Inconsolata", color = "black", hjust = 0.5, margin = margin(t=15))))

# Visualización
print(grid_cog)
print(grid_phy)



# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

message("\n")
message("================================================================")
message("FIGURAS GENERADAS")
message("================================================================")
message("")
message("1.  flow_net_aggregated.png - Networks agregados Cognitive vs Physical")
message("2. flow_skills_cognitive.png - Small multiples top Cognitive skills")
message("3. flow_skills_physical.png - Small multiples top Physical skills")
message("4. occupation_examples_quintiles. png - Ejemplos de ocupaciones")
message("5.  occupation_quintile_examples.png - Lista limpia por quintil")
message("6. flow_composition.png - Composición de oportunidades")
message("")
message("================================================================")
message("ESTADÍSTICAS CLAVE:")
message("================================================================")
message("")
message("TASAS DE ADOPCIÓN:")
message("  Cognitive - Upward:   20.5%")
message("  Cognitive - Downward: 14.9%")
message("  → Diferencia: -5.6 p.p.  (facilita subida)")
message("")
message("  Physical - Upward:    10.4%")
message("  Physical - Downward:  19.8%")
message("  → Diferencia: +9.4 p.p. (penaliza subida)")
message("")
message("RATIO:")
message("  Physical Down/Up: 1.9x (casi el doble)")
message("================================================================")





library(data.table)
library(ggplot2)
library(patchwork)
library(scales)


# dt must be data.table
setDT(dt)

# -------------------------------
# 0) PAL + THEME
# -------------------------------
pal_rc28 <- c("Cognitive" = "#19b5c6", "Physical" = "#11222b")

theme_rc28 <- theme_minimal(base_size = 22) +
  theme(
    text = element_text(family = "Inconsolata"),
    plot.title = element_text(face = "bold", size = 22, margin = margin(b = 8)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# -------------------------------
# 1) BINNING HELPERS (fast + stable with huge N)
# -------------------------------
# Use fixed number of bins. 80–150 suele verse bien.
NBINS <- 120L

# CI function (normal approx on bin means)
add_ci <- function(D) {
  D[, se := sqrt((rate * (1 - rate)) / pmax(n, 1))]
  D[, `:=`(low = pmax(rate - 1.96 * se, 0),
           high = pmin(rate + 1.96 * se, 1))]
  D
}

# -------------------------------
# 2) PANEL A: ABS(wage_gap)
# -------------------------------
dt[, wage_gap_abs := abs(wage_gap)]

# Create equal-frequency bins via rank -> integer bin
dt[, abs_bin := as.integer(NBINS * (frank(wage_gap_abs, ties.method = "random") - 1) / .N) + 1L]

stats_abs <- dt[, .(
  x = mean(wage_gap_abs),
  rate = mean(diffusion),
  n = .N
), by = .(domain, abs_bin)]

stats_abs <- add_ci(stats_abs)

p_abs <- ggplot(stats_abs, aes(x = x, y = rate, color = domain, fill = domain)) +
  #geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.12, color = NA) +
  geom_point(size = 1.6, alpha = 0.5) +
  geom_smooth(se = FALSE, method = "gam") +
  # smooth on binned points (light + stable)
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = pal_rc28) +
  scale_fill_manual(values = pal_rc28) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "A",
    x = "Absolute wage gap (|target − source|)",
    y = "Adoption rate"
  ) +
  theme_rc28

# -------------------------------
# 3) PANEL B: SIGNED wage_gap (negative vs positive around 0)
# -------------------------------
# Equal-frequency bins on signed gap
dt[, signed_bin := as.integer(NBINS * (frank(wage_gap, ties.method = "random") - 1) / .N) + 1L]

stats_signed <- dt[, .(
  x = mean(wage_gap),
  rate = mean(diffusion),
  n = .N
), by = .(domain, signed_bin)]

stats_signed <- add_ci(stats_signed)

p_signed <- ggplot(stats_signed, aes(x = x, y = rate, color = domain, fill = domain)) +
  geom_point(size = 1.6, alpha = 0.5) +
  geom_smooth(se = FALSE, method = "gam") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "gray40") +
  annotate("text", x = quantile(stats_signed$x, 0.01, na.rm = TRUE), y = 0.3,
           label = "Down (Target < Source)", color = "gray35", size = 5, hjust = 0) +
  annotate("text", x = quantile(stats_signed$x, 0.97, na.rm = TRUE), y = 0.3,
           label = "Up (Target > Source)", color = "gray35", size = 5, hjust = 1) +
  scale_color_manual(values = pal_rc28) +
  scale_fill_manual(values = pal_rc28) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "B",
    x = "Signed wage gap (target − source)",
    y = NULL
  ) +
  theme_rc28
p_signed
# -------------------------------
# 4) COMBINE
# -------------------------------
combined_plot <- (p_abs | p_signed) +
  plot_layout(guides = "collect") &      # <-- CLAVE
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.2, "lines"),
    text = element_text(family = "Inconsolata")
  )

print(combined_plot)



library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(mgcv)

# -------------------------------
# 0) CONFIGURACIÓN ESTÉTICA
# -------------------------------
pal_rc28 <- c("Cognitive" = "#19b5c6", "Physical" = "#11222b")

# Ajustamos el límite superior del eje Y al 40%
Y_LIMIT <- 0.4 

theme_rc28 <- theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "Inconsolata"),
    plot.title = element_text(face = "bold", size = 22),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# Helpers
NBINS <- 100L
add_ci <- function(D) {
  D[, se := sqrt((rate * (1 - rate)) / pmax(n, 1))]
  D[, `:=`(low = pmax(rate - 1.96 * se, 0), high = pmin(rate + 1.96 * se, 1))]
  D
}

# -------------------------------
# 1) PANEL A: STRUCTURAL DISTANCE (Filtrado y Escala 40%)
# -------------------------------
# Filtro para eliminar ruido del extremo (> 0.9)
dt_a <- dt[structural_distance <= 0.9]

dt_a[, dist_bin := as.integer(NBINS * (frank(structural_distance, ties.method = "random") - 1) / .N) + 1L]
stats_dist <- dt_a[, .(x = mean(structural_distance), rate = mean(diffusion), n = .N), by = .(domain, dist_bin)]
stats_dist <- add_ci(stats_dist)

p_dist <- ggplot(stats_dist, aes(x = x, y = rate, color = domain, fill = domain)) +
  geom_point(size = 1.8, alpha = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, linewidth = 1.5) +
  scale_color_manual(values = pal_rc28) +
  # Escala Y fijada en 40%
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, Y_LIMIT), 
                     breaks = seq(0, Y_LIMIT, 0.1)) +
  labs(title = "A", x = "Structural distance or Relatedness", y = "Adoption rate") +
  theme_rc28

# -------------------------------
# 2) PANEL B: SIGNED WAGE GAP (Escala 40%)
# -------------------------------
dt[, signed_bin := as.integer(NBINS * (frank(wage_gap, ties.method = "random") - 1) / .N) + 1L]
stats_signed <- dt[, .(x = mean(wage_gap), rate = mean(diffusion), n = .N), by = .(domain, signed_bin)]
stats_signed <- add_ci(stats_signed)

p_signed <- ggplot(stats_signed, aes(x = x, y = rate, color = domain, fill = domain)) +
  geom_point(size = 1.8, alpha = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = FALSE, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Posicionamos las etiquetas un poco más abajo del nuevo límite
  annotate("text", x = -0.8, y = Y_LIMIT * 0.85, label = "Down (Target < Source)", size = 4.5, color = "gray30") +
  annotate("text", x = 0.8, y = Y_LIMIT * 0.85, label = "Up (Target > Source)", size = 4.5, color = "gray30") +
  scale_color_manual(values = pal_rc28) +
  # Escala Y fijada en 40% (idéntica al Panel A)
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(0, Y_LIMIT), 
                     breaks = seq(0, Y_LIMIT, 0.1)) +
  labs(title = "B", x = "Signed wage gap (target - source)", y = NULL) +
  theme_rc28

# -------------------------------
# 3) COMBINACIÓN FINAL
# -------------------------------
combined_plot <- (p_dist | p_signed) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

print(combined_plot)






















