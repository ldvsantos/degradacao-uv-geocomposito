# ==============================================================================
# SCRIPT TO CREATE COMPARATIVE FIGURE: TENSILE vs. PUNCTURE
# ==============================================================================
# Objective: Create comparative graphic of Weibull parameters and statistics
#            between tensile and puncture tests
# Author: Statistical Analysis - Forced Degradation
# Date: October 15, 2025
# ==============================================================================

# Load required libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(tidyr)

# ==============================================================================
# WEIBULL PARAMETERS AND STATISTICS DATA
# ==============================================================================

# Weibull parameters
comparison_data <- data.frame(
  Test = rep(c("Tensile\n(Uniaxial)", "Puncture\n(Multiaxial)"), each = 6),
  Parameter = rep(c("β (Shape)", "η (Scale)*", "Mean Life*", 
                    "Mean CV (%)", "P10*", "P90*"), 2),
  Value = c(
    # Tensile
    3.4031,           # β
    19.93,            # η (kN/m)
    17.90,            # Mean life (kN/m)
    25.4,             # Mean CV (%)
    10.29,            # P10
    25.46,            # P90
    # Puncture
    4.4625,           # β
    1784.19/100,      # η (N) / 100 for visual scale
    1627.42/100,      # Mean life (N) / 100
    8.67,             # Mean CV (%)
    1077.54/100,      # P10 / 100
    2150.86/100       # P90 / 100
  )
)

# Note: * values normalized/scaled for visualization

# ==============================================================================
# PLOT 1: SHAPE PARAMETER (β) AND CV
# ==============================================================================

data_beta_cv <- comparison_data %>%
  filter(Parameter %in% c("β (Shape)", "Mean CV (%)"))

plot_beta_cv <- ggplot(data_beta_cv, aes(x = Test, y = Value, fill = Test)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.5) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~Parameter, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Statistical Parameters Comparison",
    subtitle = "Weibull Shape Parameter (β) and Coefficient of Variation",
    x = "",
    y = "Value"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# ==============================================================================
# PLOT 2: MEAN LIFE AND PERCENTILES
# ==============================================================================

data_life <- comparison_data %>%
  filter(Parameter %in% c("Mean Life*", "P10*", "P90*")) %>%
  mutate(Parameter = factor(Parameter, levels = c("P10*", "Mean Life*", "P90*")))

plot_life <- ggplot(data_life, aes(x = Parameter, y = Value, fill = Test)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, color = "black", linewidth = 0.5) +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Service Life and Failure Percentiles",
    subtitle = "Values normalized for visual comparison",
    x = "Parameter",
    y = "Normalized Value",
    fill = "Test Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 0),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# ==============================================================================
# PLOT 3: SUMMARY COMPARATIVE TABLE
# ==============================================================================

summary_table <- data.frame(
  Parameter = c("β (Shape)", "Mean CV (%)", "Failure Regime", 
                "Predictability", "Statistical Dispersion"),
  Tensile = c("3.40", "14-36%", "Mixed", "Moderate", "High"),
  Puncture = c("4.46", "8.7%", "Deterministic", "High", "Low"),
  Interpretation = c(
    "Higher β indicates more predictable failure",
    "Lower CV = greater homogeneity",
    "Cohesive (puncture) vs. Adhesive+Cohesive (tensile)",
    "Puncture allows better planning",
    "Puncture offers greater reliability"
  )
)

# Convert table to plot
library(gridExtra)
library(grid)

table_grob <- tableGrob(summary_table, rows = NULL, 
                         theme = ttheme_default(
                           core = list(fg_params = list(cex = 0.8)),
                           colhead = list(fg_params = list(cex = 0.9, fontface = "bold")),
                           rowhead = list(fg_params = list(cex = 0.9))
                         ))

# ==============================================================================
# COMBINE AND SAVE FIGURES - HORIZONTAL LAYOUT
# ==============================================================================

# Create directory if it doesn't exist
dir.create("graficos/comparacao", showWarnings = FALSE, recursive = TRUE)

# Save Plot 1 (β and CV)
ggsave(
  filename = "graficos/comparacao/comparacao_beta_cv.png",
  plot = plot_beta_cv,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# Save Plot 2 (Life and Percentiles)
ggsave(
  filename = "graficos/comparacao/comparacao_vida_percentis.png",
  plot = plot_life,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# ==============================================================================
# SAVE COMBINED PLOT - HORIZONTAL LAYOUT (side by side)
# ==============================================================================

plot_combined_horizontal <- grid.arrange(
  plot_beta_cv, 
  plot_life,
  ncol = 2,  # 2 columns = horizontal layout
  nrow = 1,  # 1 row
  top = textGrob("Tensile vs. Puncture Comparison: Weibull Parameters",
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

ggsave(
  filename = "graficos/comparacao/comparacao_weibull_tracao_puncao.png",
  plot = plot_combined_horizontal,
  width = 16, height = 7, dpi = 300, bg = "white"  # Wider, less tall
)

# ==============================================================================
# ALTERNATIVE VERSION: COMPACT HORIZONTAL LAYOUT
# ==============================================================================

# Create more compact version for manuscript
plot_compact <- grid.arrange(
  plot_beta_cv + theme(plot.title = element_text(size = 14),
                       plot.subtitle = element_text(size = 11)), 
  plot_life + theme(plot.title = element_text(size = 14),
                    plot.subtitle = element_text(size = 10)),
  ncol = 2,
  nrow = 1,
  top = textGrob("Statistical Parameters Comparison: Tensile vs. Puncture Tests",
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

ggsave(
  filename = "graficos/comparacao/comparacao_weibull_compacta_horizontal.png",
  plot = plot_compact,
  width = 14, height = 6, dpi = 300, bg = "white"
)

# Save table as image
png("graficos/comparacao/tabela_comparativa.png", width = 1200, height = 400, res = 150)
grid.draw(table_grob)
dev.off()

# ==============================================================================
# ADDITIONAL PLOT: RELIABILITY CURVES COMPARISON
# ==============================================================================

# Simulate Weibull reliability curves
x_seq <- seq(0, 40, length.out = 200)

# Reliability function: R(t) = exp(-(t/η)^β)
reliability_tensile <- exp(-(x_seq/19.93)^3.4031)
reliability_puncture <- exp(-(x_seq/17.84)^4.4625)  # η converted to similar scale

curves_data <- data.frame(
  Time = rep(x_seq, 2),
  Reliability = c(reliability_tensile, reliability_puncture),
  Test = rep(c("Tensile (β=3.40)", "Puncture (β=4.46)"), each = length(x_seq))
)

plot_curves <- ggplot(curves_data, aes(x = Time, y = Reliability, 
                                        color = Test, linetype = Test)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.632, linetype = "dashed", color = "gray50") +
  annotate("text", x = 35, y = 0.68, label = "63.2% (characteristic life)", 
           size = 3.5, color = "gray30") +
  scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Reliability Functions Comparison",
    subtitle = "Weibull Distribution: Tensile vs. Puncture",
    x = "Normalized Time",
    y = "Reliability R(t)",
    color = "Test Type",
    linetype = "Test Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = c(0.75, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

ggsave(
  filename = "graficos/comparacao/curvas_confiabilidade_comparacao.png",
  plot = plot_curves,
  width = 10, height = 7, dpi = 300, bg = "white"
)

# ==============================================================================
# COMPLETION MESSAGES
# ==============================================================================

cat("\n==============================================================================\n")
cat("COMPARATIVE FIGURES CREATED SUCCESSFULLY!\n")
cat("==============================================================================\n\n")
cat("Files generated in: graficos/comparacao/\n\n")
cat("1. comparacao_beta_cv.png - β and CV parameters\n")
cat("2. comparacao_vida_percentis.png - Service life and percentiles\n")
cat("3. comparacao_weibull_tracao_puncao.png - HORIZONTAL (16x7) - MAIN\n")
cat("4. comparacao_weibull_compacta_horizontal.png - COMPACT (14x6)\n")
cat("5. tabela_comparativa.png - Interpretive summary table\n")
cat("6. curvas_confiabilidade_comparacao.png - Overlaid Weibull curves\n\n")
cat(">>> Use 'comparacao_weibull_tracao_puncao.png' (HORIZONTAL) as Figure 14\n")
cat(">>> Layout: 2 side-by-side panels (16 cm x 7 cm)\n")
cat(">>> Ideal for scientific papers with two-column layout\n")
cat("\n==============================================================================\n")
