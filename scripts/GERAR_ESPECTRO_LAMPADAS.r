###############################################################################
# GERAR_ESPECTRO_LAMPADAS.r
# Generates Figure 3: Emission spectrum of UV chamber lamps
# 
# Reconstructs spectral profiles for a custom UV chamber with:
#   - UVA-340 fluorescent lamps (peak ~340 nm)
#   - UVB-313 fluorescent lamps (peak ~313 nm)  
#   - Visible-range fluorescent lamps
#   - Sample-to-lamp distance: 200 mm
#
# Corrected integrated irradiance values:
#   UV-A (315-400 nm): 6.21 W/m²
#   UV-B (280-315 nm): 2.28 W/m²
#   Visible (400-700 nm): 9.37 W/m²
#   Total: ~17.9 W/m²
###############################################################################

library(ggplot2)

# --- Wavelength grid (1 nm resolution) ---
lambda <- seq(250, 800, by = 1)

# =====================================================================
# SPECTRAL PROFILES
# =====================================================================
# Model each lamp type with realistic Gaussian/skewed profiles based on
# published spectral power distributions for fluorescent UV lamps.

# --- UVB-313 component ---
# Peak at ~313 nm, FWHM ~25 nm, with small secondary emission
uvb_peak <- 313
uvb_sigma <- 12  # gives FWHM ~28 nm
uvb_spd <- exp(-0.5 * ((lambda - uvb_peak) / uvb_sigma)^2)
# Suppress below 270 and above 360
uvb_spd[lambda < 270] <- 0
uvb_spd[lambda > 370] <- 0

# --- UVA-340 component ---
# Peak at ~340 nm, FWHM ~30 nm, slight asymmetry extending to ~400 nm
uva_peak <- 343
uva_sigma_left <- 14
uva_sigma_right <- 18
uva_spd <- ifelse(lambda <= uva_peak,
                   exp(-0.5 * ((lambda - uva_peak) / uva_sigma_left)^2),
                   exp(-0.5 * ((lambda - uva_peak) / uva_sigma_right)^2))
uva_spd[lambda < 300] <- 0
uva_spd[lambda > 420] <- 0

# --- Visible fluorescent component ---
# Typical tri-phosphor fluorescent: peaks at ~435, ~545, ~610 nm
vis_peak1 <- 435; vis_sigma1 <- 15
vis_peak2 <- 545; vis_sigma2 <- 25
vis_peak3 <- 610; vis_sigma3 <- 18
vis_spd <- 0.4 * exp(-0.5 * ((lambda - vis_peak1) / vis_sigma1)^2) +
           1.0 * exp(-0.5 * ((lambda - vis_peak2) / vis_sigma2)^2) +
           0.7 * exp(-0.5 * ((lambda - vis_peak3) / vis_sigma3)^2)
vis_spd[lambda < 380] <- 0
vis_spd[lambda > 720] <- 0

# =====================================================================
# NORMALIZE TO MATCH INTEGRATED IRRADIANCE VALUES
# =====================================================================
# Target integrated values (W/m²):
target_uvb  <- 2.28   # 280-315 nm
target_uva  <- 6.21   # 315-400 nm
target_vis  <- 9.37   # 400-700 nm

# Because lamp profiles overlap at band boundaries, solve a 3×3 linear
# system: A * [sf_uvb, sf_uva, sf_vis]' = [target_uvb, target_uva, target_vis]'
# where A[i,j] = integral of raw profile j in band i

bands_lo <- c(280, 315, 400)
bands_hi <- c(315, 400, 700)
raw_profiles <- list(uvb_spd, uva_spd, vis_spd)

A <- matrix(0, nrow = 3, ncol = 3)
for (i in 1:3) {
  mask <- lambda >= bands_lo[i] & lambda <= bands_hi[i]
  for (j in 1:3) {
    A[i, j] <- sum(raw_profiles[[j]][mask])
  }
}

sf <- solve(A, c(target_uvb, target_uva, target_vis))

# Scaled spectral irradiance (W/m²/nm)
uvb_scaled <- uvb_spd * sf[1]
uva_scaled <- uva_spd * sf[2]
vis_scaled <- vis_spd * sf[3]

# Combined UV and VIS components
uv_combined  <- uvb_scaled + uva_scaled
vis_component <- vis_scaled
total_combined <- uv_combined + vis_component

# Verify integrations
cat("=== VERIFICATION ===\n")
cat(sprintf("UV-B (280-315 nm): %.2f W/m²\n", 
            sum(total_combined[lambda >= 280 & lambda <= 315])))
cat(sprintf("UV-A (315-400 nm): %.2f W/m²\n", 
            sum(total_combined[lambda >= 315 & lambda <= 400])))
cat(sprintf("Visible (400-700 nm): %.2f W/m²\n", 
            sum(total_combined[lambda >= 400 & lambda <= 700])))
cat(sprintf("Total (280-700 nm): %.2f W/m²\n", 
            sum(total_combined[lambda >= 280 & lambda <= 700])))
cat(sprintf("Estimated spectral irradiance at 340 nm: %.3f W/m²/nm\n",
            total_combined[lambda == 340]))

# =====================================================================
# BUILD PLOT DATA
# =====================================================================
df <- data.frame(
  wavelength = rep(lambda, 3),
  irradiance = c(total_combined, uv_combined, vis_component),
  component  = rep(c("Combined (UV + VIS)", "UV component", "VIS component"), 
                   each = length(lambda))
)

# Color palette matching the original figure description
# Black = combined, Blue = UV, Red = VIS
color_map <- c("Combined (UV + VIS)" = "black",
               "UV component" = "#2166AC",
               "VIS component" = "#B2182B")

# Band shading data
bands <- data.frame(
  xmin = c(280, 315, 400),
  xmax = c(315, 400, 700),
  label = c("UV-B", "UV-A", "Visible"),
  fill  = c("#D1C4E9", "#B3E5FC", "#FFF9C4")
)

# =====================================================================
# GENERATE FIGURE
# =====================================================================
p <- ggplot() +
  # Band shading
  geom_rect(data = bands, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = label),
            alpha = 0.25, show.legend = FALSE) +
  scale_fill_manual(values = c("UV-B" = "#D1C4E9", 
                                "UV-A" = "#B3E5FC", 
                                "Visible" = "#FFF9C4")) +
  # Spectral curves
  geom_line(data = df, 
            aes(x = wavelength, y = irradiance, color = component, 
                linewidth = component, linetype = component)) +
  scale_color_manual(values = color_map, name = NULL) +
  scale_linewidth_manual(values = c("Combined (UV + VIS)" = 0.9, 
                                     "UV component" = 0.6, 
                                     "VIS component" = 0.6),
                          guide = "none") +
  scale_linetype_manual(values = c("Combined (UV + VIS)" = "solid",
                                    "UV component" = "solid",
                                    "VIS component" = "solid"),
                         guide = "none") +
  # Band labels
  annotate("text", x = 297, y = max(total_combined) * 0.92, 
           label = "UV-B\n(280–315 nm)", size = 2.8, fontface = "italic",
           color = "grey40") +
  annotate("text", x = 357, y = max(total_combined) * 0.92, 
           label = "UV-A\n(315–400 nm)", size = 2.8, fontface = "italic",
           color = "grey40") +
  annotate("text", x = 550, y = max(total_combined) * 0.92, 
           label = "Visible\n(400–700 nm)", size = 2.8, fontface = "italic",
           color = "grey40") +
  # Integrated irradiance annotations
  annotate("text", x = 297, y = max(total_combined) * 0.78,
           label = "2.28 W·m⁻²", size = 2.5, fontface = "bold",
           color = "#7B1FA2") +
  annotate("text", x = 357, y = max(total_combined) * 0.78,
           label = "6.21 W·m⁻²", size = 2.5, fontface = "bold",
           color = "#0277BD") +
  annotate("text", x = 550, y = max(total_combined) * 0.78,
           label = "9.37 W·m⁻²", size = 2.5, fontface = "bold",
           color = "#C62828") +
  # Axes
  scale_x_continuous(breaks = seq(250, 800, 50), 
                     limits = c(250, 780),
                     expand = c(0.01, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Wavelength (nm)",
       y = expression("Spectral irradiance (W·m"^-2*"·nm"^-1*")")) +
  # Theme
  theme_bw(base_size = 11) +
  theme(
    legend.position = c(0.82, 0.88),
    legend.background = element_rect(fill = alpha("white", 0.85), 
                                      color = "grey70", linewidth = 0.3),
    legend.text = element_text(size = 8.5),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(8, 12, 8, 8)
  )

# Save
out_dir <- "graficos/tracao"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "fig03_espectro_lampadas.png"), p,
       width = 8, height = 4.5, dpi = 300, bg = "white")

cat(sprintf("\nFigure saved to: %s/fig03_espectro_lampadas.png\n", out_dir))
cat("Done.\n")
