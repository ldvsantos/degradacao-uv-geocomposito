# ===============================================================================
# GERAR GRÁFICOS WEIBULL DETALHADOS PARA PUNÇÃO (DESLOCAMENTO)
# Reliability, Hazard Rate e QQ-Plot para deslocamento
# Data: 19 de outubro de 2025
# ===============================================================================

# Limpeza do ambiente
rm(list = ls())

# ===============================================================================
# 1. CARREGAMENTO DE PACOTES
# ===============================================================================

# Configurar repositório CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Instalar e carregar pacotes
if(!require(fitdistrplus)) install.packages("fitdistrplus")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readxl)) install.packages("readxl")

library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(readxl)

cat("=== GERANDO GRÁFICOS WEIBULL DETALHADOS PARA PUNÇÃO (DESLOCAMENTO) ===\n")

# ===============================================================================
# 2. CARREGAMENTO DOS DADOS DE PUNÇÃO
# ===============================================================================

file_path <- file.path(getwd(), 'dados', 'DB.xlsx')
dados_puncao_raw <- readxl::read_excel(file_path, sheet = "PUNCAO")

# Preparar dados
dados_puncao <- dados_puncao_raw %>%
  rename(
    forca_puncao = `tensão máxima (N)`,
    deslocamento_puncao = `extensão máxima (mm)`
  ) %>%
  filter(!is.na(forca_puncao) & !is.na(deslocamento_puncao))

# ===============================================================================
# 3. ANÁLISE WEIBULL E GERAÇÃO DE GRÁFICOS
# ===============================================================================

# Criar diretório para gráficos
graficos_dir <- file.path(getwd(), 'graficos', 'puncao')
dir.create(graficos_dir, showWarnings = FALSE, recursive = TRUE)

# ========================================================================
# PARÂMETROS WEIBULL VALIDADOS DO RELATÓRIO (MLE - Maximum Likelihood Estimation)
# Estes valores foram calculados com o método mais robusto
# ========================================================================

cat("\n--- Processando: Puncture Force ---\n")

# Parâmetros MLE para Força de Punção (corrigido: era 5.4625 — erro de transcrição)
shape_forca <- 4.4625
scale_forca <- 1784.19

cat("Shape (β):", round(shape_forca, 4), "| Scale (η):", round(scale_forca, 4), "\n")

# DEBUG: Mostrar label exato que será usado no gráfico
label_forca <- sprintf('β = %.4f | η = %.2f', shape_forca, scale_forca)
cat("LABEL NO GRÁFICO (Força):", label_forca, "\n")

# Usar percentil 99.9% para escala apropriada (como fizemos para tração)
x_max_forca <- qweibull(0.999, shape = shape_forca, scale = scale_forca)
x_forca <- seq(0, x_max_forca, length.out = 1000)
reliability_forca <- 1 - pweibull(x_forca, shape = shape_forca, scale = scale_forca)
hazard_forca <- (shape_forca/scale_forca) * (x_forca/scale_forca)^(shape_forca-1)

p10_forca <- qweibull(0.1, shape = shape_forca, scale = scale_forca)
p90_forca <- qweibull(0.9, shape = shape_forca, scale = scale_forca)

# Reliability Function
grafico_reliability_forca <- ggplot() +
  geom_line(aes(x = x_forca, y = reliability_forca), color = '#56B4E9', size = 1.5) +
  geom_vline(xintercept = p10_forca, linetype = 'dashed', color = '#E69F00', size = 1) +
  geom_vline(xintercept = p90_forca, linetype = 'dotted', color = '#D55E00', size = 1) +
  annotate('text', x = max(x_forca)*0.7, y = 0.75, 
           label = sprintf('β = %.4f | η = %.2f', shape_forca, scale_forca),
           size = 5, family = 'serif', color = '#222222') +
  labs(title = 'Reliability Function - Puncture Force',
       x = 'Puncture Force (N)',
       y = 'Reliability R(t)') +
  theme_classic(base_family = 'serif') +
  ylim(0, 1)

nome_arquivo <- file.path(graficos_dir, 'probabilidade_ruptura_forca_puncao_reliability_functions.png')
ggsave(nome_arquivo, grafico_reliability_forca, width = 10, height = 6, dpi = 300)
cat("Gráfico salvo:", basename(nome_arquivo), "\n")

# Hazard Rate
grafico_hazard_forca <- ggplot() +
  geom_line(aes(x = x_forca, y = hazard_forca), color = '#009E73', size = 1.5) +
  annotate('text', x = max(x_forca)*0.7, y = max(hazard_forca)*0.8, 
           label = sprintf('β = %.4f | η = %.2f', shape_forca, scale_forca), 
           size = 5, family = 'serif', color = '#222222') +
  labs(title = 'Hazard Rate - Puncture Force',
       x = 'Puncture Force (N)',
       y = 'Hazard Rate h(t)') +
  theme_classic(base_family = 'serif')

nome_arquivo <- file.path(graficos_dir, 'probabilidade_ruptura_forca_puncao_hazard_rate.png')
ggsave(nome_arquivo, grafico_hazard_forca, width = 10, height = 6, dpi = 300)
cat("Gráfico salvo:", basename(nome_arquivo), "\n")

# QQ-Plot
qq_data_forca <- data.frame(
  theoretical = qweibull(ppoints(length(dados_puncao$forca_puncao)), shape = shape_forca, scale = scale_forca),
  empirical = sort(dados_puncao$forca_puncao)
)

grafico_qq_forca <- ggplot(qq_data_forca, aes(x = theoretical, y = empirical)) +
  geom_point(color = '#E69F00', size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = '#222222', linetype = 'dashed', size = 1) +
  labs(title = 'QQ-Plot - Puncture Force',
       x = 'Theoretical Quantiles (Weibull)',
       y = 'Empirical Quantiles') +
  theme_classic(base_family = 'serif')

nome_arquivo <- file.path(graficos_dir, 'probabilidade_ruptura_forca_puncao_qq_plot.png')
ggsave(nome_arquivo, grafico_qq_forca, width = 10, height = 6, dpi = 300)
cat("Gráfico salvo:", basename(nome_arquivo), "\n")

# ========================================================================
# GRÁFICO 2: FUNÇÃO DE CONFIABILIDADE PARA DESLOCAMENTO
# ========================================================================

cat("\n--- Processando: Displacement (Elongation) ---\n")

# Parâmetros MLE para Deslocamento de Punção
shape_desl <- 5.351
scale_desl <- 13.618

cat("Shape (β):", round(shape_desl, 4), "| Scale (η):", round(scale_desl, 4), "\n")

# DEBUG: Mostrar label exato que será usado no gráfico
label_desl <- sprintf('β = %.4f | η = %.2f', shape_desl, scale_desl)
cat("LABEL NO GRÁFICO (Deslocamento):", label_desl, "\n")

# Usar percentil 99.9% para escala apropriada (como fizemos para tração)
x_max_desl <- qweibull(0.999, shape = shape_desl, scale = scale_desl)
x_desl <- seq(0, x_max_desl, length.out = 1000)
reliability_desl <- 1 - pweibull(x_desl, shape = shape_desl, scale = scale_desl)
hazard_desl <- (shape_desl/scale_desl) * (x_desl/scale_desl)^(shape_desl-1)

p10_desl <- qweibull(0.1, shape = shape_desl, scale = scale_desl)
p90_desl <- qweibull(0.9, shape = shape_desl, scale = scale_desl)

# Reliability Function
grafico_reliability_desl <- ggplot() +
  geom_line(aes(x = x_desl, y = reliability_desl), color = '#56B4E9', size = 1.5) +
  geom_vline(xintercept = p10_desl, linetype = 'dashed', color = '#E69F00', size = 1) +
  geom_vline(xintercept = p90_desl, linetype = 'dotted', color = '#D55E00', size = 1) +
  annotate('text', x = max(x_desl)*0.7, y = 0.8, 
           label = sprintf('β = %.4f | η = %.2f', shape_desl, scale_desl), 
           size = 5, family = 'serif', color = '#222222') +
  labs(title = 'Reliability Function - Displacement',
       x = 'Displacement (mm)',
       y = 'Reliability R(t)') +
  theme_classic(base_family = 'serif') +
  ylim(0, 1)

nome_arquivo <- file.path(graficos_dir, 'probabilidade_ruptura_deslocamento_puncao_reliability_functions.png')
ggsave(nome_arquivo, grafico_reliability_desl, width = 10, height = 6, dpi = 300)
cat("Gráfico salvo:", basename(nome_arquivo), "\n")

# Hazard Rate
grafico_hazard_desl <- ggplot() +
  geom_line(aes(x = x_desl, y = hazard_desl), color = '#009E73', size = 1.5) +
  annotate('text', x = max(x_desl)*0.7, y = max(hazard_desl)*0.8, 
           label = sprintf('β = %.4f | η = %.2f', shape_desl, scale_desl), 
           size = 5, family = 'serif', color = '#222222') +
  labs(title = 'Hazard Rate - Displacement',
       x = 'Displacement (mm)',
       y = 'Hazard Rate h(t)') +
  theme_classic(base_family = 'serif')

nome_arquivo <- file.path(graficos_dir, 'probabilidade_ruptura_deslocamento_puncao_hazard_rate.png')
ggsave(nome_arquivo, grafico_hazard_desl, width = 10, height = 6, dpi = 300)
cat("Gráfico salvo:", basename(nome_arquivo), "\n")

# QQ-Plot
qq_data_desl <- data.frame(
  theoretical = qweibull(ppoints(length(dados_puncao$deslocamento_puncao)), shape = shape_desl, scale = scale_desl),
  empirical = sort(dados_puncao$deslocamento_puncao)
)

grafico_qq_desl <- ggplot(qq_data_desl, aes(x = theoretical, y = empirical)) +
  geom_point(color = '#E69F00', size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = '#222222', linetype = 'dashed', size = 1) +
  labs(title = 'QQ-Plot - Displacement',
       x = 'Theoretical Quantiles (Weibull)',
       y = 'Empirical Quantiles') +
  theme_classic(base_family = 'serif')

nome_arquivo <- file.path(graficos_dir, 'probabilidade_ruptura_deslocamento_puncao_qq_plot.png')
ggsave(nome_arquivo, grafico_qq_desl, width = 10, height = 6, dpi = 300)
cat("Gráfico salvo:", basename(nome_arquivo), "\n")

# ===============================================================================
# GRÁFICOS COMBINADOS: FORÇA + DESLOCAMENTO
# ===============================================================================

cat("\n=== GERANDO GRÁFICOS COMBINADOS: FORÇA E DESLOCAMENTO ===\n")

# ---------------------------------------------------------------------------
# 1. RELIABILITY FUNCTION COMBINADA
# ---------------------------------------------------------------------------

# Criar dataframe combinado para Reliability
df_reliability <- data.frame(
  x = c(x_forca, x_desl),
  reliability = c(reliability_forca, reliability_desl),
  variable = rep(c("Puncture Force (N)", "Displacement (mm)"), 
                 c(length(x_forca), length(x_desl)))
)

# Gráfico Reliability combinado com facet
grafico_reliability_combinado <- ggplot(df_reliability, aes(x = x, y = reliability, color = variable)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    name = "Property",
    values = c("Puncture Force (N)" = "#56B4E9", 
               "Displacement (mm)" = "#009E73"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  labs(
    title = "Reliability Function R(t) - Puncture Force and Displacement",
    x = "Value",
    y = "Reliability R(t)"
  ) +
  ylim(0, 1) +
  theme_classic(base_family = 'serif') +
  theme(
    legend.position = c(0.65, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  facet_wrap(~variable, scales = "free_x", ncol = 2)

nome_arquivo_rel_comb <- file.path(graficos_dir, 
                                   'probabilidade_ruptura_reliability_COMBINADO.png')
ggsave(nome_arquivo_rel_comb, grafico_reliability_combinado, width = 14, height = 6, dpi = 300)
cat("Gráfico Reliability combinado salvo:", basename(nome_arquivo_rel_comb), "\n")

# ---------------------------------------------------------------------------
# 2. HAZARD RATE COMBINADO
# ---------------------------------------------------------------------------

# Criar dataframe combinado para Hazard Rate
df_hazard <- data.frame(
  x = c(x_forca, x_desl),
  hazard = c(hazard_forca, hazard_desl),
  variable = rep(c("Puncture Force (N)", "Displacement (mm)"), 
                 c(length(x_forca), length(x_desl)))
)

# Gráfico Hazard Rate combinado com facet
grafico_hazard_combinado <- ggplot(df_hazard, aes(x = x, y = hazard, color = variable)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    name = "Property",
    values = c("Puncture Force (N)" = "#009E73", 
               "Displacement (mm)" = "#E69F00"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  labs(
    title = "Hazard Rate h(t) - Puncture Force and Displacement",
    x = "Value",
    y = "Hazard Rate h(t)"
  ) +
  theme_classic(base_family = 'serif') +
  theme(
    legend.position = c(0.65, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  facet_wrap(~variable, scales = "free", ncol = 2)

nome_arquivo_hazard_comb <- file.path(graficos_dir, 
                                      'probabilidade_ruptura_hazard_rate_COMBINADO.png')
ggsave(nome_arquivo_hazard_comb, grafico_hazard_combinado, width = 14, height = 6, dpi = 300)
cat("Gráfico Hazard Rate combinado salvo:", basename(nome_arquivo_hazard_comb), "\n")

# ---------------------------------------------------------------------------
# 3. QQ-PLOT COMBINADO
# ---------------------------------------------------------------------------

# Criar dataframe combinado para QQ-Plot
df_qq <- data.frame(
  theoretical = c(qq_data_forca$theoretical, qq_data_desl$theoretical),
  empirical = c(qq_data_forca$empirical, qq_data_desl$empirical),
  variable = rep(c("Puncture Force (N)", "Displacement (mm)"),
                 c(nrow(qq_data_forca), nrow(qq_data_desl)))
)

# QQ-Plot combinado com facet
grafico_qq_combinado <- ggplot(df_qq, aes(x = theoretical, y = empirical, color = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  scale_color_manual(
    name = "Property",
    values = c("Puncture Force (N)" = "#E69F00", 
               "Displacement (mm)" = "#56B4E9"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  labs(
    title = "QQ-Plot - Puncture Force and Displacement",
    x = "Theoretical Quantiles (Weibull)",
    y = "Empirical Quantiles"
  ) +
  theme_classic(base_family = 'serif') +
  theme(
    legend.position = c(0.25, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  facet_wrap(~variable, scales = "free", ncol = 2)

nome_arquivo_qq_comb <- file.path(graficos_dir, 
                                  'probabilidade_ruptura_qq_plot_COMBINADO.png')
ggsave(nome_arquivo_qq_comb, grafico_qq_combinado, width = 14, height = 6, dpi = 300)
cat("QQ-Plot combinado salvo:", basename(nome_arquivo_qq_comb), "\n")

# ---------------------------------------------------------------------------
# VERSÕES SINGLE PANEL (PAINEL ÚNICO - ESTILO TRAÇÃO)
# ---------------------------------------------------------------------------

cat("\n=== GERANDO VERSÕES SINGLE PANEL (com valores normalizados) ===\n")

# Para single panel, normalizar os valores pela escala (η) para comparação justa
# x_normalizado = x / η

# RELIABILITY FUNCTION - Single Panel (valores normalizados)
x_norm_forca <- x_forca / scale_forca
x_norm_desl <- x_desl / scale_desl

df_reliability_norm <- data.frame(
  x_norm = c(x_norm_forca, x_norm_desl),
  reliability = c(reliability_forca, reliability_desl),
  variable = rep(c("Puncture Force (N)", "Displacement (mm)"), 
                 c(length(x_norm_forca), length(x_norm_desl)))
)

grafico_reliability_single <- ggplot(df_reliability_norm, aes(x = x_norm, y = reliability, color = variable, linetype = variable)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    name = "",
    values = c("Puncture Force (N)" = "#56B4E9", 
               "Displacement (mm)" = "#009E73"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  scale_linetype_manual(
    name = "",
    values = c("Puncture Force (N)" = "solid", 
               "Displacement (mm)" = "dashed"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  labs(
    title = "Reliability Function R(t) - Puncture Force and Displacement",
    x = "Normalized Value (t/η)",
    y = "Reliability R(t)"
  ) +
  ylim(0, 1) +
  theme_classic(base_family = 'serif') +
  theme(
    legend.position = c(0.70, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

nome_arquivo_rel_single <- file.path(graficos_dir, 
                                     'probabilidade_ruptura_reliability_COMBINADO_single.png')
ggsave(nome_arquivo_rel_single, grafico_reliability_single, width = 10, height = 6, dpi = 300)
cat("Gráfico Reliability single panel salvo:", basename(nome_arquivo_rel_single), "\n")

# HAZARD RATE - Single Panel (valores normalizados)
df_hazard_norm <- data.frame(
  x_norm = c(x_norm_forca, x_norm_desl),
  hazard = c(hazard_forca, hazard_desl),
  variable = rep(c("Puncture Force (N)", "Displacement (mm)"), 
                 c(length(x_norm_forca), length(x_norm_desl)))
)

grafico_hazard_single <- ggplot(df_hazard_norm, aes(x = x_norm, y = hazard, color = variable, linetype = variable)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    name = "",
    values = c("Puncture Force (N)" = "#009E73", 
               "Displacement (mm)" = "#E69F00"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  scale_linetype_manual(
    name = "",
    values = c("Puncture Force (N)" = "solid", 
               "Displacement (mm)" = "dashed"),
    labels = c(sprintf("Puncture Force (β=%.4f, η=%.2f N)", shape_forca, scale_forca),
               sprintf("Displacement (β=%.4f, η=%.2f mm)", shape_desl, scale_desl))
  ) +
  labs(
    title = "Hazard Rate h(t) - Puncture Force and Displacement",
    x = "Normalized Value (t/η)",
    y = "Hazard Rate h(t)"
  ) +
  theme_classic(base_family = 'serif') +
  theme(
    legend.position = c(0.30, 0.80),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

nome_arquivo_hazard_single <- file.path(graficos_dir, 
                                        'probabilidade_ruptura_hazard_rate_COMBINADO_single.png')
ggsave(nome_arquivo_hazard_single, grafico_hazard_single, width = 10, height = 6, dpi = 300)
cat("Gráfico Hazard Rate single panel salvo:", basename(nome_arquivo_hazard_single), "\n")

cat("\n=== GRÁFICOS WEIBULL DETALHADOS CONCLUÍDOS ===\n")
cat("Arquivos salvos em:", graficos_dir, "\n")
