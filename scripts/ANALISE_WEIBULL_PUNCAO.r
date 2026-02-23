# ===============================================================================
# ANÁLISE DE WEIBULL PARA DADOS DE PUNÇÃO
# Análise de Confiabilidade para Ensaios de Punção
# Data: 14 de outubro de 2025
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

library(fitdistrplus)
library(survival)
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)

# ===============================================================================
# 2. CARREGAMENTO DOS DADOS DE PUNÇÃO
# ===============================================================================

cat("=== ANÁLISE DE WEIBULL PARA PUNÇÃO ===\n")

file_path <- file.path(getwd(), 'dados', 'DB.xlsx')
dados_puncao_raw <- readxl::read_excel(file_path, sheet = "PUNCAO")

# Preparar dados
dados_puncao <- dados_puncao_raw %>%
  rename(
    forca_puncao = `tensão máxima (N)`,
    deslocamento_puncao = `extensão máxima (mm)`
  ) %>%
  filter(!is.na(forca_puncao))

# ===============================================================================
# 3. ANÁLISE DE WEIBULL PARA FORÇA DE PUNÇÃO
# ===============================================================================

cat("\n=== ANÁLISE DE WEIBULL - FORÇA DE PUNÇÃO ===\n")

# Ajuste da distribuição de Weibull para força de punção
fit_weibull_forca <- fitdist(dados_puncao$forca_puncao, "weibull", method = "mle")

# Parâmetros estimados
shape_forca <- fit_weibull_forca$estimate["shape"]
scale_forca <- fit_weibull_forca$estimate["scale"]

cat("Parâmetros da distribuição de Weibull - Força de Punção:\n")
cat("Forma (β):", round(shape_forca, 4), "\n")
cat("Escala (η):", round(scale_forca, 4), "\n")

# Estatísticas de vida útil
vida_media_forca <- scale_forca * gamma(1 + 1/shape_forca)
vida_mediana_forca <- scale_forca * (log(2))^(1/shape_forca)

cat("Vida média:", round(vida_media_forca, 2), "N\n")
cat("Vida mediana:", round(vida_mediana_forca, 2), "N\n")

# Percentis de interesse
p10_forca <- qweibull(0.1, shape = shape_forca, scale = scale_forca)
p90_forca <- qweibull(0.9, shape = shape_forca, scale = scale_forca)

cat("P10 (10% de falha):", round(p10_forca, 2), "N\n")
cat("P90 (90% de falha):", round(p90_forca, 2), "N\n")

# ===============================================================================
# 4. ANÁLISE DE WEIBULL PARA DESLOCAMENTO NA PUNÇÃO
# ===============================================================================

cat("\n=== ANÁLISE DE WEIBULL - DESLOCAMENTO NA PUNÇÃO ===\n")

# Ajuste da distribuição de Weibull para deslocamento na punção
fit_weibull_desl <- fitdist(dados_puncao$deslocamento_puncao, "weibull", method = "mle")

# Parâmetros estimados
shape_desl <- fit_weibull_desl$estimate["shape"]
scale_desl <- fit_weibull_desl$estimate["scale"]

cat("Parâmetros da distribuição de Weibull - Deslocamento na Punção:\n")
cat("Forma (β):", round(shape_desl, 4), "\n")
cat("Escala (η):", round(scale_desl, 4), "\n")

# Estatísticas de vida útil
vida_media_desl <- scale_desl * gamma(1 + 1/shape_desl)
vida_mediana_desl <- scale_desl * (log(2))^(1/shape_desl)

cat("Vida média:", round(vida_media_desl, 2), "mm\n")
cat("Vida mediana:", round(vida_mediana_desl, 2), "mm\n")

# Percentis de interesse
p10_desl <- qweibull(0.1, shape = shape_desl, scale = scale_desl)
p90_desl <- qweibull(0.9, shape = shape_desl, scale = scale_desl)

cat("P10 (10% de falha):", round(p10_desl, 2), "mm\n")
cat("P90 (90% de falha):", round(p90_desl, 2), "mm\n")

# ===============================================================================
# 5. GRÁFICOS DE CONFIABILIDADE PARA PUNÇÃO
# ===============================================================================

# Função de confiabilidade para força de punção
x_forca <- seq(0, max(dados_puncao$forca_puncao) * 1.2, length.out = 1000)
reliability_forca <- 1 - pweibull(x_forca, shape = shape_forca, scale = scale_forca)

# Taxa de falha para força de punção
hazard_forca <- (shape_forca/scale_forca) * (x_forca/scale_forca)^(shape_forca-1)

grafico_reliability_forca <- ggplot() +
  geom_line(aes(x = x_forca, y = reliability_forca), color = '#56B4E9', size = 1.5) +
  geom_vline(xintercept = p10_forca, linetype = 'dashed', color = '#E69F00', size = 1, show.legend = TRUE) +
  geom_vline(xintercept = p90_forca, linetype = 'dotted', color = '#D55E00', size = 1, show.legend = TRUE) +
  annotate('text', x = max(x_forca)*0.7, y = 0.8, label = paste('β =', round(shape_forca,2),'| η =', round(scale_forca,1)), size = 5, family = 'serif', color = '#222222') +
  labs(title = 'Reliability Function - Puncture Force', x = 'Puncture Force (N)', y = 'Reliability R(t)') +
  theme_classic(base_family = 'serif') +
  ylim(0, 1)

grafico_hazard_forca <- ggplot() +
  geom_line(aes(x = x_forca, y = hazard_forca), color = '#009E73', size = 1.5) +
  annotate('text', x = max(x_forca)*0.7, y = max(hazard_forca)*0.8, label = paste('β =', round(shape_forca,2),'| η =', round(scale_forca,1)), size = 5, family = 'serif', color = '#222222') +
  labs(title = 'Hazard Rate - Puncture Force', x = 'Puncture Force (N)', y = 'Hazard Rate h(t)') +
  theme_classic(base_family = 'serif')

qq_data_forca <- data.frame(
  theoretical = qweibull(ppoints(length(dados_puncao$forca_puncao)), shape = shape_forca, scale = scale_forca),
  empirical = sort(dados_puncao$forca_puncao)
)
grafico_qq_forca <- ggplot(qq_data_forca, aes(x = theoretical, y = empirical)) +
  geom_point(color = '#E69F00', size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = '#222222', linetype = 'dashed', size = 1) +
  labs(title = 'QQ-Plot - Puncture Force', x = 'Theoretical Quantiles (Weibull)', y = 'Empirical Quantiles') +
  theme_classic(base_family = 'serif')

# Repetir para deslocamento na punção
x_desl <- seq(0, max(dados_puncao$deslocamento_puncao) * 1.2, length.out = 1000)
reliability_desl <- 1 - pweibull(x_desl, shape = shape_desl, scale = scale_desl)
hazard_desl <- (shape_desl/scale_desl) * (x_desl/scale_desl)^(shape_desl-1)

graficos_dir <- file.path(getwd(), 'graficos', 'puncao')
dir.create(graficos_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(graficos_dir, 'probabilidade_ruptura_forca_puncao_reliability_functions.png'), 
  grafico_reliability_forca, width = 10, height = 6, dpi = 300)
ggsave(file.path(graficos_dir, 'probabilidade_ruptura_forca_puncao_hazard_rate.png'), 
  grafico_hazard_forca, width = 10, height = 6, dpi = 300)
ggsave(file.path(graficos_dir, 'probabilidade_ruptura_forca_puncao_qq_plot.png'), 
  grafico_qq_forca, width = 10, height = 6, dpi = 300)

# ===============================================================================
# 6. SALVAR RESULTADOS
# ===============================================================================

# Criar tabela resumo
resumo_weibull_puncao <- data.frame(
  Variavel = c("Força de Punção", "Deslocamento na Punção"),
  Forma_Beta = c(shape_forca, shape_desl),
  Escala_Eta = c(scale_forca, scale_desl),
  Vida_Media = c(vida_media_forca, vida_media_desl),
  Vida_Mediana = c(vida_mediana_forca, vida_mediana_desl),
  P10 = c(p10_forca, p10_desl),
  P90 = c(p90_forca, p90_desl)
)

print(resumo_weibull_puncao)

dados_dir <- file.path(getwd(), 'dados')
save.image(file.path(dados_dir, 'analise_weibull_puncao_resultados.RData'))

cat("\n=== ANÁLISE DE WEIBULL PARA PUNÇÃO CONCLUÍDA ===\n")