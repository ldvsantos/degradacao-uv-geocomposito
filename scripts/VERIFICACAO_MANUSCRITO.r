#!/usr/bin/env Rscript
# ==============================================================================
# VERIFICAÇÃO TOTAL DO MANUSCRITO - Valores χ², Bartlett, Weibull, Bootstrap
# Script de auditoria para confirmar todos os valores reportados
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(fitdistrplus)
  library(car)
  library(boot)
})

# ==============================================================================
# 1. CARREGAR DADOS REAIS
# ==============================================================================

db_path <- file.path(getwd(), "dados", "DB.xlsx")
cat("=== CARREGANDO DB.xlsx ===\n")
cat("Path:", db_path, "\n")

# --- TRAÇÃO ---
tracao_raw <- read_excel(db_path, sheet = "TRACAO")
tracao <- tracao_raw %>%
  rename(
    tensao_max = `tensão máxima`,
    extensao_max = `extensão máxima`,
    repeticao = `repetição`
  ) %>%
  mutate(
    tempo = case_when(
      str_detect(tratamento, "CONTROLE|controle") ~ 0,
      str_detect(tratamento, "10JN") ~ 10,
      str_detect(tratamento, "20JN") ~ 20,
      str_detect(tratamento, "30JN") ~ 30,
      str_detect(tratamento, "40JN") ~ 40,
      str_detect(tratamento, "50JN") ~ 50,
      str_detect(tratamento, "60JN") ~ 60,
      str_detect(tratamento, "70JN") ~ 70,
      str_detect(tratamento, "80JN") ~ 80,
      str_detect(tratamento, "90JN") ~ 90,
      str_detect(tratamento, "100JN") ~ 100,
      str_detect(tratamento, "110JN") ~ 110,
      str_detect(tratamento, "120JN") ~ 120,
      TRUE ~ NA_real_
    ),
    tempo_fator = as.factor(tempo)
  )

# --- PUNÇÃO ---
puncao_raw <- read_excel(db_path, sheet = "PUNCAO")
puncao <- puncao_raw %>%
  rename(
    forca_puncao = `tensão máxima (N)`,
    deslocamento_puncao = `extensão máxima (mm)`,
    repeticao = `repetição`
  ) %>%
  mutate(
    tempo = case_when(
      str_detect(tratamento, "CONTROLE|controle") ~ 0,
      str_detect(tratamento, "10JN") ~ 10,
      str_detect(tratamento, "20JN") ~ 20,
      str_detect(tratamento, "30JN") ~ 30,
      str_detect(tratamento, "40JN") ~ 40,
      str_detect(tratamento, "50JN") ~ 50,
      str_detect(tratamento, "60JN") ~ 60,
      str_detect(tratamento, "70JN") ~ 70,
      str_detect(tratamento, "80JN") ~ 80,
      str_detect(tratamento, "90JN") ~ 90,
      str_detect(tratamento, "100JN") ~ 100,
      str_detect(tratamento, "110JN") ~ 110,
      str_detect(tratamento, "120JN") ~ 120,
      TRUE ~ NA_real_
    ),
    tempo_fator = as.factor(tempo)
  )

cat("\nTração: n =", nrow(tracao), "| Tempos:", paste(sort(unique(tracao$tempo)), collapse=", "), "\n")
cat("Punção: n =", nrow(puncao), "| Tempos:", paste(sort(unique(puncao$tempo)), collapse=", "), "\n")

# ==============================================================================
# 2. GLM GAMMA (LOG LINK) - REPRODUZIR χ² DO MANUSCRITO
# ==============================================================================

cat("\n" , strrep("=", 70), "\n")
cat("=== GLM GAMMA (LOG LINK) - VALORES χ² ===\n")
cat(strrep("=", 70), "\n")

# Remover NAs
tracao_clean <- tracao %>% filter(!is.na(tempo))
puncao_clean <- puncao %>% filter(!is.na(tempo))

# --- Tensão de tração ---
glm_tensao <- glm(tensao_max ~ tempo_fator, family = Gamma(link = "log"), data = tracao_clean)
anova_tensao <- anova(glm_tensao, test = "LRT")
chi2_tensao <- anova_tensao$Deviance[2]  
df_tensao <- anova_tensao$Df[2]
p_tensao <- anova_tensao$`Pr(>Chi)`[2]
cat("\nTensão de Tração (GLM Gamma, log):\n")
cat("  χ²(", df_tensao, ") =", round(chi2_tensao, 3), "\n")
cat("  p =", format(p_tensao, digits = 4), "\n")
cat("  Manuscrito diz: χ²(11) = 1.12, p = 0.148\n")

# --- Deformação de ruptura ---
# Estender a Deformação: precisamos de modulo Young = tensao/extensao
# Aqui, como o manuscrito reporta extensao_max diretamente
glm_deform <- glm(extensao_max ~ tempo_fator, family = Gamma(link = "log"), data = tracao_clean)
anova_deform <- anova(glm_deform, test = "LRT")
chi2_deform <- anova_deform$Deviance[2]
df_deform <- anova_deform$Df[2]
p_deform <- anova_deform$`Pr(>Chi)`[2]
cat("\nDeformação de Ruptura (GLM Gamma, log):\n")
cat("  χ²(", df_deform, ") =", round(chi2_deform, 3), "\n")
cat("  p =", format(p_deform, digits = 4), "\n")
cat("  Manuscrito diz: χ²(11) = 24.8, p < 0.001\n")

# --- Módulo de Young ---
# Young's modulus = tensao_max / extensao_max (simplified for kN/m scale)
tracao_clean$modulo_young <- tracao_clean$tensao_max / tracao_clean$extensao_max
# Remover infinitos e NAs
tracao_young <- tracao_clean %>% filter(is.finite(modulo_young) & modulo_young > 0)
glm_modulo <- glm(modulo_young ~ tempo_fator, family = Gamma(link = "log"), data = tracao_young)
anova_modulo <- anova(glm_modulo, test = "LRT")
chi2_modulo <- anova_modulo$Deviance[2]
df_modulo <- anova_modulo$Df[2]
p_modulo <- anova_modulo$`Pr(>Chi)`[2]
cat("\nMódulo de Young (GLM Gamma, log):\n")
cat("  χ²(", df_modulo, ") =", round(chi2_modulo, 3), "\n")
cat("  p =", format(p_modulo, digits = 4), "\n")
cat("  Manuscrito diz: χ²(11) = 3.560, p = 0.168\n")

# --- Força de Punção ---
glm_forca <- glm(forca_puncao ~ tempo_fator, family = Gamma(link = "log"), data = puncao_clean)
anova_forca <- anova(glm_forca, test = "LRT")
chi2_forca <- anova_forca$Deviance[2]
df_forca <- anova_forca$Df[2]
p_forca <- anova_forca$`Pr(>Chi)`[2]
cat("\nForça de Punção (GLM Gamma, log):\n")
cat("  LR χ²(", df_forca, ") =", round(chi2_forca, 3), "\n")
cat("  p =", format(p_forca, digits = 4), "\n")
cat("  Manuscrito diz: LR χ²(10) = 15.8, p = 0.072\n")

# --- Deslocamento de Punção ---
glm_desl <- glm(deslocamento_puncao ~ tempo_fator, family = Gamma(link = "log"), data = puncao_clean)
anova_desl <- anova(glm_desl, test = "LRT")
chi2_desl <- anova_desl$Deviance[2]
df_desl <- anova_desl$Df[2]
p_desl <- anova_desl$`Pr(>Chi)`[2]
cat("\nDeslocamento de Punção (GLM Gamma, log):\n")
cat("  LR χ²(", df_desl, ") =", round(chi2_desl, 3), "\n")
cat("  p =", format(p_desl, digits = 4), "\n")
cat("  Manuscrito diz: LR χ²(10) = 9.2, p = 0.514\n")

# ==============================================================================
# 3. ANOVA - REPRODUZIR p-values
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== ANOVA CLÁSSICA ===\n")
cat(strrep("=", 70), "\n")

anova_ts <- aov(tensao_max ~ tempo_fator, data = tracao_clean)
cat("\nTensão (ANOVA): p =", format(summary(anova_ts)[[1]]$`Pr(>F)`[1], digits=4), "\n")
cat("  CSV diz: p = 0.333\n")

anova_df <- aov(extensao_max ~ tempo_fator, data = tracao_clean)
cat("Deformação (ANOVA): p =", format(summary(anova_df)[[1]]$`Pr(>F)`[1], digits=4), "\n")
cat("  CSV diz: p = 0.00544\n")

anova_fp <- aov(forca_puncao ~ tempo_fator, data = puncao_clean)
cat("Força Punção (ANOVA): p =", format(summary(anova_fp)[[1]]$`Pr(>F)`[1], digits=4), "\n")
cat("  CSV diz: p = 0.131\n")

anova_dp <- aov(deslocamento_puncao ~ tempo_fator, data = puncao_clean)
cat("Desl. Punção (ANOVA): p =", format(summary(anova_dp)[[1]]$`Pr(>F)`[1], digits=4), "\n")
cat("  CSV diz: p = 0.532\n")

# ==============================================================================
# 4. BARTLETT TEST
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== TESTES DE BARTLETT ===\n")
cat(strrep("=", 70), "\n")

# Tração
bart_tracao <- bartlett.test(tensao_max ~ tempo_fator, data = tracao_clean)
cat("\nBartlett Tração: K² =", round(bart_tracao$statistic, 3), 
    ", df =", bart_tracao$parameter, 
    ", p =", round(bart_tracao$p.value, 3), "\n")
cat("  Manuscrito diz: K² = 3.742, df = 4, p = 0.442\n")

# Punção - Força
bart_puncao <- bartlett.test(forca_puncao ~ tempo_fator, data = puncao_clean)
cat("\nBartlett Punção (Força): K² =", round(bart_puncao$statistic, 3), 
    ", df =", bart_puncao$parameter, 
    ", p =", round(bart_puncao$p.value, 3), "\n")
cat("  Manuscrito diz: K² = 12.43, df = 10, p = 0.258\n")

# ==============================================================================
# 5. WEIBULL MLE - RECALCULAR COM DADOS REAIS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== WEIBULL MLE (DADOS REAIS DB.xlsx) ===\n")
cat(strrep("=", 70), "\n")

# --- Tensão ---
fit_ts <- fitdist(tracao_clean$tensao_max, "weibull", method = "mle")
cat("\nTensão de Tração Weibull MLE:\n")
cat("  β =", round(fit_ts$estimate["shape"], 4), "\n")
cat("  η =", round(fit_ts$estimate["scale"], 4), "\n")
cat("  Manuscrito diz: β = 3.403, η = 19.93 kN/m (NOTA: manuscrito usa kN/m, DB.xlsx pode usar N)\n")

# --- Deformação ---
deform_valid <- tracao_clean$extensao_max[tracao_clean$extensao_max > 0]
fit_df <- fitdist(deform_valid, "weibull", method = "mle")
cat("\nDeformação de Ruptura Weibull MLE:\n")
cat("  β =", round(fit_df$estimate["shape"], 4), "\n")
cat("  η =", round(fit_df$estimate["scale"], 4), "\n")
cat("  Manuscrito diz: β = 2.640, η = 23.46%\n")

# --- Punção Força ---
fit_fp <- fitdist(puncao_clean$forca_puncao, "weibull", method = "mle")
cat("\nForça de Punção Weibull MLE:\n")
cat("  β =", round(fit_fp$estimate["shape"], 4), "\n")
cat("  η =", round(fit_fp$estimate["scale"], 4), "\n")
cat("  Manuscrito diz: β = 4.462, η = 1784.19 N\n")

# --- Punção Deslocamento ---
desl_valid <- puncao_clean$deslocamento_puncao[puncao_clean$deslocamento_puncao > 0]
fit_dp <- fitdist(desl_valid, "weibull", method = "mle")
cat("\nDeslocamento de Punção Weibull MLE:\n")
cat("  β =", round(fit_dp$estimate["shape"], 4), "\n")
cat("  η =", round(fit_dp$estimate["scale"], 4), "\n")
cat("  Manuscrito diz: β = 5.351, η = 13.618 mm\n")

# ==============================================================================
# 6. BOOTSTRAP - PUNÇÃO POR TEMPO
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== BOOTSTRAP PUNÇÃO POR TEMPO (R=1000) ===\n")
cat(strrep("=", 70), "\n")

set.seed(42)  # Reproducibilidade
tempos_puncao <- sort(unique(puncao_clean$tempo))

for (t in tempos_puncao) {
  dados_t <- puncao_clean$forca_puncao[puncao_clean$tempo == t]
  if (length(dados_t) >= 2) {
    boot_func <- function(data, indices) mean(data[indices])
    boot_result <- boot(dados_t, boot_func, R = 1000)
    ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
    cat(sprintf("Tempo %3d: n=%d, Média=%.2f N, IC95%% [%.2f - %.2f]\n",
                t, length(dados_t), mean(dados_t), 
                ci$percent[4], ci$percent[5]))
  }
}
cat("\nManuscrito diz: Controle=1416.69, 10cycles=1529.52, 30cycles=2014.91 N\n")

# ==============================================================================
# 7. BOOTSTRAP - TRAÇÃO POR TEMPO
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== BOOTSTRAP TRAÇÃO POR TEMPO (R=1000) ===\n")
cat(strrep("=", 70), "\n")

tempos_tracao <- sort(unique(tracao_clean$tempo))

for (t in tempos_tracao) {
  dados_t <- tracao_clean$tensao_max[tracao_clean$tempo == t]
  if (length(dados_t) >= 2) {
    boot_func <- function(data, indices) mean(data[indices])
    boot_result <- boot(dados_t, boot_func, R = 1000)
    ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
    cat(sprintf("Tempo %3d: n=%d, Média=%.2f N, IC95%% [%.2f - %.2f]\n",
                t, length(dados_t), mean(dados_t),
                ci$percent[4], ci$percent[5]))
  }
}
cat("\nManuscrito diz: 10c=588.80, 20c=592.48, 30c=429.53, 60c=554.87, 90c=654.21 N\n")

# ==============================================================================
# 8. CV POR TEMPO - PUNÇÃO
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== CV (%) POR TEMPO - PUNÇÃO ===\n")
cat(strrep("=", 70), "\n")

cv_puncao <- puncao_clean %>%
  group_by(tempo) %>%
  summarise(
    n = n(),
    media = mean(forca_puncao),
    dp = sd(forca_puncao),
    cv = sd(forca_puncao) / mean(forca_puncao) * 100,
    .groups = "drop"
  )

for (i in 1:nrow(cv_puncao)) {
  cat(sprintf("Tempo %3d: n=%d, Média=%.2f, DP=%.2f, CV=%.2f%%\n",
              cv_puncao$tempo[i], cv_puncao$n[i], cv_puncao$media[i], 
              cv_puncao$dp[i], cv_puncao$cv[i]))
}
cat("\nManuscrito diz: CV mínimo = 8.67% aos 90 ciclos\n")

# ==============================================================================
# 9. CV POR TEMPO - TRAÇÃO
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== CV (%) POR TEMPO - TRAÇÃO ===\n")
cat(strrep("=", 70), "\n")

cv_tracao <- tracao_clean %>%
  group_by(tempo) %>%
  summarise(
    n = n(),
    media = mean(tensao_max),
    dp = sd(tensao_max),
    cv = sd(tensao_max) / mean(tensao_max) * 100,
    .groups = "drop"
  )

for (i in 1:nrow(cv_tracao)) {
  cat(sprintf("Tempo %3d: n=%d, Média=%.2f, DP=%.2f, CV=%.2f%%\n",
              cv_tracao$tempo[i], cv_tracao$n[i], cv_tracao$media[i], 
              cv_tracao$dp[i], cv_tracao$cv[i]))
}
cat("\nManuscrito diz: CV mínimo 14.2% (60c), máximo 36.6% (20c)\n")

cat("\n", strrep("=", 70), "\n")
cat("=== VERIFICAÇÃO COMPLETA ===\n")
cat(strrep("=", 70), "\n")
