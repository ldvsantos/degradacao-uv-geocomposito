#!/usr/bin/env Rscript
# ==============================================================================
# VERIFICAÇÃO V2 — car::Anova(type="II") como no script original
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(fitdistrplus)
  library(car)
  library(boot)
})

db_path <- file.path(getwd(), "dados", "DB.xlsx")
cat("=== CARREGANDO DB.xlsx ===\n")

# --- TRAÇÃO (mesma lógica de analise_temporal_tracao.R) ---
tracao_raw <- read_excel(db_path, sheet = "TRACAO")
tracao <- tracao_raw %>%
  rename(
    tensao_tracao = `tensão máxima`,
    deformacao_ruptura = `extensão máxima`,
    repeticao = `repetição`
  ) %>%
  mutate(
    tempo_dias = case_when(
      str_detect(tratamento, "CONTROLE") ~ 0,
      str_detect(tratamento, "120JN") ~ 120,
      str_detect(tratamento, "110JN") ~ 110,
      str_detect(tratamento, "100JN") ~ 100,
      str_detect(tratamento, "10JN") ~ 10,
      str_detect(tratamento, "20JN") ~ 20,
      str_detect(tratamento, "30JN") ~ 30,
      str_detect(tratamento, "40JN") ~ 40,
      str_detect(tratamento, "50JN") ~ 50,
      str_detect(tratamento, "60JN") ~ 60,
      str_detect(tratamento, "70JN") ~ 70,
      str_detect(tratamento, "80JN") ~ 80,
      str_detect(tratamento, "90JN") ~ 90,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(tempo_dias)) %>%
  mutate(
    tempo_factor = factor(tempo_dias),
    # Simular modulo_young como no script original
    modulo_young = tensao_tracao * runif(n(), 50, 80) + rnorm(n(), 0, 500)
  )

set.seed(123) # Mesmo seed do script original!
tracao$modulo_young <- tracao$tensao_tracao * runif(nrow(tracao), 50, 80) + rnorm(nrow(tracao), 0, 500)

# --- PUNÇÃO ---
puncao_raw <- read_excel(db_path, sheet = "PUNCAO")
puncao <- puncao_raw %>%
  rename(
    forca_puncao = `tensão máxima (N)`,
    deslocamento_puncao = `extensão máxima (mm)`,
    repeticao = `repetição`
  ) %>%
  mutate(
    tempo_dias = case_when(
      str_detect(tratamento, "CONTROLE") ~ 0,
      str_detect(tratamento, "120JN") ~ 120,
      str_detect(tratamento, "110JN") ~ 110,
      str_detect(tratamento, "100JN") ~ 100,
      str_detect(tratamento, "10JN") ~ 10,
      str_detect(tratamento, "20JN") ~ 20,
      str_detect(tratamento, "30JN") ~ 30,
      str_detect(tratamento, "40JN") ~ 40,
      str_detect(tratamento, "50JN") ~ 50,
      str_detect(tratamento, "60JN") ~ 60,
      str_detect(tratamento, "70JN") ~ 70,
      str_detect(tratamento, "80JN") ~ 80,
      str_detect(tratamento, "90JN") ~ 90,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(tempo_dias)) %>%
  mutate(tempo_factor = factor(tempo_dias))

cat("\n=== DADOS CARREGADOS ===\n")
cat("Tração: n =", nrow(tracao), "\n")
cat("  Tempos:", paste(sort(unique(tracao$tempo_dias)), collapse=", "), "\n")
cat("  n/tempo:", as.character(table(tracao$tempo_factor)), "\n")
cat("Punção: n =", nrow(puncao), "\n")
cat("  Tempos:", paste(sort(unique(puncao$tempo_dias)), collapse=", "), "\n")
cat("  n/tempo:", as.character(table(puncao$tempo_factor)), "\n")

# ==============================================================================
# GLM GAMMA (LOG LINK) + car::Anova(type="II") — METODOLOGIA ORIGINAL
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== GLM Gamma + car::Anova(type='II') ===\n")
cat(strrep("=", 70), "\n")

# --- Tensão ---
glm_ts <- glm(tensao_tracao ~ tempo_factor, family = Gamma(link = "log"), data = tracao)
anova_ts <- car::Anova(glm_ts, type = "II")
cat("\nTensão de tração:\n")
print(anova_ts)
cat("  Manuscrito: χ²(11) = 1.12, p = 0.148\n")
cat("  CSV:        p_glm = 0.148\n")

# --- Deformação ---
tracao_deform <- tracao %>% filter(!is.na(deformacao_ruptura) & deformacao_ruptura > 0)
glm_df <- glm(deformacao_ruptura ~ tempo_factor, family = Gamma(link = "log"), data = tracao_deform)
anova_df <- car::Anova(glm_df, type = "II")
cat("\nDeformação de ruptura:\n")
print(anova_df)
cat("  Manuscrito: χ²(11) = 24.8, p < 0.001 (= 2.07e-08)\n")
cat("  CSV:        p_glm = 2.07e-08\n")

# --- Módulo de Young (simulado como no original) ---
tracao_mod <- tracao %>% filter(!is.na(modulo_young) & modulo_young > 0)
glm_my <- glm(modulo_young ~ tempo_factor, family = Gamma(link = "log"), data = tracao_mod)
anova_my <- car::Anova(glm_my, type = "II")
cat("\nMódulo de Young (simulado):\n")
print(anova_my)
cat("  Manuscrito: χ²(11) = 3.560, p = 0.168\n")
cat("  CSV:        p_glm = 0.168\n")

# --- Força de Punção ---
glm_fp <- glm(forca_puncao ~ tempo_factor, family = Gamma(link = "log"), data = puncao)
anova_fp <- car::Anova(glm_fp, type = "II")
cat("\nForça de punção:\n")
print(anova_fp)
cat("  Manuscrito: LR χ²(10) = 15.8, p = 0.072\n")
cat("  CSV:        p_glm = 0.072\n")

# --- Deslocamento ---
puncao_desl <- puncao %>% filter(!is.na(deslocamento_puncao) & deslocamento_puncao > 0)
glm_dp <- glm(deslocamento_puncao ~ tempo_factor, family = Gamma(link = "log"), data = puncao_desl)
anova_dp <- car::Anova(glm_dp, type = "II")
cat("\nDeslocamento de punção:\n")
print(anova_dp)
cat("  Manuscrito: LR χ²(10) = 9.2, p = 0.514\n")
cat("  CSV:        p_glm = 0.514\n")

# ==============================================================================
# ANOVA CLÁSSICA
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== ANOVA CLÁSSICA (F-test) ===\n")
cat(strrep("=", 70), "\n")

p_anova_ts <- summary(aov(tensao_tracao ~ tempo_factor, data=tracao))[[1]]$`Pr(>F)`[1]
p_anova_df <- summary(aov(deformacao_ruptura ~ tempo_factor, data=tracao_deform))[[1]]$`Pr(>F)`[1]
p_anova_fp <- summary(aov(forca_puncao ~ tempo_factor, data=puncao))[[1]]$`Pr(>F)`[1]
p_anova_dp <- summary(aov(deslocamento_puncao ~ tempo_factor, data=puncao_desl))[[1]]$`Pr(>F)`[1]

cat(sprintf("Tensão      : p_anova = %.6f   (CSV: 0.333)\n", p_anova_ts))
cat(sprintf("Deformação  : p_anova = %.6f   (CSV: 0.00544)\n", p_anova_df))
cat(sprintf("Força Punção: p_anova = %.6f   (CSV: 0.131)\n", p_anova_fp))
cat(sprintf("Desl. Punção: p_anova = %.6f   (CSV: 0.532)\n", p_anova_dp))

# ==============================================================================
# BARTLETT
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== BARTLETT ===\n")
cat(strrep("=", 70), "\n")

bart_fp <- bartlett.test(forca_puncao ~ tempo_factor, data = puncao)
cat("\nBartlett Punção (Força):\n")
cat(sprintf("  K² = %.3f, df = %d, p = %.4f\n", bart_fp$statistic, bart_fp$parameter, bart_fp$p.value))
cat("  Manuscrito: K² = 12.43, df = 10, p = 0.258\n")

bart_ts <- bartlett.test(tensao_tracao ~ tempo_factor, data = tracao)
cat("\nBartlett Tração (Tensão):\n")
cat(sprintf("  K² = %.3f, df = %d, p = %.4f\n", bart_ts$statistic, bart_ts$parameter, bart_ts$p.value))
cat("  Manuscrito: K² = 3.742, df = 4, p = 0.442\n")

# ==============================================================================
# WEIBULL MLE — DADOS REAIS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== WEIBULL MLE ===\n")
cat(strrep("=", 70), "\n")

# Tensão
tensao_valid <- tracao$tensao_tracao[!is.na(tracao$tensao_tracao) & tracao$tensao_tracao > 0]
cat("\nTensão: n =", length(tensao_valid), "valores válidos\n")
fit_ts <- fitdist(tensao_valid, "weibull", method = "mle")
cat(sprintf("  β = %.4f, η = %.4f\n", fit_ts$estimate["shape"], fit_ts$estimate["scale"]))
cat("  Manuscrito: β = 3.403, η = 19.93 kN/m\n")

# Deformação
deform_valid <- tracao$deformacao_ruptura[!is.na(tracao$deformacao_ruptura) & tracao$deformacao_ruptura > 0]
cat("\nDeformação: n =", length(deform_valid), "valores válidos\n")
fit_df <- fitdist(deform_valid, "weibull", method = "mle")
cat(sprintf("  β = %.4f, η = %.4f\n", fit_df$estimate["shape"], fit_df$estimate["scale"]))
cat("  Manuscrito: β = 2.640, η = 23.46%\n")

# Punção Força
forca_valid <- puncao$forca_puncao[!is.na(puncao$forca_puncao) & puncao$forca_puncao > 0]
cat("\nPunção (Força): n =", length(forca_valid), "valores válidos\n")
fit_fp <- fitdist(forca_valid, "weibull", method = "mle")
cat(sprintf("  β = %.4f, η = %.4f\n", fit_fp$estimate["shape"], fit_fp$estimate["scale"]))
cat("  Manuscrito: β = 4.462, η = 1784.19 N\n")

# Punção Deslocamento
desl_valid <- puncao$deslocamento_puncao[!is.na(puncao$deslocamento_puncao) & puncao$deslocamento_puncao > 0]
cat("\nPunção (Deslocamento): n =", length(desl_valid), "valores válidos\n")
fit_dp <- fitdist(desl_valid, "weibull", method = "mle")
cat(sprintf("  β = %.4f, η = %.4f\n", fit_dp$estimate["shape"], fit_dp$estimate["scale"]))
cat("  Manuscrito: β = 5.351, η = 13.618 mm\n")

# ==============================================================================
# BOOTSTRAP PUNÇÃO POR TEMPO (R=1000)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== BOOTSTRAP PUNÇÃO (R=1000) ===\n")
cat(strrep("=", 70), "\n")

set.seed(42)
for (t in sort(unique(puncao$tempo_dias))) {
  dados_t <- puncao$forca_puncao[puncao$tempo_dias == t & !is.na(puncao$forca_puncao)]
  if (length(dados_t) >= 2) {
    boot_result <- boot(dados_t, function(d, i) mean(d[i]), R = 1000)
    ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
    cat(sprintf("Tempo %3d: n=%d, Média=%.2f N, IC95%% [%.2f - %.2f]\n",
                t, length(dados_t), mean(dados_t), ci$percent[4], ci$percent[5]))
  }
}
cat("  Manuscrito: Controle=1416.69, 10c=1529.52, 30c=2014.91 N\n")

# ==============================================================================
# BOOTSTRAP TRAÇÃO POR TEMPO (R=1000)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== BOOTSTRAP TRAÇÃO (R=1000) ===\n")
cat(strrep("=", 70), "\n")

set.seed(42)
for (t in sort(unique(tracao$tempo_dias))) {
  dados_t <- tracao$tensao_tracao[tracao$tempo_dias == t & !is.na(tracao$tensao_tracao)]
  if (length(dados_t) >= 2) {
    boot_result <- boot(dados_t, function(d, i) mean(d[i]), R = 1000)
    ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
    cat(sprintf("Tempo %3d: n=%d, Média=%.2f, IC95%% [%.2f - %.2f]\n",
                t, length(dados_t), mean(dados_t), ci$percent[4], ci$percent[5]))
  }
}

# ==============================================================================
# CV POR TEMPO
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("=== CV PUNÇÃO ===\n")
cat(strrep("=", 70), "\n")

puncao %>%
  group_by(tempo_dias) %>%
  summarise(n=n(), media=mean(forca_puncao, na.rm=T), dp=sd(forca_puncao, na.rm=T),
            cv=sd(forca_puncao, na.rm=T)/mean(forca_puncao, na.rm=T)*100, .groups="drop") %>%
  arrange(tempo_dias) %>%
  {for(i in 1:nrow(.)) cat(sprintf("Tempo %3d: n=%d, M=%.2f, DP=%.2f, CV=%.2f%%\n",
    .$tempo_dias[i], .$n[i], .$media[i], .$dp[i], .$cv[i])); .} %>% invisible()

cat("\n", strrep("=", 70), "\n")
cat("=== CV TRAÇÃO ===\n")
cat(strrep("=", 70), "\n")

tracao %>%
  group_by(tempo_dias) %>%
  summarise(n=n(), media=mean(tensao_tracao, na.rm=T), dp=sd(tensao_tracao, na.rm=T),
            cv=sd(tensao_tracao, na.rm=T)/mean(tensao_tracao, na.rm=T)*100, .groups="drop") %>%
  arrange(tempo_dias) %>%
  {for(i in 1:nrow(.)) cat(sprintf("Tempo %3d: n=%d, M=%.2f, DP=%.2f, CV=%.2f%%\n",
    .$tempo_dias[i], .$n[i], .$media[i], .$dp[i], .$cv[i])); .} %>% invisible()

cat("\n", strrep("=", 70), "\n")
cat("=== VERIFICAÇÃO V2 COMPLETA ===\n")
