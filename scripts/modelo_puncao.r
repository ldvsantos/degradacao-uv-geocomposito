##############################################################################
# modelo_puncao.r
# Objetivo: Ajustar múltiplos modelos de regressão à força de punção
#           em função dos ciclos de exposição (dados reais, DB.xlsx)
#           e selecionar o melhor modelo (incluindo exponencial modificado).
##############################################################################

library(readxl)
library(dplyr)

# --- Carregar dados reais ---
db <- read_excel("dados/DB.xlsx", sheet = "PUNCAO")
names(db) <- c("tratamento", "repeticao", "forca", "deslocamento")

# Extrair ciclos numéricos
db$ciclos <- as.numeric(gsub("[^0-9]", "", db$tratamento))
db$ciclos[grepl("CONTROLE|Control", db$tratamento, ignore.case = TRUE)] <- 0

cat("========================================\n")
cat("DADOS DE PUNÇÃO (DB.xlsx)\n")
cat("========================================\n")
cat("n total:", nrow(db), "\n")
cat("Ciclos disponíveis:", paste(sort(unique(db$ciclos)), collapse=", "), "\n\n")

# Médias por ciclo
medias <- db %>% group_by(ciclos) %>% 
  summarise(media = mean(forca, na.rm=TRUE), 
            sd = sd(forca, na.rm=TRUE),
            n = n(), .groups="drop")
cat("Médias por ciclo:\n")
print(as.data.frame(medias))
cat("\n")

# --- Modelo 1: Polinomial quadrático ---
cat("========================================\n")
cat("MODELO 1: Polinomial quadrático\n")
cat("========================================\n")
m1 <- lm(forca ~ poly(ciclos, 2, raw = TRUE), data = db)
s1 <- summary(m1)
cat("F(", s1$fstatistic[2], ",", s1$fstatistic[3], ") =", 
    round(s1$fstatistic[1], 4), "\n")
cat("R² =", round(s1$r.squared, 4), "\n")
cat("R² adj =", round(s1$adj.r.squared, 4), "\n")
cat("p =", round(pf(s1$fstatistic[1], s1$fstatistic[2], s1$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m1), 2), "\n\n")

# --- Modelo 2: Log-linear (ln(ciclos+1)) ---
cat("========================================\n")
cat("MODELO 2: Log-linear ln(ciclos+1)\n")
cat("========================================\n")
m2 <- lm(forca ~ log(ciclos + 1), data = db)
s2 <- summary(m2)
cat("F(", s2$fstatistic[2], ",", s2$fstatistic[3], ") =", 
    round(s2$fstatistic[1], 4), "\n")
cat("R² =", round(s2$r.squared, 4), "\n")
cat("R² adj =", round(s2$adj.r.squared, 4), "\n")
cat("p =", round(pf(s2$fstatistic[1], s2$fstatistic[2], s2$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m2), 2), "\n\n")

# --- Modelo 3: Log-quadrático ---
cat("========================================\n")
cat("MODELO 3: Log-quadrático ln(ciclos+1) + [ln(ciclos+1)]²\n")
cat("========================================\n")
db$log_ciclos <- log(db$ciclos + 1)
m3 <- lm(forca ~ log_ciclos + I(log_ciclos^2), data = db)
s3 <- summary(m3)
cat("F(", s3$fstatistic[2], ",", s3$fstatistic[3], ") =", 
    round(s3$fstatistic[1], 4), "\n")
cat("R² =", round(s3$r.squared, 4), "\n")
cat("R² adj =", round(s3$adj.r.squared, 4), "\n")
cat("p =", round(pf(s3$fstatistic[1], s3$fstatistic[2], s3$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m3), 2), "\n\n")

# --- Modelo 4: Exponencial modificado (NLS) y = a * (1 - exp(-b * x)) + c ---
cat("========================================\n")
cat("MODELO 4: Exponencial modificado y = a*(1-exp(-b*x)) + c\n")
cat("========================================\n")
tryCatch({
  m4 <- nls(forca ~ a * (1 - exp(-b * ciclos)) + c_par, 
            data = db,
            start = list(a = 300, b = 0.05, c_par = 1400),
            control = nls.control(maxiter = 500, tol = 1e-6))
  s4 <- summary(m4)
  ss_res <- sum(residuals(m4)^2)
  ss_tot <- sum((db$forca - mean(db$forca))^2)
  r2_m4 <- 1 - ss_res/ss_tot
  n <- nrow(db)
  p <- 3  # number of parameters
  r2_adj_m4 <- 1 - (1 - r2_m4) * (n - 1) / (n - p - 1)
  f_m4 <- ((ss_tot - ss_res) / (p - 1)) / (ss_res / (n - p))
  p_val_m4 <- pf(f_m4, p - 1, n - p, lower.tail = FALSE)
  
  cat("Parâmetros:\n")
  print(coef(m4))
  cat("\nR² =", round(r2_m4, 4), "\n")
  cat("R² adj =", round(r2_adj_m4, 4), "\n")
  cat("F(", p-1, ",", n-p, ") =", round(f_m4, 4), "\n")
  cat("p =", round(p_val_m4, 6), "\n")
  cat("AIC =", round(AIC(m4), 2), "\n")
  cat("RSS =", round(ss_res, 2), "\n\n")
}, error = function(e) {
  cat("NLS não convergiu:", e$message, "\n\n")
})

# --- Modelo 5: Exponencial puro y = a * exp(b * x) ---
cat("========================================\n")
cat("MODELO 5: Exponencial puro y = a*exp(b*x)\n")
cat("========================================\n")
tryCatch({
  m5 <- nls(forca ~ a * exp(b * ciclos), 
            data = db,
            start = list(a = 1400, b = 0.001),
            control = nls.control(maxiter = 500))
  s5 <- summary(m5)
  ss_res5 <- sum(residuals(m5)^2)
  ss_tot5 <- sum((db$forca - mean(db$forca))^2)
  r2_m5 <- 1 - ss_res5/ss_tot5
  n <- nrow(db)
  p5 <- 2
  f_m5 <- ((ss_tot5 - ss_res5) / (p5 - 1)) / (ss_res5 / (n - p5))
  p_val_m5 <- pf(f_m5, p5-1, n-p5, lower.tail = FALSE)
  
  cat("Parâmetros:\n")
  print(coef(m5))
  cat("\nR² =", round(r2_m5, 4), "\n")
  cat("F(", p5-1, ",", n-p5, ") =", round(f_m5, 4), "\n")
  cat("p =", round(p_val_m5, 6), "\n")
  cat("AIC =", round(AIC(m5), 2), "\n\n")
}, error = function(e) {
  cat("NLS não convergiu:", e$message, "\n\n")
})

# --- Modelo 6: Raiz quadrada y = a + b*sqrt(x) ---
cat("========================================\n")
cat("MODELO 6: Raiz quadrada y = a + b*sqrt(ciclos)\n")
cat("========================================\n")
m6 <- lm(forca ~ sqrt(ciclos), data = db)
s6 <- summary(m6)
cat("F(", s6$fstatistic[2], ",", s6$fstatistic[3], ") =", 
    round(s6$fstatistic[1], 4), "\n")
cat("R² =", round(s6$r.squared, 4), "\n")
cat("R² adj =", round(s6$adj.r.squared, 4), "\n")
cat("p =", round(pf(s6$fstatistic[1], s6$fstatistic[2], s6$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m6), 2), "\n\n")

# --- Modelo 7: Potência y = a * x^b (somente ciclos > 0) ---
cat("========================================\n")
cat("MODELO 7: Potência log(y) = log(a) + b*log(x) [ciclos > 0]\n")
cat("========================================\n")
db_pos <- db %>% filter(ciclos > 0)
m7 <- lm(log(forca) ~ log(ciclos), data = db_pos)
s7 <- summary(m7)
cat("F(", s7$fstatistic[2], ",", s7$fstatistic[3], ") =", 
    round(s7$fstatistic[1], 4), "\n")
cat("R² (log-space) =", round(s7$r.squared, 4), "\n")
cat("p =", round(pf(s7$fstatistic[1], s7$fstatistic[2], s7$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m7), 2), "\n\n")

# --- Modelo 8: Gamma GLM com link log ---
cat("========================================\n")
cat("MODELO 8: Gamma GLM (log link) ~ ciclos\n")
cat("========================================\n")
m8 <- glm(forca ~ ciclos, data = db, family = Gamma(link = "log"))
s8 <- summary(m8)
cat("Deviance =", round(s8$deviance, 4), "\n")
cat("Null deviance =", round(s8$null.deviance, 4), "\n")
cat("Pseudo-R² (1 - Dev/NullDev) =", 
    round(1 - s8$deviance/s8$null.deviance, 4), "\n")
cat("AIC =", round(AIC(m8), 2), "\n\n")

# --- Modelo 9: Exponencial saturação com plateau y = ymax - (ymax - y0)*exp(-k*x) ---
cat("========================================\n")
cat("MODELO 9: Saturação y = ymax - (ymax-y0)*exp(-k*x)\n")
cat("========================================\n")
tryCatch({
  m9 <- nls(forca ~ ymax - (ymax - y0) * exp(-k * ciclos), 
            data = db,
            start = list(ymax = 1700, y0 = 1400, k = 0.03),
            control = nls.control(maxiter = 1000, tol = 1e-6))
  ss_res9 <- sum(residuals(m9)^2)
  ss_tot9 <- sum((db$forca - mean(db$forca))^2)
  r2_m9 <- 1 - ss_res9/ss_tot9
  n <- nrow(db)
  p9 <- 3
  r2_adj9 <- 1 - (1 - r2_m9) * (n - 1) / (n - p9 - 1)
  f_m9 <- ((ss_tot9 - ss_res9) / (p9 - 1)) / (ss_res9 / (n - p9))
  p_val_m9 <- pf(f_m9, p9-1, n-p9, lower.tail = FALSE)
  
  cat("Parâmetros:\n")
  print(coef(m9))
  cat("\nR² =", round(r2_m9, 4), "\n")
  cat("R² adj =", round(r2_adj9, 4), "\n")
  cat("F(", p9-1, ",", n-p9, ") =", round(f_m9, 4), "\n")
  cat("p =", round(p_val_m9, 6), "\n")
  cat("AIC =", round(AIC(m9), 2), "\n\n")
}, error = function(e) {
  cat("NLS não convergiu:", e$message, "\n\n")
})

# --- Modelo 10: Exponencial modificado com intercepto y = a + b*(1-exp(-c*x)) [port-selfStart] ---
cat("========================================\n")
cat("MODELO 10: Exponencial assintótico SSasymp\n")
cat("========================================\n")
tryCatch({
  m10 <- nls(forca ~ SSasymp(ciclos, Asym, R0, lrc), data = db)
  ss_res10 <- sum(residuals(m10)^2)
  ss_tot10 <- sum((db$forca - mean(db$forca))^2)
  r2_m10 <- 1 - ss_res10/ss_tot10
  n <- nrow(db)
  p10 <- 3
  r2_adj10 <- 1 - (1 - r2_m10) * (n - 1) / (n - p10 - 1)
  f_m10 <- ((ss_tot10 - ss_res10) / (p10 - 1)) / (ss_res10 / (n - p10))
  p_val_m10 <- pf(f_m10, p10-1, n-p10, lower.tail = FALSE)
  
  cat("Parâmetros:\n")
  print(coef(m10))
  cat("\nR² =", round(r2_m10, 4), "\n")
  cat("R² adj =", round(r2_adj10, 4), "\n")
  cat("F(", p10-1, ",", n-p10, ") =", round(f_m10, 4), "\n")
  cat("p =", round(p_val_m10, 6), "\n")
  cat("AIC =", round(AIC(m10), 2), "\n\n")
}, error = function(e) {
  cat("SSasymp não convergiu:", e$message, "\n\n")
})

# --- Modelo 11: Linear simples ---
cat("========================================\n")
cat("MODELO 11: Linear simples\n")
cat("========================================\n")
m11 <- lm(forca ~ ciclos, data = db)
s11 <- summary(m11)
cat("F(", s11$fstatistic[2], ",", s11$fstatistic[3], ") =", 
    round(s11$fstatistic[1], 4), "\n")
cat("R² =", round(s11$r.squared, 4), "\n")
cat("R² adj =", round(s11$adj.r.squared, 4), "\n")
cat("p =", round(pf(s11$fstatistic[1], s11$fstatistic[2], s11$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m11), 2), "\n\n")

# --- Modelo 12: Cúbico ---
cat("========================================\n")
cat("MODELO 12: Polinomial cúbico\n")
cat("========================================\n")
m12 <- lm(forca ~ poly(ciclos, 3, raw = TRUE), data = db)
s12 <- summary(m12)
cat("F(", s12$fstatistic[2], ",", s12$fstatistic[3], ") =", 
    round(s12$fstatistic[1], 4), "\n")
cat("R² =", round(s12$r.squared, 4), "\n")
cat("R² adj =", round(s12$adj.r.squared, 4), "\n")
cat("p =", round(pf(s12$fstatistic[1], s12$fstatistic[2], s12$fstatistic[3], 
                     lower.tail=FALSE), 6), "\n")
cat("AIC =", round(AIC(m12), 2), "\n\n")

# --- Modelo 13: Segmented (piecewise) - mudança em ~30 ciclos ---
cat("========================================\n")
cat("MODELO 13: Segmented (breakpoint = 30)\n")
cat("========================================\n")
tryCatch({
  library(segmented)
  m_base <- lm(forca ~ ciclos, data = db)
  m13 <- segmented(m_base, seg.Z = ~ciclos, psi = 30)
  s13 <- summary(m13)
  ss_res13 <- sum(residuals(m13)^2)
  ss_tot13 <- sum((db$forca - mean(db$forca))^2)
  r2_m13 <- 1 - ss_res13/ss_tot13
  n <- nrow(db)
  p13 <- length(coef(m13))
  r2_adj13 <- 1 - (1 - r2_m13) * (n - 1) / (n - p13 - 1)
  f_m13 <- ((ss_tot13 - ss_res13) / (p13 - 1)) / (ss_res13 / (n - p13))
  p_val_m13 <- pf(f_m13, p13-1, n-p13, lower.tail = FALSE)
  
  cat("Breakpoint:", round(m13$psi[,"Est."], 2), "\n")
  cat("R² =", round(r2_m13, 4), "\n")
  cat("R² adj =", round(r2_adj13, 4), "\n")
  cat("F(", p13-1, ",", n-p13, ") =", round(f_m13, 4), "\n")
  cat("p =", round(p_val_m13, 6), "\n")
  cat("AIC =", round(AIC(m13), 2), "\n\n")
}, error = function(e) {
  cat("Segmented não convergiu:", e$message, "\n\n")
})

# --- COMPARAÇÃO DE TODOS OS MODELOS ---
cat("========================================\n")
cat("RESUMO COMPARATIVO (AIC)\n")
cat("========================================\n")
aic_vals <- c(
  "M1-Polinomial" = AIC(m1),
  "M2-Log-linear" = AIC(m2),
  "M3-Log-quadr" = AIC(m3),
  "M6-Sqrt" = AIC(m6),
  "M11-Linear" = AIC(m11),
  "M12-Cúbico" = AIC(m12)
)

# Adicionar NLS se convergiram
tryCatch({ aic_vals["M4-Exp-mod"] <- AIC(m4) }, error = function(e) {})
tryCatch({ aic_vals["M5-Exp-puro"] <- AIC(m5) }, error = function(e) {})
tryCatch({ aic_vals["M8-Gamma-GLM"] <- AIC(m8) }, error = function(e) {})
tryCatch({ aic_vals["M9-Saturação"] <- AIC(m9) }, error = function(e) {})
tryCatch({ aic_vals["M10-SSasymp"] <- AIC(m10) }, error = function(e) {})
tryCatch({ aic_vals["M13-Segmented"] <- AIC(m13) }, error = function(e) {})

aic_df <- data.frame(Modelo = names(sort(aic_vals)), AIC = sort(aic_vals))
rownames(aic_df) <- NULL
cat("\nOrdenado por AIC (menor = melhor):\n")
print(aic_df)

cat("\n========================================\n")
cat("MELHOR MODELO:", aic_df$Modelo[1], "(AIC =", round(aic_df$AIC[1], 2), ")\n")
cat("========================================\n")
