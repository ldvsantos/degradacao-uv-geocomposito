library(readxl); library(dplyr); library(stringr)
db <- file.path(getwd(), "dados", "DB.xlsx")

# --- TRACAO ---
df <- read_excel(db, sheet = "TRACAO")
names(df) <- c("tratamento", "repeticao", "tensao", "ext")
df$tempo <- case_when(
  str_detect(df$tratamento, "120JN") ~ 120, str_detect(df$tratamento, "110JN") ~ 110,
  str_detect(df$tratamento, "100JN") ~ 100, str_detect(df$tratamento, "10JN") ~ 10,
  str_detect(df$tratamento, "20JN") ~ 20, str_detect(df$tratamento, "30JN") ~ 30,
  str_detect(df$tratamento, "40JN") ~ 40, str_detect(df$tratamento, "50JN") ~ 50,
  str_detect(df$tratamento, "60JN") ~ 60, str_detect(df$tratamento, "70JN") ~ 70,
  str_detect(df$tratamento, "80JN") ~ 80, str_detect(df$tratamento, "90JN") ~ 90,
  TRUE ~ NA_real_)
df <- df[!is.na(df$tempo) & !is.na(df$tensao), ]

cat("=== REGRESSAO POLINOMIAL 2a ORDEM — TENSAO ===\n")
cat("n =", nrow(df), "\n")
m1 <- lm(tensao ~ poly(tempo, 2), data = df)
cat("Resumo:\n")
print(summary(m1))
f_stat <- summary(m1)$fstatistic
cat(sprintf("\nF(%d,%d) = %.3f, p = %.4f\n", f_stat[2], f_stat[3], f_stat[1],
            pf(f_stat[1], f_stat[2], f_stat[3], lower.tail=FALSE)))
cat(sprintf("R² = %.3f, R²adj = %.3f\n", summary(m1)$r.squared, summary(m1)$adj.r.squared))

# --- PUNCAO ---
dp <- read_excel(db, sheet = "PUNCAO")
names(dp) <- c("tratamento", "repeticao", "forca", "desl")
dp$tempo <- case_when(
  str_detect(dp$tratamento, "CONTROLE") ~ 0,
  str_detect(dp$tratamento, "120JN") ~ 120, str_detect(dp$tratamento, "110JN") ~ 110,
  str_detect(dp$tratamento, "100JN") ~ 100, str_detect(dp$tratamento, "10JN") ~ 10,
  str_detect(dp$tratamento, "20JN") ~ 20, str_detect(dp$tratamento, "30JN") ~ 30,
  str_detect(dp$tratamento, "40JN") ~ 40, str_detect(dp$tratamento, "50JN") ~ 50,
  str_detect(dp$tratamento, "60JN") ~ 60, str_detect(dp$tratamento, "70JN") ~ 70,
  str_detect(dp$tratamento, "80JN") ~ 80, str_detect(dp$tratamento, "90JN") ~ 90,
  TRUE ~ NA_real_)
dp <- dp[!is.na(dp$tempo) & !is.na(dp$forca), ]

cat("\n=== REGRESSAO EXPONENCIAL MODIFICADA — PUNCAO ===\n")
cat("n =", nrow(dp), "\n")

# Modified exponential: y ~ a + b*(1 - exp(-c*x))
# Try: y ~ tempo + I(log(tempo+1)) or 2nd order poly
# Actually the R² = 0.187 with F(2,49) means 2 predictors, n=52
m2 <- lm(forca ~ poly(tempo, 2), data = dp)
cat("Polinomial 2a ordem:\n")
print(summary(m2))
f2 <- summary(m2)$fstatistic
cat(sprintf("\nF(%d,%d) = %.3f, p = %.4f\n", f2[2], f2[3], f2[1],
            pf(f2[1], f2[2], f2[3], lower.tail=FALSE)))
cat(sprintf("R² = %.3f\n", summary(m2)$r.squared))

# Also try with log transform as "modified exponential"
m3 <- lm(forca ~ tempo + I(log(tempo + 1)), data = dp)
cat("\n--- Log-modificado (tempo + log(tempo+1)):\n")
f3 <- summary(m3)$fstatistic
cat(sprintf("F(%d,%d) = %.3f, p = %.4f\n", f3[2], f3[3], f3[1],
            pf(f3[1], f3[2], f3[3], lower.tail=FALSE)))
cat(sprintf("R² = %.3f\n", summary(m3)$r.squared))

# Try nls exponential
cat("\n--- NLS exponencial modificada:\n")
tryCatch({
  m4 <- nls(forca ~ a + b * (1 - exp(-c * tempo)), data = dp,
            start = list(a = 1400, b = 300, c = 0.01))
  print(summary(m4))
}, error = function(e) cat("NLS falhou:", e$message, "\n"))

# Bootstrap puncture verification
cat("\n=== BOOTSTRAP PUNCAO — VALORES CHAVE ===\n")
library(boot)
set.seed(42)
for (t in c(0, 10, 30)) {
  dados_t <- dp$forca[dp$tempo == t]
  boot_result <- boot(dados_t, function(d, i) mean(d[i]), R = 1000)
  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
  cat(sprintf("Tempo %d: n=%d, M=%.2f, bootM=%.2f, CI=[%.2f, %.2f]\n",
              t, length(dados_t), mean(dados_t), mean(boot_result$t),
              ci$percent[4], ci$percent[5]))
}
cat("MS diz: 0c=1416.69(1189.14-1644.24), 10c=1529.52(1225.73-1833.31), 30c=2014.91(1725.47-2304.35)\n")
