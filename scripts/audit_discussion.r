##############################################################################
# audit_discussion.r
# Auditoria completa: valores no manuscrito vs. dados reais de DB.xlsx
##############################################################################
library(readxl)
library(dplyr)

# --- TRAÇÃO ---
cat("======= TRAÇÃO: Dados brutos =======\n")
tracao <- read_excel("dados/DB.xlsx", sheet = "TRACAO")
names(tracao) <- c("tratamento", "repeticao", "tensao", "extensao")
cat("Primeiras linhas (unidades da coluna tensão máxima):\n")
print(head(tracao, 10))
cat("\nRange tensão:", range(tracao$tensao, na.rm=TRUE), "\n")
cat("Grand mean tensão:", mean(tracao$tensao, na.rm=TRUE), "\n\n")

# Extrair ciclos
tracao$ciclos <- as.numeric(gsub("[^0-9]", "", tracao$tratamento))

# Médias por ciclo
cat("Médias por ciclo (tração tensão):\n")
med_t <- tracao %>% group_by(ciclos) %>% 
  summarise(media=mean(tensao, na.rm=TRUE), sd=sd(tensao, na.rm=TRUE), 
            n=n(), cv=sd(tensao,na.rm=TRUE)/mean(tensao,na.rm=TRUE)*100, .groups="drop")
print(as.data.frame(med_t))
cat("\nGrand mean:", mean(tracao$tensao, na.rm=TRUE), "\n")
cat("Se for N, kN/m = N / 200 × 1000:", mean(tracao$tensao, na.rm=TRUE)/200*1000, "\n\n")

# --- BOOTSTRAP TRAÇÃO (1000 iter) ---
cat("======= BOOTSTRAP TRAÇÃO =======\n")
set.seed(42)
boot_results_t <- tracao %>% group_by(ciclos) %>% 
  do({
    vals <- .$tensao
    boots <- replicate(1000, mean(sample(vals, replace=TRUE)))
    data.frame(media_boot = mean(boots),
               ci_lo = quantile(boots, 0.025),
               ci_hi = quantile(boots, 0.975))
  })
cat("Bootstrap means:\n")
print(as.data.frame(boot_results_t))

# --- PUNÇÃO ---
cat("\n======= PUNÇÃO: Dados brutos =======\n")
puncao <- read_excel("dados/DB.xlsx", sheet = "PUNCAO")
names(puncao) <- c("tratamento", "repeticao", "forca", "deslocamento")
cat("Primeiras linhas:\n")
print(head(puncao, 10))
cat("\nRange força:", range(puncao$forca, na.rm=TRUE), "\n")
cat("Grand mean força:", mean(puncao$forca, na.rm=TRUE), "\n\n")

puncao$ciclos <- as.numeric(gsub("[^0-9]", "", puncao$tratamento))
puncao$ciclos[grepl("CONTROLE|Control", puncao$tratamento, ignore.case=TRUE)] <- 0

# Médias por ciclo
cat("Médias por ciclo (punção força):\n")
med_p <- puncao %>% group_by(ciclos) %>% 
  summarise(media=mean(forca, na.rm=TRUE), sd=sd(forca, na.rm=TRUE),
            n=n(), cv=sd(forca,na.rm=TRUE)/mean(forca,na.rm=TRUE)*100, .groups="drop")
print(as.data.frame(med_p))

# --- BOOTSTRAP PUNÇÃO ---
cat("\n======= BOOTSTRAP PUNÇÃO =======\n")
set.seed(42)
boot_results_p <- puncao %>% group_by(ciclos) %>% 
  do({
    vals <- .$forca
    boots <- replicate(1000, mean(sample(vals, replace=TRUE)))
    data.frame(media_boot = mean(boots),
               ci_lo = quantile(boots, 0.025),
               ci_hi = quantile(boots, 0.975))
  })
cat("Bootstrap means:\n")
print(as.data.frame(boot_results_p))

# --- VERIFICAÇÃO: PADRÃO TEMPORAL PUNÇÃO ---
cat("\n======= PADRÃO TEMPORAL PUNÇÃO (verificação narrativa) =======\n")
cat("Variação entre ciclos consecutivos:\n")
med_ord <- med_p %>% arrange(ciclos)
for(i in 2:nrow(med_ord)) {
  delta <- med_ord$media[i] - med_ord$media[i-1]
  pct <- delta / med_ord$media[i-1] * 100
  cat(sprintf("  %d→%d: %+.1f N (%+.1f%%)\n", 
              med_ord$ciclos[i-1], med_ord$ciclos[i], delta, pct))
}

# --- VERIFICAÇÃO: CV PUNÇÃO PADRÃO TEMPORAL ---
cat("\n======= CV PUNÇÃO vs 'sistematicamente decrescente' =======\n")
cat("CV por ciclo (ordem temporal):\n")
for(i in 1:nrow(med_ord)) {
  cat(sprintf("  %3d ciclos: CV = %.2f%%\n", med_ord$ciclos[i], med_ord$cv[i]))
}
cat("\nCV mínimo:", round(min(med_ord$cv), 2), "% no ciclo", 
    med_ord$ciclos[which.min(med_ord$cv)], "\n")
cat("CV máximo:", round(max(med_ord$cv), 2), "% no ciclo", 
    med_ord$ciclos[which.max(med_ord$cv)], "\n")

# --- VERIFICAÇÃO UNIDADES TRAÇÃO ---
cat("\n======= CHECK UNIDADES =======\n")
cat("Se tensão em kN/m: grand mean =", mean(tracao$tensao, na.rm=TRUE), "kN/m\n")
cat("Se tensão em N: grand mean =", mean(tracao$tensao, na.rm=TRUE), "N → kN/m =",
    mean(tracao$tensao, na.rm=TRUE)/1000/0.2, "\n")
cat("Weibull mean manuscrito: 17.90 kN/m\n")
cat("Weibull η manuscrito: 19.93 kN/m\n")
