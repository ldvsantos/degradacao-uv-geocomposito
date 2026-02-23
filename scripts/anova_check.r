library(readxl); library(dplyr); library(stringr)
db <- file.path(getwd(),"dados","DB.xlsx")
df <- read_excel(db, sheet="TRACAO")
names(df) <- c("tratamento","repeticao","tensao","ext")
df$tempo <- case_when(
  str_detect(df$tratamento,"120JN")~120, str_detect(df$tratamento,"110JN")~110,
  str_detect(df$tratamento,"100JN")~100, str_detect(df$tratamento,"10JN")~10,
  str_detect(df$tratamento,"20JN")~20, str_detect(df$tratamento,"30JN")~30,
  str_detect(df$tratamento,"40JN")~40, str_detect(df$tratamento,"50JN")~50,
  str_detect(df$tratamento,"60JN")~60, str_detect(df$tratamento,"70JN")~70,
  str_detect(df$tratamento,"80JN")~80, str_detect(df$tratamento,"90JN")~90,
  TRUE~NA_real_)
df <- df[!is.na(df$tempo),]
df$tf <- factor(df$tempo)
cat("=== ANOVA TENSAO ===\n")
print(summary(aov(tensao ~ tf, data=df)))
cat("\n=== ANOVA EXTENSAO ===\n")
print(summary(aov(ext ~ tf, data=df)))
cat("\n=== ANOVA MODULO ===\n")
df$modulo <- df$tensao / df$ext
print(summary(aov(modulo ~ tf, data=df[df$modulo > 0 & is.finite(df$modulo),])))
cat("\n=== ANOVA PUNCAO ===\n")
dp <- read_excel(db, sheet="PUNCAO")
names(dp) <- c("tratamento","repeticao","forca","desl")
dp$tempo <- case_when(
  str_detect(dp$tratamento,"CONTROLE")~0,
  str_detect(dp$tratamento,"120JN")~120, str_detect(dp$tratamento,"110JN")~110,
  str_detect(dp$tratamento,"100JN")~100, str_detect(dp$tratamento,"10JN")~10,
  str_detect(dp$tratamento,"20JN")~20, str_detect(dp$tratamento,"30JN")~30,
  str_detect(dp$tratamento,"40JN")~40, str_detect(dp$tratamento,"50JN")~50,
  str_detect(dp$tratamento,"60JN")~60, str_detect(dp$tratamento,"70JN")~70,
  str_detect(dp$tratamento,"80JN")~80, str_detect(dp$tratamento,"90JN")~90,
  TRUE~NA_real_)
dp <- dp[!is.na(dp$tempo),]
dp$tf <- factor(dp$tempo)
print(summary(aov(forca ~ tf, data=dp)))
cat("\n=== ANOVA DESL PUNCAO ===\n")
print(summary(aov(desl ~ tf, data=dp)))
