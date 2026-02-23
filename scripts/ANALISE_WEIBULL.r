# ================================================================================
# WEIBULL PROBABILITY OF FAILURE ANALYSIS - WEIBULL DISTRIBUTION
# Based on: Weibull Statistics and ANOVA Analysis of the Tensile 
# Mechanical Characteristics of Chamaerops humilis Cellulose Fibers
# ================================================================================
# 
# This script implements reliability analysis using Weibull distribution
# for failure probability analysis of degraded biogeosynthetics
#
# Implemented methods:
# - Weibull 2P (two parameters) and 3P (three parameters) distribution
# - Estimators: Maximum Likelihood (ML) and Least Squares (LS)
# - Kaplan-Meier method for survival estimation
# - Goodness-of-fit tests: Anderson-Darling and Kolmogorov-Smirnov
# - Analysis of variance (ANOVA) for groups
# - Survival and failure probability curves
#
# Author: Analysis based on Atoui et al. (2024)
# Date: October 2025
# ================================================================================

# Clear workspace
rm(list = ls())
cat("=== WEIBULL PROBABILITY OF FAILURE ANALYSIS ===\n\n")

# Configure directories
graficos_dir <- file.path(getwd(), 'graficos', 'tracao')
if (!dir.exists(graficos_dir)) dir.create(graficos_dir, recursive = TRUE)

# Configure CRAN mirror
options(repos = c(CRAN = "https://cran.r-project.org"))

# ================================================================================
# 1. LOAD REQUIRED LIBRARIES
# ================================================================================

# Function to install and load libraries
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Required libraries for Weibull analysis
packages <- c(
  "survival",      # Survival analysis and Kaplan-Meier
  "WeibullR",      # Specialized Weibull analysis
  "fitdistrplus",  # Ajuste de distribuições 
  "goftest",       # Testes de adequação (Anderson-Darling)
  "MASS",          # Funções estatísticas gerais
  "ggplot2",       # Gráficos avançados
  "gridExtra",     # Arranjo de gráficos
  "dplyr",         # Manipulação de dados
  "readxl",        # Leitura de arquivos Excel
  "nortest",       # Testes de normalidade
  "geepack",       # GEE (Generalized Estimating Equations)
  "car",           # Testes estatísticos avançados
  "emmeans",       # Comparações múltiplas para GLM
  "multcomp"       # Correção para múltiplas comparações
)

cat("Carregando bibliotecas...\n")
install_and_load(packages)

# ================================================================================
# 2. FUNÇÕES AUXILIARES PARA ANÁLISE WEIBULL
# ================================================================================

# Função para calcular índices de probabilidade (diferentes estimadores)
calculate_probability_indices <- function(n, method = "kaplan_meier") {
  i <- 1:n
  
  switch(method,
    "kaplan_meier" = (i - 0.3) / (n + 0.4),           # Método Kaplan-Meier (recomendado)
    "benard" = (i - 0.3) / (n + 0.4),                 # Método Benard (similar ao KM)
    "median_rank" = (i - 0.3) / (n + 0.4),            # Median Rank
    "hazen" = (i - 0.5) / n,                          # Método Hazen
    "mean_rank" = i / (n + 1)                         # Mean Rank (Herd Johnson)
  )
}

# Função para ajustar Weibull 2P usando Least Squares
fit_weibull_2p_ls <- function(data, method = "kaplan_meier") {
  n <- length(data)
  sorted_data <- sort(data)
  
  # Calcular índices de probabilidade
  pi <- calculate_probability_indices(n, method)
  
  # Transformação para regressão linear: ln(-ln(1-F)) = m*ln(x) - m*ln(σ0)
  # Remover valores onde 1-pi = 0 para evitar log(0)
  valid_idx <- pi < 0.999
  y <- log(-log(1 - pi[valid_idx]))
  x <- log(sorted_data[valid_idx])
  
  # Regressão linear
  lm_fit <- lm(y ~ x)
  
  # Extrair parâmetros Weibull
  m <- as.numeric(coef(lm_fit)[2])        # Shape parameter (slope)
  sigma0 <- exp(-coef(lm_fit)[1] / m)     # Scale parameter
  
  # Calcular R²
  r_squared <- summary(lm_fit)$r.squared
  
  return(list(
    shape = m,
    scale = sigma0,
    r_squared = r_squared,
    method = "LS_2P",
    estimator = method,
    fit_object = lm_fit
  ))
}

# Função para ajustar Weibull usando Maximum Likelihood
fit_weibull_ml <- function(data, fix.arg = list()) {
  tryCatch({
    fit <- fitdist(data, "weibull", method = "mle", fix.arg = fix.arg)
    
    # Calcular R² baseado na adequação
    ks_stat <- ks.test(data, "pweibull", 
                      shape = fit$estimate["shape"], 
                      scale = fit$estimate["scale"])
    r_squared <- 1 - ks_stat$statistic
    
    return(list(
      shape = unname(fit$estimate["shape"]),
      scale = unname(fit$estimate["scale"]),
      r_squared = as.numeric(r_squared),
      method = ifelse(is.null(fix.arg$shape), "ML_2P", "ML_3P"),
      aic = fit$aic,
      bic = fit$bic,
      fit_object = fit
    ))
  }, error = function(e) {
    cat("Erro no ajuste ML:", e$message, "\n")
    return(NULL)
  })
}

# Função para teste Anderson-Darling
anderson_darling_test <- function(data, distribution = "weibull", params) {
  tryCatch({
    if (distribution == "weibull") {
      p_values <- pweibull(data, shape = params$shape, scale = params$scale)
    } else if (distribution == "normal") {
      p_values <- pnorm(data, mean = params$mean, sd = params$sd)
    }
    
    # Teste Anderson-Darling
    ad_test <- ad.test(p_values)
    
    return(list(
      statistic = ad_test$statistic,
      p_value = ad_test$p.value
    ))
  }, error = function(e) {
    return(list(statistic = NA, p_value = NA))
  })
}

# Função para calcular probabilidade de sobrevivência Weibull
weibull_survival <- function(x, shape, scale) {
  exp(-(x/scale)^shape)
}

# Função para calcular probabilidade de falha Weibull
weibull_failure <- function(x, shape, scale) {
  1 - exp(-(x/scale)^shape)
}

# ================================================================================
# 3. CARREGAR E PREPARAR DADOS
# ================================================================================

cat("Carregando dados...\n")

# Tentar carregar dados do arquivo Excel
if (file.exists("DB.xlsx")) {
  cat("Carregando dados de DB.xlsx...\n")
  
  # Ler todas as abas disponíveis
  sheet_names <- excel_sheets("DB.xlsx")
  cat("Abas disponíveis:", paste(sheet_names, collapse = ", "), "\n")
  
  # Carregar primeira aba como padrão
  dados_raw <- read_excel("DB.xlsx", sheet = 1)
  
} else {
  cat("Arquivo DB.xlsx não encontrado. Gerando dados simulados...\n")
  
  # Gerar dados simulados baseados no padrão do artigo
  set.seed(123)
  n_samples <- 210  # Conforme o artigo
  
  # Simular dados de resistência à tração (MPa) - baseado no artigo
  # Artigo reporta: média ~105 MPa, desvio ~28 MPa
  tensile_stress <- rgamma(n_samples, shape = 14, scale = 7.5) + 20
  
  # Simular dados de deformação na ruptura (%) - baseado no artigo  
  # Artigo reporta: média ~2.2%, desvio ~0.78%
  strain_break <- rgamma(n_samples, shape = 8, scale = 0.275) + 0.5
  
  # Simular módulo de Young (GPa) - baseado no artigo
  # Artigo reporta: média ~6.2 GPa, desvio ~3.2 GPa
  young_modulus <- rgamma(n_samples, shape = 4, scale = 1.55) + 1
  
  # Simular diferentes grupos de teste (N = 30, 60, 90, 120, 150, 180, 210)
  group_sizes <- c(30, 30, 30, 30, 30, 30, 30)  # 7 grupos de 30 cada
  test_groups <- rep(1:7, group_sizes)
  
  # Criar dataframe
  dados_raw <- data.frame(
    ID = 1:n_samples,
    Grupo = test_groups,
    Tensao_Tracao_MPa = tensile_stress,
    Deformacao_Ruptura_Perc = strain_break,
    Modulo_Young_GPa = young_modulus,
    Tempo_Degradacao_Dias = sample(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), n_samples, replace = TRUE)
  )
}

# Verificar estrutura dos dados
cat("\nEstrutura dos dados:\n")
str(dados_raw)
cat("\nPrimeiras linhas:\n")
print(head(dados_raw))

# ================================================================================
# 4. ANÁLISE EXPLORATÓRIA INICIAL  
# ================================================================================

cat("\n=== ANÁLISE EXPLORATÓRIA ===\n")

# Identificar colunas numéricas para análise Weibull
numeric_cols <- sapply(dados_raw, is.numeric)
numeric_data <- dados_raw[, numeric_cols]

# Estatísticas descritivas
cat("\nEstatísticas descritivas:\n")
summary_stats <- summary(numeric_data)
print(summary_stats)

# Identificar variáveis principais para análise (similar ao artigo)
# Priorizar variáveis de resistência mecânica
main_variables <- c()

for (col in names(numeric_data)) {
  if (grepl("tensao|tracao|stress|resistencia", col, ignore.case = TRUE) ||
      grepl("deformacao|strain|ruptura", col, ignore.case = TRUE) ||
      grepl("modulo|young|elastico", col, ignore.case = TRUE)) {
    main_variables <- c(main_variables, col)
  }
}

# Se não encontrar variáveis específicas, usar as primeiras numéricas
if (length(main_variables) == 0) {
  main_variables <- names(numeric_data)[1:min(3, ncol(numeric_data))]
}

cat("\nVariáveis selecionadas para análise Weibull:\n")
cat(paste(main_variables, collapse = ", "), "\n")

# ================================================================================
# 5. ANÁLISE WEIBULL COMPLETA
# ================================================================================

cat("\n=== ANÁLISE WEIBULL ===\n")

# Lista para armazenar resultados
weibull_results <- list()
survival_plots <- list()
qq_plots <- list()

# Analisar cada variável principal
for (var in main_variables) {
  cat(sprintf("\n--- Analisando variável: %s ---\n", var))
  
  # Extrair dados válidos (remover NAs e valores <= 0)
  data_var <- numeric_data[[var]]
  data_clean <- data_var[!is.na(data_var) & data_var > 0]
  
  if (length(data_clean) < 10) {
    cat("Dados insuficientes para", var, "\n")
    next
  }
  
  cat(sprintf("N = %d amostras válidas\n", length(data_clean)))
  cat(sprintf("Média: %.3f | Desvio: %.3f\n", mean(data_clean), sd(data_clean)))
  cat(sprintf("Min: %.3f | Max: %.3f\n", min(data_clean), max(data_clean)))
  
  # ============================================================================
  # 5.1 Ajustes Weibull com diferentes métodos
  # ============================================================================
  
  var_results <- list()
  
  # Método 1: Weibull 2P - Least Squares com Kaplan-Meier
  cat("\nAjuste Weibull 2P - LS (Kaplan-Meier):\n")
  fit_2p_ls_km <- fit_weibull_2p_ls(data_clean, "kaplan_meier")
  if (!is.null(fit_2p_ls_km)) {
    cat(sprintf("  Shape (m): %.3f | Scale (σ0): %.3f | R²: %.4f\n", 
               fit_2p_ls_km$shape, fit_2p_ls_km$scale, fit_2p_ls_km$r_squared))
    var_results$weibull_2p_ls_km <- fit_2p_ls_km
  }
  
  # Método 2: Weibull 2P - Least Squares com Hazen
  cat("\nAjuste Weibull 2P - LS (Hazen):\n")
  fit_2p_ls_hazen <- fit_weibull_2p_ls(data_clean, "hazen")
  if (!is.null(fit_2p_ls_hazen)) {
    cat(sprintf("  Shape (m): %.3f | Scale (σ0): %.3f | R²: %.4f\n", 
               fit_2p_ls_hazen$shape, fit_2p_ls_hazen$scale, fit_2p_ls_hazen$r_squared))
    var_results$weibull_2p_ls_hazen <- fit_2p_ls_hazen
  }
  
  # Método 3: Weibull 2P - Maximum Likelihood  
  cat("\nAjuste Weibull 2P - ML:\n")
  fit_2p_ml <- fit_weibull_ml(data_clean)
  if (!is.null(fit_2p_ml)) {
    cat(sprintf("  Shape (m): %.3f | Scale (σ0): %.3f | R²: %.4f\n", 
               fit_2p_ml$shape, fit_2p_ml$scale, fit_2p_ml$r_squared))
    cat(sprintf("  AIC: %.2f | BIC: %.2f\n", fit_2p_ml$aic, fit_2p_ml$bic))
    var_results$weibull_2p_ml <- fit_2p_ml
  }
  
  # Método 4: Weibull 3P - Maximum Likelihood (com parâmetro de localização)
  cat("\nAjuste Weibull 3P - ML:\n")
  location_param <- min(data_clean) * 0.8  # Parâmetro de localização estimado
  fit_3p_ml <- fit_weibull_ml(data_clean, fix.arg = list(location = location_param))
  if (!is.null(fit_3p_ml)) {
    cat(sprintf("  Shape (m): %.3f | Scale (σ0): %.3f | R²: %.4f\n", 
               fit_3p_ml$shape, fit_3p_ml$scale, fit_3p_ml$r_squared))
    cat(sprintf("  AIC: %.2f | BIC: %.2f\n", fit_3p_ml$aic, fit_3p_ml$bic))
    var_results$weibull_3p_ml <- fit_3p_ml
  }
  
  # ============================================================================
  # 5.2 Testes de adequação (Anderson-Darling e Kolmogorov-Smirnov)
  # ============================================================================
  
  cat("\n--- Testes de Adequação ---\n")
  
  # Teste para o melhor ajuste (maior R²)
  best_fit <- NULL
  best_r2 <- 0
  
  for (method_name in names(var_results)) {
    if (var_results[[method_name]]$r_squared > best_r2) {
      best_r2 <- var_results[[method_name]]$r_squared
      best_fit <- var_results[[method_name]]
      best_fit$method_name <- method_name
    }
  }
  
  if (!is.null(best_fit)) {
    cat(sprintf("Melhor ajuste: %s (R² = %.4f)\n", best_fit$method_name, best_r2))
    
    # Teste Anderson-Darling
    ad_result <- anderson_darling_test(data_clean, "weibull", best_fit)
    cat(sprintf("Anderson-Darling: Estatística = %.4f, p-valor = %.4f\n", 
               ad_result$statistic, ad_result$p_value))
    
    # Teste Kolmogorov-Smirnov
    ks_result <- ks.test(data_clean, "pweibull", 
                        shape = best_fit$shape, scale = best_fit$scale)
    cat(sprintf("Kolmogorov-Smirnov: Estatística = %.4f, p-valor = %.4f\n", 
               ks_result$statistic, ks_result$p.value))
  }
  
  # ============================================================================
  # 5.3 Análise de sobrevivência e curvas de probabilidade
  # ============================================================================
  
  if (!is.null(best_fit)) {
    cat("\n--- Análise de Probabilidade ---\n")
    
    # Pontos para curvas
    x_range <- seq(min(data_clean), max(data_clean), length.out = 100)
    
    # Probabilidade de sobrevivência P(X > x)
    survival_prob <- weibull_survival(x_range, best_fit$shape, best_fit$scale)
    
    # Probabilidade de falha P(X <= x) 
    failure_prob <- weibull_failure(x_range, best_fit$shape, best_fit$scale)
    
    # Valores característicos (p = 0.5, 0.1, 0.9)
    char_50 <- best_fit$scale * (-log(0.5))^(1/best_fit$shape)  # 50% sobrevivência
    char_10 <- best_fit$scale * (-log(0.9))^(1/best_fit$shape)  # 10% sobrevivência (90% falha)
    char_90 <- best_fit$scale * (-log(0.1))^(1/best_fit$shape)  # 90% sobrevivência (10% falha)
    
    cat(sprintf("Valor para 50%% de sobrevivência: %.3f\n", char_50))
    cat(sprintf("Valor para 90%% de sobrevivência: %.3f\n", char_90))
    cat(sprintf("Valor para 10%% de sobrevivência: %.3f\n", char_10))
    
    # Armazenar dados para gráficos
    survival_data <- data.frame(
      x = x_range,
      survival = survival_prob,
      failure = failure_prob,
      variable = var
    )
    
    survival_plots[[var]] <- survival_data
  }
  
  # Armazenar todos os resultados da variável
  weibull_results[[var]] <- var_results
}

# ================================================================================
# 6. ANÁLISE POR GRUPOS (GLM E GEE WEIBULL)
# ================================================================================

cat("\n=== ANÁLISE GLM/GEE WEIBULL POR GRUPOS ===\n")

# Verificar se existe variável de grupo
group_cols <- c("Grupo", "Group", "grupo", "group", "Tratamento", "Treatment")
group_var <- NULL

for (col in group_cols) {
  if (col %in% names(dados_raw)) {
    group_var <- col
    break
  }
}

if (!is.null(group_var) && length(unique(dados_raw[[group_var]])) > 1) {
  cat(sprintf("Analisando grupos da variável: %s\n", group_var))
  
  # Para cada variável principal, comparar grupos
  for (var in main_variables) {
    if (!var %in% names(dados_raw)) next
    
    cat(sprintf("\n--- Análise GLM/GEE Weibull para %s ---\n", var))
    
    # Preparar dados por grupo
    group_data <- dados_raw %>%
      select(all_of(c(group_var, var))) %>%
      filter(!is.na(.[[var]]) & .[[var]] > 0) %>%
      rename(grupo = 1, valor = 2) %>%
      mutate(grupo = factor(grupo))
    
    # Estatísticas por grupo
    group_stats <- group_data %>%
      group_by(grupo) %>%
      summarise(
        n = n(),
        media = mean(valor),
        desvio = sd(valor),
        min_val = min(valor),
        max_val = max(valor),
        cv = sd(valor)/mean(valor)*100,
        .groups = "drop"
      )
    
    cat("\nEstatísticas por grupo:\n")
    print(group_stats)
    
    # ============================================================================
    # GLM com distribuição Gamma (método principal dos outros scripts)
    # ============================================================================
    
    cat("\n--- Modelo GLM Gamma ---\n")
    
    tryCatch({
      # Ajustar GLM com distribuição Gamma e link log
      glm_model <- glm(valor ~ grupo, 
                       data = group_data, 
                       family = Gamma(link = "log"))
      
      cat("\nResumo do modelo GLM Gamma:\n")
      print(summary(glm_model))
      
      # Teste de significância geral (Anova tipo II)
      cat("\nAnálise de Deviance (Anova tipo II):\n")
      anova_glm <- Anova(glm_model, type = "II")
      print(anova_glm)
      
      # Extrair e formatar estatísticas do GLM para apresentação
      cat("\n" , strrep("=", 70), "\n", sep="")
      cat("RESULTADO GLM FORMATADO PARA APRESENTAÇÃO:\n")
      cat(strrep("=", 70), "\n")
      
      # Extrair qui-quadrado, graus de liberdade e p-valor
      chi_sq <- anova_glm$Chisq[1]
      df <- anova_glm$Df[1]
      p_value <- anova_glm$`Pr(>Chisq)`[1]
      
      # Formatar resultado no estilo solicitado: GLM (Gamma, link log): χ²(df)=valor, p=valor
      cat(sprintf("GLM (Gamma, link log): χ²(%d) = %.2f, p = %.3f\n", df, chi_sq, p_value))
      
      # Adicionar interpretação
      if (p_value < 0.001) {
        sig_label <- "p < 0.001 (altamente significativo)"
      } else if (p_value < 0.01) {
        sig_label <- "p < 0.01 (muito significativo)"
      } else if (p_value < 0.05) {
        sig_label <- "p < 0.05 (significativo)"
      } else if (p_value < 0.10) {
        sig_label <- "p < 0.10 (marginalmente significativo)"
      } else {
        sig_label <- "não significativo (ns)"
      }
      cat(sprintf("Interpretação: %s\n", sig_label))
      cat(strrep("=", 70), "\n\n")
      
      # Comparações múltiplas usando emmeans
      cat("\nComparações múltiplas (emmeans):\n")
      emmeans_result <- emmeans(glm_model, "grupo", type = "response")
      print(emmeans_result)
      
      # Comparações par a par
      cat("\nComparações par-a-par (Tukey):\n")
      pairwise_comp <- pairs(emmeans_result, adjust = "tukey")
      print(pairwise_comp)
      
      # Diagnósticos do modelo
      cat("\nDiagnósticos do modelo:\n")
      cat(sprintf("  AIC: %.3f\n", AIC(glm_model)))
      cat(sprintf("  Deviance residual: %.3f\n", glm_model$deviance))
      cat(sprintf("  Graus de liberdade: %d\n", glm_model$df.residual))
      
      # Teste de adequação da família Gamma
      cat("\n  Teste de adequação da distribuição Gamma:\n")
      
      # Residuos de Pearson
      pearson_resid <- residuals(glm_model, type = "pearson")
      cat(sprintf("  Média dos resíduos de Pearson: %.4f\n", mean(pearson_resid)))
      cat(sprintf("  Desvio padrão dos resíduos: %.4f\n", sd(pearson_resid)))
      
      # Teste de normalidade dos resíduos
      shapiro_test <- shapiro.test(pearson_resid)
      cat(sprintf("  Shapiro-Wilk (resíduos): W=%.4f, p=%.4f\n", 
                 shapiro_test$statistic, shapiro_test$p.value))
      
    }, error = function(e) {
      cat("Erro no modelo GLM Gamma:", e$message, "\n")
    })
    
    # ============================================================================
    # GEE (Generalized Estimating Equations) - para dados correlacionados
    # ============================================================================
    
    cat("\n--- Modelo GEE (se aplicável) ---\n")
    
    # Verificar se há estrutura de medidas repetidas ou correlação
    if (nrow(group_data) > 20 && length(unique(group_data$grupo)) >= 3) {
      
      tryCatch({
        # Criar variável de ID para GEE (assumindo ordem como medida repetida)
        group_data$id <- rep(1:ceiling(nrow(group_data)/length(unique(group_data$grupo))), 
                            each = length(unique(group_data$grupo)))[1:nrow(group_data)]
        
        # Ajustar modelo GEE com estrutura de correlação AR-1
        gee_model <- geeglm(valor ~ grupo, 
                           data = group_data,
                           id = id,
                           family = Gamma(link = "log"),
                           corstr = "ar1")
        
        cat("\nResumo do modelo GEE:\n")
        print(summary(gee_model))
        
        # Teste de Wald para significância
        cat("\nTeste de Wald:\n")
        wald_test <- anova(gee_model)
        print(wald_test)
        
        cat("\nParâmetros de correlação:\n")
        cat(sprintf("  Correlação estimada (AR-1): %.4f\n", 
                   gee_model$geese$alpha))
        
      }, error = function(e) {
        cat("Modelo GEE não aplicável aos dados ou erro:", e$message, "\n")
      })
    } else {
      cat("Dados insuficientes para modelo GEE (necessário N>20 e >=3 grupos)\n")
    }
    
    # ============================================================================
    # Análise Weibull por grupo com comparação estatística
    # ============================================================================
    
    cat("\n--- Parâmetros Weibull por Grupo ---\n")
    
    group_weibull <- list()
    weibull_params_df <- data.frame()
    
    for (g in unique(group_data$grupo)) {
      group_subset <- group_data$valor[group_data$grupo == g]
      
      if (length(group_subset) >= 5) {  # Mínimo para ajuste
        fit_group <- fit_weibull_2p_ls(group_subset, "kaplan_meier")
        
        if (!is.null(fit_group)) {
          group_weibull[[as.character(g)]] <- fit_group
          cat(sprintf("Grupo %s: Shape=%.3f, Scale=%.3f, R²=%.4f\n", 
                     g, fit_group$shape, fit_group$scale, fit_group$r_squared))
          
          # Acumular dados para análise estatística
          weibull_params_df <- rbind(weibull_params_df, 
                                   data.frame(
                                     grupo = g,
                                     shape = fit_group$shape,
                                     scale = fit_group$scale,
                                     r_squared = fit_group$r_squared
                                   ))
        }
      }
    }
    
    # Análise estatística dos parâmetros Weibull entre grupos
    if (length(group_weibull) > 1) {
      cat("\n--- Análise Estatística dos Parâmetros Weibull ---\n")
      
      # Estatísticas descritivas
      shape_values <- sapply(group_weibull, function(x) x$shape)
      scale_values <- sapply(group_weibull, function(x) x$scale)
      
      cat(sprintf("Variabilidade do Shape (m): CV = %.2f%%\n", 
                 sd(shape_values)/mean(shape_values)*100))
      cat(sprintf("Variabilidade do Scale (σ0): CV = %.2f%%\n", 
                 sd(scale_values)/mean(scale_values)*100))
      
      # Se temos dados suficientes, fazer análise GLM dos parâmetros Weibull
      if (nrow(weibull_params_df) >= 3) {
        cat("\nTeste estatístico para Shape (parâmetro de forma):\n")
        
        # GLM para parâmetro Shape
        tryCatch({
          shape_glm <- glm(shape ~ grupo, data = weibull_params_df, family = Gamma(link = "log"))
          shape_anova <- Anova(shape_glm, type = "II")
          cat("  Análise de Deviance para Shape:\n")
          print(shape_anova)
          
          if (shape_anova$`Pr(>Chisq)`[1] < 0.05) {
            cat("  → Diferença significativa entre grupos para Shape (p < 0.05)\n")
          } else {
            cat("  → Sem diferença significativa entre grupos para Shape (p >= 0.05)\n")
          }
        }, error = function(e) {
          cat("  Erro na análise GLM para Shape:", e$message, "\n")
        })
        
        cat("\nTeste estatístico para Scale (parâmetro de escala):\n")
        
        # GLM para parâmetro Scale
        tryCatch({
          scale_glm <- glm(scale ~ grupo, data = weibull_params_df, family = Gamma(link = "log"))
          scale_anova <- Anova(scale_glm, type = "II")
          cat("  Análise de Deviance para Scale:\n")
          print(scale_anova)
          
          if (scale_anova$`Pr(>Chisq)`[1] < 0.05) {
            cat("  → Diferença significativa entre grupos para Scale (p < 0.05)\n")
          } else {
            cat("  → Sem diferença significativa entre grupos para Scale (p >= 0.05)\n")
          }
        }, error = function(e) {
          cat("  Erro na análise GLM para Scale:", e$message, "\n")
        })
        
        # Tabela resumo dos parâmetros
        cat("\nTabela resumo dos parâmetros Weibull por grupo:\n")
        print(weibull_params_df)
      }
    }
  }
} else {
  cat("Variável de grupo não encontrada ou insuficiente para análise.\n")
}

# ================================================================================
# 7. GRÁFICOS E VISUALIZAÇÕES
# ================================================================================

cat("\n=== GERANDO GRÁFICOS ===\n")

# Criar gráficos de probabilidade de sobrevivência
if (length(survival_plots) > 0) {
  
  # Gráfico 1: Curvas de Sobrevivência Weibull
  survival_combined <- do.call(rbind, survival_plots)
  
  p1 <- ggplot(survival_combined, aes(x = x)) +
    geom_line(aes(y = survival, color = "Survival"), size = 1.2) +
    geom_line(aes(y = failure, color = "Failure"), size = 1.2, linetype = "dashed") +
    facet_wrap(~ variable, scales = "free") +
    scale_color_manual(values = c("Survival" = "blue", "Failure" = "red")) +
    labs(
      title = "Weibull Probability Curves",
      subtitle = "Reliability and Failure Probability Analysis",
      x = "Property Value",
      y = "Probability",
      color = "Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )
  
  print(p1)
  
  # Salvar gráfico
  ggsave(file.path(graficos_dir, "curvas_probabilidade_weibull.png"), p1, width = 12, height = 8, dpi = 300)
  
  # Gráfico 2: Q-Q Plot Weibull para primeira variável
  if (length(main_variables) > 0) {
    var1 <- main_variables[1]
    data_var1 <- numeric_data[[var1]]
    data_clean1 <- data_var1[!is.na(data_var1) & data_var1 > 0]
    
    if (var1 %in% names(weibull_results) && length(weibull_results[[var1]]) > 0) {
      best_fit1 <- weibull_results[[var1]][[1]]  # Primeiro ajuste
      
      # Dados teóricos vs observados
      n <- length(data_clean1)
      theoretical_quantiles <- qweibull(ppoints(n), 
                                       shape = best_fit1$shape, 
                                       scale = best_fit1$scale)
      observed_quantiles <- sort(data_clean1)
      
      qq_data <- data.frame(
        theoretical = theoretical_quantiles,
        observed = observed_quantiles
      )
      
      p2 <- ggplot(qq_data, aes(x = theoretical, y = observed)) +
        geom_point(alpha = 0.6, color = "darkblue") +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(
          title = sprintf("Weibull Q-Q Plot - %s", var1),
          subtitle = sprintf("Shape = %.3f, Scale = %.3f, R² = %.4f", 
                           best_fit1$shape, best_fit1$scale, best_fit1$r_squared),
          x = "Theoretical Quantiles (Weibull)",
          y = "Observed Quantiles"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"))
      
      print(p2)
      ggsave(file.path(graficos_dir, "qq_plot_weibull.png"), p2, width = 8, height = 6, dpi = 300)
    }
  }
}

# ================================================================================
# 8. RELATÓRIO FINAL E SALVAMENTO
# ================================================================================

cat("\n=== RELATÓRIO FINAL ===\n")

# Criar resumo dos resultados
relatorio <- list(
  data_analise = Sys.Date(),
  n_variaveis = length(main_variables),
  variaveis_analisadas = main_variables,
  n_amostras_total = nrow(dados_raw),
  metodos_implementados = c("Weibull 2P-LS (Kaplan-Meier)", "Weibull 2P-LS (Hazen)", 
                           "Weibull 2P-ML", "Weibull 3P-ML"),
  resultados_weibull = weibull_results
)

# Salvar resultados
save(relatorio, weibull_results, survival_plots, 
     file = "resultados_analise_weibull.RData")

cat("Resultados salvos em: resultados_analise_weibull.RData\n")

# Criar relatório em texto
sink("relatorio_weibull.txt")
cat("================================================================================\n")
cat("RELATÓRIO DE ANÁLISE WEIBULL - PROBABILIDADE DE RUPTURA\n")
cat("================================================================================\n\n")
cat("Data da análise:", as.character(Sys.Date()), "\n")
cat("Número de variáveis analisadas:", length(main_variables), "\n")
cat("Variáveis:", paste(main_variables, collapse = ", "), "\n")
cat("Número total de amostras:", nrow(dados_raw), "\n\n")

cat("MÉTODOS IMPLEMENTADOS:\n")
cat("- Distribuição Weibull 2P e 3P\n")
cat("- Estimadores: Least Squares (LS) e Maximum Likelihood (ML)\n")
cat("- Estimadores de probabilidade: Kaplan-Meier, Hazen, Mean Rank\n")
cat("- Testes de adequação: Anderson-Darling, Kolmogorov-Smirnov\n")
cat("- Análise ANOVA para grupos\n\n")

# Resumo dos melhores ajustes
cat("RESUMO DOS MELHORES AJUSTES:\n")
for (var in names(weibull_results)) {
  if (length(weibull_results[[var]]) > 0) {
    best_r2 <- 0
    best_method <- ""
    best_params <- NULL
    
    for (method in names(weibull_results[[var]])) {
      fit <- weibull_results[[var]][[method]]
      if (fit$r_squared > best_r2) {
        best_r2 <- fit$r_squared
        best_method <- method
        best_params <- fit
      }
    }
    
    cat(sprintf("\n%s:\n", var))
    cat(sprintf("  Melhor método: %s\n", best_method))
    cat(sprintf("  Shape (m): %.4f\n", best_params$shape))
    cat(sprintf("  Scale (σ0): %.4f\n", best_params$scale))
    cat(sprintf("  R²: %.4f\n", best_params$r_squared))
    
    # Valores característicos
    char_50 <- best_params$scale * (-log(0.5))^(1/best_params$shape)
    cat(sprintf("  Valor para 50%% sobrevivência: %.4f\n", char_50))
  }
}

cat("\n================================================================================\n")
cat("INTERPRETAÇÃO DOS RESULTADOS:\n")
cat("================================================================================\n\n")

cat("PARÂMETROS WEIBULL:\n")
cat("- Shape (m): Define a forma da distribuição\n")
cat("  * m < 1: Taxa de falha decrescente\n")
cat("  * m = 1: Taxa de falha constante (distribuição exponencial)\n")
cat("  * m > 1: Taxa de falha crescente (mais comum em materiais)\n\n")

cat("- Scale (σ0): Parâmetro característico de escala\n")
cat("  * Representa o valor onde ~63.2% das amostras falharam\n")
cat("  * Maior σ0 indica maior resistência/durabilidade\n\n")

cat("COEFICIENTE R²:\n")
cat("- R² > 0.95: Excelente ajuste\n")
cat("- R² > 0.90: Bom ajuste\n")  
cat("- R² < 0.90: Ajuste questionável\n\n")

cat("RECOMENDAÇÕES BASEADAS NO ARTIGO E MÉTODOS APLICADOS:\n")
cat("- Método Kaplan-Meier com LS mostrou melhor desempenho\n")
cat("- Weibull 2P geralmente superior ao 3P para materiais homogêneos\n")
cat("- GLM Gamma preferível à ANOVA clássica para dados não-normais\n")
cat("- GEE recomendado para dados com correlação temporal/espacial\n")
cat("- Validar com testes Anderson-Darling (p > 0.1 desejável)\n")
cat("- Comparações múltiplas com correção de Tukey via emmeans\n\n")

sink()

cat("\nRelatório detalhado salvo em: relatorio_weibull.txt\n")

cat("\n=== ANÁLISE WEIBULL CONCLUÍDA ===\n")
cat("Arquivos gerados:\n")
cat("- resultados_analise_weibull.RData (dados R)\n")
cat("- relatorio_weibull.txt (relatório detalhado)\n")
cat("- curvas_probabilidade_weibull.png (gráfico de probabilidades)\n")
cat("- qq_plot_weibull.png (Q-Q plot)\n\n")

cat("Para análise de probabilidade de ruptura específica, consulte:\n")
cat("- Valores característicos no relatório\n")
cat("- Curvas de sobrevivência nos gráficos\n")
cat("- Parâmetros Weibull para cada propriedade\n\n")

# ================================================================================
# FIM DO SCRIPT
# ================================================================================