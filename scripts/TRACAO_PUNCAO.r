# ===============================================================================
# ANÁLISE DE TRAÇÃO E PUNÇÃO - DEGRADAÇÃO FORÇADA DE GEOCOMPOSTOS
# Caracterização Mecânica de Fibras de Typha domingensis
# Combinação das análises básicas e avançadas
# Data: 12 de outubro de 2025
# ===============================================================================

# Limpeza do ambiente
rm(list = ls())

# Configurar repositório CRAN
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# ===============================================================================
# 1. CARREGAMENTO E INSTALAÇÃO DE PACOTES
# ===============================================================================

# Função para verificar e instalar pacotes se necessário
instalar_se_necessario <- function(pacotes) {
  novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
  if(length(novos_pacotes)) {
    cat("Instalando novos pacotes:", paste(novos_pacotes, collapse = ", "), "\n")
    install.packages(novos_pacotes, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  sapply(pacotes, function(pkg) {
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "carregado com sucesso\n")
      return(TRUE)
    } else {
      cat("✗ Erro ao carregar", pkg, "\n")
      return(FALSE)
    }
  })
}

# Lista de pacotes necessários
pacotes_necessarios <- c(
  "readxl",       # Leitura de arquivos Excel
  "car",          # ANOVA, testes de pressupostos
  "emmeans",      # Comparações múltiplas
  "ggplot2",      # Visualizações
  "dplyr",        # Manipulação de dados
  "stringr",      # Manipulação de strings
  "geepack",      # Equações de Estimação Generalizadas
  "boot",         # Bootstrapping
  "effectsize",   # Tamanhos de efeito
  "broom",        # Organização de outputs
  "gridExtra",    # Arranjo de gráficos
  "nortest",      # Testes de normalidade adicionais
  "lmtest",       # Testes de diagnóstico
  "knitr",        # Tabelas
  "corrplot",     # Matriz de correlação
  "reshape2",     # Reestruturação de dados
  "viridis"       # Paleta de cores
)

# Instalar e carregar pacotes
cat("=== CARREGANDO PACOTES PARA ANÁLISE MECÂNICA ===\n")
instalar_se_necessario(pacotes_necessarios)

# ===============================================================================
# 2. CARREGAMENTO E PREPARAÇÃO DOS DADOS
# ===============================================================================

# Definir caminho do arquivo
caminho_dados <- "DB.xlsx"

# Verificar se o arquivo existe
if (!file.exists(caminho_dados)) {
  cat("Arquivo DB.xlsx não encontrado no diretório atual!\n")
  cat("Tentando caminho completo...\n")
  caminho_dados <- "C:/Users/vidal/OneDrive/Documentos/1 - ACADEMICO/20 - ARTIGOS/1 - ARTIGOS PENDENTES/2 - MEUS ARTIGOS/4 - DEGRADAÇÃO FORÇADA/2 - DADOS/4 - ANALISES ESTATISTICAS/DB.xlsx"
}

if (!file.exists(caminho_dados)) {
  stop("Arquivo DB.xlsx não encontrado em nenhum dos caminhos especificados!")
}

cat("=== CARREGANDO DADOS MECÂNICOS ===\n")
cat("Arquivo:", caminho_dados, "\n")

# Listar abas disponíveis
abas_disponiveis <- readxl::excel_sheets(caminho_dados)
cat("Abas disponíveis:", paste(abas_disponiveis, collapse = ", "), "\n")

# Carregar dados (primeira aba)
dados_raw <- readxl::read_excel(caminho_dados, sheet = 1)

# Visualizar estrutura inicial
cat("\n=== ESTRUTURA INICIAL DOS DADOS ===\n")
str(dados_raw)
head(dados_raw)

# ===============================================================================
# 3. LIMPEZA E PREPARAÇÃO DOS DADOS
# ===============================================================================

cat("\n=== PREPARAÇÃO DOS DADOS MECÂNICOS ===\n")

# Limpar e padronizar dados
dados <- dados_raw %>%
  # Renomear colunas removendo espaços e acentos
  rename(
    tensao_max = `tensão máxima`,
    extensao_max = `extensão máxima`,
    repeticao = `repetição`
  ) %>%
  # Extrair informações do tratamento
  mutate(
    # Extrair tempo dos nomes de tratamento
    tempo_dias = case_when(
      str_detect(tratamento, "10JN") ~ 10,
      str_detect(tratamento, "20JN") ~ 20,
      str_detect(tratamento, "30JN") ~ 30,
      str_detect(tratamento, "60JN") ~ 60,
      str_detect(tratamento, "90JN") ~ 90,
      str_detect(tratamento, "120JN") ~ 120,
      str_detect(tratamento, "180JN") ~ 180,
      str_detect(tratamento, "CONTROLE|controle") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Extrair tipo de tratamento
    tipo_tratamento = case_when(
      str_detect(tratamento, "RESINADO|resinado") ~ "Resinado",
      str_detect(tratamento, "CONTROLE|controle") ~ "Controle", 
      str_detect(tratamento, "NATURAL|natural") ~ "Natural",
      TRUE ~ "Outro"
    ),
    
    # Calcular rigidez secante (aproximação)
    rigidez_sec = case_when(
      !is.na(tensao_max) & !is.na(extensao_max) & extensao_max > 0 ~ tensao_max / extensao_max,
      TRUE ~ NA_real_
    ),
    
    # Criar ID único
    id = row_number(),
    
    # Converter para fatores
    tipo_tratamento = factor(tipo_tratamento),
    tempo_dias = factor(tempo_dias),
    repeticao = factor(repeticao)
  ) %>%
  # Filtrar observações válidas
  filter(!is.na(tensao_max)) %>%
  # Reordenar colunas
  select(id, tratamento, tipo_tratamento, tempo_dias, repeticao, 
         tensao_max, extensao_max, rigidez_sec)

# Mostrar estrutura final
cat("\n=== ESTRUTURA FINAL DOS DADOS ===\n")
str(dados)
summary(dados)

# Distribuição dos tratamentos
cat("\n=== DISTRIBUIÇÃO DOS TRATAMENTOS ===\n")
table(dados$tipo_tratamento, dados$tempo_dias, useNA = "ifany")

# ===============================================================================
# 4. ANÁLISE EXPLORATÓRIA DOS DADOS
# ===============================================================================

cat("\n=== ANÁLISE EXPLORATÓRIA DOS DADOS MECÂNICOS ===\n")

# Estatísticas descritivas básicas
estatisticas_descritivas <- function() {
  
  # Função para calcular estatísticas
  desc_stats <- function(x) {
    c(
      n = length(x[!is.na(x)]),
      média = round(mean(x, na.rm = TRUE), 3),
      mediana = round(median(x, na.rm = TRUE), 3),
      dp = round(sd(x, na.rm = TRUE), 3),
      cv = round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100, 2),
      min = round(min(x, na.rm = TRUE), 3),
      max = round(max(x, na.rm = TRUE), 3)
    )
  }
  
  # Aplicar a variáveis numéricas
  variaveis_numericas <- c("tensao_max", "extensao_max", "rigidez_sec")
  variaveis_numericas <- variaveis_numericas[variaveis_numericas %in% names(dados)]
  
  if (length(variaveis_numericas) > 0) {
    cat("=== ESTATÍSTICAS DESCRITIVAS ===\n")
    stats_resultado <- sapply(dados[variaveis_numericas], desc_stats)
    print(t(stats_resultado))
    
    return(stats_resultado)
  }
}

# Executar estatísticas descritivas
stats_gerais <- estatisticas_descritivas()

# ===============================================================================
# 5. TESTES DE PRESSUPOSTOS
# ===============================================================================

cat("\n=== TESTES DE PRESSUPOSTOS PARA DADOS MECÂNICOS ===\n")

# Função para teste de normalidade completo
teste_normalidade_completo <- function(variavel, nome_var) {
  if (is.numeric(variavel) && length(variavel[!is.na(variavel)]) > 3) {
    cat("\n--- Testes de Normalidade para", nome_var, "---\n")
    
    var_limpa <- variavel[!is.na(variavel)]
    
    # Shapiro-Wilk
    if (length(var_limpa) <= 5000 && length(var_limpa) >= 3) {
      sw_test <- shapiro.test(var_limpa)
      cat("Shapiro-Wilk: W =", round(sw_test$statistic, 4), 
          ", p-valor =", format(sw_test$p.value, scientific = TRUE), "\n")
    }
    
    # Kolmogorov-Smirnov (Lilliefors)
    tryCatch({
      ks_test <- nortest::lillie.test(var_limpa)
      cat("Lilliefors (KS): D =", round(ks_test$statistic, 4), 
          ", p-valor =", format(ks_test$p.value, scientific = TRUE), "\n")
    }, error = function(e) cat("Erro no teste KS\n"))
    
    # Anderson-Darling
    tryCatch({
      ad_test <- nortest::ad.test(var_limpa)
      cat("Anderson-Darling: A =", round(ad_test$statistic, 4), 
          ", p-valor =", format(ad_test$p.value, scientific = TRUE), "\n")
    }, error = function(e) cat("Erro no teste AD\n"))
  }
}

# Aplicar testes de normalidade
variaveis_teste <- c("tensao_max", "extensao_max", "rigidez_sec")
for (var in variaveis_teste) {
  if (var %in% names(dados)) {
    teste_normalidade_completo(dados[[var]], var)
  }
}

# ===============================================================================
# 6. ANÁLISES ESTATÍSTICAS ESPECÍFICAS
# ===============================================================================

cat("\n=== ANÁLISES ESTATÍSTICAS ESPECÍFICAS ===\n")

# *** ANÁLISE DA TENSÃO MÁXIMA ***
analisar_tensao_maxima <- function(dados) {
  
  cat("\n=== ANÁLISE DA TENSÃO MÁXIMA ===\n")
  
  resultados <- list()
  
  # 1. ANOVA por tipo de tratamento
  if (length(unique(dados$tipo_tratamento)) > 1) {
    cat("\n1. ANOVA - Tensão Máxima por Tipo de Tratamento:\n")
    
    modelo_anova <- aov(tensao_max ~ tipo_tratamento, data = dados)
    resultados$anova_tratamento <- summary(modelo_anova)
    print(resultados$anova_tratamento)
    
    # Teste de Tukey
    if (summary(modelo_anova)[[1]][["Pr(>F)"]][1] < 0.05) {
      tukey_test <- TukeyHSD(modelo_anova)
      resultados$tukey_tratamento <- tukey_test
      cat("\nComparações múltiplas (Tukey HSD):\n")
      print(tukey_test)
    }
    
    # Teste de homocedasticidade
    levene_test <- car::leveneTest(tensao_max ~ tipo_tratamento, data = dados)
    resultados$levene_tratamento <- levene_test
    cat("\nTeste de Levene (Homocedasticidade):\n")
    print(levene_test)
  }
  
  # 2. Análise temporal
  if (!all(is.na(dados$tempo_dias)) && length(unique(dados$tempo_dias[!is.na(dados$tempo_dias)])) > 1) {
    cat("\n2. ANÁLISE TEMPORAL:\n")
    
    dados_tempo <- dados[!is.na(dados$tempo_dias), ]
    
    # ANOVA por tempo
    modelo_tempo <- aov(tensao_max ~ tempo_dias, data = dados_tempo)
    resultados$anova_tempo <- summary(modelo_tempo)
    print(resultados$anova_tempo)
    
    # Correlação com tempo
    tempo_numerico <- as.numeric(as.character(dados_tempo$tempo_dias))
    correlacao <- cor.test(dados_tempo$tensao_max, tempo_numerico, method = "pearson")
    resultados$correlacao_tempo <- correlacao
    cat("\nCorrelação tensão vs tempo:\n")
    cat("r =", round(correlacao$estimate, 4), ", p =", 
        format(correlacao$p.value, scientific = TRUE), "\n")
  }
  
  # 3. Modelo GLM Gamma
  if (all(dados$tensao_max > 0, na.rm = TRUE)) {
    cat("\n3. MODELO GLM - DISTRIBUIÇÃO GAMMA:\n")
    
    tryCatch({
      modelo_glm <- glm(tensao_max ~ tipo_tratamento, 
                        data = dados, 
                        family = Gamma(link = "log"))
      resultados$glm_gamma <- summary(modelo_glm)
      print(resultados$glm_gamma)
      
      # Análise de Deviance (Anova tipo II)
      cat("\nAnálise de Deviance (Anova tipo II):\n")
      anova_glm <- Anova(modelo_glm, type = "II")
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
      
      cat("\nCritérios do modelo GLM:\n")
      cat("AIC:", round(AIC(modelo_glm), 2), "\n")
      cat("Deviance residual:", round(modelo_glm$deviance, 2), "\n")
      cat("Dispersão:", round(modelo_glm$deviance/modelo_glm$df.residual, 4), "\n")
      
    }, error = function(e) {
      cat("Erro no modelo GLM:", e$message, "\n")
    })
  }
  
  # 4. Estatísticas por grupo
  cat("\n4. ESTATÍSTICAS DESCRITIVAS POR GRUPO:\n")
  desc_grupos <- dados %>%
    group_by(tipo_tratamento) %>%
    summarise(
      n = n(),
      media = round(mean(tensao_max, na.rm = TRUE), 2),
      mediana = round(median(tensao_max, na.rm = TRUE), 2),
      dp = round(sd(tensao_max, na.rm = TRUE), 2),
      cv = round(sd(tensao_max, na.rm = TRUE) / mean(tensao_max, na.rm = TRUE) * 100, 2),
      min_val = round(min(tensao_max, na.rm = TRUE), 2),
      max_val = round(max(tensao_max, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  resultados$desc_grupos <- desc_grupos
  print(desc_grupos)
  
  return(resultados)
}

# *** ANÁLISE DA EXTENSÃO MÁXIMA ***
analisar_extensao_maxima <- function(dados) {
  
  cat("\n=== ANÁLISE DA EXTENSÃO MÁXIMA ===\n")
  
  dados_ext <- dados[!is.na(dados$extensao_max), ]
  
  if (nrow(dados_ext) < 3) {
    cat("Dados insuficientes para análise de extensão\n")
    return(NULL)
  }
  
  resultados <- list()
  
  # ANOVA por tratamento
  if (length(unique(dados_ext$tipo_tratamento)) > 1) {
    cat("\nANOVA - Extensão Máxima por Tratamento:\n")
    
    modelo_anova_ext <- aov(extensao_max ~ tipo_tratamento, data = dados_ext)
    resultados$anova_extensao <- summary(modelo_anova_ext)
    print(resultados$anova_extensao)
    
    # Estatísticas por grupo
    desc_ext <- dados_ext %>%
      group_by(tipo_tratamento) %>%
      summarise(
        n = n(),
        media = round(mean(extensao_max, na.rm = TRUE), 3),
        mediana = round(median(extensao_max, na.rm = TRUE), 3),
        dp = round(sd(extensao_max, na.rm = TRUE), 3),
        cv = round(sd(extensao_max, na.rm = TRUE) / mean(extensao_max, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      )
    
    resultados$desc_extensao <- desc_ext
    cat("\nEstatísticas Descritivas - Extensão:\n")
    print(desc_ext)
  }
  
  return(resultados)
}

# *** ANÁLISE DA RIGIDEZ SECANTE ***
analisar_rigidez_secante <- function(dados) {
  
  cat("\n=== ANÁLISE DA RIGIDEZ SECANTE ===\n")
  
  dados_rig <- dados[!is.na(dados$rigidez_sec), ]
  
  if (nrow(dados_rig) < 3) {
    cat("Dados insuficientes para análise de rigidez\n")
    return(NULL)
  }
  
  resultados <- list()
  
  # ANOVA por tratamento
  if (length(unique(dados_rig$tipo_tratamento)) > 1) {
    cat("\nANOVA - Rigidez Secante por Tratamento:\n")
    
    modelo_anova_rig <- aov(rigidez_sec ~ tipo_tratamento, data = dados_rig)
    resultados$anova_rigidez <- summary(modelo_anova_rig)
    print(resultados$anova_rigidez)
    
    # Estatísticas por grupo
    desc_rig <- dados_rig %>%
      group_by(tipo_tratamento) %>%
      summarise(
        n = n(),
        media = round(mean(rigidez_sec, na.rm = TRUE), 2),
        mediana = round(median(rigidez_sec, na.rm = TRUE), 2),
        dp = round(sd(rigidez_sec, na.rm = TRUE), 2),
        cv = round(sd(rigidez_sec, na.rm = TRUE) / mean(rigidez_sec, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      )
    
    resultados$desc_rigidez <- desc_rig
    cat("\nEstatísticas Descritivas - Rigidez:\n")
    print(desc_rig)
  }
  
  return(resultados)
}

# Executar todas as análises
resultados_tensao <- analisar_tensao_maxima(dados)
resultados_extensao <- analisar_extensao_maxima(dados)
resultados_rigidez <- analisar_rigidez_secante(dados)

# ===============================================================================
# 7. ANÁLISES AVANÇADAS
# ===============================================================================

cat("\n=== ANÁLISES AVANÇADAS ===\n")

# *** BOOTSTRAP PARA INTERVALOS DE CONFIANÇA ***
realizar_bootstrap <- function(dados, n_replicacoes = 1000) {
  
  cat("\n--- BOOTSTRAP PARA INTERVALOS DE CONFIANÇA ---\n")
  
  # Função para calcular estatísticas bootstrap
  boot_stats <- function(data, indices) {
    d <- data[indices, ]
    return(c(
      media_tensao = mean(d$tensao_max, na.rm = TRUE),
      dp_tensao = sd(d$tensao_max, na.rm = TRUE),
      mediana_tensao = median(d$tensao_max, na.rm = TRUE)
    ))
  }
  
  # Realizar bootstrap
  set.seed(123)  # Para reprodutibilidade
  boot_resultado <- boot(dados, boot_stats, R = n_replicacoes)
  
  # Intervalos de confiança
  ic_media <- boot.ci(boot_resultado, type = "perc", index = 1)
  ic_dp <- boot.ci(boot_resultado, type = "perc", index = 2)
  ic_mediana <- boot.ci(boot_resultado, type = "perc", index = 3)
  
  cat("Intervalos de Confiança Bootstrap (95%):\n")
  cat("Média da Tensão:", round(ic_media$percent[4:5], 2), "\n")
  cat("Desvio-Padrão:", round(ic_dp$percent[4:5], 2), "\n")
  cat("Mediana:", round(ic_mediana$percent[4:5], 2), "\n")
  
  return(list(
    boot_resultado = boot_resultado,
    ic_media = ic_media,
    ic_dp = ic_dp,
    ic_mediana = ic_mediana
  ))
}

# *** DETECÇÃO DE OUTLIERS ***
detectar_outliers <- function(dados) {
  
  cat("\n--- DETECÇÃO DE OUTLIERS ---\n")
  
  # Método IQR
  Q1 <- quantile(dados$tensao_max, 0.25, na.rm = TRUE)
  Q3 <- quantile(dados$tensao_max, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  
  outliers_iqr <- which(dados$tensao_max < limite_inferior | dados$tensao_max > limite_superior)
  
  # Método Z-score
  z_scores <- abs(scale(dados$tensao_max))
  outliers_z <- which(z_scores > 2.5)
  
  cat("Método IQR:\n")
  cat("Limites:", round(limite_inferior, 2), "-", round(limite_superior, 2), "\n")
  cat("Outliers identificados:", length(outliers_iqr), "\n")
  
  cat("\nMétodo Z-score (|z| > 2.5):\n")
  cat("Outliers identificados:", length(outliers_z), "\n")
  
  if (length(outliers_iqr) > 0) {
    cat("\nObservações outliers (IQR):\n")
    print(dados[outliers_iqr, c("id", "tensao_max", "tipo_tratamento", "tempo_dias")])
  }
  
  return(list(
    outliers_iqr = outliers_iqr,
    outliers_z = outliers_z,
    limites_iqr = c(limite_inferior, limite_superior)
  ))
}

# *** ANÁLISE DE CORRELAÇÕES ***
analisar_correlacoes <- function(dados) {
  
  cat("\n--- ANÁLISE DE CORRELAÇÕES ---\n")
  
  # Correlação tensão vs extensão
  if (!all(is.na(dados$extensao_max))) {
    dados_completos <- dados[!is.na(dados$tensao_max) & !is.na(dados$extensao_max), ]
    
    if (nrow(dados_completos) > 3) {
      cor_tensao_extensao <- cor.test(dados_completos$tensao_max, dados_completos$extensao_max)
      cat("Tensão vs Extensão:\n")
      cat("r =", round(cor_tensao_extensao$estimate, 4), 
          ", p =", format(cor_tensao_extensao$p.value, scientific = TRUE), "\n")
    }
  }
  
  # Correlação tensão vs rigidez
  if (!all(is.na(dados$rigidez_sec))) {
    dados_rig <- dados[!is.na(dados$tensao_max) & !is.na(dados$rigidez_sec), ]
    
    if (nrow(dados_rig) > 3) {
      cor_tensao_rigidez <- cor.test(dados_rig$tensao_max, dados_rig$rigidez_sec)
      cat("Tensão vs Rigidez:\n")
      cat("r =", round(cor_tensao_rigidez$estimate, 4), 
          ", p =", format(cor_tensao_rigidez$p.value, scientific = TRUE), "\n")
    }
  }
  
  # Correlação com tempo
  if (!all(is.na(dados$tempo_dias))) {
    dados_tempo <- dados[!is.na(dados$tempo_dias), ]
    tempo_numerico <- as.numeric(as.character(dados_tempo$tempo_dias))
    
    cor_tempo <- cor.test(dados_tempo$tensao_max, tempo_numerico)
    cat("Tensão vs Tempo:\n")
    cat("r =", round(cor_tempo$estimate, 4), 
        ", p =", format(cor_tempo$p.value, scientific = TRUE), "\n")
  }
}

# Executar análises avançadas
bootstrap_resultados <- realizar_bootstrap(dados, 1000)
outliers_resultados <- detectar_outliers(dados)
analisar_correlacoes(dados)

# ===============================================================================
# 8. VISUALIZAÇÕES
# ===============================================================================

cat("\n=== GERANDO VISUALIZAÇÕES ===\n")

# Função para criar visualizações completas
criar_visualizacoes_completas <- function(dados) {
  
  graficos <- list()
  
  tryCatch({
    # 1. Histograma da tensão máxima
    p1 <- ggplot(dados, aes(x = tensao_max)) +
      geom_histogram(bins = 15, fill = "steelblue", alpha = 0.7, color = "black") +
      geom_vline(aes(xintercept = mean(tensao_max, na.rm = TRUE)), 
                 color = "red", linetype = "dashed", size = 1) +
      labs(title = "Distribuição da Tensão Máxima",
           subtitle = paste("Média:", round(mean(dados$tensao_max, na.rm = TRUE), 2), "N"),
           x = "Tensão Máxima (N)", y = "Frequência") +
      theme_minimal()
    graficos[["hist_tensao"]] <- p1
    
    # 2. Boxplot por tratamento
    p2 <- ggplot(dados, aes(x = tipo_tratamento, y = tensao_max, fill = tipo_tratamento)) +
      geom_boxplot(alpha = 0.7, outlier.color = "red") +
      geom_jitter(width = 0.2, alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black") +
      labs(title = "Tensão Máxima por Tipo de Tratamento",
           x = "Tipo de Tratamento", y = "Tensão Máxima (N)",
           fill = "Tratamento") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    graficos[["box_tratamento"]] <- p2
    
    # 3. Evolução temporal (se disponível)
    if (!all(is.na(dados$tempo_dias))) {
      p3 <- ggplot(dados, aes(x = tempo_dias, y = tensao_max, color = tipo_tratamento)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
        labs(title = "Evolução Temporal da Tensão Máxima",
             x = "Tempo (dias)", y = "Tensão Máxima (N)",
             color = "Tipo de Tratamento") +
        theme_minimal() +
        theme(legend.position = "bottom")
      graficos[["evolucao_temporal"]] <- p3
    }
    
    # 4. Correlação tensão vs extensão
    if (!all(is.na(dados$extensao_max))) {
      p4 <- ggplot(dados, aes(x = tensao_max, y = extensao_max, color = tipo_tratamento)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
        labs(title = "Relação Tensão vs Extensão Máxima",
             x = "Tensão Máxima (N)", y = "Extensão Máxima (%)",
             color = "Tipo de Tratamento") +
        theme_minimal() +
        theme(legend.position = "bottom")
      graficos[["tensao_extensao"]] <- p4
    }
    
    # 5. Gráfico de rigidez
    if (!all(is.na(dados$rigidez_sec))) {
      p5 <- ggplot(dados, aes(x = tipo_tratamento, y = rigidez_sec, fill = tipo_tratamento)) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(title = "Rigidez Secante por Tipo de Tratamento",
             x = "Tipo de Tratamento", y = "Rigidez Secante (N/%)",
             fill = "Tratamento") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      graficos[["rigidez_tratamento"]] <- p5
    }
    
    # 6. Diagnóstico de outliers
    p6 <- ggplot(dados, aes(x = "", y = tensao_max)) +
      geom_boxplot(fill = "lightblue", alpha = 0.7) +
      geom_jitter(width = 0.1, alpha = 0.5, color = "darkblue") +
      labs(title = "Diagnóstico de Outliers - Tensão Máxima",
           x = "", y = "Tensão Máxima (N)") +
      theme_minimal() +
      coord_flip()
    graficos[["outliers"]] <- p6
    
  }, error = function(e) {
    cat("Erro ao criar gráficos:", e$message, "\n")
  })
  
  return(graficos)
}

# Criar e exibir gráficos
graficos_completos <- criar_visualizacoes_completas(dados)

# Exibir gráficos individuais
if (length(graficos_completos) > 0) {
  for (nome_grafico in names(graficos_completos)) {
    print(graficos_completos[[nome_grafico]])
  }
}

# ===============================================================================
# 9. RELATÓRIO FINAL CONSOLIDADO
# ===============================================================================

cat("\n=== RELATÓRIO FINAL - ANÁLISE MECÂNICA ===\n")
cat("Análise concluída em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Arquivo analisado:", caminho_dados, "\n")
cat("Total de observações:", nrow(dados), "\n")
cat("Variáveis analisadas: tensão máxima, extensão máxima, rigidez secante\n")

# Resumo dos resultados principais
cat("\n=== RESUMO DOS RESULTADOS ===\n")
cat("1. Tensão máxima média:", round(mean(dados$tensao_max, na.rm = TRUE), 2), "N\n")
cat("2. Desvio-padrão:", round(sd(dados$tensao_max, na.rm = TRUE), 2), "N\n")
cat("3. Coeficiente de variação:", round(sd(dados$tensao_max, na.rm = TRUE) / mean(dados$tensao_max, na.rm = TRUE) * 100, 2), "%\n")

if (length(outliers_resultados$outliers_iqr) >= 0) {
  cat("4. Outliers detectados (IQR):", length(outliers_resultados$outliers_iqr), "\n")
}

if (!all(is.na(dados$tempo_dias))) {
  cat("5. Período de análise:", min(dados$tempo_dias, na.rm = TRUE), "a", 
      max(dados$tempo_dias, na.rm = TRUE), "dias\n")
}

cat("\n=== PRÓXIMOS PASSOS ===\n")
cat("1. Análise das propriedades hidrofísicas (arquivo separado)\n")
cat("2. Integração dos resultados mecânicos e hidrofísicos\n")
cat("3. Análise de correlação canônica\n")
cat("4. Modelagem não-linear das curvas de degradação\n")

# Salvar workspace
save.image("tracao_puncao_resultados.RData")
cat("\nWorkspace salvo como: tracao_puncao_resultados.RData\n")

cat("\n=== ANÁLISE MECÂNICA CONCLUÍDA ===\n")

# ===============================================================================
# 10. INFORMAÇÕES DA SESSÃO
# ===============================================================================

cat("\n=== INFORMAÇÕES DA SESSÃO R ===\n")
sessionInfo()

# ===============================================================================
# NOTAS PARA O USUÁRIO:
# 
# Este script consolida as análises básicas e avançadas dos dados mecânicos
# (tração e punção) dos geocompostos. As análises incluem:
# 
# - Estatísticas descritivas completas
# - Testes de pressupostos (normalidade, homocedasticidade)
# - ANOVA e comparações múltiplas
# - Modelos GLM com distribuição Gamma
# - Bootstrap para intervalos de confiança robustos
# - Detecção de outliers (IQR e Z-score)
# - Análises de correlação
# - Visualizações completas
# 
# Para análises hidrofísicas, utilize o arquivo HIDROFISICAS.r
# ===============================================================================