if(!require(psych)) install.packages("psych")
library(psych)

rm(list = ls())

# setwd("<path/to/this/repo>")  # run from the repository root


ficheiros_csv <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
ficheiros_csv <- ficheiros_csv[!grepl("dataset_completo|Resultados_|Interpretacao_", basename(ficheiros_csv))]

cat("Encontrados", length(ficheiros_csv), "ficheiros CSV (Indicadores)\n\n")


limpar_nome <- function(nome) {
  n <- as.character(nome)
  n <- sub(".*:\\s*", "", n)
  n <- sub("\\s*\\(.*\\)", "", n)
  n <- trimws(n)
  n <- toupper(n)
  return(n)
}

extrair_codigo <- function(texto) {
  c <- as.character(texto)
  c <- trimws(sub(":.*", "", c))
  return(c)
}


cat("A mapear códigos e nomes...\n")
todos_pares <- list()

for (f in ficheiros_csv) {
  d <- read.csv2(f, header = TRUE, fileEncoding = "Latin1", nrows = 500)
  if (ncol(d) >= 1) {
    p <- data.frame(
      CodigoRaw = extrair_codigo(d[,1]),
      NomeLimpo = limpar_nome(d[,1]),
      stringsAsFactors = FALSE
    )
    p <- p[!is.na(p$CodigoRaw) & p$CodigoRaw != "" & !is.na(p$NomeLimpo) & p$NomeLimpo != "", ]
   
    p$Regiao <- substr(p$CodigoRaw, 1, 1)
    todos_pares[[f]] <- unique(p)
  }
}

base_identificadores <- unique(do.call(rbind, todos_pares))

cat("A gerar base de unificação inteligente...\n")

base_identificadores$ID_UNICO <- NA
id_counter <- 0

for (i in 1:nrow(base_identificadores)) {
  if (is.na(base_identificadores$ID_UNICO[i])) {
    id_counter <- id_counter + 1
    
    nome_i <- base_identificadores$NomeLimpo[i]
    codigo_i <- base_identificadores$CodigoRaw[i]
    regiao_i <- base_identificadores$Regiao[i]
    
    indices_ligados <- which(
      base_identificadores$CodigoRaw == codigo_i | 
      (base_identificadores$NomeLimpo == nome_i & base_identificadores$Regiao == regiao_i)
    )
    
    base_identificadores$ID_UNICO[indices_ligados] <- id_counter
  }
}


tabela_mestre <- aggregate(cbind(CodigoRaw, NomeLimpo) ~ ID_UNICO, 
                           data = base_identificadores, 
                           FUN = function(x) x[1])
colnames(tabela_mestre) <- c("ID_UNICO", "Codigo_Final", "Localizacao_Final")


tabela_mestre <- tabela_mestre[!tabela_mestre$Codigo_Final %in% c("1", "2", "3", "4"), ]


for (i in 1:nrow(tabela_mestre)) {
  reg <- substr(tabela_mestre$Codigo_Final[i], 1, 1)
  if (reg == "2" && !grepl("AÇORES|R.A.A", tabela_mestre$Localizacao_Final[i])) {
    tabela_mestre$Localizacao_Final[i] <- paste(tabela_mestre$Localizacao_Final[i], "(R.A.A.)")
  } else if (reg == "3" && !grepl("MADEIRA|R.A.M", tabela_mestre$Localizacao_Final[i])) {
    tabela_mestre$Localizacao_Final[i] <- paste(tabela_mestre$Localizacao_Final[i], "(R.A.M.)")
  }
}


cat("\nA processar indicadores...\n")
lista_indicadores <- list()

for (f in ficheiros_csv) {
  nome_indicador <- gsub("\\.csv$", "", basename(f))
  cat("Processando:", nome_indicador, "\n")
  
  dados_raw <- read.csv2(f, header = TRUE, fileEncoding = "Latin1", stringsAsFactors = FALSE, na.strings = c("", " ", "NA"))
  if (ncol(dados_raw) < 2) next
  
  df_f <- data.frame(
    CodigoRaw = extrair_codigo(dados_raw[,1]),
    NomeLimpo = limpar_nome(dados_raw[,1]),
    Valor = as.character(dados_raw[,2]), 
    stringsAsFactors = FALSE
  )
  df_f <- df_f[!is.na(df_f$CodigoRaw) & df_f$CodigoRaw != "", ]
  
  
  df_f <- merge(df_f, base_identificadores[, c("CodigoRaw", "NomeLimpo", "ID_UNICO")], 
                by = c("CodigoRaw", "NomeLimpo"), all.x = TRUE)
  

  df_final_f <- aggregate(Valor ~ ID_UNICO, data = df_f, FUN = function(x) x[1])
  colnames(df_final_f)[2] <- nome_indicador
  
  lista_indicadores[[f]] <- df_final_f
}


cat("\nA realizar merge final...\n")
merged_data <- Reduce(function(x, y) merge(x, y, by = "ID_UNICO", all = TRUE), lista_indicadores)


cat("A carregar dados de população para normalização...\n")
caminho_pop <- "Habitantes - Normalizado/População residente (N.º) por Local de residência .csv"

if (file.exists(caminho_pop)) {
  dados_pop <- read.csv2(caminho_pop, header = TRUE, fileEncoding = "Latin1", stringsAsFactors = FALSE)

  df_pop <- data.frame(
    CodigoRaw = extrair_codigo(dados_pop[,1]),
    NomeLimpo = limpar_nome(dados_pop[,1]),
    Populacao = as.character(dados_pop[,2]), 
    stringsAsFactors = FALSE
  )

  df_pop$Populacao <- as.numeric(gsub(" ", "", df_pop$Populacao)) 
  
  
  df_pop_mapped <- merge(df_pop, base_identificadores[, c("CodigoRaw", "NomeLimpo", "ID_UNICO")], 
                         by = c("CodigoRaw", "NomeLimpo"), all.x = TRUE)
  
  
  df_pop_final <- aggregate(Populacao ~ ID_UNICO, data = df_pop_mapped, FUN = max, na.action = na.omit)
  
 
  merged_data <- merge(merged_data, df_pop_final, by="ID_UNICO", all.x=TRUE)
  cat("População anexada. Total regs mapeados:", nrow(df_pop_final), "\n")
} else {
  stop("ERRO: Ficheiro de população não encontrado em: ", caminho_pop)
}

dataset_final <- merge(tabela_mestre, merged_data, by = "ID_UNICO", all.x = TRUE)
dataset_final$ID_UNICO <- NULL


cat("\nA limpar formatos numéricos...\n")
for(i in 3:ncol(dataset_final)) {
  
  if(is.character(dataset_final[,i])) {
    valores_limpos <- gsub("[%‰]", "", dataset_final[,i])
    val_limpos <- gsub(" ", "", valores_limpos) # Remover espaços 
    valores_limpos <- gsub(",", ".", valores_limpos)
    dataset_final[,i] <- as.numeric(valores_limpos)
  }
}


cat("\nA aplicar normalização por habitante...\n")
if ("Populacao" %in% names(dataset_final)) {
 
  cols_ignoradas <- c("Codigo_Final", "Localizacao_Final", "Populacao")
  cols_dados <- setdiff(names(dataset_final), cols_ignoradas)
  
  for (col in cols_dados) {
  
    if (grepl("por 1000 habitantes", col, ignore.case = TRUE)) {
      cat("  [CONVERT] /1000:", col, "\n")
      dataset_final[, col] <- dataset_final[, col] / 1000
      
      novo_nome <- gsub("por 1000 habitantes", "por habitante", col, ignore.case = TRUE)
      colnames(dataset_final)[which(names(dataset_final) == col)] <- novo_nome
      
    } else {
     
      padrao_ignorar <- "por habitante|%|‰|Taxa|Índice|Gini|€ hab\\.|€m2|Valor mediano|Ganho médio|Desigualdade|Densidade|per capita|Poder de compra"
      if (!grepl(padrao_ignorar, col, ignore.case = TRUE)) {
       
        cat("  [NORM   ] / Pop :", col, "\n")
        dataset_final[, col] <- dataset_final[, col] / dataset_final$Populacao
        
     
        colnames(dataset_final)[which(names(dataset_final) == col)] <- paste(col, "por habitante")
      }
    }
  }
} else {
  warning("Coluna 'Populacao' não encontrada. Normalização abortada.")
}


cat("Linhas (Municípios):", nrow(dataset_final), "\n")
cat("Colunas:", ncol(dataset_final), "\n")

library(VIM)
aggr(dataset_final)

library(mice)
md.pattern(dataset_final)


cat("\nA realizar imputação Múltipla (MICE)...\n")
library(mice)
library(stats)


meta_data <- dataset_final[, 1:2] 
dados_numericos <- dataset_final[, 3:ncol(dataset_final)]


dados_numericos <- dados_numericos[, sapply(dados_numericos, is.numeric)]


dados_numericos <- dados_numericos[, sapply(dados_numericos, function(x) {
  v <- var(x, na.rm = TRUE)
  return(!is.na(v) && v > 0)
})]


cor_matrix <- cor(dados_numericos, use = "pairwise.complete.obs")

cor_matrix[is.na(cor_matrix)] <- 0

altamente_cor <- findCorrelation_custom <- function(matriz_cor, cutoff=0.99) {
  
  matriz_cor[lower.tri(matriz_cor, diag=TRUE)] <- 0
  pares <- which(abs(matriz_cor) > cutoff, arr.ind = TRUE)
  if(nrow(pares) > 0) return(unique(rownames(matriz_cor)[pares[,2]]))
  return(c())
}

cols_to_remove <- altamente_cor(cor_matrix)

if (length(cols_to_remove) > 0) {
  cat("A remover colunas redundantes (correlação > 0.99) para evitar erros:\n")
  print(cols_to_remove)
  dados_numericos <- dados_numericos[, !names(dados_numericos) %in% cols_to_remove]
}


cat("A iniciar MICE usando método CART (robusto a colinearidade)...\n")
imp_model <- mice(dados_numericos, 
                  m = 5, 
                  method = 'cart', 
                  maxit = 5, 
                  seed = 500, 
                  printFlag = FALSE)

dataset_numerico_imputado <- complete(imp_model, 1)


dataset_final <- cbind(meta_data, dataset_numerico_imputado)

cat("Imputação MICE (método CART) concluída com sucesso.\n")


write.csv2(dataset_final, "dataset_completo_imputado.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nFicheiro 'dataset_completo_imputado.csv' gerado com sucesso (UTF-8).\n")


cat("\nVisualização final de missings (deve estar totalmente azul):\n")
aggr(dataset_final)
print(head(dataset_final[, 1:min(5, ncol(dataset_final))]))




rm(list = ls())
# setwd("<path/to/this/repo>")  # run from the repository root


if(!require(factoextra)) install.packages("factoextra")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(corrplot)) install.packages("corrplot")

library(factoextra)
library(ggplot2)
library(corrplot)


dados <- read.csv2("dataset_completo_imputado.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)


head(dados)


dados_ativos <- dados[, 3:ncol(dados)]

vars_remover <- c(
  "Beneficiáriasos.do.rendimento.social.de.inserção..da.segurança.social.por.habitante.em.idade.ativa....",
  "Crimes.registados..N.º..pelas.autoridades.policiais.por.Localização.geográfica.por.habitante",
  "Despesas.em.artes.visuais.....dos.municípios.por.Localização.geográfica.por.habitante",
  "Despesas.em.artesanato.....dos.municípios.por.Localização.geográfica.por.habitante",
  "Despesas.em.atividades.e.equipamentos.desportivos.dos.municípios.por.habitante.....por.Localização.geográfica",
  "Despesas.em.atividades.interdisciplinares.....dos.municípios.por.Localização.geográfica.por.habitante",
  "Investimentos.em.gestão.de.resíduos.dos.municípios.....por.Localização.geográfica.por.habitante",
  "Taxa.quinquenal.de.mortalidade.infantil.....por.Local.de.residência",
  "Investimentos.na.proteção.da.biodiversidade.e.paisagem.dos.municípios.....por.Localização.geográfica.por.habitante",
  "Saldo.migratório..N.º..por.Local.de.residência..por.habitante"
)


vars_existent <- intersect(names(dados_ativos), vars_remover)
if(length(vars_existent) > 0) {
    dados_ativos <- dados_ativos[, !names(dados_ativos) %in% vars_existent]
    cat("\n[REMOÇÃO] Variáveis removidas por baixa qualidade estatística:\n")
    print(vars_existent)
}


rownames(dados_ativos) <- make.unique(as.character(dados$Localizacao_Final))


cat("\n--- TESTE DE ADEQUAÇÃO KMO (Kaiser-Meyer-Olkin) ---\n")

kmo_res <- KMO(dados_ativos)
print(kmo_res$MSA) 


cat("\n--- TESTE DE ESFERICIDADE DE BARTLETT ---\n")

bartlett_res <- cortest.bartlett(cor(dados_ativos), n = nrow(dados_ativos))
print(bartlett_res$p.value)
cat("P-value (deve ser < 0.05):", bartlett_res$p.value, "\n")
cat("----------------------------------------------------\n")


pca_preliminar <- principal(dados_ativos, nfactors = ncol(dados_ativos), rotate = "none")
eigenvalues <- pca_preliminar$values

cat("\n--- ANÁLISE DE EIGENVALUES (CRITÉRIO DE KAISER) ---\n")
print(round(eigenvalues, 3))
n_fatores_kaiser <- sum(eigenvalues > 1)



fviz_eig(prcomp(dados_ativos, scale. = TRUE), addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Scree Plot", subtitle = paste("Sugestão Kaiser:", n_fatores_kaiser, "componentes"), y = "% Variância")


n_fatores_final <- 5 
pca_final <- principal(dados_ativos, nfactors = n_fatores_final, rotate = "varimax", scores = TRUE)

cat("\n--- RESUMO DA PCA COM ROTAÇÃO VARIMAX ---\n")
print(pca_final)


cargas <- unclass(pca_final$loadings)
cargas_df <- as.data.frame(round(cargas, 3))

cargas_df[abs(cargas_df) < 0.3] <- "" 
write.csv2(cargas_df, "Interpretacao_Indices_Loadings.csv", row.names = TRUE, fileEncoding = "UTF-8")


fa.diagram(pca_final, main = "Estrutura dos Índices (Rotação Varimax)")


novos_indices <- cbind(dados[, 1:2], pca_final$scores) 


write.csv2(novos_indices, "Resultados_Indices_PCA.csv", row.names = FALSE, fileEncoding = "UTF-8")



rm(list = ls())
graphics.off() 
# setwd("<path/to/this/repo>")  # run from the repository root


if(!require(factoextra)) install.packages("factoextra")
if(!require(cluster)) install.packages("cluster")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(pheatmap)) install.packages("pheatmap")

library(factoextra)
library(cluster)
library(ggplot2)
library(pheatmap)


dados_pca <- read.csv2("Resultados_Indices_PCA.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)


head(dados_pca)


dados_cluster <- dados_pca[, 3:ncol(dados_pca)]


rownames(dados_cluster) <- make.unique(as.character(dados_pca$Localizacao_Final))


summary(dados_cluster)


dist_eucl <- dist(dados_cluster, method = "euclidean")

dist_manh <- dist(dados_cluster, method = "manhattan")

dist_corr <- get_dist(dados_cluster, method = "pearson")



cat("Matrizes de distância calculadas.\n")

res_hc <- hclust(dist_eucl, method = "ward.D2")

fviz_dend(res_hc, 
          cex = 0.5, 
          k = 4, 
          color_labels_by_k = TRUE, 
          rect = TRUE,
          main = "Dendrograma de Municípios (Ward's Method)",
          xlab = "Municípios",
          ylab = "Distância")
# dev.off()
cat("Dendrograma salvo como 'Clustering_Dendrograma.pdf'.\n")


p1 <- fviz_nbclust(dados_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # Sugestão inicial
  labs(title = "Método Elbow")


p2 <- fviz_nbclust(dados_cluster, kmeans, method = "silhouette") +
  labs(title = "Método Silhouette")


gridExtra::grid.arrange(p1, p2, ncol = 2)
# dev.off()

cat("Gráficos de decisão de K salvos em 'Clustering_Determinacao_K.pdf'.\n")
cat("Por favor, analise os gráficos para definir o K final para o K-Means.\n")


k_escolhido <- 5

cat(paste("\n--- EXECUTANDO K-MEANS COM K =", k_escolhido, "---\n"))


set.seed(123) 
km_res <- kmeans(dados_cluster, centers = k_escolhido, nstart = 25)


fviz_cluster(km_res, data = dados_cluster,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = paste("Mapa de Clusters (K =", k_escolhido, ")"),
             repel = TRUE) 
# dev.off()
cat("Mapa de Clusters salvo em 'Clustering_Mapa_Final.pdf'.\n")


dados_finais <- dados_pca 
dados_finais$Cluster <- as.factor(km_res$cluster) 


perfil_medias <- aggregate(. ~ Cluster, data = dados_finais[, c(3:ncol(dados_pca), ncol(dados_finais))], mean)


perfil_medias_print <- perfil_medias
cols_num <- sapply(perfil_medias_print, is.numeric)
perfil_medias_print[, cols_num] <- round(perfil_medias_print[, cols_num], 2)

print("\n--- PERFIL DOS CLUSTERS (Médias dos Scores) ---")
print(perfil_medias_print)


write.csv2(perfil_medias, "Resumo_Perfil_Clusters.csv", row.names = FALSE, fileEncoding = "UTF-8")


cols_indices <- names(perfil_medias)[-1]
perfil_long <- data.frame(Cluster = integer(), Componente = character(), Score = numeric(), stringsAsFactors = FALSE)

for(col in cols_indices){
  temp <- data.frame(
    Cluster = perfil_medias$Cluster,
    Componente = col,
    Score = perfil_medias[[col]]
  )
  perfil_long <- rbind(perfil_long, temp)
}


p_heat <- ggplot(perfil_long, aes(x = Componente, y = factor(Cluster), fill = Score)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Score, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0) +
  theme_minimal() +
  labs(title = "Perfil dos Clusters (Valores Médios Padronizados)", 
       subtitle = "Vermelho = Acima da Média Global | Azul = Abaixo da Média Global",
       y = "Cluster", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heat)
# dev.off()
cat("Heatmap salvo em 'Clustering_Heatmap_Perfil.pdf'.\n")


cat("\n--- INICIANDO PROFILING COM VARIÁVEIS EXTERNAS ('Profile - Normalizado') ---\n")


extrair_codigo <- function(texto) {
  c <- as.character(texto)
  c <- trimws(sub(":.*", "", c))
  return(c)
}
limpar_nome <- function(nome) {
  n <- as.character(nome)
  n <- sub(".*:\\s*", "", n)
  n <- sub("\\s*\\(.*\\)", "", n)
  n <- trimws(n)
  n <- toupper(n)
  return(n)
}


file_pop <- "Habitantes - Normalizado/População residente (N.º) por Local de residência .csv"
if(file.exists(file_pop)) {
  dados_pop <- read.csv2(file_pop, header = TRUE, fileEncoding = "Latin1", stringsAsFactors = FALSE)
  df_pop <- data.frame(
    Codigo = extrair_codigo(dados_pop[,1]),
    Populacao = as.numeric(gsub(" ", "", dados_pop[,2])),
    stringsAsFactors = FALSE
  )
} else {
  stop("ERRO: Ficheiro de população não encontrado para normalização.")
}


path_profile <- "Profile - Normalizado"
files_profile <- list.files(path_profile, pattern = "\\.csv$", full.names = TRUE)


df_perfil_ext <- dados_finais[, c("Codigo_Final", "Localizacao_Final", "Cluster")]

df_perfil_ext$Codigo_Match <- extrair_codigo(df_perfil_ext$Codigo_Final) 


for (f in files_profile) {
  nome_var <- gsub("\\.csv$", "", basename(f))
  cat("Processando:", nome_var, "...\n")
  
  d_raw <- read.csv2(f, header = TRUE, fileEncoding = "Latin1", stringsAsFactors = FALSE)
  d_clean <- data.frame(
    Codigo = extrair_codigo(d_raw[,1]),
    Valor = as.numeric(gsub(" ", "", d_raw[,2])),
    stringsAsFactors = FALSE
  )
  
  
  valores_finais <- rep(NA, nrow(df_perfil_ext))
  
  for(i in 1:nrow(df_perfil_ext)) {
    cod <- df_perfil_ext$Codigo_Match[i]
    
    val <- d_clean$Valor[d_clean$Codigo == cod]
    if(length(val) > 0) valores_finais[i] <- val[1]
  }
  
  
  if (grepl("natalidade", nome_var, ignore.case = TRUE)) {
    
    cat("  -> Normalizando: / 1000 (Taxa)\n")
    valores_finais <- valores_finais / 1000
    nome_var <- paste(nome_var, "(por hab)")
    
  } else {
    
    cat("  -> Normalizando: / População\n")
    
    
    pop_vals <- rep(NA, nrow(df_perfil_ext))
    for(i in 1:nrow(df_perfil_ext)) {
      cod <- df_perfil_ext$Codigo_Match[i]
      p <- df_pop$Populacao[df_pop$Codigo == cod]
      if(length(p) > 0) pop_vals[i] <- p[1]
    }
    
    valores_finais <- valores_finais / pop_vals
    nome_var <- paste(nome_var, "(por hab)")
  }
  
  df_perfil_ext[[nome_var]] <- valores_finais
}


df_perfil_ext$Codigo_Match <- NULL


vars_ext <- names(df_perfil_ext)[4:ncol(df_perfil_ext)]
perfil_ext_medias <- aggregate(. ~ Cluster, data = df_perfil_ext[, c("Cluster", vars_ext)], mean, na.rm=TRUE)

print("\n--- PERFIL ESTENDIDO (Variáveis Externas) ---")

perfil_ext_print <- perfil_ext_medias
perfil_ext_print[,-1] <- round(perfil_ext_print[,-1], 6) 
print(perfil_ext_print)

write.csv2(perfil_ext_medias, "Resumo_Perfil_Externo.csv", row.names = FALSE, fileEncoding = "UTF-8")


perfil_ext_long <- data.frame(Cluster = integer(), Variavel = character(), Valor = numeric(), stringsAsFactors = FALSE)

for(col in vars_ext){
  vals <- perfil_ext_medias[[col]]
  vals_scaled <- scale(vals)
  
  temp <- data.frame(
    Cluster = perfil_ext_medias$Cluster,
    Variavel = col,
    Valor = as.numeric(vals_scaled) 
  )
  perfil_ext_long <- rbind(perfil_ext_long, temp)
}


p_heat_ext <- ggplot(perfil_ext_long, aes(x = Variavel, y = factor(Cluster), fill = Valor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Valor, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0) +
  theme_minimal() +
  labs(title = "Perfil Extendido dos Clusters (Variáveis de Validação)", 
       subtitle = "Valores Padronizados (Z-Score) | Vermelho = Superior à Média dos Clusters",
       y = "Cluster", x = "") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size=8))

print(p_heat_ext)
# dev.off()
cat("Heatmap externo salvo em 'Clustering_Heatmap_Perfil_Externo.pdf'.\n")


cat("Heatmap externo salvo em 'Clustering_Heatmap_Perfil_Externo.pdf'.\n")


cat("\n--- GERANDO VISUALIZAÇÕES AVANÇADAS (Silhouette, Uncertainty, Scatter, Density) ---\n")


if(!require(mclust)) install.packages("mclust")
library(mclust)



sil <- silhouette(km_res$cluster, dist(dados_cluster))
fviz_silhouette(sil, palette = "jco", ggtheme = theme_minimal(), main = "Gráfico de Silhueta (K-Means)")
# dev.off()
cat("-> Silhueta salva em 'Visualizacao_Silhouette_Plot.pdf'.\n")


cat("   A executar Mclust (Model-Based) para gerar Uncertainty Plot...\n")
mc_res <- Mclust(dados_cluster, G = k_escolhido) 


plot(mc_res, what = "uncertainty", main = "Gráfico de Incerteza (Model-Based Clustering)")
# dev.off()
cat("-> Uncertainty Plot salvo em 'Visualizacao_Uncertainty_Plot.pdf'.\n")


cols_plot <- colnames(dados_cluster)[1:min(4, ncol(dados_cluster))]

pairs(dados_cluster[, cols_plot], 
      col = km_res$cluster, 
      pch = 19, 
      main = "Scatterplots Bivariados (Cores = Clusters)",
      lower.panel = NULL) 

par(xpd=TRUE)
legend("bottomleft", legend = unique(km_res$cluster), fill = unique(km_res$cluster), title = "Cluster")
# dev.off()
cat("-> Scatterplots salvos em 'Visualizacao_Scatterplots_Bivariados.pdf'.\n")


clusplot(dados_cluster, km_res$cluster, 
         color=TRUE, shade=TRUE, 
         labels=2, lines=0, 
         main="Clusplot (Componentes Principais)")
# dev.off()
cat("-> Clusplot salvo em 'Visualizacao_Clusplot.pdf'.\n")

df_plot <- cbind(dados_cluster, Cluster = as.factor(km_res$cluster))

p_dens1 <- ggplot(df_plot, aes(x = df_plot[,1], fill = Cluster)) + 
  geom_density(alpha = 0.4) +
  labs(title = paste("Densidade por Cluster -", names(df_plot)[1]), x = "Score") +
  theme_minimal()

p_dens2 <- ggplot(df_plot, aes(x = df_plot[,2], fill = Cluster)) + 
  geom_density(alpha = 0.4) +
  labs(title = paste("Densidade por Cluster -", names(df_plot)[2]), x = "Score") +
  theme_minimal()


gridExtra::grid.arrange(p_dens1, p_dens2, ncol = 2)
# dev.off()
cat("-> Density Plots salvos em 'Visualizacao_Density_Plots.pdf'.\n")


dados_finais_completo <- merge(dados_finais, df_perfil_ext[, c("Codigo_Final", vars_ext)], by="Codigo_Final")

mclust_class <- data.frame(Codigo_Final = dados_pca$Codigo_Final, Cluster_Probabilistico = mc_res$classification)
dados_finais_completo <- merge(dados_finais_completo, mclust_class, by="Codigo_Final")

write.csv2(dados_finais_completo, "Resultados_Clustering_Final.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\nAnálise Concluída!\n")
cat("1. Verifique a pasta para todos os novos PDFs ('Visualizacao_*.pdf').\n")
cat("2. Analise 'Resumo_Perfil_Clusters.csv' (Índices PCA) e 'Resumo_Perfil_Externo.csv' (Novas Variáveis).\n")
cat("3. Dataset final atualizado em 'Resultados_Clustering_Final.csv' (inclui Clusters K-means e Mclust).\n")




