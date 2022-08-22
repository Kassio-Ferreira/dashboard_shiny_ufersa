library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(DT)

metas_para_BI <- read_excel("./data/metas_para_BI.xlsx")



# Funcao 1: retorna lista de metas ----------------------------------------

lista_de_metas <- function(dados, perspectiva){
  return((metas_para_BI %>% filter(Perspectiva == perspectiva))$Meta %>% unique)
}

lista_de_metas(metas_para_BI, "Financeira") -> lista_financeira
lista_de_metas(metas_para_BI, "Sociedade") -> lista_sociedade
lista_de_metas(metas_para_BI, "Processos Internos") -> lista_processos_internos
lista_de_metas(metas_para_BI, "Aprendizagem e Crescimento") -> lista_aprendizagem_crescimento

# Filtra os dados para uma meta e retorna dataframe -----------------------

filtra_meta <- function(meta){
  
  metas_para_BI %>% filter(Meta == meta) %>%
    select(c(`2021`,`2022`,`2023`,`2024`,`2025`, "tipo")) %>% as.data.frame() %>%
    pivot_longer(cols = c(`2021`,`2022`,`2023`,`2024`,`2025`)) -> dados
  
  colnames(dados) <- c("tipo", "ano", "valor")
  
  return(dados)
  
}


# Filtra Perspectiva ------------------------------------------------------

filtra_perspectiva <- function(meta){
  (metas_para_BI %>% filter(Meta == meta))$Perspectiva %>% unique
}


# Filtra objetivo ---------------------------------------------------------

filtra_objetivo <- function(meta){
  (metas_para_BI %>% filter(Meta == meta))$`Objetivo Estratégico` %>% unique
}


# Filtra Status -----------------------------------------------------------

filtra_status <- function(meta){
  (metas_para_BI %>% filter(Meta == meta))$Status %>% unique
}


# Grafico ggplot2 ---------------------------------------------------------


# Funcao para fazer os graficos 

plot_meta <- function(dados_meta){
  
  q <- ggplot(data = dados_meta)
  q + geom_bar(aes(x = ano, y = valor, fill = tipo), stat = "identity",
               position = "dodge")
  
}

# cria tabela -------------------------------------------------------------

# cria_tabela <- function(dados){
#   
#   dados %>% datatable(rownames = FALSE)
#   
# }
# 
