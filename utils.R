library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(reshape2)
library(DT)
library(scales)

metas_para_BI <- read_excel("data/metas_para_BI.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "text", "text", "text"))



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
  
  dados_meta$ano <- as.factor(dados_meta$ano)

  # Here we define spaces as the big separator
  point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

  dados_meta %>% ggplot(aes(x = ano, y = valor, fill = tipo)) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill=guide_legend(title="")) +
    scale_y_continuous(labels = point) + theme_minimal()
  
  
  # dados_meta %>%
  #   plot_ly(x = ~ ano, y = ~ valor, color = ~tipo, type = "bar",
  #           text = df$valor, textposition = "auto",
  #           textfont = list(color = 'rgb(0,0,0)')) %>%
  #   config(displaylogo = FALSE,
  #          modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
  #                                     "zoomIn2d", "zoomOut2d", "autoScale2d",
  #                                     "resetScale2d", "hoverClosestCartesian",
  #                                     "hoverCompareCartesian"))
  
  
}

# cria tabela -------------------------------------------------------------

# cria_tabela <- function(dados){
#   
#   dados %>% datatable(rownames = FALSE)
#   
# }
# 

filtra_tabela <- function(meta_alvo){
  
  metas_para_BI[, -(12:13)] %>% filter(Meta == meta_alvo) %>%
    datatable(rownames = FALSE, 
              extensions = "Buttons",
              options = list(
                dom = "Bfrtip",
                buttons = c('copy', 'csv','excel','pdf', 'print')
              )
    )

}


