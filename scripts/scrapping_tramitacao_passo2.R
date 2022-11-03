

# ---
#
# Pesquisador:
# Virgílio de Araújo Mendes
#
# Contato:
# https://github.com/virgiliomendes/
# https://virgiliomendes.github.io/
# e-mail: virgilioebm@gmail.com
#
# ---
# 03/11/2022
# ---


# Codigo para raspagem de dados das informações
# referentes a tramitacao das proposicoes de cada 
# projeto na ALMG

## PREAMBULO -------------------------------------------------------------------


# Limpando nosso ambiente:
rm(list = ls())

# Se achar necessário:
options(mc.cores = parallel::detectCores())
gc()
memory.size(max=F)

# Carrega pacotes:
library(readxl)
library(tidyverse)
library(rvest)
library(stringr)

options(scipen = 999)


# RASPAGEM DE DADOS ALMG -------------------------------------------------------


# Tramitação

# Banco de raspagem tem que resultar nessas informações

# Linha:
#   - Projeto
# Coluna: 
#   - Tempo de tramitação
# - Aprovado ou não 
# - Modificado ou não 
# - Veto mantido ou derrubado


# criando um objeto com o endereço da pagina que queremos coletar
url <- "https://www.almg.gov.br/atividade_parlamentar/tramitacao_projetos/interna.html?a=2022&n=3827&t=PL"

html <- read_html(url) # lento e colocando em um objeto o conteudo da pagia coletada

pl <- html_nodes(html, xpath = '//*[@id="container"]/div[5]/h2') %>% html_text()

tramitacao <- html_nodes(html, xpath = '//div[5]/div[3]') %>% html_text()

tramitacaoc <- html_nodes(html, xpath = '//*[@id="js_tabTramitacao"]') %>% html_text()




# Criacao de Links para acesso --------------------------------------------

# link de cada tramitação
links <- read_excel("voto_almg_PEC.xlsx") %>% 
  select(`link completo`) %>% 
  mutate(id = 1:length(`link completo`))


# cria banco vazio
dados <- NULL


for(val in 1:length(links$id)){
  
  # pegar o link quando o id for igual a `i`
  
  #links$id == `i`
  url <- links %>% 
    filter(id == val) %>% 
    select(`link completo`)
  
  # tranforma em html
  html <- read_html(url$`link completo`)
  
  # Posição 1
  pl <- html_nodes(html, xpath = '//*[@id="container"]/div[5]/h2') %>% html_text()
  geral <- html_nodes(html, xpath = '///*[@id="container"]/div[5]/div[3]') %>% html_text()
  tramitacao <- html_nodes(html, xpath = '//*[@id="js_tabTramitacao"]') %>% html_text()
  
  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(pl, geral, tramitacao,
                   stringsAsFactors = F)
  
  # aqgui estamos jutando o data.frame df com o objeto que está fora do loop
  # o dados.
  dados <- rbind(dados, df)
  
  
  
  print(val) # o print i no final da coleta serve para exibir do console qual pagina foi coletada com sucesso
}
  

# Exportando dados raspados
writexl::write_xlsx(dados, "voto_teste.xlsx")

saveRDS(dados, "voto_teste.rds")


