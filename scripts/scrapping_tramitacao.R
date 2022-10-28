


# RASPAGEM DE DADOS ALMG -------------------------------------------------------

# apagando todos objeto da area de trabalho
rm(list = ls())



library(readxl)
library(tidyverse)

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




# ------------------------------------------------------------------------------

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


# TRATAMENTO -------------------------------------------------------------------



# importa
dados <- readRDS("voto_teste.rds")

# tratando dados:
dados$geral <- str_replace_all(dados$geral, "\t", "")
dados$geral <- str_replace_all(dados$geral, "\n", " ")

dados$tramitacao <- str_replace_all(dados$tramitacao, "\t", "")
dados$tramitacao <- str_replace_all(dados$tramitacao, "\n", " ")


# exporta:
writexl::write_xlsx(dados, "voto_tramitacao_PEC.xlsx")

# JUNTANDO DADOS ---------------------------------------------------------------


# importa
bd1 <- readxl::read_xlsx("voto_tramitacao.xlsx")

bd2 <- readxl::read_xlsx("voto_almg_governo.xlsx")


unique(bd1$pl)
unique(bd2$titulo)

names(bd2)

bd_final <- bd2 %>% 
  left_join(bd1, by = "pl")


writexl::write_xlsx(bd_final, "final_tramitacao.xlsx")


# TRATANDO DADOS ---------------------------------------------------------------

rm(list = ls())

bd <- readxl::read_xlsx("final_tramitacao.xlsx")

# O que é para retirar:
# bd$Local <- "Regime de Tramitação"
# bd$`Situação atual` <- "Local"
# bd$Ementa <- "Situação Atual"
# bd$status <- "Situação: "

bd$status <- stringr::str_remove(bd$status, "Situação: ")

bd$Local <- stringr::str_remove(bd$Local, "Regime de Tramitação")

bd$`Situação atual` <- stringr::str_remove(bd$`Situação atual`, "Local")

bd$Ementa <- stringr::str_remove(bd$Ementa, "Situação Atual")


writexl::write_xlsx(bd, "final_tramitacao(15.09.2022).xlsx")


# 
teste <- bd %>% 
  rename(tramitacao1 = `Tramitação Detalhes`) %>% 
  select(pl, tramitacao1)


teste$datas <- str_extract_all(teste$tramitacao1, "[:digit:]{2}\\/[:digit:]{2}\\/[:digit:]{4}")
teste$datas <- as.character(teste$datas)

writexl::write_xlsx(teste, "teste(15.09.2022).xlsx")


# contando intervalo de tramitacao

library(lubridate)

bd <- readxl::read_xlsx("teste(15.09.2022).xlsx")



bd$intervalo <- ymd(bd$inicio) %--% ymd(bd$final) 

bd$intervalo <- bd$intervalo / ddays(1)               # Número de dias


## [1] 30

writexl::write_xlsx(bd, "teste(15.09.2022).xlsx")

# Votação

bd$votacao <- stringr::str_split(bd$tramitacao1, "Votação")

bd$votacao <- as.character(bd$votacao)



writexl::write_xlsx(bd, "teste(15.09.2022).xlsx")


# Split Votação ------------------------------

rm(list = ls())

# Carrega pacotes:
library(readxl)
library(tidyverse)
library(janitor)
library(questionr)
library(data.table)

# Importa dados:
bd <- read_xlsx("final(10.10.2022).xlsx") %>% 
  select(pl, tramitacao_detalhes)

# Separar votacoes para cada PL:

## separador "votação"
# referencia: https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
vetor_sep <- stringr::str_split(bd$tramitacao_detalhes, pattern = "Votação", simplify = F, n = 20)

# cumputa numero de vetores nas listas
indx <- sapply(vetor_sep, length)

# separa as strings em cada coluna (com a col MAX vindo da lista) 
bd_final <- as.data.frame(do.call(rbind,lapply(vetor_sep, `length<-`,
                                          max(indx))))

# renomeia as colunas
colnames(bd_final) <- names(vetor_sep[[which.max(indx)]])

# remove
rm(vetor_sep, indx)

# cria ID
bd$id <- 1:length(bd$pl)

bd_final$id <- 1:116

# juntar bancos tratados
bd_f <- cbind(bd, bd_final)

# Exporta dados:
writexl::write_xlsx(bd_f, "votacao-proposicao(14.10.2022).xlsx")

# Fim!

# Modificado ------------------------------

rm(list = ls())

# Carrega pacotes:
library(readxl)
library(tidyverse)
library(janitor)
library(questionr)
library(data.table)

# Importa dados:
bd <- read_xlsx("banco.tramitacao.final(21.10.2022).xlsx") %>% 
  select(pl, tramitacao_detalhes)


# Cria variaveis:
bd$substitutivo <- NA

bd$emenda <- NA

bd$emenda1t <- NA

bd$emenda2t <- NA


# Identifica quais PLs teve substitutivo a partir da variavel tramitacao:
bd$substitutivo[str_detect(bd$tramitacao_detalhes, "Substitutivo | ubstitutivo")] = "Sim"
bd$emenda[str_detect(bd$tramitacao_detalhes, "Emenda | menda")] = "Sim"
bd$emenda1t[str_detect(bd$tramitacao_detalhes, "Emenda 1| menda 1")] = "Sim"
bd$emenda2t[str_detect(bd$tramitacao_detalhes, "Emenda 2| menda 2")] = "Sim"

# Transformar NA de colunas que viram resposta em "NA"
bd[, 3:6][is.na(bd[, 3:6])] <- "NSA"


# Identifica quais PLs foram modificadas
bd = bd %>% 
  mutate(modificado = case_when(
    substitutivo == "NSA" & emenda == "Sim" ~ "Sim",
    substitutivo == "Sim" & emenda == "NSA" ~ "Sim",
    substitutivo == "Sim" & emenda == "Sim" ~ "Sim",
    TRUE ~ "Não"
  ))


# Exporta dados:
writexl::write_xlsx(bd, "tramitacao-modificado(21.10.2022).xlsx")

# Ajuste votacao ------------------------------

rm(list = ls())

# Carrega pacotes:
library(readxl)
library(tidyverse)
library(janitor)
library(questionr)
library(data.table)

# Importa dados:
bd <- read_xlsx("entrega final/votacao-proposicao(28.10.2022).xlsx", sheet = 2) %>% 
  clean_names() %>% 
  select(pl, votacao)

bd$votacao <- str_replace_all(bd$votacao, pattern = "-", replacement = ",")
bd$votacao <- str_replace_all(bd$votacao, pattern = "–", replacement = ",")
bd$votacao <- str_replace_all(bd$votacao, pattern = "\\)", replacement = ",")

#bd$votacao <- janitor::make_clean_names(bd$votacao)
partido <- read_xlsx("extras/partido.xlsx")

# Remove caracteres extras
bd$votacao <- str_remove_all(bd$votacao, "\\(")

# Remove partidos
bd$votacao <- str_remove_all(bd$votacao, "Avante")
bd$votacao <- str_remove_all(bd$votacao, "AVANTE")
bd$votacao <- str_remove_all(bd$votacao, "Cidadania")
bd$votacao <- str_remove_all(bd$votacao, "DEM")
bd$votacao <- str_remove_all(bd$votacao, "MDB")
bd$votacao <- str_remove_all(bd$votacao, "Novo")
bd$votacao <- str_remove_all(bd$votacao, "NOVO")
bd$votacao <- str_remove_all(bd$votacao, "Patri")
bd$votacao <- str_remove_all(bd$votacao, "PATRI")
bd$votacao <- str_remove_all(bd$votacao, "PCdoB")
bd$votacao <- str_remove_all(bd$votacao, "PDT")
bd$votacao <- str_remove_all(bd$votacao, "PL")
bd$votacao <- str_remove_all(bd$votacao, "Pode")
bd$votacao <- str_remove_all(bd$votacao, "PODE")
bd$votacao <- str_remove_all(bd$votacao, "PP")
bd$votacao <- str_remove_all(bd$votacao, "Pros")
bd$votacao <- str_remove_all(bd$votacao, "PROS")
bd$votacao <- str_remove_all(bd$votacao, "PRTB")
bd$votacao <- str_remove_all(bd$votacao, "PSB")
bd$votacao <- str_remove_all(bd$votacao, "PSC")
bd$votacao <- str_remove_all(bd$votacao, "PSD")
bd$votacao <- str_remove_all(bd$votacao, "PSD")
bd$votacao <- str_remove_all(bd$votacao, "PSDB")
bd$votacao <- str_remove_all(bd$votacao, "PSL")
bd$votacao <- str_remove_all(bd$votacao, "Psol")
bd$votacao <- str_remove_all(bd$votacao, "PSOL")
bd$votacao <- str_remove_all(bd$votacao, "PT")
bd$votacao <- str_remove_all(bd$votacao, "PTB")
bd$votacao <- str_remove_all(bd$votacao, "PV")
bd$votacao <- str_remove_all(bd$votacao, "Rede")
bd$votacao <- str_remove_all(bd$votacao, "REDE")
bd$votacao <- str_remove_all(bd$votacao, "Republicanos")
bd$votacao <- str_remove_all(bd$votacao, "REPUBLICANOS")
bd$votacao <- str_remove_all(bd$votacao, "Solidariedade")
bd$votacao <- str_remove_all(bd$votacao, "SOLIDARIEDADE")



# Exporta dados:
writexl::write_xlsx(bd, "votacao.nomes(28.10.2022).xlsx")


# Classificacao Votacao Final ------------------------------

rm(list = ls())

# Carrega pacotes:
library(readxl)
library(tidyverse)
library(janitor)
library(questionr)
library(data.table)

# Importa dados:
bd <- read_xlsx("votacao.nomes(28.10.2022).xlsx", sheet = 1) %>% 
  clean_names()


# se o padrão textual "nome do deputado" (Linha X, coluna 01) está presente na célula, 
# colocar "Sim", senão manter "NA".

bd$pl_1005_2019[str_detect(bd$pl_1005_2019, pattern = bd$deputado)] = "Sim"
bd$pl_1005_2019[str_detect(bd$pl_1005_2019, pattern = "Sim", negate = T)] = "Não"

























