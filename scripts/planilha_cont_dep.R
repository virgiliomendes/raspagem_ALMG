######################################################################
# Ajuste das planilhas de controle para o IPD de Deputados Federais
# Quaest Pesquisa e Consultoria
# Equipe de dados: 
# Virgilio Mendes e Ana Beatriz
#
# ---
# 24/02/2021
# ---
######################################################################

# Preamble:

# ---------------------------------------------------------------------

# limpeza de environment:
rm(list = ls())

# roda codigo para raspagem de dados no site da camara
source("scrapping_deputados.R", encoding = "UTF-8")

# limpeza de environment:
rm(list = ls())

# Bibliotecas necessárias
library(tidyverse)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(questionr)
library(janitor)
library(here)

# tratamento de caracteres especiais:
source("remove_accent.R", encoding = "UTF-8")

# ---------------------------------------------------------------------

# Controle para reaproveitar perfis de Deputados Federais já coletados

## Passo-a-passo:

# 1 - Organização das planilhas com os nomes dos deputados atuais já coletados

# 2 - Organização das planilhas com os nomes atualizados dos deputados federais

# 3 - Join entre as planilhas pelo NOME DOS DEPUTADOS

# 4 - Teremos 2 outputs:
#     4.1 - Um data frame com os nomes corretos a partir do join 
#           (que estão presentes tanto nos dados já coletados quanto na lista de deputados atualizados)
#     4.2 - um data frame com os nomes sobresalentes, isto é, não tem pares em ambas as planilhas
#           (os deputados que não tem essa correspondência sairam do pleito e precisam ser substituídos)

# 5 - Com isso feito, ajustamos as correções de nomes e planilhas de controle

# ---------------------------------------------------------------------

# Passo 1:

# importa banco de controle existente e empilha em um unico data frame
bd1 <- NULL

for(i in 1:27){
  read_xlsx(here::here("data", "IC - DEPUTADOS BRASIL.xlsx"),
                  sheet = i) -> dados
  
  bd1 <- rbind(bd1, dados)

  print(i)
}

rm(dados, i)
# ---------------------------------------------------------------------

# Passo 2:

# importa banco de controle atualizado:

bd2 = read_xlsx(here::here("data", "dep_exercicio.xlsx"))

# ---------------------------------------------------------------------

# Passo 3:

# Tratando as variaveis de NOME DOS DEPUTADOS

# aplica a função names() para todos os objetos que estão no environment
# que começam com "bd"
map(mget(ls(pattern = "bd")), names)

# conta a quantidade de NAs total no Banco
sum(is.na(bd1))
sum(is.na(bd2))

# Posições onde há NA na variavel ID_1
which(is.na(bd1))
# Posições onde há NA na variavel ID_2
which(is.na(bd2))

# seleciona as colunas de interesse e retira NA
bd1 = bd1 %>% select(c(`Nome Padronizado`, Partido, UF, Facebook, Instagram, Twitter)) %>% 
  rename(Partido_1 = Partido,
         UF_1 = UF) %>% 
  filter(`Nome Padronizado` != is.na(T)) %>% 
  filter(Partido_1 != is.na(T)) %>% 
  filter(UF_1 != is.na(T))

# padronizando nomes
# 1.1 - SepararNome Padronizado do BD2 em 3 variaveis:
# Nome padronizado, Partido e UF
bd2 = bd2 %>% separate(`Nome Padronizado`, sep = "([(-)])", into = c("Nome Padronizado", "Partido")) %>% 
  separate(Partido, sep = "([-])", into = c("Partido", "UF"))


bd2$`Nome Padronizado` = bd2$`Nome Padronizado` %>% remove_accent()
bd2$`Nome Padronizado` = toupper(bd2$`Nome Padronizado`)

# remove espaco em braco no final
bd2$`Nome Padronizado` = stringr::str_trim(bd2$`Nome Padronizado`, side = "right")

# Mantem no BD2 apenas os Deputados em Exercicio
unique(bd2$status)

bd2 = bd2 %>% 
  filter(status == "Em exercício")


# unifica todas as linhas e colunas, mesmo sem correspondencia
bd_final = bd2 %>% left_join(bd1, by = "Nome Padronizado")



names(bd_final)


#
library(questionr)

teste_duplicata = freq(bd_final$`Nome Padronizado`)

teste_duplicata %>% filter(n > 1)

# ---------------------------------------------------------------------

# Passo 3:

write_xlsx(bd_final, "output/planilha_controle_atualizada.xlsx")

# ---------------------------------------------------------------------

# Fim

rm(list = ls())













