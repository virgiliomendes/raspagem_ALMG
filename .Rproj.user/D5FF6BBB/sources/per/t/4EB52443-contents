
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


# Codigo para tratamento dos dados coletados da ALMG.
# Os dados compoe no total informações sobre as proposições,
# que incluem as tramitações (para cada tipo de proposição)
# e as votaçoes (1 ou 2 turno) para cada projeto

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
library(janitor)
library(questionr)
library(data.table)

options(scipen = 999)

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


# contando intervalo de tramitacao - numero de dias

library(lubridate)

bd <- readxl::read_xlsx("teste(15.09.2022).xlsx")

bd$intervalo <- ymd(bd$inicio) %--% ymd(bd$final)

# Número de dias
bd$intervalo <- bd$intervalo / ddays(1)


# exportação

writexl::write_xlsx(bd, "teste(15.09.2022).xlsx")

# Votação

bd$votacao <- stringr::str_split(bd$tramitacao1, "Votação")

bd$votacao <- as.character(bd$votacao)



writexl::write_xlsx(bd, "teste(15.09.2022).xlsx")


# Fim!

# Incluindo Substitutivo e Modificado ------------------------------

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

# Previa votacao: Partidos ------------------------------

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
bd <- read_xlsx("entrega final/votacao.nomes(30.10.2022).xlsx", sheet = 1)



# se o padrão textual "nome do deputado" (Linha X, coluna 01) está presente na célula, 
# colocar "Sim", senão manter "NA".

# Ex:
# bd$PL_1005_2019[str_detect(bd$PL_1005_2019, pattern = bd$Deputado)] = "Sim"
# bd$PL_1005_2019[str_detect(bd$PL_1005_2019, pattern = "Sim", negate = T)] = "Não"




# Reordenando as variaveis
bd <- bd %>% 
  select(Deputado, PL_1005_2019, PL_1006_2019, PL_1006_2019_1t, PL_1006_2019_2t, PL_1007_2019, PL_1008_2019, 
         PL_1009_2019, PL_1009_2019_1t, PL_1010_2019, PL_1010_2019_1t, PL_1010_2019_2t, PL_1011_2019, 
         PL_1013_2019_1t, PL_1013_2019_2t, PL_1014_2019_1t, PL_1014_2019_2t, PL_1015_2019_1t, PL_1015_2019_2t, 
         PL_1016_2019_1t, PL_1016_2019_2t, PL_1085_2019, PL_1125_2019, PL_1126_2019, PL_1127_2019, PL_1165_2019, 
         PL_1166_2019, PL_1167_2019, PL_1202_2019, PL_1203_2019, PL_1204_2019_1t, PL_1204_2019_2t, PL_1205_2019_1t, 
         PL_1205_2019_2t, PL_1287_2019, PL_1355_2019_1t, PL_1355_2019_2t, PL_1440_2020, PL_1451_2020_1t, 
         PL_1451_2020_2t, PL_1725_2020, PL_1726_2020, PL_1750_2020, PL_1751_2020, PL_1752_2020, PL_1938_2020, 
         PL_1966_2020, PL_2052_2020, PL_2136_2020, PL_2141_2020, PL_2150_2020, PL_2150_2020_1t, PL_2150_2020_2t, 
         PL_2201_2020, PL_2202_2020, PL_2252_2020, PL_2273_2020, PL_2274_2020, PL_2275_2020, PL_2275_2020_1t, 
         PL_2275_2020_2t, PL_2276_2020, PL_2508_2021, PL_2509_2021, PL_2657_2021, PL_2707_2021, PL_2770_2021, 
         PL_2771_2021, PL_2884_2021, PL_2885_2021, PL_2924_2021, PL_2937_2021, PL_2976_2021, PL_3191_2021, 
         PL_3192_2021, PL_3199_2021, PL_3211_2021, PL_3256_2021, PL_3399_2021, PL_3407_2021, PL_3409_2021, 
         PL_367_2019, PL_368_2019, PL_450_2019_1t, PL_450_2019_2t, PL_451_2019, PL_452_2019, PL_734_2019, 
         PL_872_2019, PL_873_2019_1t, PL_876_2019_1t, PL_877_2019, PLC_10_2019_1t, PLC_10_2019_2t, PLC_28_2019_1t,
         PLC_28_2019_2t, PLC_38_2020, PLC_46_2020, PLC_46_2020_1t, PLC_46_2020_2t, PLC_48_2020, PLC_60_2021,
         PLC_60_2021_1t, PLC_60_2021_2t, PLC_64_2021, PLC_65_2021, PLC_75_2021, PLC_75_2021_1t, PLC_75_2021_2t,
         VET_10_2019, VET_11_2019, VET_12_2019, VET_13_2019, VET_14_2019, VET_15_2019, VET_16_2020, VET_17_2020, 
         VET_18_2020, VET_19_2020, VET_20_2020, VET_21_2020, VET_22_2020, VET_23_2020, VET_24_2021, VET_25_2021,
         VET_26_2021, VET_27_2021, VET_28_2021, VET_29_2021, VET_3_2019, VET_30_2021, VET_31_2021, VET_32_2021, 
         VET_33_2021, VET_4_2019, VET_5_2019, VET_6_2019, VET_7_2019, VET_8_2019, VET_9_2019, PEC_71_2021, PEC_57_2020, 
         PEC_55_2020, PEC_55_2020_1t, PEC_55_2020_2t)

# Codigo para determinar voto "Sim"
bd$PL_1005_2019[str_detect(bd$PL_1005_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1006_2019[str_detect(bd$PL_1006_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1006_2019_1t[str_detect(bd$PL_1006_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1006_2019_2t[str_detect(bd$PL_1006_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1007_2019[str_detect(bd$PL_1007_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1008_2019[str_detect(bd$PL_1008_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1009_2019[str_detect(bd$PL_1009_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1009_2019_1t[str_detect(bd$PL_1009_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1010_2019[str_detect(bd$PL_1010_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1010_2019_1t[str_detect(bd$PL_1010_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1010_2019_2t[str_detect(bd$PL_1010_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1011_2019[str_detect(bd$PL_1011_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1013_2019_1t[str_detect(bd$PL_1013_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1013_2019_2t[str_detect(bd$PL_1013_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1014_2019_1t[str_detect(bd$PL_1014_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1014_2019_2t[str_detect(bd$PL_1014_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1015_2019_1t[str_detect(bd$PL_1015_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1015_2019_2t[str_detect(bd$PL_1015_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1016_2019_1t[str_detect(bd$PL_1016_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1016_2019_2t[str_detect(bd$PL_1016_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1085_2019[str_detect(bd$PL_1085_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1125_2019[str_detect(bd$PL_1125_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1126_2019[str_detect(bd$PL_1126_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1127_2019[str_detect(bd$PL_1127_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1165_2019[str_detect(bd$PL_1165_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1166_2019[str_detect(bd$PL_1166_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1167_2019[str_detect(bd$PL_1167_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1202_2019[str_detect(bd$PL_1202_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1203_2019[str_detect(bd$PL_1203_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1204_2019_1t[str_detect(bd$PL_1204_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1204_2019_2t[str_detect(bd$PL_1204_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1205_2019_1t[str_detect(bd$PL_1205_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1205_2019_2t[str_detect(bd$PL_1205_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1287_2019[str_detect(bd$PL_1287_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_1355_2019_1t[str_detect(bd$PL_1355_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1355_2019_2t[str_detect(bd$PL_1355_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1440_2020[str_detect(bd$PL_1440_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1451_2020_1t[str_detect(bd$PL_1451_2020_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_1451_2020_2t[str_detect(bd$PL_1451_2020_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_1725_2020[str_detect(bd$PL_1725_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1726_2020[str_detect(bd$PL_1726_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1750_2020[str_detect(bd$PL_1750_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1751_2020[str_detect(bd$PL_1751_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1752_2020[str_detect(bd$PL_1752_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1938_2020[str_detect(bd$PL_1938_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_1966_2020[str_detect(bd$PL_1966_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2052_2020[str_detect(bd$PL_2052_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2136_2020[str_detect(bd$PL_2136_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2141_2020[str_detect(bd$PL_2141_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2150_2020[str_detect(bd$PL_2150_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2150_2020_1t[str_detect(bd$PL_2150_2020_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_2150_2020_2t[str_detect(bd$PL_2150_2020_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_2201_2020[str_detect(bd$PL_2201_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2202_2020[str_detect(bd$PL_2202_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2252_2020[str_detect(bd$PL_2252_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2273_2020[str_detect(bd$PL_2273_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2274_2020[str_detect(bd$PL_2274_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2275_2020[str_detect(bd$PL_2275_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2275_2020_1t[str_detect(bd$PL_2275_2020_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_2275_2020_2t[str_detect(bd$PL_2275_2020_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_2276_2020[str_detect(bd$PL_2276_2020, pattern = bd$Deputado)] = "Sim"
bd$PL_2508_2021[str_detect(bd$PL_2508_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2509_2021[str_detect(bd$PL_2509_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2657_2021[str_detect(bd$PL_2657_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2707_2021[str_detect(bd$PL_2707_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2770_2021[str_detect(bd$PL_2770_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2771_2021[str_detect(bd$PL_2771_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2884_2021[str_detect(bd$PL_2884_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2885_2021[str_detect(bd$PL_2885_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2924_2021[str_detect(bd$PL_2924_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2937_2021[str_detect(bd$PL_2937_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_2976_2021[str_detect(bd$PL_2976_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3191_2021[str_detect(bd$PL_3191_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3192_2021[str_detect(bd$PL_3192_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3199_2021[str_detect(bd$PL_3199_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3211_2021[str_detect(bd$PL_3211_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3256_2021[str_detect(bd$PL_3256_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3399_2021[str_detect(bd$PL_3399_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3407_2021[str_detect(bd$PL_3407_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_3409_2021[str_detect(bd$PL_3409_2021, pattern = bd$Deputado)] = "Sim"
bd$PL_367_2019[str_detect(bd$PL_367_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_368_2019[str_detect(bd$PL_368_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_450_2019_1t[str_detect(bd$PL_450_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_450_2019_2t[str_detect(bd$PL_450_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PL_451_2019[str_detect(bd$PL_451_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_452_2019[str_detect(bd$PL_452_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_734_2019[str_detect(bd$PL_734_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_872_2019[str_detect(bd$PL_872_2019, pattern = bd$Deputado)] = "Sim"
bd$PL_873_2019_1t[str_detect(bd$PL_873_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_876_2019_1t[str_detect(bd$PL_876_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PL_877_2019[str_detect(bd$PL_877_2019, pattern = bd$Deputado)] = "Sim"
bd$PLC_10_2019_1t[str_detect(bd$PLC_10_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PLC_10_2019_2t[str_detect(bd$PLC_10_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PLC_28_2019_1t[str_detect(bd$PLC_28_2019_1t, pattern = bd$Deputado)] = "Sim"
bd$PLC_28_2019_2t[str_detect(bd$PLC_28_2019_2t, pattern = bd$Deputado)] = "Sim"
bd$PLC_38_2020[str_detect(bd$PLC_38_2020, pattern = bd$Deputado)] = "Sim"
bd$PLC_46_2020[str_detect(bd$PLC_46_2020, pattern = bd$Deputado)] = "Sim"
bd$PLC_46_2020_1t[str_detect(bd$PLC_46_2020_1t, pattern = bd$Deputado)] = "Sim"
bd$PLC_46_2020_2t[str_detect(bd$PLC_46_2020_2t, pattern = bd$Deputado)] = "Sim"
bd$PLC_48_2020[str_detect(bd$PLC_48_2020, pattern = bd$Deputado)] = "Sim"
bd$PLC_60_2021[str_detect(bd$PLC_60_2021, pattern = bd$Deputado)] = "Sim"
bd$PLC_60_2021_1t[str_detect(bd$PLC_60_2021_1t, pattern = bd$Deputado)] = "Sim"
bd$PLC_60_2021_2t[str_detect(bd$PLC_60_2021_2t, pattern = bd$Deputado)] = "Sim"
bd$PLC_64_2021[str_detect(bd$PLC_64_2021, pattern = bd$Deputado)] = "Sim"
bd$PLC_65_2021[str_detect(bd$PLC_65_2021, pattern = bd$Deputado)] = "Sim"
bd$PLC_75_2021[str_detect(bd$PLC_75_2021, pattern = bd$Deputado)] = "Sim"
bd$PLC_75_2021_1t[str_detect(bd$PLC_75_2021_1t, pattern = bd$Deputado)] = "Sim"
bd$PLC_75_2021_2t[str_detect(bd$PLC_75_2021_2t, pattern = bd$Deputado)] = "Sim"
bd$VET_10_2019[str_detect(bd$VET_10_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_11_2019[str_detect(bd$VET_11_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_12_2019[str_detect(bd$VET_12_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_13_2019[str_detect(bd$VET_13_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_14_2019[str_detect(bd$VET_14_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_15_2019[str_detect(bd$VET_15_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_16_2020[str_detect(bd$VET_16_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_17_2020[str_detect(bd$VET_17_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_18_2020[str_detect(bd$VET_18_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_19_2020[str_detect(bd$VET_19_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_20_2020[str_detect(bd$VET_20_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_21_2020[str_detect(bd$VET_21_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_22_2020[str_detect(bd$VET_22_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_23_2020[str_detect(bd$VET_23_2020, pattern = bd$Deputado)] = "Sim"
bd$VET_24_2021[str_detect(bd$VET_24_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_25_2021[str_detect(bd$VET_25_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_26_2021[str_detect(bd$VET_26_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_27_2021[str_detect(bd$VET_27_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_28_2021[str_detect(bd$VET_28_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_29_2021[str_detect(bd$VET_29_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_3_2019[str_detect(bd$VET_3_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_30_2021[str_detect(bd$VET_30_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_31_2021[str_detect(bd$VET_31_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_32_2021[str_detect(bd$VET_32_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_33_2021[str_detect(bd$VET_33_2021, pattern = bd$Deputado)] = "Sim"
bd$VET_4_2019[str_detect(bd$VET_4_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_5_2019[str_detect(bd$VET_5_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_6_2019[str_detect(bd$VET_6_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_7_2019[str_detect(bd$VET_7_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_8_2019[str_detect(bd$VET_8_2019, pattern = bd$Deputado)] = "Sim"
bd$VET_9_2019[str_detect(bd$VET_9_2019, pattern = bd$Deputado)] = "Sim"
bd$PEC_71_2021[str_detect(bd$PEC_71_2021, pattern = bd$Deputado)] = "Sim"
bd$PEC_57_2020[str_detect(bd$PEC_57_2020, pattern = bd$Deputado)] = "Sim"
bd$PEC_55_2020[str_detect(bd$PEC_55_2020, pattern = bd$Deputado)] = "Sim"
bd$PEC_55_2020_1t [str_detect(bd$PEC_55_2020_1t , pattern = bd$Deputado)] = "Sim"
bd$PEC_55_2020_2t [str_detect(bd$PEC_55_2020_2t , pattern = bd$Deputado)] = "Sim"


# Codigo para determinar voto "Não votou"/"Branco"

bd <- bd %>% mutate(PL_1005_2019 = case_when(PL_1005_2019 != "Branco" & PL_1005_2019 != "Contrario" & PL_1005_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1005_2019),
                    PL_1006_2019 = case_when(PL_1006_2019 != "Branco" & PL_1006_2019 != "Contrario" & PL_1006_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1006_2019),
                    PL_1006_2019_1t = case_when(PL_1006_2019_1t != "Branco" & PL_1006_2019_1t != "Contrario" & PL_1006_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1006_2019_1t),
                    PL_1006_2019_2t = case_when(PL_1006_2019_2t != "Branco" & PL_1006_2019_2t != "Contrario" & PL_1006_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1006_2019_2t),
                    PL_1007_2019 = case_when(PL_1007_2019 != "Branco" & PL_1007_2019 != "Contrario" & PL_1007_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1007_2019),
                    PL_1008_2019 = case_when(PL_1008_2019 != "Branco" & PL_1008_2019 != "Contrario" & PL_1008_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1008_2019),
                    PL_1009_2019 = case_when(PL_1009_2019 != "Branco" & PL_1009_2019 != "Contrario" & PL_1009_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1009_2019),
                    PL_1009_2019_1t = case_when(PL_1009_2019_1t != "Branco" & PL_1009_2019_1t != "Contrario" & PL_1009_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1009_2019_1t),
                    PL_1010_2019 = case_when(PL_1010_2019 != "Branco" & PL_1010_2019 != "Contrario" & PL_1010_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1010_2019),
                    PL_1010_2019_1t = case_when(PL_1010_2019_1t != "Branco" & PL_1010_2019_1t != "Contrario" & PL_1010_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1010_2019_1t),
                    PL_1010_2019_2t = case_when(PL_1010_2019_2t != "Branco" & PL_1010_2019_2t != "Contrario" & PL_1010_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1010_2019_2t),
                    PL_1011_2019 = case_when(PL_1011_2019 != "Branco" & PL_1011_2019 != "Contrario" & PL_1011_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1011_2019),
                    PL_1013_2019_1t = case_when(PL_1013_2019_1t != "Branco" & PL_1013_2019_1t != "Contrario" & PL_1013_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1013_2019_1t),
                    PL_1013_2019_2t = case_when(PL_1013_2019_2t != "Branco" & PL_1013_2019_2t != "Contrario" & PL_1013_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1013_2019_2t),
                    PL_1014_2019_1t = case_when(PL_1014_2019_1t != "Branco" & PL_1014_2019_1t != "Contrario" & PL_1014_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1014_2019_1t),
                    PL_1014_2019_2t = case_when(PL_1014_2019_2t != "Branco" & PL_1014_2019_2t != "Contrario" & PL_1014_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1014_2019_2t),
                    PL_1015_2019_1t = case_when(PL_1015_2019_1t != "Branco" & PL_1015_2019_1t != "Contrario" & PL_1015_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1015_2019_1t),
                    PL_1015_2019_2t = case_when(PL_1015_2019_2t != "Branco" & PL_1015_2019_2t != "Contrario" & PL_1015_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1015_2019_2t),
                    PL_1016_2019_1t = case_when(PL_1016_2019_1t != "Branco" & PL_1016_2019_1t != "Contrario" & PL_1016_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1016_2019_1t),
                    PL_1016_2019_2t = case_when(PL_1016_2019_2t != "Branco" & PL_1016_2019_2t != "Contrario" & PL_1016_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1016_2019_2t),
                    PL_1085_2019 = case_when(PL_1085_2019 != "Branco" & PL_1085_2019 != "Contrario" & PL_1085_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1085_2019),
                    PL_1125_2019 = case_when(PL_1125_2019 != "Branco" & PL_1125_2019 != "Contrario" & PL_1125_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1125_2019),
                    PL_1126_2019 = case_when(PL_1126_2019 != "Branco" & PL_1126_2019 != "Contrario" & PL_1126_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1126_2019),
                    PL_1127_2019 = case_when(PL_1127_2019 != "Branco" & PL_1127_2019 != "Contrario" & PL_1127_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1127_2019),
                    PL_1165_2019 = case_when(PL_1165_2019 != "Branco" & PL_1165_2019 != "Contrario" & PL_1165_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1165_2019),
                    PL_1166_2019 = case_when(PL_1166_2019 != "Branco" & PL_1166_2019 != "Contrario" & PL_1166_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1166_2019),
                    PL_1167_2019 = case_when(PL_1167_2019 != "Branco" & PL_1167_2019 != "Contrario" & PL_1167_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1167_2019),
                    PL_1202_2019 = case_when(PL_1202_2019 != "Branco" & PL_1202_2019 != "Contrario" & PL_1202_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1202_2019),
                    PL_1203_2019 = case_when(PL_1203_2019 != "Branco" & PL_1203_2019 != "Contrario" & PL_1203_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1203_2019),
                    PL_1204_2019_1t = case_when(PL_1204_2019_1t != "Branco" & PL_1204_2019_1t != "Contrario" & PL_1204_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1204_2019_1t),
                    PL_1204_2019_2t = case_when(PL_1204_2019_2t != "Branco" & PL_1204_2019_2t != "Contrario" & PL_1204_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1204_2019_2t),
                    PL_1205_2019_1t = case_when(PL_1205_2019_1t != "Branco" & PL_1205_2019_1t != "Contrario" & PL_1205_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1205_2019_1t),
                    PL_1205_2019_2t = case_when(PL_1205_2019_2t != "Branco" & PL_1205_2019_2t != "Contrario" & PL_1205_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1205_2019_2t),
                    PL_1287_2019 = case_when(PL_1287_2019 != "Branco" & PL_1287_2019 != "Contrario" & PL_1287_2019 != "Sim" ~ "Não votou", TRUE ~ PL_1287_2019),
                    PL_1355_2019_1t = case_when(PL_1355_2019_1t != "Branco" & PL_1355_2019_1t != "Contrario" & PL_1355_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_1355_2019_1t),
                    PL_1355_2019_2t = case_when(PL_1355_2019_2t != "Branco" & PL_1355_2019_2t != "Contrario" & PL_1355_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_1355_2019_2t),
                    PL_1440_2020 = case_when(PL_1440_2020 != "Branco" & PL_1440_2020 != "Contrario" & PL_1440_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1440_2020),
                    PL_1451_2020_1t = case_when(PL_1451_2020_1t != "Branco" & PL_1451_2020_1t != "Contrario" & PL_1451_2020_1t != "Sim" ~ "Não votou", TRUE ~ PL_1451_2020_1t),
                    PL_1451_2020_2t = case_when(PL_1451_2020_2t != "Branco" & PL_1451_2020_2t != "Contrario" & PL_1451_2020_2t != "Sim" ~ "Não votou", TRUE ~ PL_1451_2020_2t),
                    PL_1725_2020 = case_when(PL_1725_2020 != "Branco" & PL_1725_2020 != "Contrario" & PL_1725_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1725_2020),
                    PL_1726_2020 = case_when(PL_1726_2020 != "Branco" & PL_1726_2020 != "Contrario" & PL_1726_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1726_2020),
                    PL_1750_2020 = case_when(PL_1750_2020 != "Branco" & PL_1750_2020 != "Contrario" & PL_1750_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1750_2020),
                    PL_1751_2020 = case_when(PL_1751_2020 != "Branco" & PL_1751_2020 != "Contrario" & PL_1751_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1751_2020),
                    PL_1752_2020 = case_when(PL_1752_2020 != "Branco" & PL_1752_2020 != "Contrario" & PL_1752_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1752_2020),
                    PL_1938_2020 = case_when(PL_1938_2020 != "Branco" & PL_1938_2020 != "Contrario" & PL_1938_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1938_2020),
                    PL_1966_2020 = case_when(PL_1966_2020 != "Branco" & PL_1966_2020 != "Contrario" & PL_1966_2020 != "Sim" ~ "Não votou", TRUE ~ PL_1966_2020),
                    PL_2052_2020 = case_when(PL_2052_2020 != "Branco" & PL_2052_2020 != "Contrario" & PL_2052_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2052_2020),
                    PL_2136_2020 = case_when(PL_2136_2020 != "Branco" & PL_2136_2020 != "Contrario" & PL_2136_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2136_2020),
                    PL_2141_2020 = case_when(PL_2141_2020 != "Branco" & PL_2141_2020 != "Contrario" & PL_2141_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2141_2020),
                    PL_2150_2020 = case_when(PL_2150_2020 != "Branco" & PL_2150_2020 != "Contrario" & PL_2150_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2150_2020),
                    PL_2150_2020_1t = case_when(PL_2150_2020_1t != "Branco" & PL_2150_2020_1t != "Contrario" & PL_2150_2020_1t != "Sim" ~ "Não votou", TRUE ~ PL_2150_2020_1t),
                    PL_2150_2020_2t = case_when(PL_2150_2020_2t != "Branco" & PL_2150_2020_2t != "Contrario" & PL_2150_2020_2t != "Sim" ~ "Não votou", TRUE ~ PL_2150_2020_2t),
                    PL_2201_2020 = case_when(PL_2201_2020 != "Branco" & PL_2201_2020 != "Contrario" & PL_2201_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2201_2020),
                    PL_2202_2020 = case_when(PL_2202_2020 != "Branco" & PL_2202_2020 != "Contrario" & PL_2202_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2202_2020),
                    PL_2252_2020 = case_when(PL_2252_2020 != "Branco" & PL_2252_2020 != "Contrario" & PL_2252_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2252_2020),
                    PL_2273_2020 = case_when(PL_2273_2020 != "Branco" & PL_2273_2020 != "Contrario" & PL_2273_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2273_2020),
                    PL_2274_2020 = case_when(PL_2274_2020 != "Branco" & PL_2274_2020 != "Contrario" & PL_2274_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2274_2020),
                    PL_2275_2020 = case_when(PL_2275_2020 != "Branco" & PL_2275_2020 != "Contrario" & PL_2275_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2275_2020),
                    PL_2275_2020_1t = case_when(PL_2275_2020_1t != "Branco" & PL_2275_2020_1t != "Contrario" & PL_2275_2020_1t != "Sim" ~ "Não votou", TRUE ~ PL_2275_2020_1t),
                    PL_2275_2020_2t = case_when(PL_2275_2020_2t != "Branco" & PL_2275_2020_2t != "Contrario" & PL_2275_2020_2t != "Sim" ~ "Não votou", TRUE ~ PL_2275_2020_2t),
                    PL_2276_2020 = case_when(PL_2276_2020 != "Branco" & PL_2276_2020 != "Contrario" & PL_2276_2020 != "Sim" ~ "Não votou", TRUE ~ PL_2276_2020),
                    PL_2508_2021 = case_when(PL_2508_2021 != "Branco" & PL_2508_2021 != "Contrario" & PL_2508_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2508_2021),
                    PL_2509_2021 = case_when(PL_2509_2021 != "Branco" & PL_2509_2021 != "Contrario" & PL_2509_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2509_2021),
                    PL_2657_2021 = case_when(PL_2657_2021 != "Branco" & PL_2657_2021 != "Contrario" & PL_2657_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2657_2021),
                    PL_2707_2021 = case_when(PL_2707_2021 != "Branco" & PL_2707_2021 != "Contrario" & PL_2707_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2707_2021),
                    PL_2770_2021 = case_when(PL_2770_2021 != "Branco" & PL_2770_2021 != "Contrario" & PL_2770_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2770_2021),
                    PL_2771_2021 = case_when(PL_2771_2021 != "Branco" & PL_2771_2021 != "Contrario" & PL_2771_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2771_2021),
                    PL_2884_2021 = case_when(PL_2884_2021 != "Branco" & PL_2884_2021 != "Contrario" & PL_2884_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2884_2021),
                    PL_2885_2021 = case_when(PL_2885_2021 != "Branco" & PL_2885_2021 != "Contrario" & PL_2885_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2885_2021),
                    PL_2924_2021 = case_when(PL_2924_2021 != "Branco" & PL_2924_2021 != "Contrario" & PL_2924_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2924_2021),
                    PL_2937_2021 = case_when(PL_2937_2021 != "Branco" & PL_2937_2021 != "Contrario" & PL_2937_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2937_2021),
                    PL_2976_2021 = case_when(PL_2976_2021 != "Branco" & PL_2976_2021 != "Contrario" & PL_2976_2021 != "Sim" ~ "Não votou", TRUE ~ PL_2976_2021),
                    PL_3191_2021 = case_when(PL_3191_2021 != "Branco" & PL_3191_2021 != "Contrario" & PL_3191_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3191_2021),
                    PL_3192_2021 = case_when(PL_3192_2021 != "Branco" & PL_3192_2021 != "Contrario" & PL_3192_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3192_2021),
                    PL_3199_2021 = case_when(PL_3199_2021 != "Branco" & PL_3199_2021 != "Contrario" & PL_3199_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3199_2021),
                    PL_3211_2021 = case_when(PL_3211_2021 != "Branco" & PL_3211_2021 != "Contrario" & PL_3211_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3211_2021),
                    PL_3256_2021 = case_when(PL_3256_2021 != "Branco" & PL_3256_2021 != "Contrario" & PL_3256_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3256_2021),
                    PL_3399_2021 = case_when(PL_3399_2021 != "Branco" & PL_3399_2021 != "Contrario" & PL_3399_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3399_2021),
                    PL_3407_2021 = case_when(PL_3407_2021 != "Branco" & PL_3407_2021 != "Contrario" & PL_3407_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3407_2021),
                    PL_3409_2021 = case_when(PL_3409_2021 != "Branco" & PL_3409_2021 != "Contrario" & PL_3409_2021 != "Sim" ~ "Não votou", TRUE ~ PL_3409_2021),
                    PL_367_2019 = case_when(PL_367_2019 != "Branco" & PL_367_2019 != "Contrario" & PL_367_2019 != "Sim" ~ "Não votou", TRUE ~ PL_367_2019),
                    PL_368_2019 = case_when(PL_368_2019 != "Branco" & PL_368_2019 != "Contrario" & PL_368_2019 != "Sim" ~ "Não votou", TRUE ~ PL_368_2019),
                    PL_450_2019_1t = case_when(PL_450_2019_1t != "Branco" & PL_450_2019_1t != "Contrario" & PL_450_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_450_2019_1t),
                    PL_450_2019_2t = case_when(PL_450_2019_2t != "Branco" & PL_450_2019_2t != "Contrario" & PL_450_2019_2t != "Sim" ~ "Não votou", TRUE ~ PL_450_2019_2t),
                    PL_451_2019 = case_when(PL_451_2019 != "Branco" & PL_451_2019 != "Contrario" & PL_451_2019 != "Sim" ~ "Não votou", TRUE ~ PL_451_2019),
                    PL_452_2019 = case_when(PL_452_2019 != "Branco" & PL_452_2019 != "Contrario" & PL_452_2019 != "Sim" ~ "Não votou", TRUE ~ PL_452_2019),
                    PL_734_2019 = case_when(PL_734_2019 != "Branco" & PL_734_2019 != "Contrario" & PL_734_2019 != "Sim" ~ "Não votou", TRUE ~ PL_734_2019),
                    PL_872_2019 = case_when(PL_872_2019 != "Branco" & PL_872_2019 != "Contrario" & PL_872_2019 != "Sim" ~ "Não votou", TRUE ~ PL_872_2019),
                    PL_873_2019_1t = case_when(PL_873_2019_1t != "Branco" & PL_873_2019_1t != "Contrario" & PL_873_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_873_2019_1t),
                    PL_876_2019_1t = case_when(PL_876_2019_1t != "Branco" & PL_876_2019_1t != "Contrario" & PL_876_2019_1t != "Sim" ~ "Não votou", TRUE ~ PL_876_2019_1t),
                    PL_877_2019 = case_when(PL_877_2019 != "Branco" & PL_877_2019 != "Contrario" & PL_877_2019 != "Sim" ~ "Não votou", TRUE ~ PL_877_2019),
                    PLC_10_2019_1t = case_when(PLC_10_2019_1t != "Branco" & PLC_10_2019_1t != "Contrario" & PLC_10_2019_1t != "Sim" ~ "Não votou", TRUE ~ PLC_10_2019_1t),
                    PLC_10_2019_2t = case_when(PLC_10_2019_2t != "Branco" & PLC_10_2019_2t != "Contrario" & PLC_10_2019_2t != "Sim" ~ "Não votou", TRUE ~ PLC_10_2019_2t),
                    PLC_28_2019_1t = case_when(PLC_28_2019_1t != "Branco" & PLC_28_2019_1t != "Contrario" & PLC_28_2019_1t != "Sim" ~ "Não votou", TRUE ~ PLC_28_2019_1t),
                    PLC_28_2019_2t = case_when(PLC_28_2019_2t != "Branco" & PLC_28_2019_2t != "Contrario" & PLC_28_2019_2t != "Sim" ~ "Não votou", TRUE ~ PLC_28_2019_2t),
                    PLC_38_2020 = case_when(PLC_38_2020 != "Branco" & PLC_38_2020 != "Contrario" & PLC_38_2020 != "Sim" ~ "Não votou", TRUE ~ PLC_38_2020),
                    PLC_46_2020 = case_when(PLC_46_2020 != "Branco" & PLC_46_2020 != "Contrario" & PLC_46_2020 != "Sim" ~ "Não votou", TRUE ~ PLC_46_2020),
                    PLC_46_2020_1t = case_when(PLC_46_2020_1t != "Branco" & PLC_46_2020_1t != "Contrario" & PLC_46_2020_1t != "Sim" ~ "Não votou", TRUE ~ PLC_46_2020_1t),
                    PLC_46_2020_2t = case_when(PLC_46_2020_2t != "Branco" & PLC_46_2020_2t != "Contrario" & PLC_46_2020_2t != "Sim" ~ "Não votou", TRUE ~ PLC_46_2020_2t),
                    PLC_48_2020 = case_when(PLC_48_2020 != "Branco" & PLC_48_2020 != "Contrario" & PLC_48_2020 != "Sim" ~ "Não votou", TRUE ~ PLC_48_2020),
                    PLC_60_2021 = case_when(PLC_60_2021 != "Branco" & PLC_60_2021 != "Contrario" & PLC_60_2021 != "Sim" ~ "Não votou", TRUE ~ PLC_60_2021),
                    PLC_60_2021_1t = case_when(PLC_60_2021_1t != "Branco" & PLC_60_2021_1t != "Contrario" & PLC_60_2021_1t != "Sim" ~ "Não votou", TRUE ~ PLC_60_2021_1t),
                    PLC_60_2021_2t = case_when(PLC_60_2021_2t != "Branco" & PLC_60_2021_2t != "Contrario" & PLC_60_2021_2t != "Sim" ~ "Não votou", TRUE ~ PLC_60_2021_2t),
                    PLC_64_2021 = case_when(PLC_64_2021 != "Branco" & PLC_64_2021 != "Contrario" & PLC_64_2021 != "Sim" ~ "Não votou", TRUE ~ PLC_64_2021),
                    PLC_65_2021 = case_when(PLC_65_2021 != "Branco" & PLC_65_2021 != "Contrario" & PLC_65_2021 != "Sim" ~ "Não votou", TRUE ~ PLC_65_2021),
                    PLC_75_2021 = case_when(PLC_75_2021 != "Branco" & PLC_75_2021 != "Contrario" & PLC_75_2021 != "Sim" ~ "Não votou", TRUE ~ PLC_75_2021),
                    PLC_75_2021_1t = case_when(PLC_75_2021_1t != "Branco" & PLC_75_2021_1t != "Contrario" & PLC_75_2021_1t != "Sim" ~ "Não votou", TRUE ~ PLC_75_2021_1t),
                    PLC_75_2021_2t = case_when(PLC_75_2021_2t != "Branco" & PLC_75_2021_2t != "Contrario" & PLC_75_2021_2t != "Sim" ~ "Não votou", TRUE ~ PLC_75_2021_2t),
                    VET_10_2019 = case_when(VET_10_2019 != "Branco" & VET_10_2019 != "Contrario" & VET_10_2019 != "Sim" ~ "Não votou", TRUE ~ VET_10_2019),
                    VET_11_2019 = case_when(VET_11_2019 != "Branco" & VET_11_2019 != "Contrario" & VET_11_2019 != "Sim" ~ "Não votou", TRUE ~ VET_11_2019),
                    VET_12_2019 = case_when(VET_12_2019 != "Branco" & VET_12_2019 != "Contrario" & VET_12_2019 != "Sim" ~ "Não votou", TRUE ~ VET_12_2019),
                    VET_13_2019 = case_when(VET_13_2019 != "Branco" & VET_13_2019 != "Contrario" & VET_13_2019 != "Sim" ~ "Não votou", TRUE ~ VET_13_2019),
                    VET_14_2019 = case_when(VET_14_2019 != "Branco" & VET_14_2019 != "Contrario" & VET_14_2019 != "Sim" ~ "Não votou", TRUE ~ VET_14_2019),
                    VET_15_2019 = case_when(VET_15_2019 != "Branco" & VET_15_2019 != "Contrario" & VET_15_2019 != "Sim" ~ "Não votou", TRUE ~ VET_15_2019),
                    VET_16_2020 = case_when(VET_16_2020 != "Branco" & VET_16_2020 != "Contrario" & VET_16_2020 != "Sim" ~ "Não votou", TRUE ~ VET_16_2020),
                    VET_17_2020 = case_when(VET_17_2020 != "Branco" & VET_17_2020 != "Contrario" & VET_17_2020 != "Sim" ~ "Não votou", TRUE ~ VET_17_2020),
                    VET_18_2020 = case_when(VET_18_2020 != "Branco" & VET_18_2020 != "Contrario" & VET_18_2020 != "Sim" ~ "Não votou", TRUE ~ VET_18_2020),
                    VET_19_2020 = case_when(VET_19_2020 != "Branco" & VET_19_2020 != "Contrario" & VET_19_2020 != "Sim" ~ "Não votou", TRUE ~ VET_19_2020),
                    VET_20_2020 = case_when(VET_20_2020 != "Branco" & VET_20_2020 != "Contrario" & VET_20_2020 != "Sim" ~ "Não votou", TRUE ~ VET_20_2020),
                    VET_21_2020 = case_when(VET_21_2020 != "Branco" & VET_21_2020 != "Contrario" & VET_21_2020 != "Sim" ~ "Não votou", TRUE ~ VET_21_2020),
                    VET_22_2020 = case_when(VET_22_2020 != "Branco" & VET_22_2020 != "Contrario" & VET_22_2020 != "Sim" ~ "Não votou", TRUE ~ VET_22_2020),
                    VET_23_2020 = case_when(VET_23_2020 != "Branco" & VET_23_2020 != "Contrario" & VET_23_2020 != "Sim" ~ "Não votou", TRUE ~ VET_23_2020),
                    VET_24_2021 = case_when(VET_24_2021 != "Branco" & VET_24_2021 != "Contrario" & VET_24_2021 != "Sim" ~ "Não votou", TRUE ~ VET_24_2021),
                    VET_25_2021 = case_when(VET_25_2021 != "Branco" & VET_25_2021 != "Contrario" & VET_25_2021 != "Sim" ~ "Não votou", TRUE ~ VET_25_2021),
                    VET_26_2021 = case_when(VET_26_2021 != "Branco" & VET_26_2021 != "Contrario" & VET_26_2021 != "Sim" ~ "Não votou", TRUE ~ VET_26_2021),
                    VET_27_2021 = case_when(VET_27_2021 != "Branco" & VET_27_2021 != "Contrario" & VET_27_2021 != "Sim" ~ "Não votou", TRUE ~ VET_27_2021),
                    VET_28_2021 = case_when(VET_28_2021 != "Branco" & VET_28_2021 != "Contrario" & VET_28_2021 != "Sim" ~ "Não votou", TRUE ~ VET_28_2021),
                    VET_29_2021 = case_when(VET_29_2021 != "Branco" & VET_29_2021 != "Contrario" & VET_29_2021 != "Sim" ~ "Não votou", TRUE ~ VET_29_2021),
                    VET_3_2019 = case_when(VET_3_2019 != "Branco" & VET_3_2019 != "Contrario" & VET_3_2019 != "Sim" ~ "Não votou", TRUE ~ VET_3_2019),
                    VET_30_2021 = case_when(VET_30_2021 != "Branco" & VET_30_2021 != "Contrario" & VET_30_2021 != "Sim" ~ "Não votou", TRUE ~ VET_30_2021),
                    VET_31_2021 = case_when(VET_31_2021 != "Branco" & VET_31_2021 != "Contrario" & VET_31_2021 != "Sim" ~ "Não votou", TRUE ~ VET_31_2021),
                    VET_32_2021 = case_when(VET_32_2021 != "Branco" & VET_32_2021 != "Contrario" & VET_32_2021 != "Sim" ~ "Não votou", TRUE ~ VET_32_2021),
                    VET_33_2021 = case_when(VET_33_2021 != "Branco" & VET_33_2021 != "Contrario" & VET_33_2021 != "Sim" ~ "Não votou", TRUE ~ VET_33_2021),
                    VET_4_2019 = case_when(VET_4_2019 != "Branco" & VET_4_2019 != "Contrario" & VET_4_2019 != "Sim" ~ "Não votou", TRUE ~ VET_4_2019),
                    VET_5_2019 = case_when(VET_5_2019 != "Branco" & VET_5_2019 != "Contrario" & VET_5_2019 != "Sim" ~ "Não votou", TRUE ~ VET_5_2019),
                    VET_6_2019 = case_when(VET_6_2019 != "Branco" & VET_6_2019 != "Contrario" & VET_6_2019 != "Sim" ~ "Não votou", TRUE ~ VET_6_2019),
                    VET_7_2019 = case_when(VET_7_2019 != "Branco" & VET_7_2019 != "Contrario" & VET_7_2019 != "Sim" ~ "Não votou", TRUE ~ VET_7_2019),
                    VET_8_2019 = case_when(VET_8_2019 != "Branco" & VET_8_2019 != "Contrario" & VET_8_2019 != "Sim" ~ "Não votou", TRUE ~ VET_8_2019),
                    VET_9_2019 = case_when(VET_9_2019 != "Branco" & VET_9_2019 != "Contrario" & VET_9_2019 != "Sim" ~ "Não votou", TRUE ~ VET_9_2019),
                    PEC_71_2021 = case_when(PEC_71_2021 != "Branco" & PEC_71_2021 != "Contrario" & PEC_71_2021 != "Sim" ~ "Não votou", TRUE ~ PEC_71_2021),
                    PEC_57_2020 = case_when(PEC_57_2020 != "Branco" & PEC_57_2020 != "Contrario" & PEC_57_2020 != "Sim" ~ "Não votou", TRUE ~ PEC_57_2020),
                    PEC_55_2020 = case_when(PEC_55_2020 != "Branco" & PEC_55_2020 != "Contrario" & PEC_55_2020 != "Sim" ~ "Não votou", TRUE ~ PEC_55_2020),
                    PEC_55_2020_1t = case_when(PEC_55_2020_1t != "Branco" & PEC_55_2020_1t != "Contrario" & PEC_55_2020_1t != "Sim" ~ "Não votou", TRUE ~ PEC_55_2020_1t),
                    PEC_55_2020_2t = case_when(PEC_55_2020_2t != "Branco" & PEC_55_2020_2t != "Contrario" & PEC_55_2020_2t != "Sim" ~ "Não votou", TRUE ~ PEC_55_2020_2t)
)



# Teste da classificação
map(bd, freq)


# Exporta dados
writexl::write_xlsx(bd, "votacao.completa(03.11.2022).xlsx")

# Exporta: formato SPSS ---------
library(haven)

bd <- readxl::read_xlsx("entrega final/banco.tramitacao.final(24.10.2022).xlsx")

haven::write_sav(bd, "banco.tramitacao.final(03.11.2022).sav")

bd1 <- readxl::read_xlsx("entrega final/votacao.nomes(30.10.2022).xlsx", sheet = 1)
write_sav(bd1, "votacao.completa(03.11.2022).sav")









