

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


# Codigo para raspagem de dados primariamente 
# para coletar todas as proposições dado o recorte de 
# tempo desejado (no caso foi de 2018 ate 2020).
# As informações coletadas constam as proposicoes e 
# os links de cada uma para uma proxima raspagem de dados.

## PREAMBULO -------------------------------------------------------------------


# Limpando nosso ambiente:
rm(list = ls())

# Se achar necessário:
options(mc.cores = parallel::detectCores())
gc()
memory.size(max=F)

# Carrega pacotes:
library(rvest)
library(stringr)

options(scipen = 999)


# web scraping

# http://electionsbr.com/livro/web-scraping.html
# https://www.ibpad.com.br/blog/comunicacao-digital/webscraping-ou-como-raspar-todas-receitas-de-bolo-de-cenoura/
# https://bookdown.org/davi_moreira/txt4cs/scrape.html


# criando um objeto com o endereço da pagina que queremos coletat
url <- "https://www.almg.gov.br/atividade_parlamentar/tramitacao_projetos/index.html?txtAssunto=&txtAutor=&txtIdProj=&txtAno=&sltGrupoTipo=6&txtEmTram=&txtTramEnc=&txtPeriodoDe=01/01/2019&txtPeriodoAte=05/07/2022&sltSituacao=&txtTramitacao=&txtTh=&search=&ordem=0&advanced=simples&tp=10&txtPalavras=&first=false&aba=js_tabpesquisaAvancada&sltSituacaoGeral=&run=1&pagina=1"

html <- read_html(url) # lento e colocando em um objeto o conteudo da pagia coletada

# escolhento o que parte da pagina estamos coletando pelo endereco xpath,
# poderiamos usa pelo seletor css, para isso ver o exemplo do site imdb:
# https://rstudio-pubs-static.s3.amazonaws.com/299676_4c7fcd7a66fc42c08bc44865bdf31e44.html
titulo <- html_nodes(html, xpath = '//*[@id="js_tabTramitacao"]') %>% html_text()
link <- html_attr(html_nodes(html, xpath = '//div/div[10]/div[1]/a'), 'href')
resumo <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[1]') %>% html_text()
tipo <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[2]') %>% html_text()
autor <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[3]') %>% html_text()
status <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[4]') %>% html_text()


# mesmo exemplo aplicando um loop (for) para coletar varias paginas do mesmo site
# nesse exemplo varias paginas da busca sobre violencia

# como essa pagina (url) tem um argumento que e' o numero da pagina podemos criar
# no lugar no numero um elemento diferente do numero e outros conteudo que da pagina
# que possa ser subtituido pelo numero na pagina que vai ser coletada a cada rodada do loop
# nesse caso foi YYYY
url <- "https://www.almg.gov.br/atividade_parlamentar/tramitacao_projetos/index.html?advanced=simples&first=false&pagina=1&aba=js_tabpesquisaAvancada&sltGrupoTipo=9&txtIdProj=&txtAno=&txtAssunto=&txtAutor=Governador+Romeu+Zema+Neto&sltSituacao=&txtTramitacao=&txtPeriodoDe=01%2F01%2F2019&txtPeriodoAte=31%2F12%2F2021&ordem=0&tp=10&pagina=YYYY"

# com vai ser feita coleta e mais coleta com loop, é necessario cria um objeto
# fora do ambiente do loop que vai receber o que foi coletado dentro do loop
# nesse caso um data.frame com a variavel titulo da noticia 
# esse objeto criamos por padrao vazio, pois, ele so passa ter conteudo apos
# a primeira rodada do loop
dados <- NULL

# os argumentos do for sao um elemento (indice) que vai receber o valor do loop a
# cada rodada nesse caso escolhemo "i",  o in que dize que indice está dentro de um
# vetor que nesse caso que vai de 1 a 10 (1:10), ou seja, sera coletado da primeira
# a decima pagina, e tudo que vai se coletado fica dentro chaves que que iniciam
# e fecham o loop
for(i in 1:1){
  
  # unica alteracao para o exemplo acima é que criamos um objeto url2 que subtituir
  # dentro do objeto url o YYYY por o i que é o nosso indice do loop
  url2 <- str_replace(url, "YYYY", as.character(i))
  html <- read_html(url2)
  # Posição 1
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[1]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[1]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[1]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[1]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[1]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[1]/div[2]/div[4]') %>% html_text()
  
  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status, 
                   stringsAsFactors = F)
  
  # aqgui estamos jutando o data.frame df com o objeto que está fora do loop
  # o dados.
  dados <- rbind(dados, df)
  
  # Posição 2
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[2]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[2]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[2]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[2]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[2]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[2]/div[2]/div[4]') %>% html_text()
  
  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status, 
                   stringsAsFactors = F)
  
  # aqgui estamos jutando o data.frame df com o objeto que está fora do loop
  # o dados.
  dados <- rbind(dados, df)
  
  # Posição 3
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[3]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[3]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[3]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[3]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[3]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[3]/div[2]/div[4]') %>% html_text()
  
  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status, 
                   stringsAsFactors = F)
  
  dados <- rbind(dados, df)


  # Posição 4
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[4]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[4]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[4]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[4]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[4]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[4]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)

  dados <- rbind(dados, df)

  # Posição 5
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[5]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[5]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[5]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[5]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[5]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[5]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)

  dados <- rbind(dados, df)

  # Posição 6
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[6]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[6]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[6]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[6]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[6]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[6]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)

  dados <- rbind(dados, df)

  # Posição 7
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[7]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[7]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[7]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[7]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[7]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[7]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)

  dados <- rbind(dados, df)

  # Posição 8
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[8]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[8]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[8]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[8]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[8]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[8]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)

  dados <- rbind(dados, df)

  # Posição 9
  titulo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[9]/div[1]/a/strong') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//*[@id="result"]/div/div[9]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[9]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//*[@id="result"]/div/div[9]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//*[@id="result"]/div/div[9]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//*[@id="result"]/div/div[9]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)

  dados <- rbind(dados, df)

  # Posição 10
  titulo <- html_nodes(html, xpath = '//div/div[10]/div[1]/a') %>% html_text()
  link <- html_attr(html_nodes(html, xpath = '//div/div[10]/div[1]/a'), 'href')
  resumo <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[1]') %>% html_text()
  tipo <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[2]') %>% html_text()
  autor <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[3]') %>% html_text()
  status <- html_nodes(html, xpath = '//div/div[10]/div[2]/div[4]') %>% html_text()

  # a outra alteração é que colocamos o vetor titulo dentro de um data.frame
  df <- data.frame(titulo, link, resumo, tipo, autor, status,
                   stringsAsFactors = F)
  
  dados <- rbind(dados, df)
  
  print(i) # o print i no final da coleta serve para exibir do console qual pagina foi coletada com sucesso
}


# Renomeando
library(tidyverse)

dados <- dados %>% 
  rename(`Nome Padronizado` = titulo)


# Exportando dados raspados
writexl::write_xlsx(dados, "voto_almg_PEC.xlsx")

