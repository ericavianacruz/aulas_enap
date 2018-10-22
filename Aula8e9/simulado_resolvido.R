## Simulado

## Parte 1 ----

# Carregue o tidyverse e o lubridate
library(tidyverse)
library(lubridate)

# 1. Carregue o arquivo `decisoes.rds` em um objeto chamado `decisoes`.
setwd("F:/ENAP_Especializacao/D6_Analise_de_dados/aulas_enap/") ## Diretório de trabalho.

decisoes <- read_rds("dados/decisoes.rds")


# 2. Separe a coluna `classe_assunto` em duas colunas, uma contendo a `classe` 
# e outra contendo o `assunto`
decisoes <- decisoes %>%
  separate(classe_assunto, 
           c('classe', 'assunto'), 
           sep = ' / ', 
           extra = 'merge', 
           fill = 'right') 

decisoes %>% 
  select(id_decisao,municipio,data_decisao,juiz) %>% 
  filter (dmy(data_decisao) >= dmy("01-01-2017"), dmy(data_decisao) <= dmy("01-01-2018"))

decisoes_sem_na %>% filter (!is.na(txt_decisao))

##mutate para pegar deciçoes que tratam apenas de drogas
(decisoes_drogas <- decisoes %>% filter (!is.na(txt_decisao)) %>%
  mutate(txt_decisao_minisculas = tolower(txt_decisao), 
         droga = str_detect(txt_decisao, "drogas|haxixe|coca[í]na")) %>%
## Ordernar, porque não está fucionando??
    arrange(desc(ymd(dmy(data_decisao)))))

decisoes_drogas %>% dplyr::select(n_processo, droga)

sum (decisoes_drogas$droga == 'TRUE')
sum (decisoes_drogas$droga == 'FALSE')

## filter
(decisoes_select <- decisoes %>% filter(municipio == "São Paulo")) 

# 3. Elabore um data.frame em que as linhas sejam o assunto, as colunas sejam os anos e os valores 
# sejam as quantidades de decisões 
# Dica 1: crie uma variável `ano` e exclua os casos em que não há informação sobre data da decisão
# Dica 2: agrupar por assunto e ano e fazer o spread

resultado <- decisoes %>%
  mutate(ano=lubridate::year(dmy(data_decisao))) %>%
  dplyr::filter(!is.na(ano)) %>%
  group_by(assunto,ano) %>%
  summarise(n=n()) %>%
  spread(ano,n,fill = 0)

## Outra maneira de resolver usando count
resultado <- decisoes %>%
  mutate(ano=lubridate::year(dmy(data_decisao))) %>%
  dplyr::filter(!is.na(ano)) %>%
  count(assunto,ano) %>%
  spread(ano,n,fill = 0)

## Parte 2 ----
# 1. Caregue os pacotes "tidyverse", "survey" e "srvyr"
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis","survey","srvyr") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

# 2. Leia o conjunto de dados 'api' do pacote survey usando o comando data(api)
data(api)

# 3. Elimine os objetos 'apiclus1', 'apiclus2', 'apipop' e 'apistrat'
# mantendo apenas o objeto apisrs
rm(apiclus1,apiclus2,apipop,apistrat)

# 4. crie o objeto 'pesos' selecionando as colunas 'stypr' e 'pw' do objeto 'apisrs'
pesos <- apisrs %>%
  dplyr::select(stype,pw) %>% distinct()

# 5. crie o objeto 'escolas_notas' selecionando as colunas 
# 'cds', 'stype', 'api99' e 'api00' do objeto apisrs
escolas_notas <- apisrs %>%
  dplyr::select(cds,stype,api99,api00)

# 7. Quantas linhas tem o novo objeto `pesos`, sem as duplicidades
nrow(pesos)

# 8. Traga a variável `pw` para `escola_notas`, criando um novo objeto `amostra` 
# resultado da união (join) do objeto `pesos` ao objeto `escolas_notas` 
# dica use left_join, com `escola_notas` na esquerda.
amostra <- escolas_notas %>%
  left_join(pesos)

# 9. Crie o objeto tbl_svy `amostra_expandida` expandindo a amostra aleatória simples (`amostra`)
# usando apenas a variável (coluna) "pw", contendo o peso amostral. 
amostra_expandida <- amostra %>%
  as_survey(weight = pw)

## Criando variáveis
amostra_expandida <- amostra_expandida %>% 
  mutate(nivel=case_when(
    stype=="E"~"Fundamental",
    stype=="M"~"Fundamental",
    stype=="H"~"Médio"))

# 10. Faça um gráfico de barras comparando a variação média 
# das notas de 1999 (`api99`) e 2000 (`api00`) por tipo de escola (`stype`) 
# utilize as estimativas intervalares para construir barras com o intervalo de confiança
amostra_expandida %>%
  group_by(nivel) %>%
  summarise(api_diff = survey_mean (api00 - api99, vartype = "ci")) %>%
  ggplot(aes(x = nivel, y = api_diff, fill = nivel, color = nivel,
             ymax = api_diff_upp, ymin = api_diff_low)) +
  geom_bar(stat = "identity",alpha=0.6) +
  geom_errorbar(width = 0,size=3) 

