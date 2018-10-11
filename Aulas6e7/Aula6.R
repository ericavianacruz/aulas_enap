### Instalando Pacotes ## macetinhos
# Exercícios aula 06 
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis", "readr") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()
#############################################################################################################################################
####  Carregando o arquivo
setwd("E:/ENAP_Especializacao/D6_Analise_de_dados/aulas_enap/dados/")
decisoes <- read_rds("decisoes.rds")

## Exemplo de inner join:


(inner_join <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo) %>% 
  inner_join(processos, by=c("n_processo"="n_processo")))

##############################################################################################################################################
####### Exercicio

### - Crie um objeto contendo informações sobre os tamanhos das bancadas dos partidos (arquivo `bancadas.rds`),
### suas respectivas coligações eleitorais para 2018 (arquivo `coligacoes.xlsx`) 
### e o grau de concordância com a agenda do Gov Temer (arquivo `governismo_temer.xlsx`). 

##Carregando os objetos
(bancadas <- read_rds("bancadas.rds"))
(coligacoes <- read_xlsx("coligacoes.xlsx"))
(governismo_temer <-read_xlsx("governismo_temer.xlsx"))

(bancadas_coligacoes_governismo <- bancadas %>% left_join (coligacoes, by="party") %>% left_join (governismo_temer, by="party"))
## ou
(bancadas_coligacoes_governismo <- bancadas %>% left_join (coligacoes) %>% left_join (governismo_temer))

## Crie uma coluna unindo partido e candidato, sem excluir as originais
## usando unite mas pode usar mutate
(bancadas2 <- bancadas_coligacoes_governismo %>% 
  unite (pres_ppres, # nome da coluna criada
         president,
         partypresid, 
         sep =  " - ",
         ## padrao remove igual a true
         remove=FALSE))

### -- qual candidato tem a coligação com maior soma da proporção total de assentos.
(governismo_coligacao <-  bancadas2 %>% 
  mutate(prop=prop.table(size))) %>%
  group_by(president, partypresid) %>%
  summarise (prop_total = sum(prop, na.rm = TRUE),
             apoio = mean(governismo, na.rm = TRUE))
  




