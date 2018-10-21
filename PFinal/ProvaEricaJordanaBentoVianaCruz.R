########### Prova da Disciplina Coleta e Anáalise de Dados ###############################################
## ENAP - Escola Nacional de Administracao Publica                                                     ##
## Professor: Frederico                                                                                ## 
## Aluna: Erica Jordana Bento Viana Cruz                                                               ##
#########################################################################################################

## Usando os pacotes necessarios #######################################################################
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis",
                     "survey","srvyr", "ggplot2") 
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()
########################################################################################################
##Parte 1

## Lendo arquivo de decisões
setwd("F:/ENAP_Especializacao/D6_Analise_de_dados/aulas_enap/") ## Diretório de trabalho.

decisoes <- read_rds("dados/decisoes.rds")



#######################################################################################################
## Parte 2
## Carregando os dados da api de desempenho escolar
data(api)

# Elimine os objetos 'apiclus1', 'apiclus2', 'apipop' e 'apistrat'
# mantendo apenas o objeto apisrs
rm(apiclus1,apiclus2,apipop,apistrat)




