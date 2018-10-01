#### Aula4

### Instalando Pacotes
# Exercícios aula 04
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis", "readr") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

#### 
setwd("E:/ENAP_Especializacao/D6_Analise_de_dados/aulas_enap/dados/")
decisoes <- read_rds("decisoes.rds")
