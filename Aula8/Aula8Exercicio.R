
## Usando os pacotes
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis","survey","srvyr") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

## Carregando os dados
data(api) #dados de desempenho escolar


## Carregando os dados e o modelo juntos
(strat_design_survey <- apisrs %>%
as_survey(strata = stype, fpc = fpc))

