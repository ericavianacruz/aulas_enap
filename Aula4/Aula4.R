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

####  Carregando o arquivo
setwd("E:/ENAP_Especializacao/D6_Analise_de_dados/aulas_enap/dados/")
decisoes <- read_rds("decisoes.rds")

# Crie um objeto contendo o tempo médio entre decisão e registro por juiz, apenas para processos relacionados a drogas nos municípios de Campinas ou Limeira. ----
## Obs.: a nova "singularidade" da base de dados será o `juiz`. Na base original, a singularidade era o `processo`

juizes_drogas_CL <-  decisoes %>%
  select(juiz,municipio,txt_decisao,data_registro,data_decisao) %>%
  ##criando a variavel droga
  mutate(txt_decisao = tolower(txt_decisao), 
         droga = str_detect(txt_decisao, "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na"),
         # variavel tempo
         tempo = dmy(data_registro) - dmy(data_decisao)) %>%
  ## filtrando
  filter(droga ==TRUE, municipio %in% c("Campinas", "Limeira")) %>%
  group_by (juiz) %>%
  summarize (tempo_medio = mean(tempo,na.rm=T))
  
  
# Salve o objeto resultante em um arquivo chamado `juizes_drogas_CL.rds` ----

write_rds(juizes_drogas_CL,"E:/ENAP_Especializacao/D6_Analise_de_dados/aulas_enap/dados/juizes_drogas_CL.rds")

# Faça commit e push do script e do arquivo `.rds` ----



# Qual juiz julga a maior proporção de processos que tratam de drogas ----


# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 

# Bônus: use `group_by` e `summarise` para identificar qual candidato tem a ----
# coligação com menor média de concordância e qual candidato 
# tem a maior proporção total de assentos.
