
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

## Exercício

## Carregue os dados de exemplo do pacote survey `data(api)`, use o data.frame `apisrs` e expanda a amostra usando apenas variável fpc contendo a contagem finita de população `as_survey(fpc=fpc)`

## Usando a variável `stype` crie uma nova variável indicando se a escola é de nível fundamental (categorias **E** e **M** de `stype`)  ou de nível médio (categoria *H* de `stype`). Dica: use `mutate`e `cas_when`.
## Carregando os dados e o modelo juntos
(srs_design_survey <- apisrs %>%
as_survey(fpc = fpc))

## Criando variáveis
srs_design_survey <- srs_design_survey %>% 
  mutate(nivel=case_when(
    stype=="E"~"Fundamental",
    stype=="M"~"Fundamental",
    stype=="H"~"Médio"))

## Qual a proporção de escolas por nível (Fundamental, Médio)? (use a variável nova que você criou e a função `survey_mean`), 
(resolucao <- srs_design_survey %>%
  group_by(nivel) %>%
  ## cv = Coeficiente de variação
  summarize(proporcao = survey_mean(vartype = "cv"),
            # ci = intervalo de confiança
            totaldeescolas = survey_total(vartype = "ci")))

## Qual o coeficiente de varição dessa proporção?