#### Coleta e Análise de dados secundários
install.packages(c(tidyverse,magrittr)) #para instalar pacotes do tidyverse


library(tidyverse)
library(magrittr)



# Exercícios Aula 01 ----

# 1. Reescreva a expressão abaixo utilizando o %>%.

round(mean(divide_by(sum(1:10),3)),digits = 1)

1:10 %>% #cria um vetor de 1 a 10
  sum() %>% #soma os valores do vetor
  divide_by(3) %>% #divide o resultado da soma anterior por 3
  mean() %>% #calcula a média da divisão anterior.
  round(1) #arredonda o número com apenas uma casa decimal (ou round(digits = 1))



# 2. Sem rodar, diga qual a saída do código abaixo. Consulte o help das funções caso precise.

2 %>% #começa com o valor 2
  add(2) %>%  #adiciona 2 ao valor 2, resultando em 4
  c(6, NA) %>% #insere o valor 4 no início do vetor "6, NA"
  mean(na.rm = T) %>%  #calcula a média dos valores do vetor, desconsiderando "NA"
  equals(5) #retorna o resultado de 5 equals ( = ) 5, ou seja, TRUE



#############################################


#para chamar uma função (add, por exemplo) do magrittr, por exemplo, sem rodar o library, basta escrever assim:

magrittr::add()


################################################

decisoes <- read_rds("C:/Users/aluno/Downloads/dados/decisoes.rds")

glimpse(decisoes) #uma forma diferente de ver o conjunto de dados. É possível saber que tipos de variáveis há no meu conjunto de dados, visualizar os primeiros valores dessa variável

#Cinco funções principais do dplyr:
#select: selecionar colunas
#filter: filtrar linhas
#mutate: criar colunas
#summarise: sumarizar colunas
#arrange: ordenar linhas

#antes do select, sempre chamar o pacote dplyr,uma vez que há essa função em diversos pacotes.

#SELECT
#Utilizar starts_with(x) -> começa com x
#ends_with(x) -> termina com x
#contais(x) -> coluna que contenha x
#matches(x) -> o valor é exatamente x
#one_of (x) -> um dos valores numa lista de vetores é x


decisoes %>%
  select(id_decisao, n_processo, municipio, juiz)


decisoes %>%
  select(id_decisao:municipio) #de id_decisao até municipio

decisoes_select <- decisoes %>%
  select(id_decisao, n_processo, municipio, juiz)


#selecionar as colunas que acabam com "cisao"

decisoes %>%
  select(ends_with("cisao"))

#tirar as colunas de texto = 'txt_decisao" e classe/assunto = 'classe_assunto'

decisoes %>%
  select(-txt_decisao, -classe_assunto)


#FILTER
#Se o select é selecionar variáveis, o filter é filtrar observações
#use "," ou "&" para "e" e "|" para "ou"
#condições separadas por vírgulas? É o mesmo que separar por &

decisoes %>%
  select (n_processo, id_decisao, municipio, juiz) %>%
  filter(municipio == 'São Paulo')


library(lubridate) #esse pacote serve para manusear data. É importante sempre chamar "library" porque é assim que se carrega um pacote

decisoes %>%
  select (id_decisao, municipio, data_decisao, juiz) %>%
  #município igual a Campinas ou Jaú, OU dia da decisão maior ou igual a 25
  filter(municipio %in% c('Campinas','Jaú') | day(dmy(data_decisao)) >= 25) #municipio "in" seguido de uma lista. 'day(dmy(data_decisao))' pega o dia da decisão. Para mês, month. Para ano, year.

decisoes %>%
  select (id_decisao, municipio, data_decisao, juiz) %>%
  #data maior ou igual a 01-01-2017 e menor ou igual a 01-01-2018
  filter(dmy(data_decisao) >= dmy("01-01-2017"),
          dmy(data_decisao) <= dmy("01-01-2018"))


#mais ação

decisoes %>%
  select(juiz) %>%
  #filtra juizes que têm 'Z' ou 'z' no nome
  filter (str_detect(juiz, regex("z", ignore_case = TRUE))) %>% #NÃO ENTENDI NADA
  #conta e ordena os juízes em ordem decrescente
  count(juiz, sort = TRUE) %>%
  head(5)


#filtrar apenas casos em que o id_decisao não é NA

decisoes %>%
  filter(!is.na(id_decisao)) #is.na é uma função que traz valores lógicos. A negação da função é !is.na

decisoes %>%
  filter(complete.cases(.)) #complete.cases() é uma função que traz apenas linhas em que todas as colunas possuem dados completos. O "." representa o conjunto de dados "decisoes"


#mutate em ação

decisoes %>%
  select (n_processo, data_decisao, data_registro) %>%
  mutate(tempo = dmy(data_registro) - dmy(data_decisao)) #mutate cria uma nova variável ou transforma o tipo do dado



#crie uma coluna binária "drogas" que vale TRUE se no texto da decisão algo é falado de drogas e FALSE caso contrário - dica: srt_detect

decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao_minusculas = tolower(txt_decisao),  #tolower transforma tudo em minúsculo
         droga = str_detect(txt_decisao_minusculas, "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na")) %>% #esse monte de "ou" funciona dentro das aspas, mesmo. O str lê assim.
  dplyr::select(n_processo,droga)
#no mutate, se eu crio uma variável na linha acima, já posso usá-la na linha abaixo.



#summarise -> agrupar dados e transformar esses dados

decisoes %>%
  select(n_processo, municipio, data_decisao) %>%
  #pega ano da decisão
  mutate(ano_julgamento = year(dmy(data_decisao)),
         #pega o ano do processo 0057003-20,2017,8,26,0000 -> "2017"
         ano_proc = str_sub(n_processo,12,15), #substring! str_sub serve para pegar a informação contida na posição 12 até a posição 15, ou seja, O ANO CONTIDO NO NÚMERO DO PROCESSO.
         #transforma o ano em inteiro
         ano_proc = as.numeric(ano_proc),
         #calcula o tempo em anos
         tempo_anos = ano_julgamento - ano_proc) %>% #calcula a quantidade de anos que demorou para um processo ser julgado (data da decisão - data do processo)
  group_by(municipio) %>%
  summarise (n = n(), #a função "n" conta os registros.
             media_anos = mean(tempo_anos),
             min_anos = min(tempo_anos),
             max_anos = max(tempo_anos))



decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(tamanho = str_length(txt_decisao)) %>%
  group_by(juiz) %>%
  summarise (n = n(),
             tamanho_mediana = median(tamanho)) %>% #mediana é o número de caracteres da decisão
  filter(n >= 10) %>%
  arrange(desc(tamanho_mediana)) %>% #arrange ordena a variável. É um "sort"
  head(5)


#############################################


#Aula 3!

lista.de.pacotes = c("tidyverse", "lubridate") #escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])] #procura na coluna Package, no "installed.packages", o pacote que faz parte desse vetor
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)} #se o comprimeiro for maior que zero, instala os novos pacotes
lapply(lista.de.pacotes, require, character.only = T) #se não tiver pacotes para instalar, lapply chama todos os valores do vetor "lista.de.pacotes" e require, que é o mesmo que livrary, exige que só seja recebido valores do tipo texto.
rm(lista.de.pacotes, novos.pacotes)
gc() #garbage colector. Quando se trabalha com muito dado, e sabendo que o R trabalha com muita memória RAM, é ideal fazer uma limpeza do que está na memória.


###################################################

#Revisão:
#SELECT
#Selecionar VARIÁVEIS. Sempre colunas.

decisoes %>%
  select(id_decisao, startsWith('data_')) #busca a coluna 'id_decisao' E TAMBÉM todas as outras que começam com 'data_'

#ou

decisoes %>%
  select(endsWith('decisao')) #busca todas as colunas que terminam com 'decisao'

#FILTER

decisoes %>%
  select (n_processo, id_decisao, municipio, juiz) %>%
  filter (municipio == 'São Paulo') #traz apenas os resultados da minha consulta em que o nome armazenado na coluna municipio é igual a 'São Paulo'

#MUTATE

#mutate(var = formula ou variável) -> mutate cria uma nova coluna com uma fórmula ou variável

miguel <- data.frame(a=1:10, b=11:20)

miguel$c <- miguel$b + 2

miguel <- miguel %>%
  mutate(c = b+2)

miguel <- miguel%>%
  mutate(b = "haha", #cria uma nova coluna 'b'
         d=paste(b,a,sep="_")) #cola 'b' e 'a', separados por "_", numa nova coluna 'd'

miguel <- miguel%>%
  mutate(e = str_sub(d,3,5)) #cria uma nova coluna 'e' que recebe o conteúdo das posições 3, 4 e 5 da coluna 'd'

miguel <- miguel%>%
  mutate(e = str_sub(d,-2)) #cria uma nova coluna 'e' que recebe o conteúdo das posições -2 (do final, volta duas posições) até o final da coluna 'd'. Ou seja, quando eu quero o conteúdo de uma posição específica até a última posição, basta não especificar o segundo valor do str_sub.

#SUMMARISE
#SEMMMMPRE relacionado com o comando group by. group by eu agrupo, no summarise eu crio novas variáveis
#agrupo por um, dois, três ou n campos, sumarizo pelos campos que desejo e da forma que desejo: contando registros do tipo texto, ou somando registros do tipo numérico, ou fazendo fórmulas em colunas
#no summarise eu escrevo sempre o nome da variável que eu quero criar, seguido de "igual a" e a fórmula ou o cálculo.

#ARRANGE

#Estudar em casa: case_when(), mutate_if, mutate_at

#ROWWISE -> agrupamento por linha. Para fazer cálculos com várias colunas, é preciso usar rowwise() e, ao final dessa função, é preciso usar ungroup()

#acessar https://www.curso-r.com/blog/2017-07-17-rstudio-e-github/ -> nessa p?gina aprendemos a criar um reposit?rio no Github e a baixar o git

#para o trabalho do fim da disciplina: uma apresenta??o (slides em html) contendo representa??es gr?ficas que sinalizem intui??es sobre um conjunto qualquer de dados secund?rios ? nossa escolha.
#uma dica bacana e que d? pra usar expans?o de amostra: dados da PDAD da codeplan
#o professor disse que podemos pegar o trabalho da outra disciplina de R e deixar os dados mais limpos, com c?digos dessa disciplina, as novas fun??es e todo o c?digo comentado
#vai ter que estar tudo no github. Esses slides em html devem estar online para o professor


#Aula 4!

lista.de.pacotes = c("tidyverse","lubridate") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

#Carregue o arquivo decisoes.rds em um objeto chamado decisoes.

decisoes <- read_rds("C:/Users/aluno.ENAP/Downloads/D6_Leo/leomosqueira/decisoes.rds")

decisoes

#Crie um objeto contendo o tempo médio entre decisão e registro por juiz, 
#apenas para processos relacionados a drogas nos munic?pios de Campinas ou Limeira

juizes_drogas_CL <- decisoes %>% 
  # selecionando as colunas utilizadas (só pra usar o select)
  select(juiz,municipio,txt_decisao,data_registro,data_decisao) %>%
  # criando variável "droga" a partir do texto da decisão
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[?o]pico|maconha|haxixe|coca[?i]na"),
         # cria variável tempo
         tempo = dmy(data_registro) - dmy(data_decisao)) %>% 
  # Filtrando munic?pios com processos que tratam de drogas
  filter(droga == TRUE,municipio %in% c("Campinas","Limeira")) %>%
  group_by(juiz) %>%
  summarise(tempo_medio = mean(tempo,na.rm=T))

write_rds(juizes_drogas_CL, "C:/Users/aluno.ENAP/Downloads/D6_Leo/leomosqueira/juizes_drogas_CL.rds")

#########################


decisoes %>% 
  filter(!is.na(id_decisao)) %>% 
  select(id_decisao:data_registro) %>% 
  # 1. nome da coluna que vai guardar os nomes de colunas empilhadas
  # 2. nome da coluna que vai guardar os valores das colunas
  # 3. seleção das colunas a serem empilhadas
  gather(key="variavel", value="valor", -id_decisao) %>% 
  arrange(id_decisao)


decisoes %>% 
  filter(!is.na(id_decisao)) %>% 
  select(id_decisao:data_registro) %>% 
  gather(key, value, -id_decisao) %>% 
  # 1. coluna a ser espalhada
  # 2. valores da coluna
  spread(key, value)


# Qual juiz julga a maior proporção de processos que tratam de drogas
# Dica: construa um data.frame contendo as colunas juiz, n_processos_drogas, 
# n_processos_n_drogas e total_processos, remodelando os dados para haver um juiz por linha e utilizando spread()

juiz_droga <- decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[?o]pico|maconha|haxixe|coca[?i]na"),
         droga=case_when( #criar uma nova vari?vel
           droga==TRUE ~ "droga",
           droga==FALSE ~ "n_droga"
         )) %>%
  group_by(juiz,droga) %>% #agrupar os dados
  summarise(n=n()) #sumarizar os dados
  spread(droga,n,fill = 0) %>%
  mutate(total=droga+n_droga,
         proporcao=droga/total)
  arrange(-proporcao)

#######################################################
  
  
# Qual é a quantidade mensal de processos por juiz

decisoes <- read_rds("C:/Users/aluno/Downloads/decisoes.rds")
  
qtde_mensal_proc <- decisoes %>% #jogar o resultado todo no novo objeto qtde_mensal_proc
  select(id_decisao,data_decisao,juiz) %>%  #selecionar os campos que desejo
  mutate(mes = month(dmy(data_decisao))) %>% #criar coluna "mes" e colocar nela os meses do campo data_decisao
  filter(!is.na(mes)) %>% #elimina da coluna "mes" qualquer valor NA por conta de erros de leitura de datas ou de dados inseridos errados
  group_by(juiz,mes) %>% #agrupar todo o meu resultado por juiz e mes
  summarise(qtde_processos=n()) %>% #sumarizar as linhas pelo campo qtde_processos e por um contador "n"
  spread(mes, qtde_processos, fill = 0) #a coluna "mes" vira várias colunas, em cada uma delas ficam os valores de "qtde_processos" e aonde não houver falor, "fill" (preenche) com zero.

  qtde_mensal_proc #exibe na tela o resultado 
  
########################################################
  
  
#Funções separate para separar e unite para unir
#separar a coluna "classe_assunto" em duas colunas "classe" e "assunto"
  
decisoes_sep <- decisoes %>%
    select (n_processo, classe_assunto) %>%
    separate(classe_assunto, #variável a ser separada
             c('classe', 'assunto'), #vetor com os nomes das colunas que receberão os valores da coluna a ser separada
             sep = ' / ', #indica qual é o separador a ser considerado para dividir o texto de "classe_processo" entre as colunas "classe" e "assunto"
             extra = 'merge', #recurso extra do separate. Quando é igual a merge, tudo o que houver de ' / ' extra no campo "classe_assunto" é jogado para a segunda coluna. Se eu colocasse drop, o separate descartaria tudo o que existisse depois da segunda ' / '.
             fill = 'right') %>% #se não houver o separador, fill indica o que fazer com o conteúdo de "classe_assunto". No caso, "classe" recebe o valor e "assunto" recebe NA, pois fill= right
   count(assunto, sort = TRUE)

decisoes_sep

#########################################################


processos <- read_rds("C:/Users/aluno/Downloads/processos_nested.rds")

d_partes <- processos %>%
  select(n_processo,partes) %>% #seleciono os campos que desejo
  unnest(partes) #desmembra os dados de uma coluna em diversos dados, um em cada linha. O resultado fica um agrupado de numero processo e parte.

d_partes


########################################################

#Pacote janitor::get_dupes() ("faxineiro") -> aqui nesse pacote, a criação de uma tabela não é mais com "table", e sim com "taby"

#Estudar o pacote survey (para trabalharmos com dados amostrais)
  