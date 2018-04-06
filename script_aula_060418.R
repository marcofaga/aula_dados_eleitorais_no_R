#Aula dia 06 de abril de 2018

#Captação de Dados Eleitorais usando o R.

#Breve introdução sobre o uso do R.

#O que é o R: é uma linguagem de programação.

#Linguagem x Programa (app)

#GUI (Interface Gráfica do Utilizador) x Linha de Comando.

###Baixando o R + RStudio###
#Google: "baixar o R" ou https://www.r-project.org/
#Interface RStudio: https://www.rstudio.com/
#Instalar os dois.

#Vantagens do R:
##totalmente gratuito (open source)
##Comunidade de desenvolvedores
#Uma variedade de pacotes (Em torno de 6700 pacotes feitos) Ex: https://twitter.com/grbails/status/885543687559811073?s=08
##Potencial criativo e de trabalho
##Disponível em todas as plataforams (MAC, Windows, Linux)
##Não há limites de linhas
##Criação de Scripts: registro e reprodução da construção e manipulação dos dados.

#Desvantagens
##Necessidade de saber e conhecer comandos
##Necessidade de entender um pouco de programação
##Curva de aprendizado: díficil para iniciantes.
##Não existe Ctrl+Z!
##Exigência do inglês.

#Função que retorna a versão do R em uso.
R.Version()


############Básico do R################

#Tutoriais: https://cran.r-project.org/doc/contrib/Landeiro-Introducao.pdf
#Em inglês: https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf

#O Básico do Básico: R como calculadora
#Use a linha de comando.... + - * / %% %/% ^

#Aplicativo voltado para a manipulação de objetos.
#Objeto é todo tipo de dado qeu o programa armazena em sua memória.
#Vamos criar um objeto...

#Antes... Determinar a pasta de trabalho
setwd("C:\\Users\\Marco Faganello\\Dropbox\\Academia\\PED\\HZ244 - Comportamento Eleitoral e Eleições\\aula_060418\\R")
getwd()

qualquernome <- 3
qualqueroutronome <- 70

#Chame os objetos criados!

#ATENÇÂO: O R É CASE-SENSITIVE.
qualQuernome

#Funções: nomedafuncao()
remove() #remove o que?
?remove
remove(qualqueroutonome)

#Criando mais objetos
#Com dado de tipo numérico
obja <- 5
class(obja) #Nos retorna a classe do tipo de dado e não do tipo de objeto.

#Objeto de tipo (character)
objb <- texto #Por que não funcionou?
objb <- "texto"
class(objb)

#Objeto do tipo vetor
objc <- c(1,2,3,90,80,100,300:1000)

#Manipulando objetos
objc*2
objd <- objc*2
xpto <- c(7,4,2,11,90,100,200:900)
objc * xpto

#Criando objetos Data.Frame
obje <- data.frame(ColunaA = objc,colunaB = objd)
View(obje)
head(obje)
tail(obje)

#Criaçãod e uma coluna do DataFrame
#Uma coluna que seja a soma das outras duas colunas
obje$ColunaC <- obje$ColunaA + obje$colunaB

#Estatística descritiva
summary(obje$ColunaC)
hist(obje$ColunaC)

#Dados pré-programados
objf <- mtcars
?mtcars

#Faça o histograma da variável mpg de mtcars
hist(objf$mpg)

save.image()
remove(list = ls())


#######Chega... Vamos pegar dados do pacote ElectionsBr#####

#Pacote usado: electionsBR
#Manual: https://cran.r-project.org/web/packages/electionsBR/electionsBR.pdf

#instalando e ativando o pacote
install.packages("electionsBR")
library(electionsBR)

#O pacote usa os dados que estão no repositório de dados do TSE.
#http://www.tse.jus.br/eleitor-e-eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais

#tipos de dados:
#Candidatos - listagem de todos os candidatos
#Eleitorado - Dados descritivos do eleitorado brasileiro
#Resultados - Resultado das eleições, votação nominal, por seção, zona e votação partidária
#Prestação de contas - Dados de prestação de contas das campanhas

#Vamos analisar a eleição presidencial de 2014.
#os dados estão originalmente em formato csv (tabela separado por vírgula)
#aplicativos recomendados para abrir esse tipo de dado: notepad++, atom, sublime text 3, excel
#codificação Latin1 (ISO-8859-1)

#usando o pacote electionsBR
#Para a lista de candidatos eles usam a função candidate_fed ou candidate_local
candidatos <- candidate_fed(2014)
#load("candidatos.RData")

#filtrando a base
install.packages("dplyr")
library(dplyr)
candidatos_sp <- candidatos %>% filter(SIGLA_UF == "SP")

#pegando dados de resultado de eleição função: vote_mun_zone_fed ou vote_mun_zone_local
#eleicao <- vote_mun_zone_fed(2014) #Arquivo muito grande 7 mi de linhas
#eleicaoPres <- eleicao %>% filter(DESCRICAO_CARGO == "PRESIDENTE") 
load("eleicaoPres.RData")

###Objetivo criar uma tabela com o total de votos de cada candidato por Estado.
#Depois plotar no mapa

#O arquivo do mapa que usaremos está em formato shapefile (.shp) e pode ser baixado
#no site do IBGE. 
#Link: https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais.html

#Primeiro passo preparação de uma tabela com os dados indicando UF e Voto

#vamos ver os candidatos?
unique(eleicaoPres$NOME_CANDIDATO)

#vamos ver o total de votos nominais?
sum(eleicaoPres$TOTAL_VOTOS) #deu certo? Pq não?

eleicaoPres$TOTAL_VOTOS <- as.numeric(eleicaoPres$TOTAL_VOTOS)
sum(eleicaoPres$TOTAL_VOTOS) #tudo isso? Pq?


#Filtrando para ficarmos com os dados apenas de 2 turno
eleicaoPres2 <- eleicaoPres %>% filter(NUM_TURNO == "2")
sum(eleicaoPres2$TOTAL_VOTOS) #somando para ver o resultado

#Vamos construir na mão agora...

###Sair###
dilma <- eleicaoPres2 %>% filter(NOME_CANDIDATO == "DILMA VANA ROUSSEFF") #filtrando os dados da Dilma
aecio <- eleicaoPres2 %>% filter(NOME_CANDIDATO == "AÉCIO NEVES DA CUNHA") #filtrando o do aecio

dilma <- dilma %>% group_by(SIGLA_UF) %>% summarise(VOTOS = sum(TOTAL_VOTOS)) #agrupando votos por UF Dilma
aecio <- aecio %>% group_by(SIGLA_UF) %>% summarise(VOTOS = sum(TOTAL_VOTOS)) #agrupando votos por UF Aécio

#criando coluna de códigos das UF para fazer o join de tabelas no mapa
dilma$ufcod <- c("12",
                  "27",
                  "13",
                  "16",
                  "29",
                  "23",
                  "53",
                  "32",
                  "52",
                  "21",
                  "31",
                  "50",
                  "51",
                  "15",
                  "25",
                  "26",
                  "22",
                  "41",
                  "33",
                  "24",
                  "11",
                  "14",
                  "43",
                  "42",
                  "28",
                  "35",
                  "17",
                  "ND")

aecio$ufcod <- dilma$ufcod #copiando os dados que foram feitos na tabela dilma para a tab aécio

#diferença de voto entre Dilma e Aécio
sum(dilma$VOTOS) - sum(aecio$VOTOS) 

#excluindo votos em trânsito (ZZ)
dilma <- dilma[dilma$SIGLA_UF != "ZZ" ,]
aecio <- aecio[aecio$SIGLA_UF != "ZZ" ,]

#Criando coluna com os percentuais do votos em porcentagem do candidato em cada estado
dilma$votoper <- dilma$VOTOS / sum(dilma$VOTOS) * 100
aecio$votoper <- aecio$VOTOS / sum(aecio$VOTOS) * 100

#criando tabela com o total do eleitorado por uf
votouf <- eleicaoPres2 %>% group_by(SIGLA_UF) %>% summarise(VOTOS = sum(TOTAL_VOTOS))

#agregando os dados de votouf em dilma e aécio
dilma$votouf <- votouf$VOTOS[match(dilma$SIGLA_UF, votouf$SIGLA_UF)]
aecio$votouf <- votouf$VOTOS[match(aecio$SIGLA_UF, votouf$SIGLA_UF)]

#criando coluna de percentuais de voto por estado
dilma$votoperuf <- dilma$VOTOS / dilma$votouf * 100
aecio$votoperuf <- aecio$VOTOS / aecio$votouf * 100

#tabela pronta.. vamos exportar para o formato csv!
write.csv(dilma, file = "dilma.csv", row.names = F)
write.csv(aecio, file = "aecio.csv", row.names = F)

#agora no QGIS...

#salvando seu workspace...
save.image()
