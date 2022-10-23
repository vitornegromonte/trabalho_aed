library(tidyr)
library(dplyr)
library(openxlsx)
library(ds)
library(ggplot2)
library(esquisse)
library(ggthemes)
library(tibble)
library(magrittr)
library(sampling)
library(TeachingSampling)
library(data.table)
library(outliers)

massa_dados = read.xlsx('~/dados/massa_de_dados.xlsx')

### Categorizando variáveis ###
# Qualitativas/nominais #
turma = as.character(massa_dados$Turma)
sexo = as.character(massa_dados$Sexo)
fuma = as.character(massa_dados$Fuma)
toler = as.character(massa_dados$Tolerancia)
opCinema = as.character(massa_dados$OpCine)
opTV = as.character(massa_dados$OpTV)

# Quantitativas/numéricas #
idade = as.numeric(massa_dados$Idade)
altura = as.numeric(massa_dados$Alt)
peso = as.numeric(massa_dados$Peso)
filhos = as.numeric(massa_dados$Filhos)
exercicios = as.numeric(massa_dados$Exer)
cinema = as.numeric(massa_dados$Cine)
TV = as.numeric(massa_dados$TV)

# Variáveis qualitativas #
## Quantidade de pessoas por turma ##
### Verical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Turma) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Turma", y = "Contagem", 
 title = "Distribuição dos alunos nas turmas A e B", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Turma) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Turma", y = "Contagem", 
 title = "Distribuição dos alunos nas turmas A e B", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()
## Distribuição de pessoas pelo sexo ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Sexo) +
 geom_bar(fill = "#E45C5C") +
 labs(title = "Distribuição de pessoas por sexo", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Sexo) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Sexo", y = "Quantidade", 
 title = "Distribuição de pessoas por sexo", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()
## Indivíduos com o hábito de fumar ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Fuma) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Hábito de fumar", y = "Quantidade", 
 title = "Distribuição do indivíduos que possuem ou não o hábito de fumar", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Fuma) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Hábito de fumar", y = "Quantidade", 
 title = "Distribuição do indivíduos que possuem ou não o hábito de fumar", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()
## Tolerância dos indivíduos ao cigarro ##
### Vertical ###

### Horizontal ###

## Opinião sobre as salas de cinema da cidade ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpCine) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Opinião a respeito das salas de cinema na cidade", 
 y = "Quantidade", title = "Gráfico da opinião dos indivíduos a respeito das salas de cinema na cidade", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpCine) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Opinião a respeito das salas de cinema na cidade", 
 y = "Quantidade", title = "Gráfico da opinião dos indivíduos a respeito das salas de cinema na cidade", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()
## Opinião a respeito da qualidade da programação na TV ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpTV) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Opinião a respeito da qualidade da programação na TV", 
 y = "Quantidade", title = "Distribuição das opiniões dos indivíduos a respeito da qualidade da programação na TV", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpTV) +
 geom_bar(fill = "#E45C5C") +
 labs(x = "Opinião a respeito da qualidade da programação na TV", 
 y = "Quantidade", title = "Distribuição das opiniões dos indivíduos a respeito da qualidade da programação na TV", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


# Variáveis quantitativas #
## Variável idade ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Idade) +
 geom_histogram(bins = 35L, fill = "#E45C5C") +
 labs(x = "Idades", 
 y = "Quantitade ", title = "Distribuição dos indivíduos de acordo com a idade", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
ggplot(massa_dados) +
 aes(x = Idade) +
 geom_histogram(bins = 35L, fill = "#E45C5C") +
 labs(x = "Idades", 
 y = "Quantitade ", title = "Distribuição dos indivíduos de acordo com a idade", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()

## Variável altura ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Alt) +
 geom_histogram(bins = 30L, fill = "#E45C5C") +
 labs(x = "Altura", 
 y = "Quantidade", title = "Distribuição dos indivíduos quanto a sua altura", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
ggplot(massa_dados) +
 aes(x = Alt) +
 geom_histogram(bins = 30L, fill = "#E45C5C") +
 labs(x = "Altura", 
 y = "Quantidade", title = "Distribuição dos indivíduos quanto a sua altura", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()

## Variável peso ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Peso) +
 geom_histogram(bins = 30L, fill = "#E45C5C") +
 labs(x = "Peso", 
 y = "Quantidade", title = "Distribuição dos indivíduos quanto ao peso", subtitle = "Legenda", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
## Horizontal ##
ggplot(massa_dados) +
 aes(x = Peso) +
 geom_histogram(bins = 30L, fill = "#E45C5C") +
 labs(x = "Peso", 
 y = "Quantidade", title = "Distribuição dos indivíduos quanto ao peso", subtitle = "Legenda", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()

## Filhos ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Filhos) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "X", 
 y = "Y", title = "Distribuição dos indivídus quanto ao número de filhos", subtitle = "Legenda", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
ggplot(massa_dados) +
 aes(x = Filhos) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "X", 
 y = "Y", title = "Distribuição dos indivídus quanto ao número de filhos", subtitle = "Legenda", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()

## Exercicios ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Exer) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Quantidade de horas", 
 y = "Volume de pessoas", title = "Distribuição dos indivíduos de acordo as horas dedicadas aos exercícios físicos, por semana", 
 subtitle = "Legenda", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
ggplot(massa_dados) +
 aes(x = Exer) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Quantidade de horas", 
 y = "Volume de pessoas", title = "Distribuição dos indivíduos de acordo as horas dedicadas aos exercícios físicos, por semana", 
 subtitle = "Legenda", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()

## Cinema ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Cine) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Horas", 
 y = "Volume de pessoas", title = "Horas semanais no cinema", subtitle = "Legenda", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
ggplot(massa_dados) +
 aes(x = Cine) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Horas", 
 y = "Volume de pessoas", title = "Horas semanais no cinema", subtitle = "Legenda", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()

## Televisão ##
### Vertical ###
ggplot(massa_dados) +
 aes(x = TV) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Horas", 
 y = "Volume de pessoas", title = "Horas semanais assistindo televisão", subtitle = "Legenda", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()
### Horizontal ###
ggplot(massa_dados) +
 aes(x = TV) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Horas", 
 y = "Volume de pessoas", title = "Horas semanais assistindo televisão", subtitle = "Legenda", caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()



