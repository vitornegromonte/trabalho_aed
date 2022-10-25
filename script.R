## Instalação dos pacotes necessários ##
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

## Chamando o arquivo e disponibilizando no R ##
massa_dados = read.xlsx('/home/vitor/Development/trabalho_aed/dados/massa_de_dados.xlsx')

### Categorizando variáveis ###

## Qualitativass ##
turma = as.character(massa_dados$Turma)
sexo = as.character(massa_dados$Sexo)
fuma = as.character(massa_dados$Fuma)
toler = as.character(massa_dados$Tolerancia)
opCinema = as.character(massa_dados$OpCine)
opTV = as.character(massa_dados$OpTV)

## Quantitativas/numéricas ##
idade = as.numeric(massa_dados$Idade)
altura = as.numeric(massa_dados$Alt)
peso = as.numeric(massa_dados$Peso)
filhos = as.numeric(massa_dados$Filhos)
exercicios = as.numeric(massa_dados$Exer)
cinema = as.numeric(massa_dados$Cine)
TV = as.numeric(massa_dados$TV)

###### Variáveis qualitativas ######

## Quantidade de pessoas por turma ##
### Verical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Turma) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Turma",
 y = "Contagem", 
 title = "Gráfico de barras da variável turma",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Turma) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Turma",
 y = "Contagem", 
 title = "Gráfico de barras da variável turma",
 subtitle = "A: Turma A; B: Turma B",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## Distribuição de pessoas pelo sexo ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Sexo) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Sexo",
 y = "Quantidade",
 title = "Gráfico de barras da variável sexo",
 subtitle = "F: Feminino; M: Masculino",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Sexo) +
 geom_bar(
 fill = "#E45C5C") +
 labs(x = "Sexo", y = "Quantidade", 
 title = "Gráfico de barras da variável sexo",
 subtitle = "F: Feminino; M: Masculino",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## Indivíduos com o hábito de fumar ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Fuma) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Hábito de fumar",
 y = "Quantidade", 
 title = "Gráfico de barras da variável fumar",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Fuma) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Hábito de fumar",
 y = "Quantidade", 
 title = "Gráfico de barras da variável fumar",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## Tolerância dos indivíduos ao cigarro ##
### Vertical ###
ggplot(massa_dados) +
 aes(x = Tolerancia) +
 geom_bar(
 fill = "#E45C5C") +
 labs(x = "Tolerância ", y = "Quantidade", 
 title = "Histograma da variável tolerância ao fumo",
 subtitle = "I: indiferente; P: incomoda pouco; M: incomoda muito", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = Tolerancia) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Tolerância ", y = "Quantidade", 
 title = "Histograma da variável tolerância ao fumo",
 subtitle = "I: indiferente; P: incomoda pouco; M: incomoda muito", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## OpCine ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpCine) +
 geom_bar(
 fill = "#E45C5C") +
 labs(x = "Opinião a respeito das salas de cinema na cidade", 
 y = "Quantidade",
 title = "Gráfico de barras da variável OpCine",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpCine) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Opinião a respeito das salas de cinema na cidade", 
 y = "Quantidade",
 title = "Gráfico de barras da variável OpCine",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## OpTV ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpTV) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Opinião a respeito da qualidade da programação na TV", 
 y = "Quantidade",
 title = "Gráfico de barras da variável OpTV",
 subtitle = "R: ruim; M: média; B: boa; M: muito boa",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = OpTV) +
 geom_bar(
 fill = "#E45C5C") +
 labs(
 x = "Opinião a respeito da qualidade da programação na TV", 
 y = "Quantidade",
 title = "Gráfico de barras da variável OpTV", 
 subtitle = "R: ruim; M: média; B: boa; M: muito boa",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


##### Variáveis quantitativas ######

## idade ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Idade) +
 geom_histogram(bins = 35L,
 fill = "#E45C5C") +
 labs(
 x = "Idades", 
 y = "Quantitade ",
 title = "Histograma da variável idade",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = Idade) +
 geom_histogram(bins = 35L, 
 fill = "#E45C5C") +
 labs(
 x = "Idades", 
 y = "Quantitade ",
 title = "Histograma da variável idade",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## altura ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Alt) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Altura", 
 y = "Quantidade",
 title = "Histograma da variável altura",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = Alt) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Altura", 
 y = "Quantidade",
 title = "Histograma da variável altura",
 subtitle ="",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## peso ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Peso) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Peso", 
 y = "Quantidade",
 title = "Histograma da variável peso",
 subtitle = "", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

## Horizontal ##
ggplot(massa_dados) +
 aes(x = Peso) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Peso", 
 y = "Quantidade",
 title = "Histograma da variável peso",
 subtitle = "", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## Filhos ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Filhos) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "X", 
 y = "Y",
 title = "Histograma da variável filhos",
 subtitle = "", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = Filhos) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "X", 
 y = "Y",
 title = "Histograma da variável filhos",
 subtitle = "", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## Exercicios ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Exer) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Quantidade de horas", 
 y = "Volume de pessoas",
 title = "Histograma da variável exercícios", 
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = Exer) +
 fill = "#E45C5C") +
 labs(
 x = "Quantidade de horas", 
 y = "Volume de pessoas",
 title = "Histograma da variável exercícios", 
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## Cine ##
### Vertical ###
library(ggplot2)
ggplot(massa_dados) +
 aes(x = Cine) +
 geom_histogram(bins = 30L, fill = "#E45C5C") +
 labs(
 x = "Horas", 
 y = "Volume de pessoas",
 title = "Histograma da variável cinema", 
 subtitle = "", 
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = Cine) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Horas", 
 y = "Volume de pessoas",
 title = "Histograma da variável cinema",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()


## TV ##
### Vertical ###
ggplot(massa_dados) +
 aes(x = TV) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Horas", 
 y = "Volume de pessoas",
 title = "Histograma da variável televisão",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 theme_linedraw()

### Horizontal ###
ggplot(massa_dados) +
 aes(x = TV) +
 geom_histogram(bins = 30L,
 fill = "#E45C5C") +
 labs(
 x = "Horas", 
 y = "Volume de pessoas",
 title = "Histograma da variável televisão",
 subtitle = "",
 caption = "Fonte: Tabela 1.1, Magalhães, M,N, Lima, A.C.P. (2004). Noções de Probabilidade e Estatística. 6a edição, revisada., São Paulo: EDUSP.") +
 coord_flip() +
 theme_linedraw()