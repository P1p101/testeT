library(FSA)
library(readxl)
library(stats) #Test barlett

dados <- read_excel("C:/Users/pietr/OneDrive/Documentos/pastinha/testeT.xlsx", sheet = "t1")

#Teste T

#normalidade (0,05)
## H0: dados normais
## H1: dados não normais
shapiro.test(dados$vendas)

qqnorm(dados$vendas)
qqline(dados$vendas)

#Variância
## H0: variâncias iguais ou próximas
## H1: variâncias diferentes
bartlett.test(vendas ~ empresa, data = dados)

boxplot(vendas ~ empresa, data = dados)

#teste
## H0: as médias iguais ou próximas
## H1: as médias são diferentes
t.test(dados$vendas ~ dados$empresa)

dados <- read_excel("C:/Users/pietr/OneDrive/Documentos/pastinha/testeT.xlsx", sheet = "t2")

#Normalidade (0,05)
## H0: dados normais
## H1: dados não normais
shapiro.test(dados$carga)

qqnorm(dados$carga)
qqline(dados$carga)


#Variância
## H0: variâncias iguais ou próximas
## H1: variâncias diferentes
bartlett.test(carga ~ etaria, data = dados)

boxplot(carga ~ etaria, data = dados)

t.test(carga ~ etaria, data = dados)
Summarize(carga ~ etaria, data = dados)

dados <- read_excel("C:/Users/pietr/OneDrive/Documentos/pastinha/testeT.xlsx", sheet = "t3")

shapiro.test(dados$idade)

qnnorm(dados$idade)
qqline(dados$idade)

bartlett.test(idade ~ programa, data = dados)
boxplot(idade ~ programa, data = dados)

#Programa favorito X idade
## H0: As médias de idade entre os acompanhantes de documentários e filmes são iguais.
## H1: As médias de idade entre os acompanhantes de documentários e filmes são diferentes.

dados <- read_excel("C:/Users/pietr/OneDrive/Documentos/pastinha/testeT.xlsx", sheet = "tPareado1")

shapiro.test(dados$depois)
t.test(dados$antes, dados$depois, paired = TRUE)

shapiro.test(dados$antes)
t.test(dados$antes, dados$depois, paired = TRUE)
