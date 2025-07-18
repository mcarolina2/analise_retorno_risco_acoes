---
title: "Análise do Ativo LWSA3"
format:
  html:
    code-fold: true
    code-tools: true
    toc: true
    toc-depth: 3
    number-sections: true
editor: visual
---

#Importação de dados do Yahoo! Finance
{r}
# Remoção de objetos anteriores
remove(list = ls())
gc()
par(mfrow = c(1, 1))
options(scipen = 999, max.print = 100000)
date()

# Diretório de trabalho
setwd("/Users/Carolina/Desktop/Projetos/Gestão_de_Riscos")

# Carregamento do pacote
library(tseries)


#Definição do intervalo de datas
{r}
dataini <- "2025-04-01"
datafim <- "2025-06-28"


#Coleta de dados do ativo LWSA3

{r}
lwsa3 <- get.hist.quote("lwsa3.sa",
                        quote = "Close",
                        start = dataini,
                        end = datafim)

lwsa3 <- na.omit(lwsa3)  # Remove valores ausentes, se houver

length(lwsa3)  # Quantidade de observações
lwsa3  # Série de preços
plot(lwsa3, main = "Preços de Fechamento - LWSA3")


#Estatísticas descritivas
#Cálculo da média e do desvio padrão dos preços

{r}
Media.lwsa3 <- mean(lwsa3)
Media.lwsa3  # Valor médio dos preços

Desvio.lwsa3 <- sd(lwsa3)
Desvio.lwsa3  # Desvio padrão dos preços


#Cálculo do coeficiente de variação dos preços (risco relativo)
{r}
cvlwsa3 <- sd(lwsa3) / mean(lwsa3) * 100
cvlwsa3  # Coeficiente de variação em %


#Análise dos retornos
#Cálculo dos retornos logarítmicos

{r}
rlwsa3 <- diff(log(lwsa3))
rlwsa3 <- na.omit(rlwsa3)
rlwsa3  # Retornos logarítmicos


#Média e desvio padrão dos retornos

{r}
mrcple <- mean(rlwsa3)
mrcple * 100  # Retorno médio diário em %

Drcple <- sd(rlwsa3)
Drcple  # Desvio padrão dos retornos


#Cálculo do risco (CV) dos retornos

{r}
CVcple <- Drcple / (mrcple * 100)
CVcple * 100  # Coeficiente de variação dos retornos em %


#Distribuição dos preços e retornos
#Histogramas

{r}
par(mfrow = c(1, 2))
plot(lwsa3, main = "Série de Preços")
hist(lwsa3, nclass = 30, col = "blue", main = "Histograma dos Preços")

plot(rlwsa3, main = "Série de Retornos")
hist(rlwsa3, nclass = 30, col = "blue", main = "Histograma dos Retornos")


#Medida de volatilidade anualizada (amostra)
{r}
Drcple <- sd(rlwsa3)
Volcple <- (Drcple * 100) * sqrt(252)
Volcple  # Volatilidade anualizada em %


#Análise da distribuição normal
#Gráficos de densidade para 1, 2 e 3 desvios padrões

{r}
Media.lwsa3
Desvio.lwsa3

par(mfrow = c(2, 2))

# 1 DP
x <- seq(Media.lwsa3 - Desvio.lwsa3, Media.lwsa3 + Desvio.lwsa3, length = 60)
y <- dnorm(x, Media.lwsa3, Desvio.lwsa3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 1 DP")

# 2 DP
x <- seq(Media.lwsa3 - 2 * Desvio.lwsa3, Media.lwsa3 + 2 * Desvio.lwsa3, length = 60)
y <- dnorm(x, Media.lwsa3, Desvio.lwsa3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 2 DP")

# 3 DP
x <- seq(Media.lwsa3 - 3 * Desvio.lwsa3, Media.lwsa3 + 3 * Desvio.lwsa3, length = 60)
y <- dnorm(x, Media.lwsa3, Desvio.lwsa3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 3 DP")

# Faixa maior
x <- seq(Media.lwsa3 - 2.5 * Desvio.lwsa3, Media.lwsa3 + 2.5 * Desvio.lwsa3, length = 60)
y <- dnorm(x, Media.lwsa3, Desvio.lwsa3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo mais amplo")


#Probabilidades dentro dos intervalos da média
{r}
# 1 DP
pnorm(Media.lwsa3 + Desvio.lwsa3, Media.lwsa3, Desvio.lwsa3) -
  pnorm(Media.lwsa3 - Desvio.lwsa3, Media.lwsa3, Desvio.lwsa3)

# 2 DP
pnorm(Media.lwsa3 + 2 * Desvio.lwsa3, Media.lwsa3, Desvio.lwsa3) -
  pnorm(Media.lwsa3 - 2 * Desvio.lwsa3, Media.lwsa3, Desvio.lwsa3)

# 3 DP
pnorm(Media.lwsa3 + 3 * Desvio.lwsa3, Media.lwsa3, Desvio.lwsa3) -
  pnorm(Media.lwsa3 - 3 * Desvio.lwsa3, Media.lwsa3, Desvio.lwsa3)


#Probabilidade de preços abaixo de um valor
{r}
pnorm(34.875, Media.lwsa3, Desvio.lwsa3)
pnorm(38, Media.lwsa3, Desvio.lwsa3)
pnorm(32, Media.lwsa3, Desvio.lwsa3)
pnorm(33, Media.lwsa3, Desvio.lwsa3)
pnorm(37.13255, Media.lwsa3, Desvio.lwsa3, lower.tail = TRUE)
pnorm(39, Media.lwsa3, Desvio.lwsa3, lower.tail = TRUE)
pnorm(41, Media.lwsa3, Desvio.lwsa3, lower.tail = TRUE)


#Cálculo de quantis para níveis de confiança
{r}
qnorm(0.90, Media.lwsa3, Desvio.lwsa3)
qnorm(0.95, Media.lwsa3, Desvio.lwsa3)
qnorm(0.975, Media.lwsa3, Desvio.lwsa3)
qnorm(0.99, Media.lwsa3, Desvio.lwsa3)


#Cálculo da densidade para valores específicos
{r}
dnorm(34.875, Media.lwsa3, Desvio.lwsa3)
dnorm(38, Media.lwsa3, Desvio.lwsa3)
dnorm(37.13255, Media.lwsa3, Desvio.lwsa3)

