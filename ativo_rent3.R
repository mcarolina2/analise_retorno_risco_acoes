---
title: "Análise do Ativo RENT3"
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


#Coleta de dados do ativo RENT3
{r}
rent3 <- get.hist.quote("rent3.sa",
                        quote = "Close",
                        start = dataini,
                        end = datafim)

rent3 <- na.omit(rent3)  # Remove valores ausentes, se houver

length(rent3)  # Quantidade de observações
rent3  # Série de preços
plot(rent3, main = "Preços de Fechamento - RENT3")


#Estatísticas descritivas
#Cálculo da média e do desvio padrão dos preços

{r}
Media.rent3 <- mean(rent3)
Media.rent3  # Valor médio dos preços

Desvio.rent3 <- sd(rent3)
Desvio.rent3  # Desvio padrão dos preços


#Cálculo do coeficiente de variação dos preços (risco relativo)
{r}
cvrent3 <- sd(rent3) / mean(rent3) * 100
cvrent3  # Coeficiente de variação em %


#Análise dos retornos
#Cálculo dos retornos logarítmicos

{r}
rrent3 <- diff(log(rent3))
rrent3 <- na.omit(rent3)
rrent3  # Retornos logarítmicos


#Média e desvio padrão dos retornos

{r}
mrcple <- mean(rrent3)
mrcple * 100  # Retorno médio diário em %

Drcple <- sd(rent3)
Drcple  # Desvio padrão dos retornos


#Cálculo do risco (CV) dos retornos

{r}
CVcple <- Drcple / (mrcple * 100)
CVcple * 100  # Coeficiente de variação dos retornos em %


#Distribuição dos preços e retornos
#Histogramas

{r}
par(mfrow = c(1, 2))
plot(rent3, main = "Série de Preços")
hist(rent3, nclass = 30, col = "blue", main = "Histograma dos Preços")

plot(rent3, main = "Série de Retornos")
hist(rent3, nclass = 30, col = "blue", main = "Histograma dos Retornos")


#Medida de volatilidade anualizada (amostra)

{r}
Drcple <- sd(rrent3)
Volcple <- (Drcple * 100) * sqrt(252)
Volcple  # Volatilidade anualizada em %


#Análise da distribuição normal
#Gráficos de densidade para 1, 2 e 3 desvios padrões

{r}
Media.rent3
Desvio.rent3

par(mfrow = c(2, 2))

# 1 DP
x <- seq(Media.rent3 - Desvio.rent3, Media.rent3 + Desvio.rent3, length = 60)
y <- dnorm(x, Media.rent3, Desvio.rent3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 1 DP")

# 2 DP
x <- seq(Media.rent3 - 2 * Desvio.rent3, Media.rent3 + 2 * Desvio.rent3, length = 60)
y <- dnorm(x, Media.rent3, Desvio.rent3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 2 DP")

# 3 DP
x <- seq(Media.rent3 - 3 * Desvio.rent3, Media.rent3 + 3 * Desvio.rent3, length = 60)
y <- dnorm(x, Media.rent3, Desvio.rent3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 3 DP")

# Faixa maior
x <- seq(Media.rent3 - 2.5 * Desvio.rent3, Media.rent3 + 2.5 * Desvio.rent3, length = 60)
y <- dnorm(x, Media.rent3, Desvio.rent3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo mais amplo")


#Probabilidades dentro dos intervalos da média

{r}
# 1 DP
pnorm(Media.rent3 + Desvio.rent3, Media.rent3, Desvio.rent3) -
  pnorm(Media.rent3 - Desvio.rent3, Media.rent3, Desvio.rent3)

# 2 DP
pnorm(Media.rent3 + 2 * Desvio.rent3, Media.rent3, Desvio.rent3) -
  pnorm(Media.rent3 - 2 * Desvio.rent3, Media.rent3, Desvio.rent3)

# 3 DP
pnorm(Media.rent3 + 3 * Desvio.rent3, Media.rent3, Desvio.rent3) -
  pnorm(Media.rent3 - 3 * Desvio.rent3, Media.rent3, Desvio.rent3)


#Probabilidade de preços abaixo de um valor

{r}
pnorm(34.875, Media.rent3, Desvio.rent3)
pnorm(38, Media.rent3, Desvio.rent3)
pnorm(32, Media.rent3, Desvio.rent3)
pnorm(33, Media.rent3, Desvio.rent3)
pnorm(37.13255, Media.rent3, Desvio.rent3, lower.tail = TRUE)
pnorm(39, Media.rent3, Desvio.rent3, lower.tail = TRUE)
pnorm(41, Media.rent3, Desvio.rent3, lower.tail = TRUE)


#Cálculo de quantis para níveis de confiança
{r}
qnorm(0.90, Media.rent3, Desvio.rent3)
qnorm(0.95, Media.rent3, Desvio.rent3)
qnorm(0.975, Media.rent3, Desvio.rent3)
qnorm(0.99, Media.rent3, Desvio.rent3)


#Cálculo da densidade para valores específicos

{r}
dnorm(34.875, Media.rent3, Desvio.rent3)
dnorm(38, Media.rent3, Desvio.rent3)
dnorm(37.13255, Media.rent3, Desvio.rent3)

