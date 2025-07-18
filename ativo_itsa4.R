---
title: "Análise do Ativo ITSA4"
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

#Coleta de dados do ativo undefined

{r}
itsa4 <- get.hist.quote("itsa4.sa",
                        quote = "Close",
                        start = dataini,
                        end = datafim)

itsa4 <- na.omit(itsa4)  # Remove valores ausentes, se houver

length(itsa4)  # Quantidade de observações
itsa4  # Série de preços
plot(itsa4, main = "Preços de Fechamento - ITSA4")


#Estatísticas descritivas
#Cálculo da média e do desvio padrão dos preços

{r}
Media.itsa4 <- mean(itsa4)
Media.itsa4  # Valor médio dos preços

Desvio.itsa4 <- sd(itsa4)
Desvio.itsa4  # Desvio padrão dos preços

#Cálculo do coeficiente de variação dos preços (risco relativo)
{r}
cvitsa4 <- sd(itsa4) / mean(itsa4) * 100
cvitsa4  # Coeficiente de variação em %

#Análise dos retornos
#Cálculo dos retornos logarítmicos

{r}
ritsa4 <- diff(log(itsa4))
ritsa4 <- na.omit(ritsa4)
ritsa4  # Retornos logarítmicos

#Média e desvio padrão dos retornos

{r}
mrcple <- mean(ritsa4)
mrcple * 100  # Retorno médio diário em %

Drcple <- sd(ritsa4)
Drcple  # Desvio padrão dos retornos

#Cálculo do risco (CV) dos retornos

{r}
CVcple <- Drcple / (mrcple * 100)
CVcple * 100  # Coeficiente de variação dos retornos em %

#Distribuição dos preços e retornos
#Histogramas

{r}
par(mfrow = c(1, 2))
plot(itsa4, main = "Série de Preços")
hist(itsa4, nclass = 30, col = "blue", main = "Histograma dos Preços")

plot(ritsa4, main = "Série de Retornos")
hist(ritsa4, nclass = 30, col = "blue", main = "Histograma dos Retornos")

#Medida de volatilidade anualizada (amostra)

{r}
Drcple <- sd(ritsa4)
Volcple <- (Drcple * 100) * sqrt(252)
Volcple  # Volatilidade anualizada em %

#Análise da distribuição normal
#Gráficos de densidade para 1, 2 e 3 desvios padrões

{r}
Media.itsa4
Desvio.itsa4

par(mfrow = c(2, 2))

# 1 DP
x <- seq(Media.itsa4 - Desvio.itsa4, Media.itsa4 + Desvio.itsa4, length = 60)
y <- dnorm(x, Media.itsa4, Desvio.itsa4)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 1 DP")

# 2 DP
x <- seq(Media.itsa4 - 2 * Desvio.itsa4, Media.itsa4 + 2 * Desvio.itsa4, length = 60)
y <- dnorm(x, Media.itsa4, Desvio.itsa4)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 2 DP")

# 3 DP
x <- seq(Media.itsa4 - 3 * Desvio.itsa4, Media.itsa4 + 3 * Desvio.itsa4, length = 60)
y <- dnorm(x, Media.itsa4, Desvio.itsa4)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 3 DP")

# Faixa maior
x <- seq(Media.itsa4 - 2.5 * Desvio.itsa4, Media.itsa4 + 2.5 * Desvio.itsa4, length = 60)
y <- dnorm(x, Media.itsa4, Desvio.itsa4)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo mais amplo")

#Probabilidades dentro dos intervalos da média
{r}
# 1 DP
pnorm(Media.itsa4 + Desvio.itsa4, Media.itsa4, Desvio.itsa4) -
  pnorm(Media.itsa4 - Desvio.itsa4, Media.itsa4, Desvio.itsa4)

# 2 DP
pnorm(Media.itsa4 + 2 * Desvio.itsa4, Media.itsa4, Desvio.itsa4) -
  pnorm(Media.itsa4 - 2 * Desvio.itsa4, Media.itsa4, Desvio.itsa4)

# 3 DP
pnorm(Media.itsa4 + 3 * Desvio.itsa4, Media.itsa4, Desvio.itsa4) -
  pnorm(Media.itsa4 - 3 * Desvio.itsa4, Media.itsa4, Desvio.itsa4)

#Probabilidade de preços abaixo de um valor

{r}
pnorm(34.875, Media.itsa4, Desvio.itsa4)
pnorm(38, Media.itsa4, Desvio.itsa4)
pnorm(32, Media.itsa4, Desvio.itsa4)
pnorm(33, Media.itsa4, Desvio.itsa4)
pnorm(37.13255, Media.itsa4, Desvio.itsa4, lower.tail = TRUE)
pnorm(39, Media.itsa4, Desvio.itsa4, lower.tail = TRUE)
pnorm(41, Media.itsa4, Desvio.itsa4, lower.tail = TRUE)

#Cálculo de quantis para níveis de confiança

{r}
qnorm(0.90, Media.itsa4, Desvio.itsa4)
qnorm(0.95, Media.itsa4, Desvio.itsa4)
qnorm(0.975, Media.itsa4, Desvio.itsa4)
qnorm(0.99, Media.itsa4, Desvio.itsa4)

#Cálculo da densidade para valores específicos

{r}
dnorm(34.875, Media.itsa4, Desvio.itsa4)
dnorm(38, Media.itsa4, Desvio.itsa4)
dnorm(37.13255, Media.itsa4, Desvio.itsa4)



