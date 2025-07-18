---
title: "Análise do Ativo ELET6"
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

#Coleta de dados do ativo ELET6

{r}
elet6 <- get.hist.quote("elet6.sa",
                        quote = "Close",
                        start = dataini,
                        end = datafim)

elet6 <- na.omit(elet6)  # Remove valores ausentes, se houver

length(elet6)  # Quantidade de observações
elet6  # Série de preços
plot(elet6, main = "Preços de Fechamento - ELET6")


#Estatísticas descritivas
#Cálculo da média e do desvio padrão dos preços

{r}
Media.elet6 <- mean(elet6)
Media.elet6  # Valor médio dos preços

Desvio.elet6 <- sd(elet6)
Desvio.elet6  # Desvio padrão dos preços

#Cálculo do coeficiente de variação dos preços (risco relativo)
{r}
cvelet6 <- sd(elet6) / mean(elet6) * 100
cvelet6  # Coeficiente de variação em %

#Análise dos retornos
#Cálculo dos retornos logarítmicos

{r}
relet6 <- diff(log(elet6))
relet6 <- na.omit(relet6)
relet6  # Retornos logarítmicos

#Média e desvio padrão dos retornos
{r}
mrcple <- mean(relet6)
mrcple * 100  # Retorno médio diário em %

Drcple <- sd(relet6)
Drcple  # Desvio padrão dos retornos

#Cálculo do risco (CV) dos retornos
{r}
CVcple <- Drcple / (mrcple * 100)
CVcple * 100  # Coeficiente de variação dos retornos em %

#Distribuição dos preços e retornos
#Histogramas

{r}
par(mfrow = c(1, 2))
plot(elet6, main = "Série de Preços")
hist(elet6, nclass = 30, col = "blue", main = "Histograma dos Preços")

plot(relet6, main = "Série de Retornos")
hist(relet6, nclass = 30, col = "blue", main = "Histograma dos Retornos")

#Medida de volatilidade anualizada (amostra)

{r}
Drcple <- sd(relet6)
Volcple <- (Drcple * 100) * sqrt(252)
Volcple  # Volatilidade anualizada em %

#Análise da distribuição normal
#Gráficos de densidade para 1, 2 e 3 desvios padrões

{r}
Media.elet6
Desvio.elet6

par(mfrow = c(2, 2))

# 1 DP
x <- seq(Media.elet6 - Desvio.elet6, Media.elet6 + Desvio.elet6, length = 60)
y <- dnorm(x, Media.elet6, Desvio.elet6)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 1 DP")

# 2 DP
x <- seq(Media.elet6 - 2 * Desvio.elet6, Media.elet6 + 2 * Desvio.elet6, length = 60)
y <- dnorm(x, Media.elet6, Desvio.elet6)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 2 DP")

# 3 DP
x <- seq(Media.elet6 - 3 * Desvio.elet6, Media.elet6 + 3 * Desvio.elet6, length = 60)
y <- dnorm(x, Media.elet6, Desvio.elet6)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 3 DP")

# Faixa maior
x <- seq(Media.elet6 - 2.5 * Desvio.elet6, Media.elet6 + 2.5 * Desvio.elet6, length = 60)
y <- dnorm(x, Media.elet6, Desvio.elet6)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo mais amplo")

#Probabilidades dentro dos intervalos da média

{r}
# 1 DP
pnorm(Media.elet6 + Desvio.elet6, Media.elet6, Desvio.elet6) -
  pnorm(Media.elet6 - Desvio.elet6, Media.elet6, Desvio.elet6)

# 2 DP
pnorm(Media.elet6 + 2 * Desvio.elet6, Media.elet6, Desvio.elet6) -
  pnorm(Media.elet6 - 2 * Desvio.elet6, Media.elet6, Desvio.elet6)

# 3 DP
pnorm(Media.elet6 + 3 * Desvio.elet6, Media.elet6, Desvio.elet6) -
  pnorm(Media.elet6 - 3 * Desvio.elet6, Media.elet6, Desvio.elet6)

#Probabilidade de preços abaixo de um valor

{r}
pnorm(34.875, Media.elet6, Desvio.elet6)
pnorm(38, Media.elet6, Desvio.elet6)
pnorm(32, Media.elet6, Desvio.elet6)
pnorm(33, Media.elet6, Desvio.elet6)
pnorm(37.13255, Media.elet6, Desvio.elet6, lower.tail = TRUE)
pnorm(39, Media.elet6, Desvio.elet6, lower.tail = TRUE)
pnorm(41, Media.elet6, Desvio.elet6, lower.tail = TRUE)

#Cálculo de quantis para níveis de confiança

{r}
qnorm(0.90, Media.elet6, Desvio.elet6)
qnorm(0.95, Media.elet6, Desvio.elet6)
qnorm(0.975, Media.elet6, Desvio.elet6)
qnorm(0.99, Media.elet6, Desvio.elet6)

#Cálculo da densidade para valores específicos

{r}
dnorm(34.875, Media.elet6, Desvio.elet6)
dnorm(38, Media.elet6, Desvio.elet6)
dnorm(37.13255, Media.elet6, Desvio.elet6)


