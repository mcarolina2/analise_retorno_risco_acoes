---
title: "Análise do Ativo BRFS3"
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

#Coleta de dados do ativo BRFS3
{r}
brfs3 <- get.hist.quote("brfs3.sa",
                        quote = "Close",
                        start = dataini,
                        end = datafim)

brfs3 <- na.omit(brfs3)  # Remove valores ausentes, se houver

length(brfs3)  # Quantidade de observações
brfs3  # Série de preços
plot(brfs3, main = "Preços de Fechamento - BRFS3")

#Estatísticas descritivas
#Cálculo da média e do desvio padrão dos preços
{r}
Media.brfs3 <- mean(brfs3)
Media.brfs3  # Valor médio dos preços

Desvio.brfs3 <- sd(brfs3)
Desvio.brfs3  # Desvio padrão dos preços

#Cálculo do coeficiente de variação dos preços (risco relativo)

{r}
cvbrfs3 <- sd(brfs3) / mean(brfs3) * 100
cvbrfs3  # Coeficiente de variação em %

#Análise dos retornos
#Cálculo dos retornos logarítmicos
{r}
rbrfs3 <- diff(log(brfs3))
rbrfs3 <- na.omit(rbrfs3)
rbrfs3  # Retornos logarítmicos

#Média e desvio padrão dos retornos
{r}
mrcple <- mean(rbrfs3)
mrcple * 100  # Retorno médio diário em %

Drcple <- sd(rbrfs3)
Drcple  # Desvio padrão dos retornos

#Cálculo do risco (CV) dos retornos
{r}
CVcple <- Drcple / (mrcple * 100)
CVcple * 100  # Coeficiente de variação dos retornos em %


#Distribuição dos preços e retornos
#Histogramas
{r}
par(mfrow = c(1, 2))
plot(brfs3, main = "Série de Preços")
hist(brfs3, nclass = 30, col = "blue", main = "Histograma dos Preços")

plot(rbrfs3, main = "Série de Retornos")
hist(rbrfs3, nclass = 30, col = "blue", main = "Histograma dos Retornos")

#Medida de volatilidade anualizada (amostra)
{r}
Drcple <- sd(rbrfs3)
Volcple <- (Drcple * 100) * sqrt(252)
Volcple  # Volatilidade anualizada em %

#Análise da distribuição normal

#Gráficos de densidade para 1, 2 e 3 desvios padrões
{r}
Media.brfs3
Desvio.brfs3

par(mfrow = c(2, 2))

# 1 DP
x <- seq(Media.brfs3 - Desvio.brfs3, Media.brfs3 + Desvio.brfs3, length = 60)
y <- dnorm(x, Media.brfs3, Desvio.brfs3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 1 DP")

# 2 DP
x <- seq(Media.brfs3 - 2 * Desvio.brfs3, Media.brfs3 + 2 * Desvio.brfs3, length = 60)
y <- dnorm(x, Media.brfs3, Desvio.brfs3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 2 DP")

# 3 DP
x <- seq(Media.brfs3 - 3 * Desvio.brfs3, Media.brfs3 + 3 * Desvio.brfs3, length = 60)
y <- dnorm(x, Media.brfs3, Desvio.brfs3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo de 3 DP")

# Faixa maior
x <- seq(Media.brfs3 - 2.5 * Desvio.brfs3, Media.brfs3 + 2.5 * Desvio.brfs3, length = 60)
y <- dnorm(x, Media.brfs3, Desvio.brfs3)
plot(x, y, type = "l", lwd = 2, col = "red", main = "Intervalo mais amplo") 

#Probabilidades dentro dos intervalos da média
{r}
# 1 DP
pnorm(Media.brfs3 + Desvio.brfs3, Media.brfs3, Desvio.brfs3) -
  pnorm(Media.brfs3 - Desvio.brfs3, Media.brfs3, Desvio.brfs3)

# 2 DP
pnorm(Media.brfs3 + 2 * Desvio.brfs3, Media.brfs3, Desvio.brfs3) -
  pnorm(Media.brfs3 - 2 * Desvio.brfs3, Media.brfs3, Desvio.brfs3)

# 3 DP
pnorm(Media.brfs3 + 3 * Desvio.brfs3, Media.brfs3, Desvio.brfs3) -
  pnorm(Media.brfs3 - 3 * Desvio.brfs3, Media.brfs3, Desvio.brfs3)
 
 #Probabilidade de preços abaixo de um valor
 {r}
pnorm(34.875, Media.brfs3, Desvio.brfs3)
pnorm(38, Media.brfs3, Desvio.brfs3)
pnorm(32, Media.brfs3, Desvio.brfs3)
pnorm(33, Media.brfs3, Desvio.brfs3)
pnorm(37.13255, Media.brfs3, Desvio.brfs3, lower.tail = TRUE)
pnorm(39, Media.brfs3, Desvio.brfs3, lower.tail = TRUE)
pnorm(41, Media.brfs3, Desvio.brfs3, lower.tail = TRUE)

#Cálculo de quantis para níveis de confiança
{r}
qnorm(0.90, Media.brfs3, Desvio.brfs3)
qnorm(0.95, Media.brfs3, Desvio.brfs3)
qnorm(0.975, Media.brfs3, Desvio.brfs3)
qnorm(0.99, Media.brfs3, Desvio.brfs3)

#Cálculo da densidade para valores específicos
{r}
dnorm(34.875, Media.brfs3, Desvio.brfs3)
dnorm(38, Media.brfs3, Desvio.brfs3)
dnorm(37.13255, Media.brfs3, Desvio.brfs3)