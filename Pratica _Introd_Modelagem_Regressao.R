## Importar o arquivo "GalapagosData.txt" do DIRET�RIO DE TRABALHO
## com os t�tulos das colunas na 1a. linha e das linhas na 1a. coluna

Galapagos2 <- read.table("Galapagos2.txt", header=T, row.names=1)

## Confira se tudo correu bem
Galapagos2

## Torne as vari�veis contidas em Galapagos dispon�veis pelo nome
attach(Galapagos2)

## Plote LogArea x LogSpecies

## adicionando t�tulos para os eixos
plot(LogArea, LogSpecies, xlab="Log Island Area", ylab="Log Number of Species")


## Usando o comando lm ('linear model')
lm(LogSpecies~LogArea)


## Criando um objeto para guardar o modelo
model1 <- lm(LogSpecies~LogArea)

## plotando modelo ajustado aos dados
abline(model1)

## Valores estimados de y (LogSpecies)
predito <- predict(model1)

## Res�duos
residuos <- LogSpecies - predito; residuos

hist(residuos)

plot(residuos ~ LogSpecies)

## Quadrados dos desvios devida a regress�o
dreg <- (predito-mean(LogSpecies))^2
## Soma dos Quadrados devida a regress�o
SQreg <- sum(dreg)

SQreg

## Quadrados dos desvios residuais, parte n�o explicada pela regress�o
dresid <- ((LogSpecies-predito)^2)
## Soma dos Quadrados residual
SQresid <- sum(dresid)

SQresid

## Quadrados dos desvios totais, entre cada observa��o e a m�dia
dtot <- (LogSpecies - mean(LogSpecies))^2
## Soma dos quadrados dos desvios total
SQtot <- sum(dtot)

SQtot

plot(LogIdade, LogSpecies)

model2 <- lm(LogSpecies ~ LogIdade); model2

abline(model2)

## Valores estimados de y (LogSpecies)
predito2 <- predict(model2)

## Res�duos
residuos2 <- LogSpecies - predito2

hist(residuos2)

## Quadrados dos desvios residuais, parte da varia��o n�o explicada pela regress�o
dresid2 <- ((LogSpecies-predito2)^2)
## Soma dos Quadrados residual
SQresid2 <- sum(dresid2)

SQresid2
