## Importar o arquivo "GalapagosData.txt" do DIRETÓRIO DE TRABALHO
## com os títulos das colunas na 1a. linha e das linhas na 1a. coluna

Galapagos2 <- read.table("Galapagos2.txt", header=T, row.names=1)

## Confira se tudo correu bem
Galapagos2

## Torne as variáveis contidas em Galapagos disponíveis pelo nome
attach(Galapagos2)

## Plote LogArea x LogSpecies

## adicionando títulos para os eixos
plot(LogArea, LogSpecies, xlab="Log Island Area", ylab="Log Number of Species")


## Usando o comando lm ('linear model')
lm(LogSpecies~LogArea)


## Criando um objeto para guardar o modelo
model1 <- lm(LogSpecies~LogArea)

## plotando modelo ajustado aos dados
abline(model1)

## Valores estimados de y (LogSpecies)
predito <- predict(model1)

## Resíduos
residuos <- LogSpecies - predito; residuos

hist(residuos)

plot(residuos ~ LogSpecies)

## Quadrados dos desvios devida a regressão
dreg <- (predito-mean(LogSpecies))^2
## Soma dos Quadrados devida a regressão
SQreg <- sum(dreg)

SQreg

## Quadrados dos desvios residuais, parte não explicada pela regressão
dresid <- ((LogSpecies-predito)^2)
## Soma dos Quadrados residual
SQresid <- sum(dresid)

SQresid

## Quadrados dos desvios totais, entre cada observação e a média
dtot <- (LogSpecies - mean(LogSpecies))^2
## Soma dos quadrados dos desvios total
SQtot <- sum(dtot)

SQtot

plot(LogIdade, LogSpecies)

model2 <- lm(LogSpecies ~ LogIdade); model2

abline(model2)

## Valores estimados de y (LogSpecies)
predito2 <- predict(model2)

## Resíduos
residuos2 <- LogSpecies - predito2

hist(residuos2)

## Quadrados dos desvios residuais, parte da variação não explicada pela regressão
dresid2 <- ((LogSpecies-predito2)^2)
## Soma dos Quadrados residual
SQresid2 <- sum(dresid2)

SQresid2
