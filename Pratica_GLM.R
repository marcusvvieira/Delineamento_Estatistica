## Base de dados sobre riqueza de esp�cies e mam�feros em ilhas

ilhas <- read.table(file = "ilhas.txt", header = TRUE)
head(ilhas)

args(glm)

# Modelo com vari�vel aleat�ria gaussiana
modelo.gaus <- glm(riqueza ~ log(area) + ilha + arquipelago, family = gaussian(), data = ilhas)
modelo.gaus
summary(modelo.gaus)

# Mesmo modelo, mas com vari�vel aleat�ria Poisson
modelo.pois <- glm(riqueza ~ log(area) + ilha + arquipelago, family = poisson(), data = ilhas)
modelo.pois
summary(modelo.pois)

# Comparando s� os coeficientes dos modelos
modelo.gaus$coefficients

modelo.pois$coefficients

# Calculando a dispers�o do modelo Poisson
modelo.pois$deviance

theta2 <- modelo.pois2$deviance/modelo.pois2$df.residual
theta2

# Partindo para a sele��o de modelos

library(MuMIn)
options(na.action = na.fail) # necess�rio para que a fun��o funcione - previne que os modelos sejam ajustados a conjuntos de dados diferentes
model.sel(modelo.gaus, modelo.pois)

## Agora criando um modelo mais complexo, com intera��es entre log(area) e arquip�lago
modelo.pois2 <- glm(riqueza ~ log(area) * ilha + arquipelago, family = poisson(), data = ilhas)

# e modelo nulo com var. aleat�ria Poisson
modelo.nulo <- glm(riqueza ~ 1, family = poisson(), data = ilhas)

# Selecionando entre estes modelos
model.sel(modelo.pois, modelo.pois2, modelo.nulo)

# Fazendo uma sele��o entre todos os submodelos poss�veis
selecao <- dredge(modelo.pois2)
selecao

## Fazendo pescaria: selecionando todos os submodelos poss�veis do modelo mais complexo
modelo <- glm(riqueza ~ log(area) * ilha + arquipelago + produtividade + populacao + habitat + montanha + temperatura + precipitacao, family = poisson(), data = ilhas)
args(dredge)
selecao <- dredge(modelo, beta)
selecao
head(selecao)

model.avg(selecao)


## Um modelo para dados binomiais: modelando a probabilidade de haver mam�feros entre as esp�cies amostradas (n�o a abundancia de mam�feros)

model.bin <- glm(cbind(mamiferos, riqueza) ~ log(area) * ilha + arquipelago, data = ilhas, family = binomial())
model.bin

# Comparando com os submodelos poss�veis
dredge(model.bin)

## Fazendo pescaria novamente

modelo1 <- glm(cbind(mamiferos, riqueza) ~ arquipelago + log(area) + ilha + produtividade + populacao + habitat + montanha + temperatura + precipitacao, family = binomial(), data = ilhas)
args(dredge)

modelo1

modelo2 <- glm(cbind(mamiferos, riqueza) ~ arquipelago + log(area) * ilha + produtividade + populacao + habitat + montanha + temperatura + precipitacao, family = binomial(), data = ilhas)
args(dredge)

modelo2

model.sel(modelo1, modelo2)



selecao <- dredge(modelo)
selecao
head(selecao, 10)

importance(selecao)

model.avg(selecao)
