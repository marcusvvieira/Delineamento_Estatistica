##############################################################################
## Verossimilhan�a e -Log Verossim. para fun��o de verossimilhan�a Poisson ###
##############################################################################

## Suponha que estas sejam 5 contagens do n�mero de individuos por quadrat, e supondo uma taxa de encontro m�dia na popula��o = 2 

# Vamos ent�o calcular as probabiliades de cada resultado segundo uma vari�vel aleat�ria Poisson
lambda2 <- dpois(c(1, 4, 2, 1, 7), 2)

# Agora o produto destas vari�veis, ou seja, a verossimilhan�a da hip�tese de lambda = 2 consideranto os dados
prod(lambda2)

# O negativo do logaritmo da verossimilhan�a 
-log(prod(lambda2))

# A mesma coisa de outra forma alg�brica, veja o resultado
sum(-log(lambda2))

# Ainda outra, confira o resultado
-sum(log(lambda2))

# Agora para a hip�tese de lambda = 4
lambda4 <- dpois(c(1, 4, 2, 1, 7), 4)

# O log da verossimilhan�a negativa
sum(-log(lambda4))

# m�dia das contagens � o estimador de Verossim. M�xima de uma vari�vel aleat�ria Poisson
# (a demonstra��o pode ser feita por meio da derivada da fun��o de Poisson quando seu valor � zero)
mean(c(1, 4, 2, 1, 7))

# Calculando as probabilidaes de cada resultado segundo a hip�tese de lambda = 3
lambda3 <- dpois(c(1, 4, 2, 1, 7), 3)

# Agora a log-verossimilhan�a negativa de lambda = 3
sum(-log(lambda3))

## Vamos calcular a verossimilhan�a de uma sequencia de 100 poss�veis valores de lambda

# Gerando 100 valores
lambdas <- seq(0.1, 10, 0.1); lambdas

contagens <- c(1, 4, 2, 1, 7)

# Detalhe t�cnico, apenas criando um vetor com 100 linhas, garantindo que todas tenham zero para come�ar
verosim.pois <- numeric(100)
verosim.pois

# Calculando a verossimilhan�a para o primeiro lambda da sequencia, i = 1, e guardando na linha 1 
verosim.pois[1] <- prod(dpois(contagens, lambdas[1]))
verosim.pois  # s� a linha[1] do vetor deve ter um valor diferente de zero.

# Agora fazendo um 'loop' que automatiza o processo e calcular para os 100 valores de lambda
verosim.pois <- numeric(100)
for(i in 1:100) {verosim.pois[i] <- prod(dpois(contagens, lambdas[i]))}
verosim.pois

# Plotando temos uma forma gr�fica de encontrar o valor de lambda com maior verossimilhan�a
plot(lambdas, verosim.pois)

# Adicionando uma linha conectando valores
lines(lambdas, verosim.pois)

# Encontrando a posi��o/linha no vetor verossimilhan�as com maior valor
which.max(verosim.pois)
# Encontrando o valor de lambda correspondende a esta linha
lambdas[which.max(verosim.pois)]

# Adicionando uma linha vertical ao gr�fico com o valor de VM (Verossim. M�xima)
abline(v=3, lty=2)  # uma linha vertical tracejada, lty = 2

abline(v=lambdas[which.max(verosim.pois)]) # Incorpando o comando, agora com uma linha cont�nua


## Repita o que foi feito acima, mas com a log-verossimilhan�a negativa
