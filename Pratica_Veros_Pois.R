##############################################################################
## Verossimilhança e -Log Verossim. para função de verossimilhança Poisson ###
##############################################################################

## Suponha que estas sejam 5 contagens do número de individuos por quadrat, e supondo uma taxa de encontro média na população = 2 

# Vamos então calcular as probabiliades de cada resultado segundo uma variável aleatória Poisson
lambda2 <- dpois(c(1, 4, 2, 1, 7), 2)

# Agora o produto destas variáveis, ou seja, a verossimilhança da hipótese de lambda = 2 consideranto os dados
prod(lambda2)

# O negativo do logaritmo da verossimilhança 
-log(prod(lambda2))

# A mesma coisa de outra forma algébrica, veja o resultado
sum(-log(lambda2))

# Ainda outra, confira o resultado
-sum(log(lambda2))

# Agora para a hipótese de lambda = 4
lambda4 <- dpois(c(1, 4, 2, 1, 7), 4)

# O log da verossimilhança negativa
sum(-log(lambda4))

# média das contagens é o estimador de Verossim. Máxima de uma variável aleatória Poisson
# (a demonstração pode ser feita por meio da derivada da função de Poisson quando seu valor é zero)
mean(c(1, 4, 2, 1, 7))

# Calculando as probabilidaes de cada resultado segundo a hipótese de lambda = 3
lambda3 <- dpois(c(1, 4, 2, 1, 7), 3)

# Agora a log-verossimilhança negativa de lambda = 3
sum(-log(lambda3))

## Vamos calcular a verossimilhança de uma sequencia de 100 possíveis valores de lambda

# Gerando 100 valores
lambdas <- seq(0.1, 10, 0.1); lambdas

contagens <- c(1, 4, 2, 1, 7)

# Detalhe técnico, apenas criando um vetor com 100 linhas, garantindo que todas tenham zero para começar
verosim.pois <- numeric(100)
verosim.pois

# Calculando a verossimilhança para o primeiro lambda da sequencia, i = 1, e guardando na linha 1 
verosim.pois[1] <- prod(dpois(contagens, lambdas[1]))
verosim.pois  # só a linha[1] do vetor deve ter um valor diferente de zero.

# Agora fazendo um 'loop' que automatiza o processo e calcular para os 100 valores de lambda
verosim.pois <- numeric(100)
for(i in 1:100) {verosim.pois[i] <- prod(dpois(contagens, lambdas[i]))}
verosim.pois

# Plotando temos uma forma gráfica de encontrar o valor de lambda com maior verossimilhança
plot(lambdas, verosim.pois)

# Adicionando uma linha conectando valores
lines(lambdas, verosim.pois)

# Encontrando a posição/linha no vetor verossimilhanças com maior valor
which.max(verosim.pois)
# Encontrando o valor de lambda correspondende a esta linha
lambdas[which.max(verosim.pois)]

# Adicionando uma linha vertical ao gráfico com o valor de VM (Verossim. Máxima)
abline(v=3, lty=2)  # uma linha vertical tracejada, lty = 2

abline(v=lambdas[which.max(verosim.pois)]) # Incorpando o comando, agora com uma linha contínua


## Repita o que foi feito acima, mas com a log-verossimilhança negativa
