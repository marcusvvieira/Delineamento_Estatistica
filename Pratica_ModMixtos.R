## Base de dados sobre riqueza de espécies e mamíferos em ilhas

ilhas <- read.table(file = "ilhas.txt", header = TRUE)
head(ilhas)

library(lme4)

modelo1 <- lmer(log(riqueza) ~ ilha + log(area) + (1|arquipelago), data= ilhas, REML=FALSE)
modelo1

modelo2 <- lmer(log(riqueza) ~ ilha + log(area) + (1|arquipelago), data= ilhas, REML=TRUE)
modelo2

modelo3 <- glmer(riqueza ~ ilha + log(area) + (1|arquipelago), data= ilhas, family = poisson())

modelo5 <- lmer(log(riqueza) ~ (1|ilha) + log(area) + (1|arquipelago), data= ilhas)

modelo6 <- glmer(riqueza ~ (1|ilha) + log(area) + (1|arquipelago), data= ilhas, family=poisson())

