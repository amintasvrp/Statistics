# Questão 2
# Constantes

Indices_Observados = c(8.1,6.8,7.0,7.4,7.7,7.5,7.6,8.0)
Num_Ataques = c(5,13,20,28,41,49,61,62)
Duracao = c(118,132,119,153,91,118,132,105)

# Letra A

Y = cbind(Indices_Observados)
Y

X = cbind(rep(1,8), Num_Ataques, Duracao)
X

# Letra B

beta_chapeu = solve(t(X) %*% X) %*% (t(X) %*% Y)
beta_chapeu

# Letra C

ajuste = lm(Indices_Observados ~ Num_Ataques + Duracao)
ajuste

# Obtida pelo beta chapéu:     Y = 8.372602064 + 0.005095369 * Num.Ataques - 0.008576885 * Duracao 
# Obtida pelo lm():            Y = 8.372602 + 0.005095 * Num.Ataques - 0.008577 * Duracao

# Letra D

anova(ajuste)
summary(ajuste)


# Letra E

Indice_Previsto = 8.372602 + 0.005095 * 25 - 0.008577 * 100
Indice_Previsto

# Letra F 


plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)


plot(Experiencia,residuals(ajuste),xlab="Experiencia",ylab="Resíduos")
abline(h=0)


boxplot(residuals(ajuste)~ Sexo)


qqnorm(residuals(ajuste), ylab="Resíduos")
qqline(residuals(ajuste))

