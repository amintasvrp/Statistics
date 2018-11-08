# Questão 1

# Constantes

freqObs = c(72,204,228,101,20,625)

prop0 = dbinom(0, 4, 0.4)
prop1 = dbinom(1, 4, 0.4)
prop2 = dbinom(2, 4, 0.4)
prop3 = dbinom(3, 4, 0.4)
prop4 = dbinom(4, 4, 0.4)

# Funções

freqEsperada = function(p){
  return(625 * p)
}


x2obs = function(fObs, fEsp){
  result = 0
  for (i in 1:5) {
    result = result + ((fObs[i] - fEsp[i])^2)/fEsp[i]
  }
  return(result)
}

# Resolução

freqEsp = c(freqEsperada(prop0), freqEsperada(prop1), freqEsperada(prop2), freqEsperada(prop3), freqEsperada(prop4))
x2obs_1 = x2obs(freqObs, freqEsp)
x2_4_5 = 9.488

print(x2_4_5)
print(x2obs_1)


# Questão 2

# Constantes

regiaoDeProcedencia <- c("Capital", "Interior", "Outra")
grauDeInstrucao <-c("EF", "EM","ES")
valoresObs <- c(4,3,5,5,7,6,2,2,2)

# Letra a

margens <- expand.grid(regiaoDeProcedencia = regiaoDeProcedencia, grauDeInstrucao = grauDeInstrucao)

class <- transform(cbind(margens, valoresObs), 
  regiaoDeProcedencia = factor(regiaoDeProcedencia),
  grauDeInstrucao = factor(grauDeInstrucao))

tabelaDistribuicaoConjunta <- with(class,
               tapply(valoresObs,
     list(grauDeInstrucao, regiaoDeProcedencia),
     sum))

# Letra b

b = matrix(c(4,3,5,5,7,6,2,2,2), 3,3, byrow=TRUE)
row.names(b) = c("EF", "EM","ES")
colnames(b) = c("Capital", "Interior", "Outra")

x2_4_5

chisq.test(b)

# Letra c

gmodels::CrossTable(b, chisq=TRUE)

# Letra d

d = matrix(c(7,5,6,12,3,3), nrow = 3, ncol = 2,  byrow=TRUE)
row.names(d) = c("EF", "EM","ES")
colnames(d) = c("Solteiro", "Casado")

x2_2_5 = 5.991
chisq.test(d)

# Letra e

salarios_minimos = c(4.0, 4.56, 5.25, 5.73, 6.26, 6.66, 6.86, 7.39, 7.59, 7.44, 8.12, 8.46, 
             8.74, 8.95,9.13,9.35, 9.77, 9.8, 10.53, 10.76,11.06,11.59, 12.0,12.79,
             13.23, 13.6, 13.85, 14.69, 14.71, 15.99, 16.22, 16.61, 17.26, 18.75, 19.4,
             23.3)

idades_meses = c(315,394,437,250,487,336,492,520,428,282,402,335,449,530,365,464,379,475,
308,448,369,410,492,313,389,420,559,356,486,430,377,436,523,403,587,506)

plot(salarios_minimos, idades_meses)

r = cor(salarios_minimos, idades_meses)

print(r)

t = r * sqrt(34/(1 - r^2))
print(t)

tc_34_5 = 2.0322

