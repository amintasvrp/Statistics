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

freq0 = freqEsperada(prop0)
freq1 = freqEsperada(prop1)
freq2 = freqEsperada(prop2)
freq3 = freqEsperada(prop3)
freq4 = freqEsperada(prop4)

freqEsp = c(freq0, freq1, freq2, freq3, freq4)


x24 = 9.488
x2obs(freqObs, freqEsp)

tab = matrix(c(4,3,5,5,7,6,2,2,2), 3, 3, byrow=TRUE)
row.names(tab) = c("EF", "EM", "ES")
colnames(tab) = c("C", "I", "O")



# Questão 2

# Letra a

#tabelaDistribuicaoConj = data.frame()


