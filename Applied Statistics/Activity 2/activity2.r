# Questao 1

m = 8000
a = 1
c = 5

geraUniformes = function(){
  result = c()
  anterior = 8
  
  for(i in 1:10000){
    x = (a * anterior + c) %% m
    result[i] = x 
    anterior = x
  }
  return(result / m)
}

uniformes = geraUniformes()
hist(uniformes)

## Observamos uma presen�a mais frequente de valores que est�o entre 0 e 2000 em comparação aos valors que vão de 2000 a 8000. 
## Desta forma, chegamos a conclusão que o comportamento do histograma difere do comportamento usualmente observado numa distribuição normal

# Quest�o 2

fda = function(t){
  result = 1 - exp(-10 * t)
  return(result)
}

invfda = function(y){
  logaritmando = 1 - y
  result = log(logaritmando) / (-10)
  return(result)
}

exponenciais = invfda(uniformes)
hist(exponenciais)
curve(fda, uniformes)

## Questao 3

tabelaPoisson = rpois(1000,5)
hist(tabelaPoisson)

## Questao 4

tabelaSigmas = function() {
  result = c()
  for(i in 1:1000){
    result[i] = sum(rnorm(100, mean = 5, sd = 1))
  }
  return(result)
}

tabelaSomatorios = tabelaSigmas()
hist(tabelaSomatorios)

