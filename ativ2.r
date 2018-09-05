# Questao 1
m = 8000

pseudos = function(){
  result = c()
  a = 1
  c = 5
  anterior = 8
  for(i in 1:10000){
    x = (a * anterior + c) %% m
    result[i] = x
    anterior = x
  }
  return(result)
}


# Criacao de amostras
tabela = pseudos()
tabelaUniforme = pseudos() / 8000 

hist(tabela)

## Observamos uma presença mais frequente de valores que estão entre 0 e 2000 em comparação aos valors que vão de 2000 a 8000. 
## Desta forma, chegamos a conclusão que o comportamento do histograma difere do comportamento usualmente observado numa distribuição normal

invfda = function(y){
  logaritmando = 1 - y
  result = log(logaritmando) / (-10)
  return(result)
}

exponeciais = invfda(tabelaUniforme)

hist(exponeciais)
## Depois terminar a curva -> curve(dexp(exponenciais, 10))

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
