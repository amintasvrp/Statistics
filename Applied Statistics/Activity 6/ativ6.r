# Questao 1
## Variaveis globais
favoraveis = rep(c(1), times=650135)
desfavoraveis = rep(c(0), times=592953)
pop = c(favoraveis, desfavoraveis)

## Funcoes

rc = function(n, z) {
  a = 0.5 * (1 - 0.5)
  b = sqrt(a / n)
  c = z * b
  return(0.5 + c)
}



## Letra a

proporcaoPopA = mean(pop)

## Letra b

n1 = 25
n2 = 100
n3 = 2200

rc1 = rc(n1, 1.65)
rc2 = rc(n2, 1.65)
rc3 = rc(n3, 1.65)

pChapeu1 = mean(sample(pop, n1, replace = FALSE))
pChapeu2 = mean(sample(pop, n2, replace = FALSE))
pChapeu3 = mean(sample(pop, n3, replace = FALSE))

h11 = pChapeu1 > rc1
h12 = pChapeu2 > rc2
h13 = pChapeu3 > rc3

### A confiança é maior no teste n = 2200, devido ao fato de que, quanto maior o tamanho da amostra obtida, os valores das estatísticas da amostra (no caso, a proporção amostral) tendem a se aproximar dos
### valores dos parâmetros da população (no caso, a proporção amostral) 

# Questao 2

homens = c(72.31, 77.49, 77.15, 74.62, 74.17, 75.71, 77.10, 76.16, 76.96, 78.33, 77.86, 77.51, 79.22, 73.50, 75.16, 80.16, 73.59, 69.88, 72.68, 72.61, 74.71, 73.49, 81.71, 73.72)

h1 = t.test(homens, alternative = "greater", mu = 75, conf.level = 0.95)



