# Questão 1

## Variaveis Globais

populacao = c(4,5,2,9,1,4,4,6,7,2,2,4,4,7,4,5,6,8,1,2,6,4,2,3,2,3,2,4,5,6,8,5,2,3,4,1,6,3,2,3,5,4,8,5,4,2,4,3,2,4,5,9,5,6,4,3,4,5,4,2,9,8,18,8,7,9,6,14,8,9,22,8,9,14,9,9,8,8,15,7,7,9,9,8,7,12,8,9,8,8)
tamanhoPop = 90

## Funções

quantMaioresQue5 = function(amostra){
  tamanho = length(amostra)
  result = 0
  for(i in 1:tamanho){
    if(amostra[i] > 5) {
      result = result + 1
    }
  }
  return(result)
}

propAmostrasContemValVerdadeiro = function(limInf, limSup, valVerdadeiro){
  quantAmostras = length(limInf) 
  amostrasContemVal = 0
  for(i in 1:quantAmostras){
    if((limInf[i] <= valVerdadeiro) && (valVerdadeiro <= limSup[i])){
      amostrasContemVal = amostrasContemVal + 1
    }
  }
  result = amostrasContemVal / quantAmostras
  return(result)
}

### Letra A

propMaisDe5 = quantMaioresQue5(populacao) / tamanhoPop
mediaComodos = sum(populacao) / tamanhoPop


### Proporção de domicílios com Mais de 5 Cômodos
print(propMaisDe5)
### Número Médio de Cômodos por Domicílio
print(mediaComodos)

### Letra B

tamanhoAmostra = 20

limInfPropAmostral = c()
limSupPropAmostral = c()
limInfMediaAmostral = c()
limSupMediaAmostral = c()


for(i in 1:10000){
  amostra = sample(populacao, tamanhoAmostra, replace = FALSE)
  
  propAmostral = quantMaioresQue5(amostra) / tamanhoAmostra
  mediaAmostral = mean(amostra)
  
  eProp = 1.96 * sqrt(((propAmostral * (1 - propAmostral))/tamanhoAmostra))
  eMedia = 1.729 * (sd(amostra)/sqrt(tamanhoAmostra))
  
  limInfPropAmostral[i] = propAmostral - eProp
  limSupPropAmostral[i] = propAmostral + eProp
  
  limInfMediaAmostral[i] = mediaAmostral - eMedia
  limSupMediaAmostral[i] = mediaAmostral + eMedia 
}

icPropAmostral = matrix(c(limInfPropAmostral, limSupPropAmostral), ncol = 2, nrow = 10000)
icMediaAmostral = matrix(c(limInfMediaAmostral, limSupMediaAmostral), ncol = 2, nrow = 10000)

### Intervalos de Confiança de p Amostral 
print(icPropAmostral)
### Intervalos de Confiança de X Amostral
print(icMediaAmostral)

### Letra C

propIntervalosPropPop = propAmostrasContemValVerdadeiro(limInfPropAmostral, limSupPropAmostral, propMaisDe5) 
propIntervalosMediaPop = propAmostrasContemValVerdadeiro(limInfMediaAmostral, limSupMediaAmostral, mediaComodos)


### Proporção de Intervalos que Contém o Parâmetro p Verdadeiro
print(propIntervalosPropPop)
### Proporção de Intervalos que Contém o Parâmetro X Verdadeiro
print(propIntervalosMediaPop)
