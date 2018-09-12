#Questao 1

populacao = rep(c(1, 0), times = c(75,25))

#Obter medias

mediar = function(n, repor){
  medias = c()
  for(i in 1:1000){
    amostra = sample(populacao, size=n, replace=repor)
    media = mean(amostra)
    medias[i] = media
  }
  return(medias)
}

medias10Rep = mediar(10, TRUE)
medias30Rep = mediar(30, TRUE)
medias50Rep = mediar(50, TRUE)
medias10 = mediar(10, FALSE)
medias30 = mediar(30, FALSE)
medias50 = mediar(50, FALSE)

variar = function(n, repor){
  variancias = c()
  for(i in 1:1000){
    amostra = sample(populacao, size=n, replace=repor)
    variancia = var(amostra)/ n
    variancias[i] = variancia
  }
  return(variancias)
}

variancias10Rep = variar(10, TRUE)
variancias30Rep = variar(30, TRUE)
variancias50Rep = variar(50, TRUE)
variancias10 = variar(10, FALSE)
variancias30 = variar(30, FALSE)
variancias50 = variar(50, FALSE)

hist(medias10Rep)
hist(medias30Rep)
hist(medias50Rep)
hist(medias10)
hist(medias30)
hist(medias50)

hist(variancias10Rep)
hist(variancias30Rep)
hist(variancias50Rep)
hist(variancias10)
hist(variancias30)
hist(variancias50)

mediaMedias10Rep = median(medias10Rep)
mediaMedias30Rep = median(medias30Rep)
mediaMedias50Rep = median(medias50Rep)
mediaMedias10 = median(medias10)
mediaMedias30 = median(medias30)
mediaMedias50 = median(medias50)

varMedias10Rep = var(medias10Rep) / 10
varMedias30Rep = var(medias30Rep) / 30
varMedias50Rep = var(medias50Rep) / 50
varMedias10 = var(medias10) / 10
varMedias30 = var(medias30) / 30
varMedias50 = var(medias50) / 50

mediaVariancias10Rep = median(variancias10Rep)
mediaVariancias30Rep = median(variancias30Rep)
mediaVariancias50Rep = median(variancias50Rep)
mediaVariancias10 = median(variancias10)
mediaVariancias30 = median(variancias30)
mediaVariancias50 = median(variancias50)

varVariancias10Rep = var(variancias10Rep) / 10
varVariancias30Rep = var(variancias30Rep) / 30
varVariancias50Rep = var(variancias50Rep) / 50
varVariancias10 = var(variancias10) / 10
varVariancias30 = var(variancias30) / 30
varVariancias50 = var(variancias50) / 50

mediaPop = mean(populacao)
varPop = var(populacao) / 100





