#Questao 1

populacao = rep(c(1, 0), times = c(75,25))

#Obter medias

obterMedia = function(n, repor){
  medias = c()
  for(i in 1:1000){
    amostra = sample(populacao, size=n, replace=repor)
    media = mean(amostra)
    medias[i] = media
  }
  return(medias)
}

medias10Rep = obterMedia(10, TRUE)
medias30Rep = obterMedia(30, TRUE)
medias50Rep = obterMedia(50, TRUE)
medias10 = obterMedia(10, FALSE)
medias30 = obterMedia(30, FALSE)
medias50 = obterMedia(50, FALSE)

# Obter variancias

obterVariancia = function(n, repor){
  variancias = c()
  for(i in 1:1000){
    amostra = sample(populacao, size=n, replace=repor)
    variancia = var(amostra)/ n
    variancias[i] = variancia
  }
  return(variancias)
}

variancias10Rep = obterVariancia(10, TRUE)
variancias30Rep = obterVariancia(30, TRUE)
variancias50Rep = obterVariancia(50, TRUE)
variancias10 = obterVariancia(10, FALSE)
variancias30 = obterVariancia(30, FALSE)
variancias50 = obterVariancia(50, FALSE)


# Plotar histograma das Medias

hist(medias10Rep)
hist(medias30Rep)
hist(medias50Rep)
hist(medias10)
hist(medias30)
hist(medias50)

# Plotar histograma das Variancias

hist(variancias10Rep)
hist(variancias30Rep)
hist(variancias50Rep)
hist(variancias10)
hist(variancias30)
hist(variancias50)

# Media das Medias

mediaMedias10Rep = median(medias10Rep)
mediaMedias30Rep = median(medias30Rep)
mediaMedias50Rep = median(medias50Rep)
mediaMedias10 = median(medias10)
mediaMedias30 = median(medias30)
mediaMedias50 = median(medias50)

# Variancia das Medias

varMedias10Rep = var(medias10Rep) / 10
varMedias30Rep = var(medias30Rep) / 30
varMedias50Rep = var(medias50Rep) / 50
varMedias10 = var(medias10) / 10
varMedias30 = var(medias30) / 30
varMedias50 = var(medias50) / 50

# Media das Variancias

mediaVariancias10Rep = median(variancias10Rep)
mediaVariancias30Rep = median(variancias30Rep)
mediaVariancias50Rep = median(variancias50Rep)
mediaVariancias10 = median(variancias10)
mediaVariancias30 = median(variancias30)
mediaVariancias50 = median(variancias50)

# Variancia das variancias

varVariancias10Rep = var(variancias10Rep) / 10
varVariancias30Rep = var(variancias30Rep) / 30
varVariancias50Rep = var(variancias50Rep) / 50
varVariancias10 = var(variancias10) / 10
varVariancias30 = var(variancias30) / 30
varVariancias50 = var(variancias50) / 50

# Media e variancia da populacao

mediaPop = mean(populacao)
varPop = var(populacao) / 100


#Analise Estatistica
#1. N = 10 e Com Repeticao
#1.1. Media das Medias"
print(mediaMedias10Rep)
#1.2. Variancia das Medias
print(varMedias10Rep)
#1.3. Media das Variancias
print(mediaVariancias10Rep)
#1.4. Variancia das Variancias
print(varVariancias10Rep)
#2. N = 30 e Com Repeticao
#2.1. Media das Medias
print(mediaMedias30Rep)
#2.2. Variancia das Medias
print(varMedias30Rep)
#2.3. Media das Variancias
print(mediaVariancias30Rep)
#2.4. Variancia das Variancias
print(varVariancias30Rep)
#3. N = 50 e Com Repeticao
#3.1. Media das Medias
print(mediaMedias50Rep)
#3.2. Variancia das Medias
print(varMedias50Rep)
#3.3. Media das Variancias
print(mediaVariancias50Rep)
#3.4. Variancia das Variancias
print(varVariancias50Rep)
#4. N = 10 e Sem Repeticao
#4.1. Media das Medias
print(mediaMedias10)
#4.2. Variancia das Medias
print(varMedias10)
#4.3. Media das Variancias
print(mediaVariancias10)
#4.4. Variancia das Variancias
print(varVariancias10)
#5. N = 30 e Sem Repeticao
#5.1. Media das Medias
print(mediaMedias30)
#5.2. Variancia das Medias
print(varMedias30)
#5.3. Media das Variancias
print(mediaVariancias30)
#5.4. Variancia das Variancias
print(varVariancias30)
#6. N = 50 e Com Repeticao
#6.1. Media das Medias
print(mediaMedias50)
#6.2. Variancia das Medias
print(varMedias50)
#6.3. Media das Variancias
print(mediaVariancias50)
#6.4. Variancia das Variancias
print(varVariancias50)
#7. Populacao
#7.1. Media
print(mediaPop)
#7.2. Variancia
print(varPop)


# Questao 2

mediasPoisson <- c()
varianciasPoisson <- c()
for (i in 1:1000) {
  amostra_p <- rpois(100,10)
  mediasPoisson[i] <- mean(amostra_p)
  varianciasPoisson[i] <- var(amostra_p)
}

hist(mediasPoisson)
hist(varianciasPoisson)

# Questao 3

estimador <- c()

for (i in 1:1000) {
  amostraExp <- rexp(100,5)
  estimador[i] <- 1 / mean(amostraExp)
}

hist(estimador)
mean(estimador)
var(estimador)