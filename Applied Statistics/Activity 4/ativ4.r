# Questao 1

mean_pop = 500
sd_pop = 10

n_10 <- 10
n_20 <- 20
n_30 <- 30
n_50 <- 50
n_100 <- 100

s2_amostra_10 = c()
sigma2_amostra_10 = c()
s2_amostra_20 = c()
sigma2_amostra_20 = c()
s2_amostra_30 = c()
sigma2_amostra_30 = c()
s2_amostra_50 = c()
sigma2_amostra_50 = c()
s2_amostra_100 = c()
sigma2_amostra_100 = c()

s2 = function(amostra, n){
  x_barra = mean(amostra)
  somatorio_quadratico = 0
  for(i in 1:n){
    somatorio_quadratico <- somatorio_quadratico + ((amostra[i] - x_barra)**2)
  }
  return(somatorio_quadratico/(n-1))
}

sigma2 = function(amostra, n){
  x_barra = mean(amostra)
  somatorio_quadratico = 0
  for(i in 1:n){
    somatorio_quadratico <- somatorio_quadratico + ((amostra[i] - x_barra)**2)
  }
  return(somatorio_quadratico/n)
}

# gerando s2 e sigma2 de n = 10
for(i in 1:10000){
  amostra_10 = rnorm(n_10, mean_pop, sd_pop)
  s2_amostra_10[i] = s2(amostra_10, n_10)
  sigma2_amostra_10[i] = sigma2(amostra_10, n_10)
}

# gerando s2 e sigma2 de n = 20
for(i in 1:10000){
  amostra_20 = rnorm(n_20, mean_pop, sd_pop)
  s2_amostra_20[i] = s2(amostra_20, n_20)
  sigma2_amostra_20[i] = sigma2(amostra_20, n_20)
}

# gerando s2 e sigma2 de n = 30
for(i in 1:10000){
  amostra_30 = rnorm(n_30, mean_pop, sd_pop)
  s2_amostra_30[i] = s2(amostra_30, n_30)
  sigma2_amostra_30[i] = sigma2(amostra_30, n_30)
}

# gerando s2 e sigma2 de n = 50
for(i in 1:10000){
  amostra_50 = rnorm(n_50, mean_pop, sd_pop)
  s2_amostra_50[i] = s2(amostra_50, n_50)
  sigma2_amostra_50[i] = sigma2(amostra_50, n_50)
}

# gerando s2 e sigma2 de n = 100
for(i in 1:10000){
  amostra_100 = rnorm(n_100, mean_pop, sd_pop)
  s2_amostra_100[i] = s2(amostra_100, n_100)
  sigma2_amostra_100[i] = sigma2(amostra_100, n_100)
}

# histogramas
hist(s2_amostra_10)
hist(s2_amostra_20)
hist(s2_amostra_30)
hist(s2_amostra_50)
hist(s2_amostra_100)
hist(sigma2_amostra_10)
hist(sigma2_amostra_20)
hist(sigma2_amostra_30)
hist(sigma2_amostra_50)
hist(sigma2_amostra_100)

# medias dos s2
mean(s2_amostra_10)
mean(s2_amostra_20)
mean(s2_amostra_30)
mean(s2_amostra_50)
mean(s2_amostra_100)

# variancia dos s2
var(s2_amostra_10)
var(s2_amostra_20)
var(s2_amostra_30)
var(s2_amostra_50)
var(s2_amostra_100)

# media dos sigma2
mean(sigma2_amostra_10)
mean(sigma2_amostra_20)
mean(sigma2_amostra_30)
mean(sigma2_amostra_50)
mean(sigma2_amostra_100)

# variancia dos sigma2
var(sigma2_amostra_10)
var(sigma2_amostra_20)
var(sigma2_amostra_30)
var(sigma2_amostra_50)
var(sigma2_amostra_100)

# Como podemos perceber, conforme aumentarmos o n (tamanho da amostra), os valores de média e variância de S2 se aproximam mais rapidamente
# dos valores da população do que os valores de média e variãncia de Sigma2

# Questão 2

n = 100
t1 = c()
t2 = c()

for(i in 1:10000){
  amostra = runif(n, min = 0, max = 500)
  t1[i] = 2 * mean(amostra)
  t2[i] = ((n + 1) / n) * max(amostra)
}

vies_t1 = c()
vies_t2 = c()
eqm_t1 = c()
eqm_t2 = c()

e_t1 = mean(t1)
e_t2 = mean(t2)
var_t1 = var(t1)
var_t2 = var(t2)

vies_t1 = e_t1 - 500
vies_t2 = e_t2 - 500

eqm_t1 = var_t1 + (vies_t1)**2
eqm_t2 = var_t2 + (vies_t2)**2

# histogramas
hist(t1)
hist(t2)

# prints
print(vies_t1)
print(vies_t2)
print(eqm_t1)
print(eqm_t2)

# t2 porque se distancia menos e eqm é menor

