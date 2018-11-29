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

summary(ajuste)

# Significancia global: 0.05 < 0.5158 -> Não Rejeitamos H0(beta_chapeu[i] == 0 | para todo i)
# Significância individual: 0.01 < 0.5693 e 0.01 < 0.4179 -> Não Rejeitamos H0(beta_chapeu[i] == 0 | para todo i)
# R2 Baixo -> modelo pouco ajustado

# Letra E

Indice_Previsto = 8.372602 + 0.005095 * 25 - 0.008577 * 100
Indice_Previsto

# Letra F 

# Gráficos Q-Q Plot
qqplot(Num_Ataques, residuals(ajuste), xlab="Num.Ataques",ylab="Resíduos")
qqplot(Duracao, residuals(ajuste), xlab="Duração",ylab="Resíduos")

# Gráfico de Envelope

fit.model <- ajuste
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.model)$sigma
r <- resid(fit.model)
tsi <- r/(si*sqrt(1-h))

ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)

for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }

for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }

med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)

par(pty="s")
qqnorm(tsi,xlab="Percentil da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16, main="")
par(new=TRUE)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")

# Teste de Kolmogorv-Smirnov

ks.test(residuals(ajuste), "pnorm")

# (pvalor = 0.3657) > 0.05 = resíduos seguem distribuição normal

# Homocedasticidade

plot(Num_Ataques, residuals(ajuste),xlab="Num.Ataques",ylab="Resíduos")
abline(h=0)

plot(Duracao, residuals(ajuste),xlab="Duração",ylab="Resíduos")
abline(h=0)

# Indícios de rejeição de homocedasticidade (NumAtaques)
