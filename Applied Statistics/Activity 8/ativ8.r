# Questão 1

# Constantes

volume_de_trafego = c(3,3,5,5,10,10,15,15,20,20,25,25,30,30)
velocidade_media = c(95.60, 93.80, 74.40, 74.80, 50.50, 51.50, 44.60, 42.40, 35.80, 38.70, 32.00, 32.00, 30.10, 29.10)

# Letra a

plot(volume_de_trafego, velocidade_media)


# Letra b

## Aparentemente, a relação entre as variáveis "volume de tráfego" e "velocidade média" pode ser descrita a partir do gráfico de uma função semelhante a y = 1/|x| 

# Letra c

r = cor(volume_de_trafego, velocidade_media)
print(r)

## Verificamos através do coeficiente de correlação linear que as variáveis "volume de tráfego" e "velocidade média" possuem uma forte relação linear, de forma que a inclinação da reta é decrescente

# Letra D

modelo_linear = lm(velocidade_media ~ volume_de_trafego)
modelo_linear

## Percebemos, através da saída do comando lm() que o modelo da relação linear entre as variáveis trabalhadas pode ser definido da seguinte forma:
## velocidade_media = 85.340 - 2.173 * volume_de_trafego

# Letra E

abline(modelo_linear)

# Questão 2

# Letra A

xA = volume_de_trafego
yA = velocidade_media

modeloA = lm(yA ~ xA)
abline(modeloA)
cor(xA,yA)
anova(modeloA)

# Letra B

xB = volume_de_trafego ** 2
yB = velocidade_media

modeloB = lm(yB ~ xB)
abline(modeloB)
cor(xB,yB)
anova(modeloB)

# Letra C

xC = log10(volume_de_trafego)
yC = velocidade_media

modeloC = lm(yC ~ xC)
abline(modeloC)
cor(xC,yC)
anova(modeloC)

# Letra D

xD = sqrt(volume_de_trafego)
yD = velocidade_media

modeloD = lm(yD ~ xD)
abline(modeloD)
cor(xD,yD)
anova(modeloD)


# Letra E

xE = 1 / sqrt(volume_de_trafego)
yE = velocidade_media

modeloE = lm(yE ~ xE)
abline(modeloE)
cor(xE,yE)
anova(modeloE)

regressaoA = 5836 / (5836 + 1226)
print(regressaoA)

regressaoB = 4537.4 / (4537.4 + 2524.6)
print(regressaoB)

regressaoC = 6911.0 / (6911.0 + 151.1)
print(regressaoC)

regressaoD = 6472.0 / (6472.0 + 590.1)
print(regressaoD)

regressaoE = 7044.1 / (7044.1 + 18.0)
print(regressaoE)

## Como a parte sistemática do caso proposto pela letra E possui a maior influência no modelo, concluímos que o caso da letra E consiste naquele que se ajusta melhor aos dados