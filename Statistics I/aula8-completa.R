numeros <- c(1, 2, 3, 5, 6, 7, 8, 11, 2, 3, 44, 55, 67, 12, 34, 56)
var(numeros)
variancia <- var(numeros)
sqrt(variancia)
sd(numeros)
nums <- read.csv(file="/home/alura/numeros.csv")
nums
nums$X1
hist(nums$X1)