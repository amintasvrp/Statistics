#Adicionando arquivo a uma vari�vel
dados<-read.csv("DadosCiaMB.csv", header=TRUE,sep=";",dec=",")

#Adicionando ao sistema
attach(dados)

#Gr�fico em barras 
barplot(table(Instrucao))
barplot(prop.table(table(Instrucao)),col=c("darkgreen","darkblue","black"),main="Gr�fico de Barras da Vari�vel Grau de Instru��o",xlab="Grau de Instru��o")

ni<-c(12,18,6)
names(ni)<-c("Fundamental","M�dio","Superior")
barplot(ni,col="yellow",ylab="frequ�ncia(ni)",xlab="Grau de instru��o",ylim=range(0,20))

tabela1<-table(Filhos)
barplot(tabela1,main="Gr�fico em barras para a vari�vel N�mero de Filhos",col=c(1,2,3,4,5))

#Gr�fico de pizza
table(Instrucao)
pie(table(Instrucao))
pie(table(Instrucao),main="Grau de Instru��o",labels=c("33%","50%","17%"),col=c(1,2,3))
legend("topright",fill=c(1,2,3),legend=c("fundamental","m�dio","superior"))

#Histograma 
hist(salario)

hist(salario,prob=T,ylim=c(0,0.1),breaks=c(4,8,12,16,20,24),col="green",main="Histograma da variávelvel salário",xlim=c(4,24))
text(locator(n=5),c("28%","33%","22%","14%","3%"))

#Boxplot
boxplot(salario,col="yellow")
text(locator(n=5),c("max(x_(1),LI)","q_1","Md","q_3","min(x_(n),LS)"))

boxplot(salario)
boxplot(salario~Instrucao,col="blue")
boxplot(salario~procedencia)

#Gr�fico de dispers�o
summary(idade)
plot(salario~idade)
cor(salario,idade)