#Bibliotecas
install.packages('OpenImageR')
library(OpenImageR)

#Ler imagem (tudo)
tudo=readImage("tudo.jpg")
imageShow(tudo)
dim(tudo) #linhas x colunas x matrix(R, G, B)
900*1600 #cada matriz

#imagem[Linha,Coluna,Matrix]
tudo2=tudo #naoperder original
tudo2[,1:1600,3]=0 #zerar blue colunas
imageShow(tudo2)

tudo2[,1:1600,2]=0 #zerar B,G colunas
imageShow(tudo2)

tudo2[,1:1600,1]=0 #zerar R,G,B colunas
imageShow(tudo2)

#lendo imagem mar
mar=readImage('mar.jpg')
imageShow(mar)

#lendo imagem poluicao
poluicao=readImage('poluicao.jpg')
imageShow(poluicao)

#lendo imagem ceu
ceu=readImage('ceu.jpg')
imageShow(ceu)

dim(mar)
900*1600

#(R, G, B)
c(mar[,,1])
length(c(mar[,,1]))
mmar=cbind(c(mar[,,1]),
             c(mar[,,2]),
             c(mar[,,3]))

mpoluicao=cbind(c(poluicao[,,1]),
             c(poluicao[,,2]),
             c(poluicao[,,3]))

mceu=cbind(c(ceu[,,1]),
            c(ceu[,,2]),
            c(ceu[,,3]))

dim(mmar)
dim(mpoluicao)
dim(mceu)

#Coletando amostras das imagens
mmar=mmar[sample(1:1440000,10000),]
dim(mmar)
mpoluicao=mpoluicao[sample(1:1440000,10000),]
dim(mpoluicao)
mceu=mceu[sample(1:1440000,10000),]
dim(mceu)

#arrumando dados (0=poluicao, 1=ceu, 2=mar)
cbind(mpoluicao,0)
cbind(mceu,1)
cbind(mmar,2)

dados=rbind(cbind(mpoluicao,0), cbind(mceu,1), cbind(mmar,2))
head(dados)
colnames(dados)=c("R","G","B",'Y')
head(dados)
dim(dados)

install.packages("randomForest")
library(randomForest)
modelo=randomForest(as.factor(Y)~R+G+B,data=dados)
print(modelo)
importance(modelo)

install.packages("ggplot2")
library(ggplot2)

# Criar o data frame com as informações
plot = data.frame(
  Cor = c("R", "G", "B"),
  Valor = c(5849.743, 6122.896, 7953.953))

cores <- c("blue", "green", "red")


# Plot do gráfico de barras
ggplot(plot, aes(x = Cor, y = Valor, fill = Cor))+
  geom_bar(stat = "identity", width = 0.5)+
  scale_fill_manual(values = cores) +
  labs(title = "Grafico de importancia", x = "Cor", y = "Valor") +
  theme_minimal()


#predicao imagem (tudo)
mtudo=cbind(c(tudo[,,1]),
            c(tudo[,,2]),
            c(tudo[,,3]))

head(mtudo)
colnames(mtudo)=c('R',"G","B")
head(mtudo)

pred=predict(modelo,newdata = mtudo)
table(pred)
table(as.numeric(pred)) #transforma numero

pred=as.numeric(pred)-1 #voltar ao original
table(pred)

ncol(tudo[,,2]) #quantas colunas original
mpred=matrix(pred,ncol=1600)

imageShow(mpred)
imageShow(tudo)

#mar
tudo2=tudo
tudo2[,,1][pred==2]=1
imageShow(tudo2)

#ceu
tudo2=tudo
tudo2[,,2][pred==1]=1
imageShow(tudo2)

#poluicao
tudo2=tudo
tudo2[,,3][pred==0]=1
imageShow(tudo2)

#Imagem classificada
tudo2[,,1][pred==2]=1
tudo2[,,2][pred==1]=1
tudo2[,,3][pred==0]=1
imageShow(tudo2)
