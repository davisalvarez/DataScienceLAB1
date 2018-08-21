
#Instalar librerias 
#install.packages("fpc")
#install.packages("e1071")
#install.packages("corrplot")
#install.packages("plyr")
#install.packages("ggplot2")

#Importar librerias

library(ggplot2)
library(cluster)
library(fpc)
library(e1071)
library(mclust) 
library(corrplot)
library(plyr)
library(dplyr)

#Leer datos 
trainCSVNum <- read.csv("./Data/descargas/train.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)
trainCSVCat <- read.csv("./Data/descargas/train.csv", header = TRUE, sep = "," , stringsAsFactors = TRUE)
testCSV <- read.csv("./Data/descargas/test.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)
priceCSV <- read.csv("./Data/descargas/sample_submission.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)

#obtenemos las variables numericas
trainNum <- trainCSVNum[,(unlist(lapply(trainCSVNum, is.numeric)))]
trainNum$Id <- NULL #eliminamos el ID, que no representa una escala

#obtenemos las variables categoricas
trainCat <- trainCSVCat[,!(unlist(lapply(trainCSVCat, is.numeric)))]

#Convertimos a factores las variables categoricas
trainCat_convert<-sapply(trainCat,is.factor)
inter<-sapply(trainCat[,trainCat_convert],unclass)
trainCatConverted <-cbind(trainCat[,!trainCat_convert],inter)

#Reemplazamos los NA por 0
trainCatConverted[is.na(trainCatConverted)] <- 0
corNoNumeric<-cor(trainCatConverted)
absCor <- abs(corNoNumeric)
test <- as.data.frame(absCor[,any > 0.5])

#graficamos el mapa de correlacion (todas contra todas)
corrplot(corNoNumeric, method="number")

#Seleccionamos las variables que presentan mayor correlacion
noNumericCorre <- trainCatConverted[,c("Exterior1st","Exterior2nd","Heating","HeatingQC","Foundation",
                                               "KitchenQual","ExterQual","LandSlope","LandContour","CentralAir")]
#graficamos el mapa de correlacion
corrplot(cor(noNumericCorre), method="number")

#realizamos el clostering 
data<-noNumericCorre
km<-kmeans(noNumericCorre[,],3)
data$grupo<-km$cluster


g1<- data[data$grupo==1,]
prop.table(table(g1$Species))*100
nrow(g1)
summary(g1)

#grafica la ubicación de los clusters
plotcluster(noNumericCorre[,],km$cluster) 


#glimpse(trainCat)


cols<- c("MSZoning","Street","Alley")
table(trainCat$MSZoning)

names <- colnames(trainCat)
for (i in 1:length(names)){
  barplot(prop.table(table(trainCat[i] )),xlab = colnames(trainCat[i]))
  #print(table(trainCat[i]))
}
