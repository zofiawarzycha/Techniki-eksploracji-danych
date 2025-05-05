#zadanie 1
library(class)
library(ggplot2)
class(diamonds)
colnames(diamonds)
dia = as.data.frame(diamonds)
class(dia)

class(dia[,"cut"])
str(dia[,"cut"])
levels(dia[,"cut"])

dia1 = dia[,c(1,5,6,7,8,9,10)]
d = c(0.24,55.8,55,322,3.87,4.01, 2.86)
dia2 = rbind(dia1,d)

nor = function(x) {  (x - min(x))/(max(x) - min(x))}

dia2.norm = as.data.frame(lapply(dia2[,1:7], nor))
dia2.norm

d1 = dia2.norm[nrow(dia2.norm),]
d1

dia3.norm = dia2.norm[-nrow(dia2.norm),]

dia.target = dia[,2]
dia.target

pr = knn(dia3.norm, d1, cl = dia.target, k = 80)
pr

#zadanie 2
library(ggplot2)
set.seed(240)

dia = data.frame(diamonds)
ran = sample(1:nrow(dia), 0.7 * nrow(dia))
ran

nor = function(x) {  (x - min(x))/(max(x) - min(x))}

dia.nor=as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)], nor))

dia.train=dia.nor[ran,]
dia.test=dia.nor[-ran,]

dia.target=as.factor(dia[ran,2])
dia.target
test.target=as.factor(dia[-ran,2])
test.target

library(class)

accur=function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

k.optm=1
ks=1
ke=50

for (i in ks:ke){
  pr <- knn(dia.train, dia.test, cl=dia.target, k=i)
  tb = table(pr,test.target)
  k.optm[i] = accur(tb)
  cat(i,'=',k.optm[i],'
')}

plot(ks:ke, k.optm[ks:ke], type="b", xlab="k- Value",ylab="Accuracy level")
best_k = which.max(k.optm)
best_k

d = c(0.24,55.8,55,322,3.87,4.01,2.86)
dia1 = dia[ran,c(1,5,6,7,8,9,10)]
dia2 = rbind.data.frame(dia1,d)

dia2.norm = as.data.frame(lapply(dia2[,1:7],nor))
d1 = dia2.norm[nrow(dia2.norm),]
dia3.norm = dia2.norm[-nrow(dia2.norm),]

dia.target.cut = dia[ran,2]
dia.target.cut

pr=knn(dia3.norm, d1, cl=dia.target.cut, k=16)
pr

#zadanie 3
library(class)
library(ggplot2)

colnames(diamonds)
dia = as.data.frame(diamonds)
class(dia[,"cut"])
levels(dia[,"cut"])

str(dia$color)
dia$color<- as.numeric(dia$color)
dia$color
dia$clarity <- as.numeric(dia$clarity)
dia$clarity

dia$cut <- NULL
d = c(0.24,4,6,55.8,55,322,3.87,4.01, 2.86) #G=4, VVS2=6
dia2 = rbind(dia,d)

nor = function(x) { (x-min(x))/(max(x)-min(x))   }

dia2.norm = as.data.frame(lapply(dia2[,1:9], nor))
d1 = dia2.norm[nrow(dia2.norm),]
d1
dia3.norm = dia2.norm[-nrow(dia2.norm),]
dia.target = diamonds$cut
dia.target

pr = knn(dia3.norm, d1, cl = dia.target, k = 15)
pr

#zadanie 4
library(carData)
class(Chile)
dane <- Chile
colnames(dane)

dane <- dane[which(complete.cases(dane[,1:7])),]
str(dane)
dane$region <- model.matrix(~region-1, data = dane)
dane$sex <- model.matrix(~sex-1, data = dane)
dane$region
dane$sex

dane$education <- factor(dane$education,levels = c("P", "S", "PS"), ordered = TRUE)
dane$education <- as.numeric(dane$education)

#danecz1 = dane[which(complete.cases(dane[,1:7])),] #wyrzuca wiersza w których jest NA w kolumach 1,2,3
danena <- dane[which(is.na.data.frame(dane[,"vote"])),]
danena
nrow(danena)
danecc <- dane[which(complete.cases(dane)),]
danecc

nor = function(x) { (x -min(x))/(max(x)-min(x))   }
library(class)
set.seed(123)

ran <- sample(1:nrow(danecc), 0.7 * nrow(danecc))
dane.nor <- as.data.frame(lapply(danecc[,1:7], nor))
dane.nor

dane.train <- dane.nor[ran,]
dane.train
dane.test <- dane.nor[-ran,]
dane.test
dane.target <- as.factor(danecc[ran,8])
dane.target
test.target <- as.factor(danecc[-ran,8])
test.target

accur = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accurk <- numeric(150)
accurk
for (i in 1:150){
  pr <- knn(dane.train, dane.test, cl=dane.target, k=i)
  tb=table(pr,test.target)
  accurk[i]=accur(tb)
  cat(i,'=',accurk[i],'
')}

plot(accurk, type="b", xlab="k- Value",ylab="Accuracy level")
which.max(accurk)

danecale <- rbind.data.frame(danecc[,1:7], danena[,1:7]) #łączymy dane kompletne i dane z vote NA bez kolumny vote
danecale.norm <- as.data.frame(lapply(danecale, nor)) #normujemy
dane.train <- danecale[1:nrow(danecc),] #dane do uczenia
dane.uzup <- danecale[-(1:nrow(danecc)),] #dane do uzupełnienia, klasyfikacji kolumny vote
cl.target <- danecc[,8] #zadane wartości vote w zbiorze uczącym
cl.target
pr <- knn(dane.train, dane.uzup, cl = cl.target, k = 34)
pr #uzupełnienie, predykacja
dane.uzup <- cbind.data.frame(danena[,1:7],vote = pr)#uzupełnienie vote w NA
danebezna <- rbind(danecc, dane.uzup)#całe dane z uzupełnionymi vote