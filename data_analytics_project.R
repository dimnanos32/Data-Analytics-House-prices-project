#Final Project
#ΤASK 1
#a)
rawdata=read.table("C:/Users/nanos/OneDrive/Υπολογιστής/Data Analytics College Link/Final project/data.txt",header=T,dec=",")
data=read.table("C:/Users/nanos/OneDrive/Υπολογιστής/Data Analytics College Link/Final project/data.txt",header=T,dec=",")
View(data)
View(rawdata)
str(data)
dim(data)
for (i in 1:117){
  for (j in 1:7){
    if (data[i,j]=="*"){
      data[i,j]=NA
    }
  }
}
apply(is.na(data),2,sum)

#b)
data$NE=factor(data$NE, levels=0:1, labels=c("NO","YES"))
data$COR=factor(data$COR, levels=0:1, labels=c("NO","YES"))

#c)
del=which((apply(is.na(data), 1, sum))>1)
data=data[-del,]
row.names(data)=c(1:109)

#d)
data$SQFT=data$SQFT/10.764
names(data)[2]="SQM"
data$SQM=round(data$SQM,2)
str(data)
data$TAX=as.numeric(data$TAX)
data$AGE=as.integer(data$AGE)

#e)
sub=data.frame(data[,-c(5,6)])
lmf=lm(AGE~.,data=sub)
str(sub)
pred=0
c=which(is.na(data$AGE))
for(i in c){
  pred=predict(lmf,sub[i,],interval="confidence",level=0.95)
  data$AGE[i]=round(pred,0)
}
lmf2=lm(TAX~.,data=sub)
d=which(is.na(data$TAX))
pred2=0
for (i in d){
  pred2=predict(lmf2,sub[i,],interval="confidence",level=0.95)
data$TAX[i]=round(pred2,0)
}

#TASK 2
#a)
library(psych)
describe(data$PRICE)
describe(data$SQM)
describe(data$AGE)
describe(data$FEATS)
describe(data$TAX)
table(data$NE)
table(data$COR)
prop.table(table(data$FEATS))

#b)
library(PerformanceAnalytics)
library(corrplot)
chart.Correlation(data[,-c(5,6)])
COR=cor(data[,-c(5,6)])
COR
corrplot(COR, type = "upper")

#c)
prop.table(table(data$NE))
prop.table(table(data$COR))
prop.table(table(data$FEATS))

barplot(prop.table(table(data$FEATS)), col="green", main="Barplot of Feats", 
        xlab="Feats",ylim=c(0:1), ylab = "Propability", cex.lab=1.5, cex.main=2, axes=T)

barplot(prop.table(table(data$NE)), col=c("firebrick2", "green"), main="Barplot of NE", 
        xlab="NE",ylim=c(0:1), ylab = "Propability", cex.lab=1.5, cex.main=2, axes=T)

barplot(prop.table(table(data$COR)), col=c("firebrick2", "green"), main="Barplot of COR", 
        xlab="NE",ylim=c(0:1), ylab = "Propability", cex.lab=1.5, cex.main=2, axes=T)

par(mfcol=c(1,3))

#d)
table(data$PRICE,data$NE)
table(data$PRICE,data$COR)
boxplot(data$PRICE~data$NE,ylab="House Price", xlab="NE",col=c("red","green"))
boxplot(data$PRICE~data$COR,ylab="House Price", xlab="COR",col=c("red","green"))

par(mfcol=c(1,2))
mtext("Boxplots of PRICE for the categorical variables",outer=T,line=-2,cex=1.3)

#e)
priceYNE=data$PRICE[which(data$NE=="YES")]
mean(priceYNE)
priceNNE=data$PRICE[which(data$NE=="NO")]
mean(priceNNE)
shapiro.test(priceYNE)#oxi kanonikes kai oi 2
shapiro.test(priceNNE)
wilcox.test(priceNNE,priceYNE)#Statistika einai to idio

priceYCOR=data$PRICE[which(data$COR=="YES")]
mean(priceYCOR)
priceNCOR=data$PRICE[which(data$COR=="NO")]
mean(priceNCOR)
shapiro.test(priceYCOR)#se epipedo 5% kanoniki
shapiro.test(priceNCOR)#oxi kanoniki
wilcox.test(priceNCOR,priceYCOR)#Statistika einai to idio

#f)
library(e1071)
skewness(data$PRICE)
skewness(data$SQM)
skewness(data$AGE)
skewness(data$FEATS)
skewness(data$TAX)
shapiro.test(data$PRICE) #mi kanoniki
shapiro.test(data$SQM) #mi kanoniki
shapiro.test(data$TAX) #mi kanoniki
shapiro.test(log(data$PRICE)) # mi kanoniki
shapiro.test(log(data$SQM)) #kanoniki
shapiro.test(log(data$TAX)) #kanoniki
skewness(log(data$PRICE))  #beltiwsh sthn assymetria kai stis 3 metavlhtes
skewness(log(data$SQM))
skewness(log(data$TAX))
names(data)[1]="logPRICE"
names(data)[2]="logSQM"
names(data)[7]="logTAX"
data$logPRICE=log(data$logPRICE)
data$logSQM=log(data$logSQM)
data$logTAX=log(data$logTAX)
par(mfrow=c(1,1))
#g)
chart.Correlation(data[,-c(5,6)])
CO=cor(data[,-c(5,6)])
CO
corrplot(CO, type = "upper")

table(data$logPRICE,data$NE)
table(data$logPRICE,data$COR)
boxplot(data$logPRICE~data$NE,ylab="House Price", xlab="NE",col=c("red","green"))
boxplot(data$logPRICE~data$COR,ylab="House Price", xlab="COR",col=c("red","green"))

par(mfcol=c(1,2))
mtext("Boxplots of PRICE for the categorical variables",outer=T,line=-2,cex=1.3)






