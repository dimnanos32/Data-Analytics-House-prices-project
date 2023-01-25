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
sub=data.frame(data[-which(is.na(data$TAX)),])
row.names(sub)=c(1:107)
View(sub)
lmf=lm(AGE~.,data=sub)
summary(lmf)
pred=0
c=which(is.na(data$AGE))
for(i in c){
  pred=predict(lmf,sub[i,],interval="confidence",level=0.95)
  data$AGE[i]=round(pred,0)
}
lmf2=lm(TAX~.,data=data)
pred2=0
d=which(is.na(data$TAX))
for (j in d){
  pred2=predict(lmf2,data[j,],interval="confidence",level=0.95)
  data$TAX[j]=round(pred2,0)
}
summary(lmf2)
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
round(prop.table(table(data$FEATS)),4)

#b)
library(PerformanceAnalytics)
library(corrplot)
chart.Correlation(data[,-c(5,6)])
COR=cor(data[,-c(5,6)])
str(data)
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
        xlab="COR",ylim=c(0:1), ylab = "Propability", cex.lab=1.5, cex.main=2, axes=T)

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
mtext("Boxplots of logPRICE for the categorical variables",outer=T,line=-2,cex=1.3)

#TASK 3
#a)
lml=lm(logPRICE~.,data=data)
summary(lml)
#b)
final=step(lm(logPRICE~., data=data), direction="both")
summary(final)
par(mfrow=c(2,3))
plot(final,which=1:6)
load("lmtest.rda")
lmtest(final)

#c)
max(data$FEATS)
min(data$FEATS)
catFEATS = cut(data$FEATS, breaks=c(-1,3,7,8), labels=c("Low", "Moderate", "High"))
data[,8]=catFEATS
names(data)[8]="catFEATS"
anovalogPRICE=aov(data$logPRICE~data$catFEATS)
summary(anovalogPRICE) # h metavlhth catfeats epidra sthn log price

#d)
finalp=step(glm(as.numeric(data$NE)-1~., data=data, family = "binomial"), direction = "both")
summary(finalp)
attach(data)
xnew=data.frame(logSQM=log(180),AGE=15,FEATS=5,logTAX=log(1000))
logitp=predict(finalp, xnew)
p=exp(logitp)/(1+exp(logitp))
p

#e)
library(tree)
fitcat1=tree(COR~.,data=data[,-8])
summary(fitcat1)
plot(fitcat1)
text(fitcat1,cex=0.6)
mtext("Decision Tree for COR", side=3, line=1, cex=1.5)
tcattree=table(data$COR, predict(fitcat1,type="class"))
sum(diag(tcattree))/sum(tcattree)