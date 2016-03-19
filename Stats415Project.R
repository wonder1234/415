#Blake Arnold
#Jane Xiang
#Jack Zhao
#Stats 415 Project

setwd("/Users/blakearnold/Downloads")
mydata = read.csv("train.csv")
summary(mydata)
pca = princomp(mydata[,-c(1, 371)], cor = T)

var0 = numeric(371)
vars = var(mydata)
for(i in 1:371){
  if(vars[i,i] == 0){
    var0[i] = 1
  }
}
sum(var0)
names(var0) = colnames(mydata)
var0[var0 == 1]

summary(mydata[,names(var0[var0 == 1])])

mydata = mydata[,names(var0[var0 == 0])]
pca = princomp(mydata[complete.cases(mydata),-c(1, 337)], cor = T)
covariates = mydata[,-c(1,337)]
pca = princomp(covariates, cor = F)
summary(mydata$TARGET)
mean(mydata[mydata$var15 > 40,]$TARGET)
mean(mydata[mydata$var15 <= 40,]$TARGET)
#Seems that those over 40 years old are much more likely to be dissatisfied
colnames(covariates)
plot(density(covariates$var3))
hist(covariates$var3)
summary(covariates$var3)
boxplot(covariates$var3)
tail(covariates$var3[order(covariates$var3)], 500)
eigen_cov = eigen(var(covariates))
eigen_cov$values



