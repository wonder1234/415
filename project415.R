#Blake Arnold
#Stats 415 Project
setwd("/Users/blakearnold/Documents/Stats 415/Project")
credit = read.csv("credit.csv")
summary(credit)
sum(!complete.cases(credit))
#No missing values
class(credit$X)
head(credit$X)
col_names = c("ID", "Cred_Amnt", "Gender", "Education", "Married", "Age", "Repay_9", "Repay_8", "Repay_7", "Repay_6", "Repay_5", "Repay_4", "Bill_9", "Bill_8", "Bill_7", "Bill_6", "Bill_5", "Bill_4", "Payment_9", "Payment_8", "Payment_7", "Payment_6", "Payment_5", "Payment_4", "Default")
colnames(credit) = col_names
credit = credit[-1,]
credit[1,]
credit$Cred_Amnt = as.numeric(credit$Cred_Amnt)
credit$Age = as.numeric(credit$Age)
credit$Bill_9 = as.numeric(credit$Bill_9)
credit$Bill_8 = as.numeric(credit$Bill_8)
credit$Bill_7 = as.numeric(credit$Bill_7)
credit$Bill_6 = as.numeric(credit$Bill_6)
credit$Bill_5 = as.numeric(credit$Bill_5)
credit$Bill_4 = as.numeric(credit$Bill_4)
credit$Payment_9 = as.numeric(credit$Payment_9)
credit$Payment_8 = as.numeric(credit$Payment_8)
credit$Payment_7 = as.numeric(credit$Payment_7)
credit$Payment_6 = as.numeric(credit$Payment_6)
credit$Payment_5 = as.numeric(credit$Payment_5)
credit$Payment_4 = as.numeric(credit$Payment_4)
credit$Repay_9 = as.numeric(credit$Repay_9)
credit$Repay_8 = as.numeric(credit$Repay_8)
credit$Repay_7 = as.numeric(credit$Repay_7)
credit$Repay_6 = as.numeric(credit$Repay_6)
credit$Repay_5 = as.numeric(credit$Repay_5)
credit$Repay_4 = as.numeric(credit$Repay_4)
credit = credit[,-1]

summary(credit)

attach(credit)
plot(density(Age))
credit$Age_Grp = numeric(nrow(credit))
summary(Education[Age < 10])
#Why are a quarter of people less than 10 years old
levels(credit$Default)
levels(credit$Default) = c(0, 1, 0)
levels(credit$Default)
summary(credit$Default)

#Gender
summary(credit$Default[credit$Gender == 1])[[2]]/(summary(credit$Default[credit$Gender == 1])[[1]] + summary(credit$Default[credit$Gender == 1])[[2]])
#0.242
summary(credit$Default[credit$Gender == 2])[[2]]/(summary(credit$Default[credit$Gender == 2])[[1]] + summary(credit$Default[credit$Gender == 2])[[2]])
#0.208

#Education
#Graduate School
summary(credit$Default[credit$Education == 1])[[2]]/(summary(credit$Default[credit$Education == 1])[[1]] + summary(credit$Default[credit$Education == 1])[[2]])
#University
summary(credit$Default[credit$Education == 2])[[2]]/(summary(credit$Default[credit$Education == 2])[[1]] + summary(credit$Default[credit$Education == 2])[[2]])
#High School
summary(credit$Default[credit$Education == 3])[[2]]/(summary(credit$Default[credit$Education == 3])[[1]] + summary(credit$Default[credit$Education == 3])[[2]])
#Other
summary(credit$Default[credit$Education == 4])[[2]]/(summary(credit$Default[credit$Education == 4])[[1]] + summary(credit$Default[credit$Education == 4])[[2]])

#Only 123 people with "other" education
set.seed(123)
train_obs = sample(30000, 20000, rep = F)
test_obs = -train_obs

train = credit[train_obs,]
test = credit[test_obs,]

step(glm(Default ~ ., data = credit, family = "binomial"))










