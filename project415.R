#Blake Arnold
#Stats 415 Project
setwd("/Users/blakearnold/Documents/Stats 415/Project")
credit = read.csv("credit.csv")
summary(credit)
sum(!complete.cases(credit))
#No missing values
col_names = c("Cred_Amnt", "Gender", "Education", "Married", "Age", "Repay_9", "Repay_8", "Repay_7", "Repay_6", "Repay_5", "Repay_4", "Bill_9", "Bill_8", "Bill_7", "Bill_6", "Bill_5", "Bill_4", "Payment_9", "Payment_8", "Payment_7", "Payment_6", "Payment_5", "Payment_4", "Default")
colnames(credit) = col_names
credit[1,]
summary(credit)

credit$Gender = factor(credit$Gender)
credit$Education = factor(credit$Education)
credit$Married = factor(credit$Married)
credit$Default = factor(credit$Default)
credit$Repay_9 = factor(credit$Repay_9)
credit$Repay_8 = factor(credit$Repay_8)
credit$Repay_7 = factor(credit$Repay_7)
credit$Repay_6 = factor(credit$Repay_6)
credit$Repay_5 = factor(credit$Repay_5)
credit$Repay_4 = factor(credit$Repay_4)

levels(credit$Repay_9)[1:3] = c("0", "0", "0")
levels(credit$Repay_8)[1:3] = c("0", "0", "0")
levels(credit$Repay_7)[1:3] = c("0", "0", "0")
levels(credit$Repay_6)[1:3] = c("0", "0", "0")
levels(credit$Repay_5)[1:3] = c("0", "0", "0")
levels(credit$Repay_5)[1:3] = c("0", "0", "0")
levels(credit$Repay_4)[1:3] = c("0", "0", "0")


levels(credit$Education) = c("0", "1", "2", "3", "0", "0", "0")
levels(credit$Married) = c("3", "1", "2", "3")
levels(credit$Default)

#Default Rate By Gender
summary(credit$Default[credit$Gender == 1])[[2]]/(summary(credit$Default[credit$Gender == 1])[[1]] + summary(credit$Default[credit$Gender == 1])[[2]])
#0.242
summary(credit$Default[credit$Gender == 2])[[2]]/(summary(credit$Default[credit$Gender == 2])[[1]] + summary(credit$Default[credit$Gender == 2])[[2]])
#0.208

#Default by Education
#Graduate School
summary(credit$Default[credit$Education == 1])[[2]]/(summary(credit$Default[credit$Education == 1])[[1]] + summary(credit$Default[credit$Education == 1])[[2]])
#University
summary(credit$Default[credit$Education == 2])[[2]]/(summary(credit$Default[credit$Education == 2])[[1]] + summary(credit$Default[credit$Education == 2])[[2]])
#High School
summary(credit$Default[credit$Education == 3])[[2]]/(summary(credit$Default[credit$Education == 3])[[1]] + summary(credit$Default[credit$Education == 3])[[2]])
#Other
summary(credit$Default[credit$Education == 4])[[2]]/(summary(credit$Default[credit$Education == 4])[[1]] + summary(credit$Default[credit$Education == 4])[[2]])

#Only 468 people with "other" education


set.seed(123)
train_obs = sample(30000, 20000, rep = F)
test_obs = -train_obs

train = credit[train_obs,]
test = credit[test_obs,]

m = step(glm(Default ~ ., data = train, family = "binomial"))
summary(m)
# Call:
#   glm(formula = Default ~ Cred_Amnt + Gender + Education + Married + 
#         Repay_9 + Repay_8 + Repay_7 + Repay_6 + Repay_4 + Bill_7 + 
#         Bill_4 + Payment_9 + Payment_8 + Payment_4, family = "binomial", 
#       data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2719  -0.5951  -0.5126  -0.3160   3.5428  
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.328e+00  2.882e-01  -8.075 6.74e-16 ***
#   Cred_Amnt   -1.540e-06  1.992e-07  -7.732 1.06e-14 ***
#   Gender2     -1.773e-01  3.908e-02  -4.538 5.68e-06 ***
#   Education1   1.087e+00  2.322e-01   4.684 2.81e-06 ***
#   Education2   1.114e+00  2.311e-01   4.822 1.42e-06 ***
#   Education3   1.147e+00  2.340e-01   4.901 9.54e-07 ***
#   Married1     1.503e-01  1.694e-01   0.887 0.375012    
# Married2    -3.801e-02  1.694e-01  -0.224 0.822502    
# Repay_91     7.540e-01  6.105e-02  12.351  < 2e-16 ***
#   Repay_92     2.110e+00  6.868e-02  30.719  < 2e-16 ***
#   Repay_93     2.048e+00  1.847e-01  11.086  < 2e-16 ***
#   Repay_94     1.780e+00  3.655e-01   4.871 1.11e-06 ***
#   Repay_95     1.379e+00  5.941e-01   2.322 0.020257 *  
#   Repay_96     7.709e-01  1.064e+00   0.724 0.468799    
# Repay_97     1.480e+01  2.391e+02   0.062 0.950643    
# Repay_98    -1.951e+01  4.967e+02  -0.039 0.968664    
# Repay_81     4.304e-02  6.931e-01   0.062 0.950487    
# Repay_82     2.104e-01  7.431e-02   2.832 0.004628 ** 
#   Repay_83     4.095e-01  1.820e-01   2.250 0.024431 *  
#   Repay_84    -6.114e-01  3.809e-01  -1.605 0.108502    
# Repay_85     7.795e-01  8.709e-01   0.895 0.370804    
# Repay_86    -3.331e-01  1.689e+00  -0.197 0.843689    
# Repay_87            NA         NA      NA       NA    
# Repay_71    -1.264e+01  3.771e+02  -0.034 0.973256    
# Repay_72     2.494e-01  7.387e-02   3.377 0.000734 ***
#   Repay_73     4.628e-01  2.392e-01   1.935 0.053044 .  
# Repay_74     2.931e-01  4.729e-01   0.620 0.535425    
# Repay_75     1.191e-01  9.091e-01   0.131 0.895805    
# Repay_76     2.260e+01  4.967e+02   0.045 0.963711    
# Repay_77    -2.668e-01  9.214e-01  -0.290 0.772184    
# Repay_78    -1.694e+00  1.936e+00  -0.875 0.381635    
# Repay_61     1.416e+01  3.771e+02   0.038 0.970040    
# Repay_62     4.819e-01  7.253e-02   6.644 3.06e-11 ***
#   Repay_63     2.230e-01  2.484e-01   0.898 0.369349    
# Repay_64     1.781e-01  4.624e-01   0.385 0.700179    
# Repay_65    -1.695e+00  8.249e-01  -2.054 0.039945 *  
#   Repay_66    -1.152e+01  3.750e+02  -0.031 0.975502    
# Repay_67     1.170e+00  1.498e+00   0.781 0.434734    
# Repay_68    -1.378e+01  3.693e+02  -0.037 0.970247    
# Repay_4-1   -1.980e-01  6.505e-02  -3.044 0.002335 ** 
#   Repay_40    -4.360e-01  6.139e-02  -7.102 1.23e-12 ***
#   Repay_42     6.345e-02  8.497e-02   0.747 0.455270    
# Repay_43     5.289e-01  2.572e-01   2.056 0.039747 *  
#   Repay_44    -3.456e-02  5.020e-01  -0.069 0.945101    
# Repay_45     2.366e-01  8.192e-01   0.289 0.772690    
# Repay_46     4.055e-01  1.223e+00   0.331 0.740297    
# Repay_47    -3.379e-01  1.526e+00  -0.221 0.824777    
# Repay_48     2.720e+01  4.961e+02   0.055 0.956276    
# Bill_7       2.874e-06  6.592e-07   4.359 1.31e-05 ***
#   Bill_4      -1.521e-06  7.466e-07  -2.038 0.041550 *  
#   Payment_9   -1.266e-05  2.792e-06  -4.535 5.75e-06 ***
#   Payment_8   -1.265e-05  2.621e-06  -4.827 1.39e-06 ***
#   Payment_4   -2.424e-06  1.532e-06  -1.583 0.113524    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 21247  on 19999  degrees of freedom
# Residual deviance: 17399  on 19948  degrees of freedom
# AIC: 17503
# 
# Number of Fisher Scoring iterations: 12

#Further cleaning of data. There appear to be some categories with very few observations
summary(credit$Repay_9)
summary(credit$Repay_8)
summary(credit$Repay_7)
summary(credit$Repay_6)
summary(credit$Repay_5)
summary(credit$Repay_4)

#Releveling repayment variables
levels(credit$Repay_9) = c("0", "1", "2", "3", "4", "4","4", "4", "4")
levels(credit$Repay_8) = c("0", "1", "2", "3", "4", "4","4", "4", "4")
levels(credit$Repay_7) = c("0", "1", "2", "3", "4", "4","4", "4", "4")
levels(credit$Repay_6) = c("0", "1", "2", "3", "4", "4","4", "4", "4")
levels(credit$Repay_5) = c("0", "1", "2", "3", "4", "4","4", "4", "4")
levels(credit$Repay_4) = c("0", "1", "2", "3", "4", "4","4", "4", "4")

R9 = table(credit$Repay_9, credit$Default)
R8 = table(credit$Repay_8, credit$Default)
R7 = table(credit$Repay_7, credit$Default)
R6 = table(credit$Repay_6, credit$Default)
R5 = table(credit$Repay_5, credit$Default)
R4 = table(credit$Repay_4, credit$Default)

#Testing difference between categories of repayment vars
chisq.test(R9[4:5,])
chisq.test(R9[3:4,])
chisq.test(R9[1:2,])
chisq.test(R9[2:3,])
#All are significantly different
chisq.test(R8[4:5,])
#not significantly different
chisq.test(R8[3:4,])
chisq.test(R8[1:2,])
#not significantly different
chisq.test(R8[2:3,])
#All are significantly different
chisq.test(R7[4:5,])
chisq.test(R7[3:4,])
chisq.test(R7[1:2,])
chisq.test(R7[2:3,])
#All are significantly different


quantile(credit$Payment_9, c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975, 0.99, 1))
quantile(credit$Payment_8, c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975, 0.99, 1))
quantile(credit$Payment_7, c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975, 0.99, 1))
quantile(credit$Payment_6, c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975, 0.99, 1))
quantile(credit$Payment_5, c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975, 0.99, 1))
quantile(credit$Payment_4, c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975, 0.99, 1))
#Perhaps eliminate top 1% of observations for each of these?
no_outliers = credit[credit$Payment_9 < 35000,]
no_outliers = no_outliers[no_outliers$Payment_8 < 35000, ]
no_outliers = no_outliers[no_outliers$Payment_7 < 35000, ]
no_outliers = no_outliers[no_outliers$Payment_6 < 35000, ]
no_outliers = no_outliers[no_outliers$Payment_5 < 35000, ]
no_outliers = no_outliers[no_outliers$Payment_4 < 35000, ]
set.seed(123)
trainobs2 = sample(1:nrow(no_outliers), 15000, rep = F)
train2 = no_outliers[trainobs2,]
test2 = no_outliers[-trainobs2,]
m_no_outliers = step(glm(Default ~., data = train2, family = "binomial"))
summary(m_no_outliers)

library(pROC)
auc(train2$Default, fitted(m_no_outliers), type = "response")
roc = plot.roc(x = train2$Default, fitted(m_no_outliers), print.auc = T)
roc = plot.roc(x = test2$Default, predict(m_no_outliers, test2, type = "response"), print.auc = T)
#Doesn't improve fit much to remove outliers

summary(credit$Cred_Amnt)
summary(credit)

train = credit[train_obs,]
test = credit[test_obs,]

m = step(glm(Default ~., data = train, family = "binomial"))
summary(m)
plot(m)

m_interact = step(glm(Default ~., data = train, family = "binomial"), scope = . ~ .^2)
m_interact = glm(Default ~ Cred_Amnt + Gender + Education + Married + Repay_9 + Repay_8 + Repay_7 + Repay_6 + Repay_4 + poly(Bill_4, 3) + poly(Bill_7, 3) + poly(Payment_9, 3) + poly(Payment_8, 3) + poly(Payment_4, 3), data = train, family = "binomial")
summary(m_interact)
library(pROC)
auc(train$Default, fitted(m), type = "response")
roc = plot.roc(x = train$Default, fitted(m), print.auc = T)

auc(train$Default, fitted(m_interact), type = "response")
roc = plot.roc(x = train$Default, fitted(m_interact), print.auc = T)
roc = plot.roc(x = test$Default, predict(m_interact, test, type = "response"), print.auc = T)

#AUC for test set
auc(test$Default, predict(m, test, type = "response"))
roc = plot.roc(x = test$Default, predict(m, test, type = "response"), print.auc = T)
               
test_preds = predict(m, test, type = "response")
head(test_preds)

test_binary = numeric(length(test_preds))
threshold_correct = numeric(length(seq(from = 0.1, to = 0.7, by = 0.05)))
threshold_sens = numeric(length(seq(from = 0.1, to = 0.7, by = 0.05)))
threshold_spec = numeric(length(seq(from = 0.1, to = 0.7, by = 0.05)))
j = 1

for(i in seq(from = 0.1, to = 0.7, by = 0.05)){
  test_binary = numeric(length(test_preds))
  test_binary[test_preds >= i] = 1
  threshold_correct[j] = mean(test_binary == test$Default)
  threshold_sens[j] = mean(test_binary[test$Default == 1] == 1)
  threshold_spec[j] = mean(test_binary[test$Default == 0] == 0)
  j = j + 1
}

names(threshold_correct) = seq(from = 0.1, to = 0.7, by = 0.05)
names(threshold_sens) = seq(from = 0.1, to = 0.7, by = 0.05)
names(threshold_spec) = seq(from = 0.1, to = 0.7, by = 0.05)
threshold_errors
threshold_sens
threshold_spec

test_preds[test_preds >= 0.25] = 1
test_preds[test_preds < 0.25] = 0
mean(test_preds != test$Default)
mean(test_preds[test$Default == 1] != 1)
mean(test_preds[test$Default == 0] != 0)

#Plots with releveled variables
barplot(height = c(sum(credit$Default[credit$Gender == 1] == 1)/sum(credit$Gender == 1), sum(credit$Default[credit$Gender == 2] == 1)/sum(credit$Gender == 2)), names.arg = c("Men", "Women"), main = "Default Rate by Gender", ylab = "Default Rate", xlab = "Gender", ylim = c(0,0.25), col = "purple")
barplot(height = c(sum(credit$Default[credit$Education == 1] == 1)/sum(credit$Education == 1), sum(credit$Default[credit$Education == 2] == 1)/sum(credit$Education == 2), sum(credit$Default[credit$Education == 3] == 1)/sum(credit$Education == 3), sum(credit$Default[credit$Education == 0] == 1)/sum(credit$Education == 0)), names.arg = c("Graduate", "College", "High School", "Other"), main = "Default Rate by Educational Attainment", ylab = "Default Rate", xlab = "Education", ylim = c(0,0.3), col = "red")
barplot(height = c(sum(credit$Default[credit$Married == 1] == 1)/sum(credit$Married == 1), sum(credit$Default[credit$Married == 2] == 1)/sum(credit$Married == 2), sum(credit$Default[credit$Married == 3] == 1)/sum(credit$Married == 3)), names.arg = c("Married", "Single", "Other"), main = "Default Rate by Marital Status", ylab = "Default Rate", xlab = "Marital Status", ylim = c(0,0.25), col = "Blue")

#Repayment
barplot(height = c(sum(credit$Default[credit$Repay_9 == 1] == 1)/sum(credit$Repay_9 == 1), sum(credit$Default[credit$Repay_9 == 2] == 1)/sum(credit$Repay_9 == 2), sum(credit$Default[credit$Repay_9 == 3] == 1)/sum(credit$Repay_9 == 3), sum(credit$Default[credit$Repay_9 == 4] == 1)/sum(credit$Repay_9 == 4), sum(credit$Default[credit$Repay_9 == 0] == 1)/sum(credit$Repay_9 == 0)), names.arg = c("1 Month", "2 Months", "3 Months", "> 4 Months", "Up to Date"), main = "Default Rate by Payment Status in September", ylab = "Default Rate", xlab = "Number of Months Late", ylim = c(0,1), col = "cyan")
barplot(height = c(sum(credit$Default[credit$Repay_8 == 1] == 1)/sum(credit$Repay_8 == 1), sum(credit$Default[credit$Repay_8 == 2] == 1)/sum(credit$Repay_8 == 2), sum(credit$Default[credit$Repay_8 == 3] == 1)/sum(credit$Repay_8 == 3), sum(credit$Default[credit$Repay_8 == 4] == 1)/sum(credit$Repay_8 == 4), sum(credit$Default[credit$Repay_8 == 0] == 1)/sum(credit$Repay_8 == 0)), names.arg = c("1 Month", "2 Months", "3 Months", "> 4 Months", "Up to Date"), main = "Default Rate by Payment Status in August", ylab = "Default Rate", xlab = "Number of Months Late", ylim = c(0,1), col = "cyan")
barplot(height = c(sum(credit$Default[credit$Repay_7 == 1] == 1)/sum(credit$Repay_7 == 1), sum(credit$Default[credit$Repay_7 == 2] == 1)/sum(credit$Repay_7 == 2), sum(credit$Default[credit$Repay_7 == 3] == 1)/sum(credit$Repay_7 == 3), sum(credit$Default[credit$Repay_7 == 4] == 1)/sum(credit$Repay_7 == 4), sum(credit$Default[credit$Repay_7 == 0] == 1)/sum(credit$Repay_7 == 0)), names.arg = c("1 Month", "2 Months", "3 Months", "> 4 Months", "Up to Date"), main = "Default Rate by Payment Status in July", ylab = "Default Rate", xlab = "Number of Months Late", ylim = c(0,1), col = "cyan")
barplot(height = c(sum(credit$Default[credit$Repay_6 == 1] == 1)/sum(credit$Repay_6 == 1), sum(credit$Default[credit$Repay_6 == 2] == 1)/sum(credit$Repay_6 == 2), sum(credit$Default[credit$Repay_6 == 3] == 1)/sum(credit$Repay_6 == 3), sum(credit$Default[credit$Repay_6 == 4] == 1)/sum(credit$Repay_6 == 4), sum(credit$Default[credit$Repay_6 == 0] == 1)/sum(credit$Repay_6 == 0)), names.arg = c("1 Month", "2 Months", "3 Months", "> 4 Months", "Up to Date"), main = "Default Rate by Payment Status in June", ylab = "Default Rate", xlab = "Number of Months Late", ylim = c(0,1), col = "cyan")
barplot(height = c(sum(credit$Default[credit$Repay_5 == 1] == 1)/sum(credit$Repay_5 == 1), sum(credit$Default[credit$Repay_5 == 2] == 1)/sum(credit$Repay_5 == 2), sum(credit$Default[credit$Repay_5 == 3] == 1)/sum(credit$Repay_5 == 3), sum(credit$Default[credit$Repay_5 == 4] == 1)/sum(credit$Repay_5 == 4), sum(credit$Default[credit$Repay_5 == 0] == 1)/sum(credit$Repay_5 == 0)), names.arg = c("1 Month", "2 Months", "3 Months", "> 4 Months", "Up to Date"), main = "Default Rate by Payment Status in May", ylab = "Default Rate", xlab = "Number of Months Late", ylim = c(0,1), col = "cyan")
barplot(height = c(sum(credit$Default[credit$Repay_4 == 1] == 1)/sum(credit$Repay_4 == 1), sum(credit$Default[credit$Repay_4 == 2] == 1)/sum(credit$Repay_4 == 2), sum(credit$Default[credit$Repay_4 == 3] == 1)/sum(credit$Repay_4 == 3), sum(credit$Default[credit$Repay_4 == 4] == 1)/sum(credit$Repay_4 == 4), sum(credit$Default[credit$Repay_4 == 0] == 1)/sum(credit$Repay_4 == 0)), names.arg = c("1 Month", "2 Months", "3 Months", "> 4 Months", "Up to Date"), main = "Default Rate by Payment Status in April", ylab = "Default Rate", xlab = "Number of Months Late", ylim = c(0,1), col = "cyan")

#Boxplots, Limit default vs non-default
boxplot(credit$Cred_Amnt ~ credit$Default, main = "Credit Limit", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
#Bill, default vs non-default
par(mfrow = c(2,3))
boxplot(credit$Bill_9 ~ credit$Default, main = "Bill in September", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Bill_8 ~ credit$Default, main = "Bill in August", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Bill_7 ~ credit$Default, main = "Bill in July", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Bill_6 ~ credit$Default, main = "Bill in June", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Bill_5 ~ credit$Default, main = "Bill in May", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Bill_4 ~ credit$Default, main = "Bill in April", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
par(new = F, mfrow = c(1,1))
#Payment, default vs non-default
#Think about constructing variable for Bill less Payment for each month
boxplot(credit$Payment_9 ~ credit$Default, main = "Payment in September", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Payment_8 ~ credit$Default, main = "Payment in August", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Payment_7 ~ credit$Default, main = "Payment in July", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Payment_6 ~ credit$Default, main = "Payment in June", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Payment_5 ~ credit$Default, main = "Payment in May", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Payment_4 ~ credit$Default, main = "Payment in April", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))

#Constructing Differences Between Bill Amount and Payment
credit$Diff_9 = credit$Bill_9 - credit$Payment_9
credit$Diff_8 = credit$Bill_8 - credit$Payment_8
credit$Diff_7 = credit$Bill_7 - credit$Payment_7
credit$Diff_6 = credit$Bill_6 - credit$Payment_6
credit$Diff_5 = credit$Bill_5 - credit$Payment_5
credit$Diff_4 = credit$Bill_4 - credit$Payment_4

boxplot(credit$Diff_9 ~ credit$Default, main = "Difference In Bill and Payment in September", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Diff_8 ~ credit$Default, main = "Difference In Bill and Payment in August", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Diff_7 ~ credit$Default, main = "Difference In Bill and Payment in July", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Diff_6 ~ credit$Default, main = "Difference In Bill and Payment in June", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Diff_5 ~ credit$Default, main = "Difference In Bill and Payment in May", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
boxplot(credit$Diff_4 ~ credit$Default, main = "Difference In Bill and Payment in April", ylab = "Taiwanese Dollars (TWD)", names = c("Non-Default", "Default"))
