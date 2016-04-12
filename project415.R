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

levels(credit$Education) = c("0", "1", "2", "3", "0", "0", "0")
levels(credit$Married) = c("3", "1", "2", "3")
levels(credit$Default)

attach(credit)
plot(density(Age))

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
library(pROC)
auc(train$Default, fitted(m))
test_preds = predict(m, test, type = "response")
head(test_preds)
test_preds[test_preds >= 0.25] = 1
test_preds[test_preds < 0.25] = 0
mean(test_preds != test$default.payment.next.month)
mean(test_preds[test$default.payment.next.month == 1] != 1)
mean(test_preds[test$default.payment.next.month == 0] != 0)

##################### LDA ######################
library(MASS)
lda.fit1=lda(Default ~ Cred_Amnt + Gender + Education + Married + Repay_9 + Bill_7 + 
         Bill_4 + Payment_9 + Payment_8 + Payment_4,data=train)
lda.fit1
# Call:
#   lda(Default ~ Cred_Amnt + Gender + Education + Married + Repay_9 + 
#         Bill_7 + Bill_4 + Payment_9 + Payment_8 + Payment_4, data = train)
# 
# Prior probabilities of groups:
#   0      1 
# 0.7766 0.2234 
# 
# Group means:
#   Cred_Amnt   Gender2 Education1 Education2 Education3  Married1  Married2  Repay_91
# 0  177670.4 0.6174994  0.3626062  0.4646536  0.1542622 0.4485578 0.5386943 0.1050734
# 1  130590.3 0.5669203  0.2987914  0.5067144  0.1893465 0.4876902 0.4986571 0.1929275
# Repay_92    Repay_93     Repay_94     Repay_95     Repay_96    Repay_97     Repay_98
# 0 0.03425187 0.003283544 0.0009013649 0.0005150657 0.0001931496 0.000000000 0.0002575328
# 1 0.27775291 0.038943599 0.0080572963 0.0020143241 0.0011190689 0.001119069 0.0017905103
# Bill_7   Bill_4 Payment_9 Payment_8 Payment_4
# 0 47685.61 39092.87  6306.659  6630.712  5792.977
# 1 45230.58 38133.16  3282.307  3264.247  3461.305
# 
# Coefficients of linear discriminants:
#   LD1
# Cred_Amnt  -1.462707e-06
# Gender2    -1.574946e-01
# Education1  6.965580e-01
# Education2  7.159993e-01
# Education3  7.410100e-01
# Married1    1.499651e-01
# Married2   -2.038142e-02
# Repay_91    1.185175e+00
# Repay_92    3.267238e+00
# Repay_93    3.652480e+00
# Repay_94    3.307964e+00
# Repay_95    2.166807e+00
# Repay_96    2.782393e+00
# Repay_97    5.061969e+00
# Repay_98    2.975444e+00
# Bill_7      5.781399e-07
# Bill_4     -2.477438e-07
# Payment_9  -3.494396e-06
# Payment_8  -1.860765e-06
# Payment_4  -1.047642e-06
plot(lda.fit1)

lda.pred=predict(lda.fit1,test)
names(lda.pred)
lda.class=lda.pred$class
Default.test=test$Default
table(lda.class,Default.test)

lda.pred=predict(lda.fit,test[,-24])
mean(lda.pred$class!=test[,24])
#           Default.test
# lda.class    0    1
          # 0 7492 1469
          # 1  340  699
# error rate = (340+1469)/(7492+1469+340+699)=0.1809
# 340/(340+7492)= 0.043
# 1569/(1468+1469)= 0.534
