#Problem 1: Fitting Model Between Proportion of bird-extinct and square km forest-area.
install.packages("readxl")
#Step 1: importing data
library(readxl)
bird<-read_excel('C://Users//probh//OneDrive//Desktop//New folder (4)//College notes//Paper 4 notes//birdextinct.xls')

#Step 2: Deciding variable under study
Y=bird$`area(km.sq.)`
x1=bird$`species at risk`
x2=bird$`# Species extinct`
X=x2/x1;
X # to find the proportion of birds that got extinct

#Step 3: Fitting the Model
model = lm(Y~X)
model

summary(model)

#Step 4: Checking the assumptions of the model through Diagnostic plot.
plot(model)

#Checking the normality using Shapiro-wilk test
#Ho: Residuals follow normal distribution
#H1: Residuals do not follow normal distribution
install.packages("lmtest")
library(lmtest)
shapiro.test(Y)

#Checking the homoscadasticity using Breusch-Pagan test
#Ho: Variance of residuals are homogenous
#H1: Variance of residuals are not homogenous

library(lmtest)
bptest(model)

# To achieve normality, we make log transformation to response variable (i.e. y transformation to log(y))
Y1 = log(Y);
Y1

model1 = lm(Y1~X); model1

# Step 4: Checking the assumptions of through Diagnostic Plot:
plot(model1)

# Obtain the estimates of parameter and checking of significance of individual regression co-efficients
summary(model1)
