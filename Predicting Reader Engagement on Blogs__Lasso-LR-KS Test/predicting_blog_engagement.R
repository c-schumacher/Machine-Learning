library(corrplot)
library(psych)
library(factoextra)
library(FactoMineR)
library(car)
library(glmnet)
library(vioplot)
library(fitdistrplus)
library(logspline)


#reading in the data and setting it up in a data frame
setwd('C://Users//Charlie//Desktop//Grad School Docs//Summer 2018//CSC 424//Final Project//BlogFeedback')
train <- read.csv("blogData_train+names.csv", header=T)

#storing the column names to back into later and changing to numbers for simplicity
#refcolnames <- colnames(train)
#colnames(train) <- c(1:281)

###################################
##### EXPLORATORY ANALYSIS ########
###################################

#correlation of days of week indicators with the target variable  
cor(train[,c(263:269, 281)])

#violin plot based on basetime day of week
#dev.new(width=8, height=2)
baseMon <- train[,281][train[,263]==1]
baseTues <- train[,281][train[,264]==1]
baseWed <- train[,281][train[,265]==1]
baseThur <- train[,281][train[,266]==1]
baseFri <- train[,281][train[,267]==1]
baseSat <- train[,281][train[,268]==1]
baseSun <- train[,281][train[,269]==1]
vioplot(baseMon, baseTues, baseWed, baseThur, baseFri, baseSat, baseSun, names=c('Monday','Tuesday','Wednesday',
                                                                                 'Thursday', 'Friday','Saturday',
                                                                                 'Sunday'),col='blue')
title(main="Number of Blog Comments - Crawl Basetime Weekday", xlab="Weekday", ylab="Number of Comments")

#violin plot based on original post publishment day of week
#dev.new(width=8, height=2)
postMon <- train[,281][train[,263]==1]
postTues <- train[,281][train[,264]==1]
postWed <- train[,281][train[,265]==1]
postThur <- train[,281][train[,266]==1]
postFri <- train[,281][train[,267]==1]
postSat <- train[,281][train[,268]==1]
postSun <- train[,281][train[,269]==1]
vioplot(postMon, postTues, postWed, postThur, postFri, postSat, postSun, names=c('Monday','Tuesday','Wednesday',
                                                                                 'Thursday', 'Friday','Saturday',
                                                                                 'Sunday'), col='blue')
title(main="Number of Blog Comments - Post Published Weekday", xlab="Weekday", ylab="Number of Comments")

#General plots of the total comments 
boxplot(train[,281])
vioplot(train[,281])  #most of the data points appear to have values under 400
head(train[,261:263])

#making new dataframes based on target variables under 750 
trainClean <- train[train[,281] < 750,] 
#setting up three predictor variable sets and the target variable set
x_trainClean <- trainClean[,c(1:62,277:280)]  #removing bag of words and days of week
xtrainClean2 <- x_trainClean[,colSums(x_trainClean) != 0]  #removing bag of words, day of week, and 0-sum cols
xtrainClean3 <- trainClean[,c(1:262, 277:280)][, colSums(trainClean[,c(1:262, 277:280)]) != 0] #removing only day of week and 0-sum cols
y_train <- trainClean[,281]
x_train <- trainClean[,1:280]

###histograms
#log log histogram of the distribution of target variable (total comments)
hist(log(log(trainClean[,281]+1)), xlim=c(-1,2), main='Histogram of Log(Log(Uncleaned Comment Totals + 1)', xlab='Comments', col='blue')
#histogram of the untransformed target variable with a comment threshold of 750
hist(trainClean[,281],main='Comment Totals < 10 (91% of Data)', xlab='Comments', col='blue')
table(train[,281] < 10)

#five number summary of the target variable
summary(trainClean[,281])

################################################################
##### Checking potential distributions this could come from ####
################################################################

#function for scaling the distribution between 0 and 1
stdscale <- function(dist){
  (dist - min(dist))/(max(dist)-min(dist))
}
#function for scaling distirbution between 0 and 1 but avoiding 0 and 1 exactly
stdscaleplus <- function(dist){
  (dist - min(dist)+0.001)/(max(dist)-min(dist)+0.002)
}

#distribution checks
descdist(trainClean[,281], boot=500, discrete=F)  #this appears to almost certainly come from a beta distribution

gammafit <- fitdist(stdscaleplus(trainClean[,281]), "gamma")
gammafit
plot(gammafit)

gammatransfit <- fitdist(stdscaleplus(log(trainClean[,281]+1)), "gamma")
gammatransfit
plot(gammatransfit)

betafit <- fitdist(stdscaleplus(trainClean[,281]), "beta")  
betafit
plot(betafit)

betatransfit <- fitdist(stdscaleplus(log(trainClean[,281]+1.75)), pbeta)  #this may fit best so far, with exception of P-P
betatransfit
plot(betatransfit)

logfit <- fitdist(log(trainClean[,281]+1), plogis)
logfit
plot(logfit)

logtransfit <- fitdist(log(trainClean[,281]+1), plogis)  
logtransfit
plot(logtransfit)

poissfit <- fitdist(trainClean[,281], ppois)
poissfit
plot(poissfit)

lognormfit <- fitdist(stdscaleplus(trainClean[,281]), plnorm)
lognormfit
plot(lognormfit)

lognormtransfit <- fitdist(stdscaleplus(log(trainClean[,281]+1)), plnorm)
lognormtransfit
plot(lognormtransfit)

normfit <- fitdist(stdscaleplus(trainClean[,281]), pnorm)
normfit
plot(lognormfit)

normtransfit <- fitdist(stdscaleplus(log(trainClean[,281]+1)), pnorm)
normtransfit
plot(normtransfit)

expfit <- fitdist(trainClean[,281], pexp)
expfit
plot(expfit)

exptransfit <- fitdist(log(trainClean[,281]+1.75), pexp) #this appears to matches quantiles better at the tail
exptransfit
plot(exptransfit)
  
####################################  
####### KS Tests Exploration #######
####################################

#finding the necessary threshold for critical value of the KS Test
1.36/sqrt(nrow(trainClean)) #threshold of 0.005

#higher level function for generating plots of ks tests and estimating optimal parameters
kspois <- function(targetvector, lambda_llim, lambda_ulim, lambda_step){
  dscores <- vector()
  pscores <- vector()
  count <- 1
  for(i in seq(lambda_llim, lambda_ulim, by=lambda_step)){
    dscores[count] <- ks.test(targetvector, rpois(length(targetvector), lambda=i))$statistic
    pscores[count] <- ks.test(targetvector, rpois(length(targetvector), lambda=i))$p.value
    count <- count+1
  }
  plot(unlist(dscores), xlab="Scores Index", ylab="D-Value", main="KS Test: Target to Poisson Distribution")
  
  #returns the minimum score D-value, p-value, and the lambda value which produced it
  return(c(min(dscores), 
           pscores[which(dscores == min(dscores))], 
           lambda_llim + (which(dscores==min(dscores))-1)*lambda_step))
}

ksbeta <- function(targetvector, alpha_llim, alpha_ulim, alpha_step, beta_llim, beta_ulim, beta_step){
  #dscores <- vector()
  #pscores <- vector()
  scores <- matrix(nrow=0, ncol=4)
  count <- 1
  for(alph in seq(alpha_llim, alpha_ulim, by=alpha_step)){
    for(bta in seq(beta_llim, beta_ulim, by=beta_step)){
      #dscores[count] <- ks.test(targetvector, rbeta(length(targetvector), alph, bta))$statistic
      #pscores[count] <- ks.test(targetvector, rbeta(length(targetvector), alph, bta))$p.value
      scores <- rbind(scores, c(ks.test(targetvector, rbeta(length(targetvector), alph, bta))$statistic, 
                                ks.test(targetvector, rbeta(length(targetvector), alph, bta))$p.value,
                                alph,
                                bta))
      count <- count+1
    }
  }
  plot(unlist(scores[,1]), xlab="Scores Index", ylab="D-Value", main="KS Test: Target to Beta Distribution")
  
  #returns the minimum score D-value, p-value, and the alpha and beta values that produced it
  return(c(min(scores[,1]), 
           scores[,2][which(scores[,1] == min(scores[,1]))], 
           scores[,3][which(scores[,1] == min(scores[,1]))],
           scores[,4][which(scores[,1] == min(scores[,1]))]))
}
alphaEst <- function(dist){
  return((((1-mean(dist))/var(dist)) - 1/mean(dist)) * (mean(dist))^2)
}
betaEst <- function(dist){
  return(alphaEst(dist) * ((1/mean(dist)) - 1))
}

scaleTarget <- stdscaleplus(log(trainClean[,281]+1))

#### ks test: log transformed with BETA ######
#testing with the scaled targt and using the mean and variance of distribution
ks.test(scaleTarget, rbeta(length(scaleTarget), shape1=alphaEst(scaleTarget), shape2=betaEst(scaleTarget)))

#searching a grid of alpha and beta parameters with the log transformed and scaled version of the target
beta_res <- ksbeta(scaleTarget, 0.01, 0.5, 0.005, 0.25, 1.5, 0.05)
beta_res
beta_res[1] #minimum D-value of 0.3237624...
beta_res[2] #...at p-value very close to 0
beta_res[3] #...produced by alpha = 0.13
beta_res[4] #...and beta 1.0

##### ks test: UNTRANSFORMED with POISSON ######
#with lambda as the mean of the target dist:
ks.test(trainClean[,281], rpois(length(trainClean[,281]), lambda=mean(trainClean[,281]))) #D = 0.76205

#testing range of values, with target dist untransformed:
poiss_test1 <- kspois(trainClean[,281], 0.01, 1, 0.01)
poiss_test1[1] #D-value=0.15885...                            ###this looks like it fits decently by KS
poiss_test1[2] #... with p-value close to 0...
poiss_test1[3] #... at lambda = 0.71


##### ks test :LOG TRANSFORMED with POISSON #####
#with lambda as mean of the log(target dist+1), keeping input distribution untransformed
ks.test(trainClean[,281], rpois(length(trainClean[,281]), lambda=mean(log(trainClean[,281]+1)))) #D =  0.168

#with lambda as mean of the log(target dist+1), and input distribution as log(target dist+1)
ks.test(log(trainClean[,281]+1), rpois(length(trainClean[,281]), lambda=mean(log(trainClean[,281]+1)))) #D =  0.222

#testing range of values, with target dist log tranformed:
poiss_test2 <- kspois(log(trainClean[,281]+1), 0.01, 1, 0.01)
poiss_test2[1] #D-value=0.155648...
poiss_test2[2] #... with p-value close to 0...
poiss_test2[3] #... at lambda = 0.52


##### ks test: UNTRANSFORMED with BETA ######
#with lambda as the mean of the target dist:
ks.test(trainClean[,281], rpois(length(trainClean[,281]), lambda=mean(trainClean[,281]))) #D = 0.76205

#testing range of values, with target dist untransformed:
poiss_test1 <- kspois(trainClean[,281], 0.01, 1, 0.01)
poiss_test1[1] #D-value=0.15885...
poiss_test1[2] #... with p-value close to 0...
poiss_test1[3] #... at lambda = 0.71


##### ks test :LOG TRANSFORMED with BETA #####
#with lambda as mean of the log(target dist+1), keeping input distribution untransformed
ks.test(trainClean[,281], rpois(length(trainClean[,281]), lambda=mean(log(trainClean[,281]+1)))) #D =  0.168

#with lambda as mean of the log(target dist+1), and input distribution as log(target dist+1)
ks.test(log(trainClean[,281]+1), rpois(length(trainClean[,281]), lambda=mean(log(trainClean[,281]+1)))) #D =  0.222

#testing range of values, with target dist log tranformed:
poiss_test2 <- kspois(log(trainClean[,281]+1), 0.01, 1, 0.01)
poiss_test2[1] #D-value=0.155648...
poiss_test2[2] #... with p-value close to 0...
poiss_test2[3] #... at lambda = 0.52


####################################################################
####### BUILDING MODELS FOR PREDICTION - MULTIPLE REGRESSION #######
####################################################################
#note: all of these perform better without the intercept included
#function to isolate the variables with p-vals < 0.05 and adjust names
isoVars = function(fit){
  temp <- matrix(names(summary(fit)$coefficients[,4][summary(fit)$coefficients[,4] < 0.05])) 
  #cleaning up the variable names
  #for(i in 2:length(temp)){
  #  temp[i,1] <- substring(temp[i,1], 24)
  #}
  temp <- as.matrix(temp[1:length(temp),1])  
  return(temp)
}
isoVars2 = function(fit){
  temp <- matrix(names(summary(fit)$coefficients[,4][summary(fit)$coefficients[,4] < 0.05])) 
  #cleaning up the variable names
  #for(i in 2:length(temp)){
  #  temp[i,1] <- substring(temp[i,1], 19)
  #}
  temp <- as.matrix(temp[1:length(temp),1])
  return(temp)
}

### testing MANUAL model building 
#testing with no adjustments made, using all of the 280 predictor variables
MRfit0 <- lm(y_train~.-1, data=x_train)
summary(MRfit0)  #R2 of .4027 and F-stat of 141

MRfit0.1 <- lm(log(y_train+1) ~ .-1, data=x_train)
summary(MRfit0.1) #R2 of 0.6018 and F-stat of 316

#testing with day of week and bag of words removed:
MRfit1 <- lm(y_train ~ .-1, data=x_trainClean)  ####R2 of 0.3959 and F-statistic of 591.2
summary(MRfit1) 

xtrainCleanA <- subset(x_trainClean, select=isoVars(MRfit1))
MRfit1.1 <- lm(y_train ~ .-1, data=xtrainCleanA)          
summary(MRfit1.1)  #worse R2 at .3814 but better F-statistic at 1899

xtrainCleanB <- subset(xtrainCleanA, select=isoVars(MRfit1.1))
MRfit1.2 <- lm(y_train ~ .-1, data=xtrainCleanB)
summary(MRfit1.2)  #slightly wore R2 at 0.3814 but better F-statistic at 2018


### testing MANUAL model building starting with features - days of week and zero-sum cols
MRfit2 <- lm(y_train ~ .-1, data=xtrainClean3)  ####R2 of 0.4005 with F-stat 147 
summary(MRfit2) 

xtrainCleanC <- subset(xtrainClean3, select=isoVars(MRfit2))
MRfit2.1 <- lm(y_train ~ .-1, data=xtrainCleanC)          
summary(MRfit2.1)  #worse R2 of 0.3844 but better F-statistic fit at 778.2

xtrainCleanD <- subset(xtrainCleanC, select=isoVars(MRfit2.1))
MRfit2.2 <- lm(y_train ~ .-1, data=xtrainCleanD)
summary(MRfit2.2)  #slightly worse R2 of 0.3838 but better F-statistic at 1124


####### REPEATING THE MANUAL MODELS WITH LOG(TARGET+1) ##########
### testing MANUAL model building starting with features - bag of words - days of week.
MRfit3 <- lm(log(y_train+1) ~ .-1, data=x_trainClean)  
summary(MRfit3) ####R2 of 0.5613 and F-statistic of 1154!

xtrainCleanE <- subset(x_trainClean, select=isoVars(MRfit3))
MRfit3.1 <- lm(log(y_train+1) ~ .-1, data=xtrainCleanE)          
summary(MRfit3.1)  #worse R2 of .5611 but better F-statistic fit at 1487

xtrainCleanF <- subset(xtrainCleanE, select=isoVars(MRfit3.1))
MRfit3.2 <- lm(log(y_train+1) ~ .-1, data=xtrainCleanF)
summary(MRfit3.2)  #same R2 at 0.5611 but better F-statistic of 1520! 

### testing MANUAL model building starting with features - days of week and zero-sum cols
MRfit4 <- lm(log(y_train+1) ~ . -1, data=xtrainClean3)  
summary(MRfit4) ####R2 of .5724 with F-stat 295.7 

xtrainCleanG <- subset(xtrainClean3, select=isoVars(MRfit4))
MRfit4.1 <- lm(log(y_train+1) ~ .-1, data=xtrainCleanG)          
summary(MRfit4.1)  #slightly worse R2 of 0.5706 but better F-statistic fit at 756

xtrainCleanH <- subset(xtrainCleanG, select=isoVars(MRfit4.1))
MRfit4.2 <- lm(log(y_train+1) ~ .-1, data=xtrainCleanH)
summary(MRfit4.2)  #slightly better R2 of 0.5707 and better F-statistic at 799.2

### testing MANUAL model building starting with ALL 280 features
MRfit5 <- lm(log(y_train+1) ~ . -1, data=x_train)  
summary(MRfit5) ####R2 of .6015 with F-stat 316.4 

xtrainCleanI <- subset(x_train, select=isoVars2(MRfit5))
MRfit5.1 <- lm(log(y_train+1) ~ .-1, data=xtrainCleanI)          
summary(MRfit5.1)  #slightly worse R2 of .6002 but better F-statistic fit at 826.2

xtrainCleanJ <- subset(xtrainCleanI, select=isoVars(MRfit5.1))
MRfit5.2 <- lm(log(y_train+1) ~ .-1, data=xtrainCleanJ)
summary(MRfit5.2)  #slightly worse R2 of .6001 but better F-statistic at 871.8

####################################################################
####### BUILDING MODELS FOR PREDICTION - LASSO REGRESSION ##########
####################################################################
cv.fit1 <- cv.glmnet(as.matrix(x_train), y_train, alpha=1, nfolds=25)
cv.fit2 <- cv.glmnet(as.matrix(x_trainClean), y_train, alpha=1, nfolds=25)
cv.fit3 <- cv.glmnet(as.matrix(xtrainClean3), y_train, alpha=1, nfolds=25)
cv.fit4 <- cv.glmnet(as.matrix(x_train), log(y_train+1), alpha=1, nfolds=25)
cv.fit5 <- cv.glmnet(as.matrix(x_trainClean), log(y_train+1), alpha=1, nfolds=25)
cv.fit6 <- cv.glmnet(as.matrix(xtrainClean3), log(y_train+1), alpha=1, nfolds=25)

#testing with full set of features and cross-fold validation
LRfit1= glmnet(as.matrix(x_train), y_train, alpha=1, lambda = cv.fit1$lambda.min)
plot(LRfit1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit1$dev.ratio  # R2 = 0.3763
LRfit1$df  #72 df
LRfit1$beta

#testing without bag of words or day of week
LRfit2= glmnet(as.matrix(x_trainClean), y_train, alpha=1, lambda = cv.fit2$lambda.min)
plot(LRfit2, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit2, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit2$dev.ratio  # R2 = 0.3753
LRfit2$df   #51 df
LRfit2$beta

#testing without day of week only
LRfit3= glmnet(as.matrix(xtrainClean3), y_train, alpha=1, lambda = cv.fit3$lambda.min)
plot(LRfit3, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit3, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit3$dev.ratio  # R2 = 0.3762
LRfit3$df   #77 df
LRfit3$beta

#repeating the above, but with log(target+1) transformed variables........

#testing with full set of features and cross-fold validation, and log(target+1)
LRfit4= glmnet(as.matrix(x_train), log(y_train+1), alpha=1, lambda = cv.fit4$lambda.min)
plot(LRfit4, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit4, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit4$dev.ratio  # R2 = 0.4704
LRfit4$df   #254 df
LRfit4$beta

#testing without bag of words or day of week
LRfit5= glmnet(as.matrix(x_trainClean), log(y_train+1), alpha=1, lambda = cv.fit5$lambda.min)
plot(LRfit5, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit5, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit5$dev.ratio  # R2 = 0.4598
LRfit5$df   #48 df
LRfit5$beta

#testing without day of week only
LRfit6= glmnet(as.matrix(xtrainClean3), log(y_train+1), alpha=1, lambda = cv.fit6$lambda.min)
plot(LRfit6, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit6, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit6$dev.ratio  # R2 = 0.4677
LRfit6$df   #242 df
LRfit6$beta

########## REPEATING THE ABOVE, USING WITHIN 1 STD DEV FOR LESS VARIABLES ############

#testing with full set of features and cross-fold validation
LRfit1.1 = glmnet(as.matrix(x_train), y_train, alpha=1, lambda = cv.fit1$lambda.1se)
plot(LRfit1.1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit1.1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit1.1$dev.ratio  # R2 = 0.3376
LRfit1.1$df  #5 df
LRfit1.1$beta

#testing without bag of words or day of week
LRfit2.1 = glmnet(as.matrix(x_trainClean), y_train, alpha=1, lambda = cv.fit2$lambda.1se)
plot(LRfit2.1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit2.1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit2.1$dev.ratio  # R2 = 0.3467
LRfit2.1$df  #6 df
LRfit2.1$beta

#testing full data set of variables
LRfit3.1 = glmnet(as.matrix(xtrainClean3), y_train, alpha=1, lambda = cv.fit3$lambda.1se)
plot(LRfit3.1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit3.1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit3.1$dev.ratio  # R2 = 0.35021 
LRfit3.1$df  #6 df
LRfit3.1$beta

#repeating the above, but with log(target+1) transformed variables........

#testing with full set of features and cross-fold validation, and log(target+1)
LRfit4.1= glmnet(as.matrix(x_train), log(y_train+1), alpha=1, lambda = cv.fit4$lambda.1se)
plot(LRfit4.1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit4.1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit4.1$dev.ratio  # R2 = 0.463
LRfit4.1$df  #212 df
LRfit4.1$beta

#testing without bag of words or day of week
LRfit5.1= glmnet(as.matrix(x_trainClean), log(y_train+1), alpha=1, lambda = cv.fit5$lambda.1se)
plot(LRfit5.1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit5.1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit5.1$dev.ratio  # R2 = 0.4539
LRfit5.1$df  #37 df
LRfit5.1$beta

#testing without day of week only
LRfit6.1= glmnet(as.matrix(xtrainClean3), log(y_train+1), alpha=1, lambda = cv.fit6$lambda.1se)
plot(LRfit6.1, xvar='lambda',label=T) #plots the log of lambda on the bottom x-axis
plot(LRfit6.1, xvar='dev',label=T) #plots the % of deviance on the bottom x-axis
LRfit6.1$dev.ratio  # R2 = 0.4599
LRfit6.1$df   #df = 200
LRfit6.1$beta

##################################################################
############ EVALUATING MODELS WITH TEST SETS ####################
##################################################################

#functions for computing the RSE
computeRSE <- function(fit, x_test, y_test){
  c <- as.matrix(coef(fit))
  yHat <- predict(fit, x_test)
  dof <- length(y_test) - length(c)
  rsePredict = sqrt(sum((y_test - yHat)^2) / abs(dof))
  return (rsePredict)
}
computeLassoRSE <- function(fit, x_test, y_test){
  c <- as.matrix(coef(fit))
  #yHat <- predict(fit, as.matrix(x_test[,1:ncol(x_test)]))
  yHat <- predict(fit, as.matrix(x_test))
  dof <- length(y_test) - length(c)
  rsePredict = sqrt(sum((y_test - yHat)^2) / abs(dof))
  return (rsePredict)
}
computeRevTransRSE <- function(fit, x_test, y_test){
  c <- as.matrix(coef(fit))
  yHat <- exp(predict(fit, x_test))-1
  dof <- length(y_test) - length(c)
  rsePredict = sqrt(sum((y_test - yHat)^2) / abs(dof))
  return (rsePredict)
}
computeLassoRevTransRSE <- function(fit, x_test, y_test){
  c <- as.matrix(coef(fit))
  yHat <- exp(predict(fit, as.matrix(x_test)))-1
  dof <- length(y_test) - length(c)
  rsePredict = sqrt(sum((y_test - yHat)^2) / abs(dof))
  return (rsePredict)
}
# RMSE Functions
computeRMSE <- function(fit, x_test, y_test){
  yHat <- predict(fit, x_test)
  n <- length(y_test)
  rmsePredict = sqrt(sum((y_test - yHat)^2) / n)
  return (rmsePredict)
} 
length(y_test1)
computeRevTransRMSE <- function(fit, x_test, y_test){
  yHat <- exp(predict(fit, x_test))-1
  n <- length(y_test)
  rmsePredict = sqrt(sum((y_test - yHat)^2) / n)
  return (rmsePredict)
} 
computeLassoRMSE <- function(fit, x_test, y_test){
  yHat <- predict(fit, as.matrix(x_test))
  n <- length(y_test)
  rmsePredict = sqrt(sum((y_test - yHat)^2) / n)
  return (rmsePredict)
}
computeLassoRevTransRMSE <- function(fit, x_test, y_test){
  yHat <- exp(predict(fit, as.matrix(x_test)))-1
  n <- length(y_test)
  rmsePredict = sqrt(sum((y_test - yHat)^2) / n)
  return (rmsePredict)
}
# MSE functions
computeMSE <- function(fit, x_test, y_test){
  yHat <- predict(fit, x_test)
  rmsePredict = sum((y_test - yHat)^2)/length(y_test)
  return (rmsePredict)
} 
computeRevTransMSE <- function(fit, x_test, y_test){
  yHat <- exp(predict(fit, x_test))-1
  rmsePredict = sum((y_test - yHat)^2)/length(y_test)
  return (rmsePredict)
} 
computeLassoMSE <- function(fit, x_test, y_test){
  yHat <- predict(fit, as.matrix(x_test))
  rmsePredict = sum((y_test - yHat)^2)/length(y_test)
  return (rmsePredict)
}
computeLassoRevTransMSE <- function(fit, x_test, y_test){
  yHat <- exp(predict(fit, as.matrix(x_test)))-1
  rmsePredict = sum((y_test - yHat)^2)/length(y_test)
  return (rmsePredict)
}

table(MRfit0$coefficients >25)
table(as.matrix(LRfit1.1$beta) > 0.1)
as.matrix(LRfit1.1$beta)
sort(as.matrix(MRfit0$coefficients))
MRfit0$coefficients[MRfit0$coefficients > 100]

#setting up the list of test files
fpath <- 'PATH'
files <- list.files(path=fpath, pattern="*.csv", full.names=T,recursive=T)
RMSEresults <- matrix(nrow=0,ncol=29)
RSEresults <- matrix(nrow=0,ncol=29)
MSEresults <- matrix(nrow=0, ncol=29)

for(i in 1:length(files)){
  temp <- read.csv(files[i], col.names=colnames(train))
  ytemp <- temp[,281]
  xtemp <- temp[,1:280]
  xtempClean <- xtemp[,c(1:62,277:280)]  
  xtempClean3 <- xtemp[,c(1:262, 277:280)][ ,colnames(xtemp[,c(1:262, 277:280)]) %in% colnames(xtrainClean3)] 
  xtempCleanA <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanA)] 
  xtempCleanB <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanB)] 
  xtempCleanC <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanC)] 
  xtempCleanD <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanD)] 
  xtempCleanE <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanE)] 
  xtempCleanF <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanF)] 
  xtempCleanG <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanG)] 
  xtempCleanH <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanH)] 
  xtempCleanI <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanI)] 
  xtempCleanJ <- xtemp[ ,colnames(xtemp) %in% colnames(xtrainCleanJ)] 

  RMSEresults <- rbind(RMSEresults, c(computeRMSE(MRfit0, xtemp, ytemp),
                                    computeRevTransRMSE(MRfit0.1, xtemp, ytemp),
                                    computeRMSE(MRfit1, xtempClean, ytemp),
                                    computeRMSE(MRfit1.1, xtempCleanA, ytemp),
                                    computeRMSE(MRfit1.2, xtempCleanB, ytemp),
                                    computeRMSE(MRfit2, xtempClean3, ytemp),
                                    computeRMSE(MRfit2.1, xtempCleanC, ytemp),
                                    computeRMSE(MRfit2.2, xtempCleanD, ytemp),
                                    computeRevTransRMSE(MRfit3, xtempClean, ytemp),
                                    computeRevTransRMSE(MRfit3.1, xtempCleanE, ytemp),
                                    computeRevTransRMSE(MRfit3.2, xtempCleanF, ytemp),
                                    computeRevTransRMSE(MRfit4, xtempClean3, ytemp),
                                    computeRevTransRMSE(MRfit4.1, xtempCleanG, ytemp),
                                    computeRevTransRMSE(MRfit4.2, xtempCleanH, ytemp),
                                    computeRevTransRMSE(MRfit5, xtemp, ytemp),
                                    computeRevTransRMSE(MRfit5.1, xtempCleanI, ytemp),
                                    computeRevTransRMSE(MRfit5.2, xtempCleanJ, ytemp),
                                    computeLassoRMSE(LRfit1, xtemp, ytemp),
                                    computeLassoRMSE(LRfit1.1, xtemp, ytemp),
                                    computeLassoRMSE(LRfit2, xtempClean, ytemp),
                                    computeLassoRMSE(LRfit2.1, xtempClean, ytemp),
                                    computeLassoRMSE(LRfit3, xtempClean3, ytemp),
                                    computeLassoRMSE(LRfit3.1, xtempClean3, ytemp),
                                    computeLassoRevTransRMSE(LRfit4, xtemp, ytemp),
                                    computeLassoRevTransRMSE(LRfit4.1, xtemp, ytemp),
                                    computeLassoRevTransRMSE(LRfit5, xtempClean, ytemp),
                                    computeLassoRevTransRMSE(LRfit5.1, xtempClean, ytemp),
                                    computeLassoRevTransRMSE(LRfit6, xtempClean3, ytemp),
                                    computeLassoRevTransRMSE(LRfit6.1, xtempClean3, ytemp)
                                    ))
  RSEresults <- rbind(RSEresults, c(computeRSE(MRfit0, xtemp, ytemp),
                                      computeRevTransRSE(MRfit0.1, xtemp, ytemp),
                                      computeRSE(MRfit1, xtempClean, ytemp),
                                      computeRSE(MRfit1.1, xtempCleanA, ytemp),
                                      computeRSE(MRfit1.2, xtempCleanB, ytemp),
                                      computeRSE(MRfit2, xtempClean3, ytemp),
                                      computeRSE(MRfit2.1, xtempCleanC, ytemp),
                                      computeRSE(MRfit2.2, xtempCleanD, ytemp),
                                      computeRevTransRSE(MRfit3, xtempClean, ytemp),
                                      computeRevTransRSE(MRfit3.1, xtempCleanE, ytemp),
                                      computeRevTransRSE(MRfit3.2, xtempCleanF, ytemp),
                                      computeRevTransRSE(MRfit4, xtempClean3, ytemp),
                                      computeRevTransRSE(MRfit4.1, xtempCleanG, ytemp),
                                      computeRevTransRSE(MRfit4.2, xtempCleanH, ytemp),
                                      computeRevTransRSE(MRfit5, xtemp, ytemp),
                                      computeRevTransRSE(MRfit5.1, xtempCleanI, ytemp),
                                      computeRevTransRSE(MRfit5.2, xtempCleanJ, ytemp),
                                      computeLassoRSE(LRfit1, xtemp, ytemp),
                                      computeLassoRSE(LRfit1.1, xtemp, ytemp),
                                      computeLassoRSE(LRfit2, xtempClean, ytemp),
                                      computeLassoRSE(LRfit2.1, xtempClean, ytemp),
                                      computeLassoRSE(LRfit3, xtempClean3, ytemp),
                                      computeLassoRSE(LRfit3.1, xtempClean3, ytemp),
                                      computeLassoRevTransRSE(LRfit4, xtemp, ytemp),
                                      computeLassoRevTransRSE(LRfit4.1, xtemp, ytemp),
                                      computeLassoRevTransRSE(LRfit5, xtempClean, ytemp),
                                      computeLassoRevTransRSE(LRfit5.1, xtempClean, ytemp),
                                      computeLassoRevTransRSE(LRfit6, xtempClean3, ytemp),
                                      computeLassoRevTransRSE(LRfit6.1, xtempClean3, ytemp)
                                    ))
  MSEresults <- rbind(MSEresults, c(computeMSE(MRfit0, xtemp, ytemp),
                                    computeRevTransMSE(MRfit0.1, xtemp, ytemp),
                                    computeMSE(MRfit1, xtempClean, ytemp),
                                    computeMSE(MRfit1.1, xtempCleanA, ytemp),
                                    computeMSE(MRfit1.2, xtempCleanB, ytemp),
                                    computeMSE(MRfit2, xtempClean3, ytemp),
                                    computeMSE(MRfit2.1, xtempCleanC, ytemp),
                                    computeMSE(MRfit2.2, xtempCleanD, ytemp),
                                    computeRevTransMSE(MRfit3, xtempClean, ytemp),
                                    computeRevTransMSE(MRfit3.1, xtempCleanE, ytemp),
                                    computeRevTransMSE(MRfit3.2, xtempCleanF, ytemp),
                                    computeRevTransMSE(MRfit4, xtempClean3, ytemp),
                                    computeRevTransMSE(MRfit4.1, xtempCleanG, ytemp),
                                    computeRevTransMSE(MRfit4.2, xtempCleanH, ytemp),
                                    computeRevTransMSE(MRfit5, xtemp, ytemp),
                                    computeRevTransMSE(MRfit5.1, xtempCleanI, ytemp),
                                    computeRevTransMSE(MRfit5.2, xtempCleanJ, ytemp),
                                    computeLassoMSE(LRfit1, xtemp, ytemp),
                                    computeLassoMSE(LRfit1.1, xtemp, ytemp),
                                    computeLassoMSE(LRfit2, xtempClean, ytemp),
                                    computeLassoMSE(LRfit2.1, xtempClean, ytemp),
                                    computeLassoMSE(LRfit3, xtempClean3, ytemp),
                                    computeLassoMSE(LRfit3.1, xtempClean3, ytemp),
                                    computeLassoRevTransMSE(LRfit4, xtemp, ytemp),
                                    computeLassoRevTransMSE(LRfit4.1, xtemp, ytemp),
                                    computeLassoRevTransMSE(LRfit5, xtempClean, ytemp),
                                    computeLassoRevTransMSE(LRfit5.1, xtempClean, ytemp),
                                    computeLassoRevTransMSE(LRfit6, xtempClean3, ytemp),
                                    computeLassoRevTransMSE(LRfit6.1, xtempClean3, ytemp)
  ))
}

# setwd("C:/Users/Charlie/Desktop/Grad School Docs/Summer 2018/CSC 424/Final Project/BlogFeedback/Test Files")
# test2 <- read.csv("blogData_test-2012.03.31.01_00.csv", header=F, col.names = colnames(train))
# y_test2 <- test2[,281]
# x_test2 <- test2[,1:280]
# hist(y_test2)
# points(predict(LRfit1.1, as.matrix(x_test2)), col='red')

#bfittest <- betareg(stdscaleplus(y_test2)~.,data=x_test2[,c(2,3,5,7,17,42,51,54,56,60)]) 
#summary(bfittest)

#some analysis and violin plots of the RSE for the models based on all of their performance
# RMSEresults
mod1 <- RMSEresults[,1]
mod2 <- RMSEresults[,2]
mod3 <- RMSEresults[,3]
mod4 <- RMSEresults[,4]
mod5 <- RMSEresults[,5]
mod6 <- RMSEresults[,6]
mod7 <- RMSEresults[,7]
mod8 <- RMSEresults[,8]
mod9 <- RMSEresults[,9]
mod10 <- RMSEresults[,10]
mod11 <- RMSEresults[,11]
mod12 <- RMSEresults[,12]
mod13 <- RMSEresults[,13]
mod14 <- RMSEresults[,14]
mod15 <- RMSEresults[,15]
mod16 <- RMSEresults[,16]
mod17 <- RMSEresults[,17]
mod18 <- RMSEresults[,18]
mod19 <- RMSEresults[,19]
mod20 <- RMSEresults[,20]
mod21 <- RMSEresults[,21]
mod22 <- RMSEresults[,22]
mod23 <- RMSEresults[,23]
mod24 <- RMSEresults[,24]
mod25 <- RMSEresults[,25]
mod26 <- RMSEresults[,26]
mod27 <- RMSEresults[,27]
mod28 <- RMSEresults[,28]
mod29 <- RMSEresults[,29]
modlist <- paste0("mod",c(1:29))


setwd("C:/Users/Charlie/Desktop/Grad School Docs/Summer 2018/CSC 424/Final Project/BlogFeedback")

#creating a table of the performance values
restable <- read.csv('resultstable.csv',header=F)
smrytable <- matrix(nrow=29, ncol=7)
for(i in 1:nrow(smrytable)){
    smrytable[i, 1] <- summary(get(modlist[i]))[1]
    smrytable[i, 2] <- summary(get(modlist[i]))[2]
    smrytable[i, 3] <- summary(get(modlist[i]))[3]
    smrytable[i, 4] <- summary(get(modlist[i]))[4]
    smrytable[i, 5] <- summary(get(modlist[i]))[5]
    smrytable[i, 6] <- summary(get(modlist[i]))[6]
}
modlist2 <- c(paste0("MRfit",c(0,0.1,1,1.1,1.2,2,2.1,2.2,3,3.1,3.2,4,4.1,4.2,5,5.1,5.2)),
              paste0("LRfit",c(1,1.1,2,2.1,3,3.1,4,4.1,5,5.1,6,6.1)))
for(i in 1:length(modlist2)){
  temp <- get(modlist2[i])
  if(i < 18){
    smrytable[i,7] <- length(coef(temp))
  }
  else{
    smrytable[i,7] <- temp$df
  }
}

colnames(restable) <- c("Model Name","Algorithm","X Set","Y Set")
colnames(smrytable) <- c("Min RMSE","1st Q. RMSE","Median RMSE","Mean RMSE","3rd Q. RMSE","Max RMSE","Num Model Vars")
restable <- cbind(restable, smrytable)
head(restable)
write.csv(restable, file="blogfeedback_RMSE_results25.csv")


#finding the best model base on mean of RMSE
bestMean = summary(mod1)[4]
bestMeanIdx = 1
bestMedian = summary(mod1)[3]
bestMedianIdx = 1
bestMax = summary(mod1)[5]
bestMaxIdx = 1
for(i in 2:length(modlist)){
  if(summary(get(modlist[i]))[4] < bestMean){
    bestMean <- summary(get(modlist[i]))[4]
    bestMeanIdx <- i
    }
  if(summary(get(modlist[i]))[3] < bestMedian){
    bestMedian <- summary(get(modlist[i]))[3]
    bestMedianIdx <- i
  }
  if(summary(get(modlist[i]))[6] < bestMax){
    bestMax <- summary(get(modlist[i]))[6]
    bestMaxIdx <- i
  }
}

bestMean  # mean of 0.7434033
bestMeanIdx  # model 25, the Lasso Regression on the full set of predictors
bestMedian  #median of 0.7153, although model 25 is close behind at 0.7174
bestMedianIdx  # model 24, the multiple regression on the full set of predictor with log(target+1)
bestMax     #max RSE on testing sets of 1.53
bestMaxIdx   #model 11, the manually tuned model 3.2 with log(target+1)


#summary(exp(mod27)-1)
### violin plots of the untransformed models ###
#dev.new(width=8, height=2)
vioplot(mod1, mod3, mod4, mod5, mod6, mod7, mod8, mod18, mod19, mod20, mod21, mod22, mod23,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','MR7','LR1','LR2','LR3','LR4','LR5','LR6'), col='blue')
title("RMSE : Non-Transformed Regression Models")


### violin plots of the log(target+1) transformed models ###
#dev.new(width=8, height=2)
#loglist <- paste0('mod',c(2,9,10,11,12,15,16,17,24,25,26,27,28,29))
vioplot(mod2, mod9, mod10, mod11, mod12, mod15, mod16, mod17, mod24, mod25, mod26, mod27, mod28, mod29,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','MR7','MR8','LR1','LR2','LR3','LR4','LR5','LR6'), 
        col='blue')
title("RMSE : Log Transformed Models")

# violin plots of the reverse transformed models
vioplot(exp(mod2)-1, exp(mod9)-1, exp(mod10)-1, exp(mod11)-1, exp(mod12)-1, exp(mod15)-1, 
        exp(mod24)-1, exp(mod25)-1, exp(mod26)-1, exp(mod27)-1, exp(mod28)-1, exp(mod29)-1,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','LR1','LR2','LR3','LR4','LR5','LR6'), 
        col='blue')
title("RMSE : Log Transformed Models (Reverse Transformed)")


#### RSE Analysis version of above
#RMSEresults
mod1 <- RSEresults[,1]
mod2 <- RSEresults[,2]
mod3 <- RSEresults[,3]
mod4 <- RSEresults[,4]
mod5 <- RSEresults[,5]
mod6 <- RSEresults[,6]
mod7 <- RSEresults[,7]
mod8 <- RSEresults[,8]
mod9 <- RSEresults[,9]
mod10 <- RSEresults[,10]
mod11 <- RSEresults[,11]
mod12 <- RSEresults[,12]
mod13 <- RSEresults[,13]
mod14 <- RSEresults[,14]
mod15 <- RSEresults[,15]
mod16 <- RSEresults[,16]
mod17 <- RSEresults[,17]
mod18 <- RSEresults[,18]
mod19 <- RSEresults[,19]
mod20 <- RSEresults[,20]
mod21 <- RSEresults[,21]
mod22 <- RSEresults[,22]
mod23 <- RSEresults[,23]
mod24 <- RSEresults[,24]
mod25 <- RSEresults[,25]
mod26 <- RSEresults[,26]
mod27 <- RSEresults[,27]
mod28 <- RSEresults[,28]
mod29 <- RSEresults[,29]
modlist <- paste0("mod",c(1:29))


#setwd("C:/Users/Charlie/Desktop/Grad School Docs/Summer 2018/CSC 424/Final Project/BlogFeedback")

#creating a table of the performance values
restable <- read.csv('resultstable.csv',header=F)
smrytable <- matrix(nrow=29, ncol=7)
for(i in 1:nrow(smrytable)){
  smrytable[i, 1] <- summary(get(modlist[i]))[1]
  smrytable[i, 2] <- summary(get(modlist[i]))[2]
  smrytable[i, 3] <- summary(get(modlist[i]))[3]
  smrytable[i, 4] <- summary(get(modlist[i]))[4]
  smrytable[i, 5] <- summary(get(modlist[i]))[5]
  smrytable[i, 6] <- summary(get(modlist[i]))[6]
}
modlist2 <- c(paste0("MRfit",c(0,0.1,1,1.1,1.2,2,2.1,2.2,3,3.1,3.2,4,4.1,4.2,5,5.1,5.2)),
              paste0("LRfit",c(1,1.1,2,2.1,3,3.1,4,4.1,5,5.1,6,6.1)))
for(i in 1:length(modlist2)){
  temp <- get(modlist2[i])
  if(i < 18){
    smrytable[i,7] <- length(coef(temp))
  }
  else{
    smrytable[i,7] <- temp$df
  }
}

colnames(restable) <- c("Model Name","Algorithm","X Set","Y Set")
colnames(smrytable) <- c("Min RSE","1st Q. RSE","Median RSE","Mean RSE","3rd Q. RSE","Max RSE","Num Model Vars")
restable <- cbind(restable, smrytable)
head(restable)
write.csv(restable, file="blogfeedback_RSE_results25.csv")


#finding the best model base on mean of RMSE
bestMean = summary(mod1)[4]
bestMeanIdx = 1
bestMedian = summary(mod1)[3]
bestMedianIdx = 1
bestMax = summary(mod1)[5]
bestMaxIdx = 1
for(i in 2:length(modlist)){
  if(summary(get(modlist[i]))[4] < bestMean){
    bestMean <- summary(get(modlist[i]))[4]
    bestMeanIdx <- i
  }
  if(summary(get(modlist[i]))[3] < bestMedian){
    bestMedian <- summary(get(modlist[i]))[3]
    bestMedianIdx <- i
  }
  if(summary(get(modlist[i]))[6] < bestMax){
    bestMax <- summary(get(modlist[i]))[6]
    bestMaxIdx <- i
  }
}

bestMean  # mean of 0.7434033
bestMeanIdx  # model 25, the Lasso Regression on the full set of predictors
bestMedian  #median of 0.7153, although model 25 is close behind at 0.7174
bestMedianIdx  # model 24, the multiple regression on the full set of predictor with log(target+1)
bestMax     #max RSE on testing sets of 1.53
bestMaxIdx   #model 11, the manually tuned model 3.2 with log(target+1)


#summary(exp(mod27)-1)
### violin plots of the untransformed models ###
#dev.new(width=8, height=2)
vioplot(mod1, mod3, mod4, mod5, mod6, mod7, mod8, mod18, mod19, mod20, mod21, mod22, mod23,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','MR7','LR1','LR2','LR3','LR4','LR5','LR6'), col='blue')
title("RMSE : Non-Transformed Regression Models")


### violin plots of the log(target+1) transformed models ###
#dev.new(width=8, height=2)
#loglist <- paste0('mod',c(2,9,10,11,12,15,16,17,24,25,26,27,28,29))
vioplot(mod2, mod9, mod10, mod11, mod12, mod15, mod16, mod17, mod24, mod25, mod26, mod27, mod28, mod29,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','MR7','MR8','LR1','LR2','LR3','LR4','LR5','LR6'), 
        col='blue')
title("Residual Square Error : Log Transformed Models")

# violin plots of the reverse transformed models
vioplot(exp(mod2)-1, exp(mod9)-1, exp(mod10)-1, exp(mod11)-1, exp(mod12)-1, exp(mod15)-1, 
        exp(mod24)-1, exp(mod25)-1, exp(mod26)-1, exp(mod27)-1, exp(mod28)-1, exp(mod29)-1,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','LR1','LR2','LR3','LR4','LR5','LR6'), 
        col='blue')
title("Residual Square Error : Log Transformed Models (Reverse Transformed)")



### plots for single example comparing the log transformed model vs not, when reversing the transformed predictions
plot(exp(predict(LRfit4, as.matrix(xtest1[1:80,])))-1, type="l", col="black")
lines(exp(predict(LRfit4.1, as.matrix(xtest1[1:80,])))-1, type="l", col="red")
lines(ytest1[1:80], col="blue")
max(exp(predict(LRfit4.1, as.matrix(xtest1))+1))


#### MSE copy of analysis above
mod1 <- MSEresults[,1]
mod2 <- MSEresults[,2]
mod3 <- MSEresults[,3]
mod4 <- MSEresults[,4]
mod5 <- MSEresults[,5]
mod6 <- MSEresults[,6]
mod7 <- MSEresults[,7]
mod8 <- MSEresults[,8]
mod9 <- MSEresults[,9]
mod10 <- MSEresults[,10]
mod11 <- MSEresults[,11]
mod12 <- MSEresults[,12]
mod13 <- MSEresults[,13]
mod14 <- MSEresults[,14]
mod15 <- MSEresults[,15]
mod16 <- MSEresults[,16]
mod17 <- MSEresults[,17]
mod18 <- MSEresults[,18]
mod19 <- MSEresults[,19]
mod20 <- MSEresults[,20]
mod21 <- MSEresults[,21]
mod22 <- MSEresults[,22]
mod23 <- MSEresults[,23]
mod24 <- MSEresults[,24]
mod25 <- MSEresults[,25]
mod26 <- MSEresults[,26]
mod27 <- MSEresults[,27]
mod28 <- MSEresults[,28]
mod29 <- MSEresults[,29]
modlist <- paste0("mod",c(1:29))


#setwd("C:/Users/Charlie/Desktop/Grad School Docs/Summer 2018/CSC 424/Final Project/BlogFeedback")

#creating a table of the performance values
restable <- read.csv('resultstable.csv',header=F)
smrytable <- matrix(nrow=29, ncol=7)
for(i in 1:nrow(smrytable)){
  smrytable[i, 1] <- summary(get(modlist[i]))[1]
  smrytable[i, 2] <- summary(get(modlist[i]))[2]
  smrytable[i, 3] <- summary(get(modlist[i]))[3]
  smrytable[i, 4] <- summary(get(modlist[i]))[4]
  smrytable[i, 5] <- summary(get(modlist[i]))[5]
  smrytable[i, 6] <- summary(get(modlist[i]))[6]
}
modlist2 <- c(paste0("MRfit",c(0,0.1,1,1.1,1.2,2,2.1,2.2,3,3.1,3.2,4,4.1,4.2,5,5.1,5.2)),
              paste0("LRfit",c(1,1.1,2,2.1,3,3.1,4,4.1,5,5.1,6,6.1)))
for(i in 1:length(modlist2)){
  temp <- get(modlist2[i])
  if(i < 18){
    smrytable[i,7] <- length(coef(temp))
  }
  else{
    smrytable[i,7] <- temp$df
  }
}

colnames(restable) <- c("Model Name","Algorithm","X Set","Y Set")
colnames(smrytable) <- c("Min MSE","1st Q. MSE","Median MSE","Mean MSE","3rd Q. MSE","Max MSE","Num Model Vars")
restable <- cbind(restable, smrytable)
head(restable)
write.csv(restable, file="blogfeedback_MSE_results25.csv")


#finding the best model base on mean of RMSE
bestMean = summary(mod1)[4]
bestMeanIdx = 1
bestMedian = summary(mod1)[3]
bestMedianIdx = 1
bestMax = summary(mod1)[5]
bestMaxIdx = 1
for(i in 2:length(modlist)){
  if(summary(get(modlist[i]))[4] < bestMean){
    bestMean <- summary(get(modlist[i]))[4]
    bestMeanIdx <- i
  }
  if(summary(get(modlist[i]))[3] < bestMedian){
    bestMedian <- summary(get(modlist[i]))[3]
    bestMedianIdx <- i
  }
  if(summary(get(modlist[i]))[6] < bestMax){
    bestMax <- summary(get(modlist[i]))[6]
    bestMaxIdx <- i
  }
}

bestMean  # mean of 0.7434033
bestMeanIdx  # model 25, the Lasso Regression on the full set of predictors
bestMedian  #median of 0.7153, although model 25 is close behind at 0.7174
bestMedianIdx  # model 24, the multiple regression on the full set of predictor with log(target+1)
bestMax     #max RSE on testing sets of 1.53
bestMaxIdx   #model 11, the manually tuned model 3.2 with log(target+1)


#summary(exp(mod27)-1)
### violin plots of the untransformed models ###
#dev.new(width=8, height=2)
vioplot(mod1, mod3, mod4, mod5, mod6, mod7, mod8, mod18, mod19, mod20, mod21, mod22, mod23,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','MR7','LR1','LR2','LR3','LR4','LR5','LR6'), col='blue')
title("RMSE : Non-Transformed Regression Models")


### violin plots of the log(target+1) transformed models ###
#dev.new(width=8, height=2)
#loglist <- paste0('mod',c(2,9,10,11,12,15,16,17,24,25,26,27,28,29))
vioplot(mod2, mod9, mod10, mod11, mod12, mod15, mod16, mod17, mod24, mod25, mod26, mod27, mod28, mod29,
        names=c('MR1','MR2','MR3','MR4','MR5','MR6','MR7','MR8','LR1','LR2','LR3','LR4','LR5','LR6'), 
        col='blue')
title("MSE : Log Transformed Models")



### plots for single example comparing the log transformed model vs not, when reversing the transformed predictions
#plot(predict(LRfit1.1, as.matrix(x_test2)), type="l", col="black")
plot(y_test2[40:110], col='black', type="l", ylim=c(-15,60), xlab='Test Sample', 
     ylab='Number of Comments', main="Target Comment Totals: Actual vs Predicted", lwd=2)
lines(predict(MRfit0, x_test2[40:110,]), type="l", col="red", lwd=2)
lines(predict(LRfit1.1, as.matrix(x_test2[40:110,])), type="l", col="blue", lwd=2)
legend("topleft", legend=c('Actual Values', 'MRfit0 Predicted Values','LRfit1.1 Predicted Values'),
       col=c('black','red','blue'), bty='n', pch=5)



