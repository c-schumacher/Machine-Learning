library(corrplot)
library(car)
library(broom)
library(ggplot2)
library(MASS)
library(relaimpo)

#reading in the data and setting it up in a data frame
path <- "PATH"
setwd(path)
rawdata <- read.csv('housedata.csv', header=TRUE)
df <- rawdata[ ,c('price','bedrooms','bathrooms','sqft_living','sqft_lot',
                   'floors','waterfront','view','condition','grade','sqft_above',
                   'sqft_basement','yr_built','yr_renovated','zipcode','sqft_living15',
                   'sqft_lot15','lat','long')]

  
##### Checking for multicollinearity   #####
#creating a scatterplot matrix, and correlation plot
#par(mar <- c(1,1,1,1))
plot(df)
corrplot(cor(df), method = 'ellipse')

#computing the full model with all of the feature variables
fit <- lm(df, data = df)
summary(fit)

#regenerating the model statistics with the 10 feature variables that had a p-value less than 5% when running the full model
fit <- lm(df[,1] ~ df[,2]+df[,4]+df[,7]+df[,8]+df[,10]+df[,13]+df[,14]+df[,15]+df[,16]+df[,18], data <- df)
summary(fit)
vif(fit)

#regenerating the model statistics again with only 9 feature variables which had a p-value less than 5% in the most recent model
fit <- lm(df[,1] ~ df[,4]+df[,7]+df[,8]+df[,10]+df[,13]+df[,14]+df[,15]+df[,16]+df[,18], data = df)
summary(fit)
vif(fit)
corrplot(cor(df[ ,c(1,4,7,8,10,13,14,15,16,18)]), method = 'ellipse')

#regenerating the model statistics with 7 most important features and generating an updated plot
fit <- lm(df[,1] ~ df[,4]+df[,7]+df[,8]+df[,13]+df[,14]+df[,15]+df[,18], data = df)
summary(fit)
corrplot(cor(df[ ,c(1,4,7,8,13,14,15,18)]), method = 'ellipse')

#regenerating the model statistics with the 4 most important features, and an updated correlation plot, and VIF values
fit <- lm(df[,1] ~ df[,4]+df[,7]+df[,8]+df[,18], data = df)
summary(fit)
corrplot(cor(df[ ,c(1,4,7,8,18)]), method = 'ellipse')
vif(fit)

#regenerating the model statistics with the 3 most important features, and an updated correlation plot, and VIF values
fit <- lm(df[,1] ~ df[,4]+df[,7]+df[,18], data = df)
summary(fit)
corrplot(cor(df[ ,c(1,4,7,18)]), method = 'ellipse')
vif(fit)

#plotting the residuals of the model
plot(df2$price, resid(fit), ylab="Residuals", xlab="Price", main='House Prices')
abline(0,0)

#alternate residual plot approaches that show slightly different things
plot(resid(fit))
ggplot(fit, aes(x = .fitted, y = .resid)) + geom_point()

#testing removal of the extreme outliers and rerunning the linear model using the same iterative process
lowerbound_extreme <- quantile(df$price)[2] - 3.0*IQR(df$price)
upperbound_extreme <- quantile(df$price)[4] + 3.0*IQR(df$price)
df2 <- df[df$price > lowerbound_extreme & df$price < upperbound_extreme,]

#skipping rewriting each individual line, iteratively updated this to get the final model
fit <- lm(df2[,1] ~ df2[,4]+df2[,8]+df2[,18], data=df2)
summary(fit)
vif(fit)
corrplot(cor(df2[ ,c(1,4,8,18)]), method='ellipse')

plot(df2$price, resid(fit), ylab="Residuals", xlab="Price", main='House Prices')
abline(0,0)
plot(fit)


##### Running the model with an automatic method #####
lfit <- lm(df2$price~1, data=df2)
ufit <- lm(df2$price~., data = df2)

#testing forward selection on the model with the extreme outliers removed
step <- step(lfit, scope=list(lower=lfit, upper=ufit), direction='forward')
summary(step)
plot(df2$price, resid(step), ylab="Residuals", xlab="Price", main='House Prices')
abline(0,0)

#testing backward selection on the model with the extreme outliers removed
step <- step(ufit, data=df2, direction='backward')
summary(step)
plot(df2$price, resid(step), ylab="Residuals", xlab="Price", main='House Prices')
abline(0,0)


##### Generating an interesting visualization #####
#added variable plots
avPlots(step)  
avPlots(lm(df2,data=df2))
avPlots(lm(df2$price ~ df2$sqft_living + df2$view + df2$lat, data=df2))

#relative importance plots
fullmodel = lm(df2, data=df2)
boot <- boot.relimp(step, b = 10, type = "lmg", rank = TRUE, 
                    diff = TRUE, rela = TRUE)
plot(booteval.relimp(boot,sort=TRUE)) 

