library(corrplot)
library(car)
library(factoextra)
library("FactoMineR")
library(psych)
 
#setting directory to new folder and reading in the file to work with
path <- "PATH"
setwd(path)
df <- read.csv("bfi.csv")

### Preprocessing
#removing the gender, education, and age columns. rmoving the 364 rows that have any missing entries
df2 <- df[,c(1:26)]
df2 <- na.omit(df2)
#running PCA and printing summary results
p <- prcomp(df2[,c(2:26)])
summary(p)
print(p)

### Screeplot of the principal components
plot(p, npcs=25)
abline(1,0)
p$sdev^2 #checking the exact values greater than 1 in order to verfiy graph

### Principal component eqns
p2 <- psych::principal(df2[,c(2:26)], rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.45, sort=T)


### Subjects with highest/lowest values for each component
scores <- cbind(df2["X"],p2$scores) #concatenating the unique IDs from cleaned df2 with the scores of PCA model 

#functions for returning the individual ID and PCA scores based on max and min for the whole survey
pcMax <- function(dframe, dframe_comp){
  return(dframe[which(dframe_comp == max(dframe_comp)), ])
}
pcMin <- function(dframe, dframe_comp){
  return(dframe[which(dframe_comp == min(dframe_comp)), ])
}

#highest value for RC1 - "Sociability"
highSociability <- pcMax(scores, scores$RC1)
highSociability
#lowest value for RC1 - "Sociability"
lowSociability <- pcMin(scores, scores$RC1)
lowSociability

#highest value for RC2 - "Emotional Instability"
highInstability <- pcMax(scores, scores$RC2)
highInstability
#lowest value for RC2 - "Emotional Instability"
lowInstability <- pcMin(scores, scores$RC2)
lowInstability

#highest value for RC3 - "Drive"
highDrive <- pcMax(scores, scores$RC3)
highDrive
#lowest value for RC3 - "Drive"
lowDrive <- pcMin(scores, scores$RC3)
lowDrive

#highest value for RC4 - "Creativity"
highCreativity <- pcMax(scores, scores$RC4)
highCreativity
#lowest value for RC4 - "Creativity"
lowCreativity <- pcMin(scores, scores$RC4)
lowCreativity

#highest value for RC5 - "Caring"
highCaring <- pcMax(scores, scores$RC5)
highCaring
#lowest value for RC1 - "Sociability"
lowCaring <- pcMin(scores, scores$RC5)
lowCaring


### Factor analysis: loadings
fit = factanal(df2[ ,c(2:26)], 5)
print(fit$loadings, cutoff=.4,sort=T)
