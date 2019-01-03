library(CCA)
library(yacca)
library(MASS)

#function from example CCA files
ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  w = rev(cumprod(rev(ev)))
  # initialize
  d1 = d2 = f = vector("numeric", k)
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

#reading in the data and setting it up in a data frame
path <- "PATH"
setwd(path)
df <- read.table("canon.txt", sep='\t', header=T)
df <- na.omit(df)

#separating the two sets of variables we want to investigate the relationship between
attitudinal <- df[,1:4]
health <- df[,5:10]

#combined correlation matrix between the two sets 
round(matcor(attitudinal, health)$XYcor[1:4,5:10], 2)

#Correlations between attitudinal and attitudinal (X)  
#Correlations between health and health (Y)
ccSurvey = cc(attitudinal, health)
ccSurvey$cor

###Using Wilk's Lambda Test to test for significant
wilksSurvey = ccaWilks(attitudinal, health, ccSurvey)
round(wilksSurvey, 2)

#Attitudinal raw coefficients (like loadings in PCA)
round(ccSurvey$xcoef, 2)
#Health Correlations raw coefficients (like loadings in PCA)
round(ccSurvey$ycoef, 2)

### Calculating the correlations btween each of the canonical variates and the original variables
loadingsSurvey = comput(attitudinal, health, ccSurvey)

#Correlation of Attitudinal scores 
# "phyheal" has a very strong positive correlation with the first variate, contributes most to first canonical variate
# "attdrug" has a strong negative correlation with the second variate, contributes the most to the second canonical variate
round(loadingsSurvey$corr.X.xscores, 2)

#Correlation Health Scores
# 'druguse' and 'menheal' have the strongest (and both positive) correlations with the first canonical variate
# ''menheal' has the strongest (and also positive) correlation with the second canonical variate
round(loadingsSurvey$corr.Y.yscores, 2)

#Plotting the two canonical variates against each other
plot(loadingsSurvey$xscores[,1], loadingsSurvey$yscores[,1])
cor(loadingsSurvey$xscores[,1], loadingsSurvey$yscores[,1])

#Investigatnig the relationship between the variables and canonical correlates from the other dataset
#This gives us a good idea of how the variables might predict each other, most important for prediction
loadingsSurvey$corr.X.yscores
loadingsSurvey$corr.Y.xscores

# A basic visualization of the cannonical correlation
plt.cc(ccSurvey)

###### Similar Evaluation Using the Yacca Package ########
c2 = cca(attitudinal, health)
c2

helio.plot(c2, cv=1, x.name="Attitudinal Values", 
           y.name="Healh Values")

helio.plot(c2, cv=2, x.name="Health Values", 
           y.name="Attitudinal Values")

# Perform a chisquare test on C2
c2$chisq
c2$df
summary(c2)

