library(ggplot2)
library(lattice)
library(faraway)
library(leaps)
library(car)
library(lmtest)
library(perturb)
library(sandwich)
#suicide has sex total for each year
#suicideYear has year total
#suicideAgeGroup has each age group and both sex
suicide<-read.csv("C:/Users/CaraY/Documents/sum.csv")
suicideYear<-read.csv("C:/Users/CaraY/Documents/sumYear.csv")
suicideAgeGroup<-read.csv("C:/Users/CaraY/Documents/ALL_sex.csv")

View(suicide)
summary(suicide)

suicide$country_year<-factor(suicide$ country_year)
suicide$country<-factor(suicide$country)
suicide$sex<-factor(suicide$sex)

suicideYear$country_year<-factor(suicideYear$country_year)
suicideYear$generation<-factor(suicideYear$year)
suicideYear$country<-factor(suicideYear$country)

suicideAgeGroup$age<-factor(suicideAgeGroup$age)
suicideAgeGroup$generation<-factor(suicideAgeGroup$generation)
suicideAgeGroup$sex<-factor(suicideAgeGroup$sex)


boxplot(suicide$SuiRate~suicide$sex,data=suicide2,
        xlab="sex",ylab="suicide rate per 100k people")
title(sub = "Figure 1: A box-plot of the suicide rate per 100k people stratified by sex")

suicideAgeGroup$age1=factor(suicideAgeGroup$age,
                 levels=levels(suicideAgeGroup$age)[c(4,1,2,3,5,6)])
boxplot(suicideAgeGroup$SuiRate~suicideAgeGroup$age1,data=suicideAgeGroup,
        xlab="age",ylab="suicide rate per 100k people")
title(sub = "Figure 2: A box-plot of the suicide rate per 100k people stratified by age")

#boxplot(suicideAgeGroup$SuiRate~suicideAgeGroup$age,data=suicideAgeGroup)

suicideAgeGroup$generation1=factor(suicideAgeGroup$generation,
                 levels=levels(suicideAgeGroup$generation)[c(2,6,1,3,5,4)])
boxplot(suicideAgeGroup$SuiRate~generation1,suicideAgeGroup,
        xlab="generation",ylab="suicide rate per 100k people")
title(sub = "Figure 3: box plots of the suicide rate per 100k 
      people stratified by generation")

#summary(suicideAgeGroup$generation1)
#summary(suicideAgeGroup$generation)


plot(SuiRate~gdp_for_year,suicide,
     xlab="GDP",ylab="suicide rate")
title(sub = "Figure 5: suicide rate per 100k people stratified by GDP for the year")

plot(SuiRate~log(gdp_for_year),suicide,
     xlab="log(GDP)",ylab="suicide rate")
title(sub = "Figure 5: suicide rate per 100k people stratified by log(GDP) for the year")

#when plot use count/popu
#maybe see the max and min suicide count of country
pdf("Suiboxplots.pdf")
for(j in levels(suicideYear$country)){
  boxplot(suicideYear$SuiRate[suicideYear$country == j])
  title(j)
}
dev.off()

pdf("SuiTimeSeriesYear.pdf")
for(j in levels(suicideYear$country)){
  plot(suicideYear$SuiRate ~suicideYear$year, subset = (suicideYear$country == j))
  title(j)
}
dev.off()

pdf("SuiTimeSeriesGDP.pdf")
for(j in levels(suicideYear$country)){
  plot(suicideYear$SuiRate ~ log(suicideYear$gdp_for_year), subset = (suicideYear$country == j))
  title(j)
}
dev.off()


pdf("SuiTimeSeriesAge.pdf")
for(j in levels(suicideAgeGroup$age)){
  plot(suicideAgeGroup$SuiRate~suicideAgeGroup$year,subset=(suicideAgeGroup$age == j))
  title(j)
}
dev.off()


pdf("SuiTimeSeriesSex.pdf")
for(j in levels(suicide$sex)){
  plot(suicide$SuiRate~suicide$year,subset=(suicide$sex == j))
  title(j)
}
dev.off()

suicide2=subset(suicide,country!="Bosnia and Herzegovi" & country!="Cabo Verde" 
                &country!="Dominica" & country!="Macau" & country!="Mongolia" &
                  country!="Nicaragua" &country!="Oman" &country!="Saint Kitts and Nevi" 
                &country!="Saint Vincent and Gr" &country!="San Marino"
                &country!="Turkey" &country!="United Arab Emirates")
View(suicide2)
gmod1<-glm(SuiCount ~ log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod2<-glm(SuiCount ~ sex + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod3<-glm(SuiCount ~ year + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod4<-glm(SuiCount ~ country + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod5<-glm(SuiCount ~ country * sex + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod6<-glm(SuiCount ~ country * year + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod7a<-glm(SuiCount ~ sex + country * year + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod8<-glm(SuiCount ~ year + country * sex + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod9<-glm(SuiCount ~ sex + country + log(gdp_for_year) + 
             offset(log(population)), data = suicide2, family = poisson)
gmod10<-glm(SuiCount ~ sex + year + log(gdp_for_year) + 
              offset(log(population)), data = suicide2, family = poisson)
gmod11<-glm(SuiCount ~ year + country + log(gdp_for_year) + 
              offset(log(population)), data = suicide2, family = poisson)
gmod12<-glm(SuiCount ~ sex * year + log(gdp_for_year) + 
              offset(log(population)), data = suicide2, family = poisson)
gmod13<-glm(SuiCount ~ country + sex * year + log(gdp_for_year) + 
              offset(log(population)), data = suicide2, family = poisson)
gmod14<-glm(SuiCount ~ year + sex + country +log(gdp_for_year) + 
              offset(log(population)), data = suicide2, family = poisson)


library(faraway) #need for sumary function
sumary(gmod7a) # the author's modified version of summary, because summary
# produces too much information

library(leaps)
AIC(gmod1)
AIC(gmod2)
AIC(gmod3)
AIC(gmod4)
AIC(gmod5)
AIC(gmod6)
AIC(gmod7a)
AIC(gmod8)
AIC(gmod9)
AIC(gmod10)
AIC(gmod11)
AIC(gmod12)
AIC(gmod13)
AIC(gmod14)

library(car)
BIC(gmod7a)
BIC(gmod8)

library(lmtest)
dwtest(gmod7a)


#plot(residuals(gmod7a)~year,na.omit(suicide),ylab="Residuals")
n <- length(residuals(gmod7a))
plot(tail(residuals(gmod7a),n-1)~head(residuals(gmod7a),n-1),
     na.omit(suicide),xlab="Residuals i",ylab="Residuals i+1",abline(h=0,v=0),
     sub="Figure 12: successive residual plot")
abline(h = 0,v = 0,col = grey(0.75))
abline(0,1)


#"Partial Residual"
mu <- predict(gmod7a,type="response")
u <- (suicide$SuiCount-mu)/mu + coef(gmod7a)[2]*log(suicide$gdp_for_year)
plot(u ~ log(gdp_for_year), suicide,ylab="Partial Residual")
abline(0,coef(gmod7a)[2])

mu <- predict(gmod7a,type="response")
u <- (suicide$SuiCount-mu)/mu + coef(gmod7a)[2]*log(suicide$year)
plot(u ~ year, suicide,ylab="Partial Residual")
abline(0,coef(gmod7a)[2])

library(car)
residualPlots(gmod7a)
#type="link" means to predict regressors, type="response" means 
#to predict responses,default is link.
plot(residuals(gmod7a) ~ predict(gmod7a,type="link"),
     xlab=expression(hat(eta)),ylab="Deviance residuals",
     sub="Figure 10: Deviance residual vs predicted regressors")
abline(h = 0,col = grey(0.75))
plot(residuals(gmod7a,type = "pearson") ~ predict(gmod7a,type="link"),
     xlab=expression(hat(eta)),ylab="Pearson residuals",
     sub="Figure 11: Pearson residual vs predicted regressors")
abline(h = 0,col = grey(0.75))
#manually calculate the residuals using (y_i - mu_hat_i)/sqrt(mu_hat_i) to double check.
#Perhaps constant variance assumption is simply violated.
plot((suicide2$SuiCount-mu)/sqrt(mu)~predict(gmod7a,type="link"),
     xlab=expression(hat(eta)),ylab="Pearson residuals",
     main="Manual Pearson residual vs predicted regressors")

resi=residuals.glm(gmod7a,type = "pearson")
max(abs(resi))
summary(resi)
resi[abs(resi)>50]
suicide2$country_year[1753]#Russian Federation2009
suicide2$country_year[1146]# Kazakhstan1993
suicide2$country_year[1754]#Russian Federation2010


length(suicide$SuiCount)
length(mu)
#check if link assumption is correct
mu <- predict(gmod7a,type="response")
z <- predict(gmod7a)+(suicide2$SuiCount-mu)/mu
plot(z ~ predict(gmod7a), xlab="Linear predictors",
     ylab="Linearized Responses",
     sub="Figure 18: Lenearized Response vs linear predictors")
abline(0,1,col="red")

summary(influence(gmod7a)$hat)
(influence(gmod7a)$hat)[(influence(gmod7a)$hat)>0.4]
suicide2$country_year[1424]#Netherlands1990


stud <- rstudent(gmod7a)
# largest magnitude studentized residual
max(abs(stud))
# since we are doing a two-sided test, we need the 1 - alpha/2n quantile, 
# not 1-alpha/n.  df = 50 - 5 - 1
qt(1 - .05/(50*2), df = 215823) # all residuals within expected quantiles
#outlierTest(gmod7a)

cook<-cooks.distance(gmod7a)
max(cook)
cook[cook>2]
suicide2$country_year[1146]#Kazakhstan1993


#outlier
library(faraway)
halfnorm(rstudent(gmod7a),labs=suicide2$country_year,
         sub="Figure 13: Half-normal plot of jackknife residuals",ylab="residual")
#leverage
halfnorm(hatvalues(gmod7a),labs=suicide2$country_year,
         sub="Figure 14: Half-normal plot of the leverages",ylab="leverage")
#max(hatvalues(gmod7a))
#(hatvalues(gmod7a))[(hatvalues(gmod7a))>0.48]

#influence
#halfnorm(cook,n=3,labs=suicide2$country_year,ylab="Cook's distances")
halfnorm(cooks.distance(gmod7a),labs=suicide2$country_year,
          sub="Figure 15: Half-normal plot of the Cook statistics")
#suicide$country_year[1663]

library(car)
influencePlot(gmod7a,sub="Figure 17: influence plot")
suicide2$country_year[1424]#Netherlands1990
suicide2$country_year[1719]#Romania2003
suicide2$country_year[1146]#Kazakhstan1993
suicide2$country_year[1173]#Kiribati1995
suicide2$country_year[1753]#Russian Federation2009

influencePlot(gmod7a,sub="scale=10")


dfbetasPlots(gmod7a, terms=~.-country*year,id.n = 3,
             sub="Figure 19: dfbetas plots")
suicide2$country_year[4004]#Russian Federation1998
suicide2$country_year[4005]#Russian Federation1999
suicide2$country_year[4006]#Russian Federation2000
suicide2$country_year[1745]#Russian Federation2001
suicide2$country_year[1733]#Russian Federation1989

dfbetasPlots(gmod7a, terms=~year,id.n = 3,
             sub="Figure 19: dfbetas plots")
suicide2$country_year[17]#Albania2005
suicide2$country_year[18]#Albania2006
suicide2$country_year[21]#Albania2009

1-pchisq(deviance(gmod7a),df.residual(gmod7a))

suicide1 = subset(suicide2, country!="Russian Federation" & country!="Romania" 
                  &country!="Kazakhstan" & country!="Netherlands"
                  & country!="Kiribati")
gmod7b<-glm(SuiCount ~ sex + country * year + log(gdp_for_year) + 
              offset(log(population)), data = suicide1, family = poisson)

compareCoefs(gmod7a,gmod7b)

gmod7a
plot(gmod7a)
suicide2$country_year[1146]#Kazakhstan1993
suicide2$country_year[1753]#Russian Federation2009
suicide2$country_year[1754]#Russian Federation2010
suicide2$country_year[1719]#Romania2003
suicide2$country_year[1748]#Russian Federation2004

summary(gmod7a)

cor(suicide2$year,log(suicide2$gdp_for_year))

y=suicide2$SuiCount
cor(y,fitted(gmod7a))
#round(cor(suicide2), 3)

library(perturb)
colldiag(gmod7a, scale = FALSE, add.intercept = FALSE)

library(car)
# examine marginal model plot
marginalModelPlots(gmod7a)
title(sub = "Figure 7: Marginal Model Plots")

# examine added variable plots
avPlots(gmod7a,terms=~log(gdp_for_year))
title(sub = "Figure 8: Added-Variable Plot of suicide rate stratified by log (GDP)")

avPlots(gmod7a,terms=~year)
title(sub = "Figure 9: Added-Variable Plot of suicide rate tratified by year")


#C+R plots not available for models with interactions.
#crPlots(gmod7a,terms=~log(gdp_for_year))
#CERES plots not available for models with interactions.
#ceresPlots(lmod)
# examine marginal model plot
marginalModelPlots(gmod7b)

# examine added variable plots
avPlots(gmod7b,terms=~log(gdp_for_year))
avPlots(gmod7b,terms=~year)


anova(gmod1, gmod7a,test = "Chisq")
anova(gmod3, gmod7a,test = "Chisq")
anova(gmod4, gmod7a,test = "Chisq")
anova(gmod11, gmod7a,test = "Chisq")


library(sandwich)
modpla <- glm(SuiCount ~ sex + country * year + log(gdp_for_year) + 
                offset(log(population)), family=poisson, data=suicide1)
(sebeta <- sqrt(diag(vcovHC(modpla))))
