#
# Modeling SAT Scores Within Connecticut Districts 
#
# by Nomi Vilvovsky
# data derived from U.S. Census website and cleaned within excel
#
# We'll start with some set up to input RStudio packages and the data table
install.packages("ggplot2")
install.packages("readxl")
install.packages("lme4")
install.packages("gridExtra")

#load relevant packages
library(readxl)
library(ggplot2)
library(gridExtra)
##############################################################################################
################## Checking assumptions#######################################################
##############################################################################################

#read data from computer harddrive
testscores <- read_excel("DATA LOCATION HERE")

#we perform test and see that our dependent variable is normal:
shapiro.test(testscores$benchmarkpc)

testscores$benchmark <- (testscores$benchmarkpc)*100 #full percent rather than decimal

testscores$ttchange2 <- (testscores$ttchange)*100 #make into percent rather than decimal

testscores$partic <- (testscores$partrate)*100

#county is a categorical variable, with boxplot
boxplot(benchmark~county,main='SAT benchmark met by County',xlab='County',
        ylab='SAT Benchmark Percent', data=testscores)
#the boxplots have very difference variances, which we should note in our analysis

##############################################################################################
################ Build Model #################################################################
##############################################################################################

#let's check correlations on scatterplot
library(ggplot2)
ggplot(testscores,aes(x=absent, y=benchmark))+geom_point() #appears to have negative corr
ggplot(testscores,aes(x=testtakers, y=benchmark))+geom_point() #no visible corr
cor.test(testscores$testtakers, testscores$benchmark, method="pearson")
#pvalue is .6818, so the correlation of testtakers is not significant

ggplot(testscores,aes(x=ttchange2, y=benchmark))+geom_point() #weak correlation
cor.test(testscores$ttchange2, testscores$benchmark, method="pearson")
#pvalue is .6095, so the correlation of testtakers is not significant

ggplot(testscores,aes(x=partic, y=benchmark))+geom_point() #appears to have correlation
cor.test(testscores$partic, testscores$benchmark, method="pearson")
#pvalue is very small, we can assume correlation is significant

a <- ggplot(testscores,aes(x=readdpi, y=benchmark))+geom_point()
b <- ggplot(testscores,aes(x=mathdpi, y=benchmark))+geom_point()
c <- ggplot(testscores,aes(x=writedpi, y=benchmark))+geom_point()
d <- ggplot(testscores,aes(x=sciencedpi, y=benchmark))+geom_point()
grid.arrange(a, b, c, d, nrow = 2)
#dpi scores all have strong positive correlations

#build model with district as a random effect, and test interactions
library(lme4)
model1=lmer(benchmark~partic + absent + readdpi + mathdpi + writedpi + sciencedpi + (1|county) + partic*absent, data=testscores)
#summary(model1)

#test normality of residuals
hist(residuals(model1))
shapiro.test(residuals(model1))
#assumption is not verified

model2=lm(benchmark~partic + absent + readdpi + mathdpi + writedpi + sciencedpi + partic*absent, data=testscores)
#summary(model2)

model3=lmer(benchmark~ttchange2 + partic + absent + readdpi + mathdpi + writedpi + sciencedpi + (1|county) + partic*absent + ttchange2*partic, data=testscores)
#summary(model3)

#test normality of residuals
hist(residuals(model3))
shapiro.test(residuals(model3))
#assumption is not verified

model4=lm(benchmark~ttchange2 + partic + absent + readdpi + mathdpi + writedpi + sciencedpi + partic*absent + ttchange2*partic, data=testscores)
#summary(model4)

model5=lmer(benchmark~ttchange2 + partic + absent + readdpi + mathdpi + writedpi + sciencedpi + (1|county), data=testscores)
#summary(model5)
hist(residuals(model5))
shapiro.test(residuals(model5))

model6=lm(benchmark~ttchange2 + partic + absent + readdpi + mathdpi + writedpi + sciencedpi, data=testscores)
#summary(model6)

anova(model1, model2, model3, model4, model5, model6) #model1 is best

summary(model1)

model7=lmer(benchmark~partic + absent + readdpi + (1|county) + partic*absent, data=testscores)

anova(model1, model7) #compare

#model 1 still stands

#################################################################################
####################### Checking More Assumptions################################
#################################################################################

#check that mean of residuals is 0
mean(resid(model1))
# approximately 0, assumption holds

