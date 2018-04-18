#PCA & Linear Regression

setwd("C:/Users/Bikramjit/Documents")
getwd()
mydata <- read.csv("mortality.csv", header=TRUE)
head(mydata)
mydata$City <- NULL 
model <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot + S02Pot, data = mydata)
lmtest::bptest(model)
 #here we see that that the residuals Vs fitted plot is not completely linear
hist(mydata$pop) # The histogram for the population data is not normaly distributed
hist(mydata$pop, right = FALSE)
mydata$pop <- log10(mydata$pop) # The histogram for the population data is tranformed
hist(mydata$pop) # The histogram for the population data is now normally distributed
model <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot + S02Pot, data = mydata)
plot(model)
hist(mydata$HCPot)
hist(mydata$HCPot, right = FALSE)
mydata$HCPot <- log10(mydata$HCPot)
hist(mydata$HCPot)
model <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot + S02Pot, data = mydata)
plot(model)
hist(mydata$S02Pot)
hist(mydata$S02Pot, right = FALSE)
mydata$S02Pot <- log10(mydata$S02Pot)
hist(mydata$S02Pot)
model <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot + S02Pot, data = mydata)
plot(model)
hist(mydata$NOxPot)
hist(mydata$NOxPot, right = FALSE)
mydata$NOxPot <- log10(mydata$NOxPot)
model <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot + S02Pot, data = mydata)
plot(model)
summary(model)
model <- lm(Mortality ~ JanTemp + Rain + NW + NOxPot, data = mydata)
summary(model)

pcadata <- mydata
pcadata$Mortality <- NULL
fit <- princomp(pcadata, cor=TRUE)
summary(fit)
plot(fit, type="lines")
mydata<- cbind(mydata, fit$scores)
head(mydata)
model <- lm(Mortality ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5, data = mydata)
summary(model)
model <- lm(Mortality ~ Comp.2, data = mydata)
summary(model)

# Exercise 2  Association Rule Mining

install.packages("arules")
install.packages("arulesViz")
library("arules")
library("arulesViz")

setwd("C:/Users/Bikramjit/Documents")
getwd()
assocs = read.transactions("transactions.csv", format = "single", sep = ",", cols = c("Transaction","Product"), rm.duplicates = FALSE)
itemFrequencyPlot(assocs,topN=50,type="absolute")
rules <- apriori(assocs, parameter = list(supp = 0.03, conf = 0.20, minlen = 1, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)
redundant_index <- is.redundant(rules)
pruned_rules <- rules[!redundant_index]
inspect(pruned_rules[1:8])
summary(pruned_rules)

plot(rules,method="graph",interactive=FALSE,shading=NA)
