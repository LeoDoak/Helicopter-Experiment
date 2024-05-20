# STT - 411 - 001 
# Final Project 
# Leo Doak, Cole Thibault, Darina Aynaeva

# Read in data from csv for project data
data<-read.csv("final_proj.csv")
print(data)

# Analysis Of Variance
mod1 <- aov(time..seconds. ~ as.factor(person) + as.factor(type.of.paper.clip) + as.factor(helicopter.size), data = data)
summary(mod1)

# Anderson Darling Test
library(nortest)
ad.test(mod1$residuals)

# Check for Normality
par(mfrow=c(2,2))
plot(mod1, which=1)
plot(mod1, which=2)
plot(mod1, which=3)
plot(mod1, which=5)

# Bartlett Test
bartlett.test(time..seconds.~as.factor(person), data=data)
bartlett.test(time..seconds.~as.factor(type.of.paper.clip), data=data)
bartlett.test(time..seconds.~as.factor(helicopter.size), data=data)

#defining groups 
library(stats)
groups<-factor(paste(data$helicopter.size ,data$type.of.paper.clip,data$person, sep=""))
groups
bartlett.test( data$time..seconds.~groups, data=data)

# Transform the response variable: time(seconds)
library(MASS)
bc<-boxcox(data$time..seconds.~as.factor(helicopter.size)*as.factor(type.of.paper.clip)*as.factor(person), data=data )
lambda<-bc$x[which.max(bc$y)]
lambda
tdata<-transform(data, ty=data$time..seconds.^(lambda))


mod3<-aov(ty~as.factor(helicopter.size)+as.factor(type.of.paper.clip)+as.factor(person), data=tdata)
summary(mod3)

# Check for Normality
par(mfrow=c(2,2))
plot(mod3, which=1)
plot(mod3, which=2)
plot(mod3, which=3)
plot(mod3, which=5)

# Anderson Darling test (**Passed**)
ad.test(mod3$residuals)
# Bartlett test for groups: (**Passed**)
bartlett.test( ty~groups, data=tdata)

# Bartlett test again with transformed data: (** All Passed**)
bartlett.test(ty~as.factor(person), data=tdata)
bartlett.test(ty~as.factor(type.of.paper.clip), data=tdata)
bartlett.test(ty~as.factor(helicopter.size), data=tdata)

# Ordered Tukey Analysis 
TukeyHSD(mod3, ordered=T)

# More Tukey
library("agricolae")
HsD<-HSD.test(mod3, c("as.factor(person)"))
HsD

HsD<-HSD.test(mod3, c("as.factor(type.of.paper.clip)"))
HsD

HsD<-HSD.test(mod3, c("as.factor(helicopter.size)"))
HsD

# Factors: person, type.of.paper.clip, helicopter.size
# Result: ty 

# Interaction Plots:
par(mfrow=c(3,3))
with(tdata, (interaction.plot( person,type.of.paper.clip,ty,type="b", pch=c(18,24,22),
                              leg.bty="o",main="Interaction Plot of Person and Type of Paper clip", xlab="person", ylab="Y")))
with(tdata, (interaction.plot( type.of.paper.clip,helicopter.size,ty,type="b", pch=c(18,24,22),
                               leg.bty="o",main="Interaction Plot of Type of Paper clip and Helicopter Size", xlab="type.of.paper.clip", ylab="Y")))
with(tdata, (interaction.plot( person,helicopter.size,ty,type="b", pch=c(18,24,22),
                               leg.bty="o",main="Interaction Plot of Type of Person and Helicopter Size", xlab="person", ylab="Y")))

# Reverse the variables just to check for more interactions: 
#par(mfrow=c(2,2))
with(tdata, (interaction.plot( type.of.paper.clip,person,ty,type="b", pch=c(18,24,22),
                               leg.bty="o",main="Interaction Plot of Type of Paper clip and Person", xlab="person", ylab="Y")))
with(tdata, (interaction.plot( helicopter.size,type.of.paper.clip,ty,type="b", pch=c(18,24,22),
                               leg.bty="o",main="Interaction Plot of Helicopter Size and Type of Paper Clip", xlab="type.of.paper.clip", ylab="Y")))
with(tdata, (interaction.plot( helicopter.size,person,ty,type="b", pch=c(18,24,22),
                               leg.bty="o",main="Interaction Plot of Helicopter Size and person", xlab="person", ylab="Y")))

# Analysis of Variance without blocking variable
mod3 <- aov(time..seconds. ~ person + type.of.paper.clip + helicopter.size, data = data)
summary(mod3)

