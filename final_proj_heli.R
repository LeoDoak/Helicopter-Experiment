data<-read.csv("final_proj.csv")
print(data)

mod1 <- aov(time ~ as.factor(person)+ as.factor(type.of.paper.clip), data = data)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1, which=1)
plot(mod1, which=2)
plot(mod1, which=3)
plot(mod1, which=5)

library(nortest)
ad.test(mod1$residuals)

bartlett.test(time~as.factor(person), data=data)
bartlett.test(time~as.factor(type.of.paper.clip), data=data)

TukeyHSD(mod1, ordered=T)

par(mfrow=c(1,2))
with(data, (interaction.plot( person, type.of.paper.clip,time, type="b", pch=c(18,24,22),
                              leg.bty="o",main="Interaction Plot of Person and Type of Paper clip", xlab="person", ylab="Y")))
with(data, (interaction.plot( type.of.paper.clip ,person,time, type="b", pch=c(18,24,22),
                              leg.bty="o",main="Interaction Plot of Type of Paper clip and Person", xlab="type.of.paper.clip", ylab="Y")))
library("agricolae")
HsD<-HSD.test(mod1, c("as.factor(Treatment)"))
HsD
TukeyHSD(mod1, ordered=T)


mod3 <- aov(time ~ person + type.of.paper.clip, data = data)
summary(mod3)