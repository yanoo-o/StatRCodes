#set your working directory (like libname in SAS)
setwd("C:/Users/Admin/Documents/Stat 148")

#reads the dataset
phones <- read.csv("Phones Dataset.csv",header=T)
with(phones, interaction.plot(Phone, Influencer, Score))
phones$Phone <- factor(phones$Phone)
phones$Influencer <- factor(phones$Influencer)

library(ggplot2)

ggplot(phones, aes(y=Score, x=Phone, group=Influencer,color=Influencer)) +
  geom_line(size=1) +
  guides(color=guide_legend(ncol=3)) +
  xlab("Phone Brand") + ylab("Score")
ggplot(phones, aes(y=Score, x=Influencer)) + geom_boxplot() +
  xlab("Influencer") + ylab("Score")
ggplot(phones, aes(y=Score, x=Phone)) + geom_boxplot() +
  xlab("Phone Brand") + ylab("Score")

phones.aov <- aov(Score~Phone+Influencer,data=phones)
summary(phones.aov)
drop1(phones.aov, test = "F")

phones2.aov <- aov(Score~Phone+Influencer+Error(Influencer),data=phones)
summary(phones2.aov)
drop1(phones2.aov, test = "F")

phones.crd.aov <- aov(Score~as.factor(Phone),data=phones)
summary(phones.crd.aov)

space <- par(mfrow=c(2,2),cex=0.8)
plot(phones.aov)
par(space)

bartlett.test(Score~Phone,data=phones)

library(MASS)
boxcox(phones.aov,lambda = seq(-4, 4, 0.1))
abline(v=1,col="blue")

library(DescTools)
PostHocTest(phones.aov, method = "duncan")
PostHocTest(phones.crd.aov, method = "duncan")
