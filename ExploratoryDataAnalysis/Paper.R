setwd("C:/Users/Admin/Documents/Stat 146")

#Reading the dataset
library(readxl)
mort<-read_excel("EDA Paper Data_Oliva.xlsx", sheet="Collated Data", range="A1:D94",
                 col_names=TRUE, col_types=c("text","numeric", "numeric","numeric"), 
                 trim_ws=TRUE, skip=0)
summary(mort)

#Constructing Stem-and-Leaf Display
stem(x = mort$Mortality, scale = 2)
stem(mort$Doctors, scale = 2)
stem(mort$Nursing, scale =2)

# Construct boxplots.
boxpt<-boxplot(mort$Mortality, mort$Doctors, mort$Nursing, range=1.5, width=NULL, varwidth=TRUE, notch=FALSE,
               outline=TRUE, names=c("Mortality", "No. of Doctors","No. of Nurses and Midwives"), plot=TRUE, horizontal=FALSE)

#Construct Quantile Plots for three variables
mort_o <- sort(mort$Mortality)
doc_o <- sort(mort$Doctors)
nur_o <- sort(mort$Nursing)

i <- 1:length(mort_o)
p.val <- (i-0.5)/length(mort_o)
mort.pi <- data.frame(mort_o, p.val)
doc.pi <- data.frame(doc_o, p.val)
nur.pi <- data.frame(nur_o, p.val)


#Quantile plot using ggplot2 package
library(ggplot2)
ggplot(mort.pi, aes(x = p.val, y = mort_o)) + geom_point() + xlab("p") + ylab("Mortality")
ggplot(doc.pi, aes(x = p.val, y = doc_o)) + geom_point() + xlab("p") + ylab("No. of Medical Doctors")
ggplot(nur.pi, aes(x = p.val, y = nur_o)) + geom_point() + xlab("p") + ylab("No. of Nurses and Midwives")


qqplot(mort_o,doc_o, plot.it = TRUE, xlab = "Mortality", ylab="No. of Medical Doctors")
abline(a=0, b=1, col = "Red")
qqplot(mort_o,nur_o, plot.it = TRUE, xlab = "Mortality", ylab="No. of Nurses and Midwives")
abline(a=0, b=1, col = "Red")
qqplot(doc_o,nur_o, plot.it = TRUE, xlab = "No. of Medical Doctors", ylab="No. of Nurses and Midwives")
abline(a=0, b=1, col = "Red")

#Fitting Resistant Lines
library(ACSWR)
md <- data.frame(mort$Doctors,mort$Mortality)
mort_doc <- md[order(mort$Doctors), ]
plot(mort_doc, xlab = "Number of Medical Doctors", ylab = "Infant Mortality Rate")
resistant_line(mort$Doctors,mort$Mortality, iter = 7)
abline(a=15.676826, b=-0.961194, col = "Blue")

mn <- data.frame(mort$Nursing,mort$Mortality)
mort_nur <- mn[order(mort$Nursing),]
plot(mort_nur, xlab = "Number of Nurses and Midwives", ylab = "Infant Mortality Rate")
resistant_line(mort$Nursing,mort$Mortality, iter = 5)
abline(a=21.1481850, b=-0.3128769, col = "Blue")


#Residuals using Classical Approach
mort.lm <- lm(Mortality ~Doctors+Nursing, data = mort)
summary(mort.lm)
unclass(mort.lm)
mort.lm$residuals

mort.lm1 <- lm(Mortality ~Doctors, data = mort)
summary(mort.lm1)

mort.lm2 <- lm(Mortality ~Nursing, data = mort)
summary(mort.lm2)

mort.aov <-aov(Mortality ~Doctors+Nursing, data = mort)
summary(mort.aov)

space <- par(mfrow=c(2,2),cex=0.8)
plot(mort.aov)

library(MASS)


#Residuals as Batches
space <- par(mfrow=c(2,2),cex=0.8)
plot(mort.lm$fitted.values, mort.lm$residuals, ylab= "Residuals", xlab = "Fitted Values",
     main = "Residuals vs Fitted")

hatvalues(mort.lm)
den <- sqrt(1-hatvalues(mort.lm))
adjmort <- mort.lm$residuals/den
plot(mort.lm$fitted.values, adjmort, ylab= "Adjusted Residuals", xlab = "Fitted Values",
     main = "Adjusted Residuals vs Fitted")

sdmort <- rstandard(mort.lm)
plot(mort.lm$fitted.values, sdmort, ylab= "Standardized Residuals", xlab = "Fitted Values",
     main = "Standardized Residuals vs Fitted")

mortsr <- studres(mort.lm)
plot(mort.lm$fitted.values,mortsr,ylab= "Studentized Residuals", xlab = "Fitted Values",
     main = "Studentized Residuals vs Fitted")

#Scaled Residuals Using Manual Coding
sdres <- mort.lm$residuals/sd(mort.lm$residuals)
plot(mort.lm$fitted.values, sdres,ylab= "Scaled Residuals (SD)", xlab = "Fitted Values",
     main = "Scaled Residuals (SD) vs Fitted")

library(DescTools)
mad <- MeanAD(mort.lm$residuals, center = Median)
rmad <- (mort.lm$residuals - Median(mort.lm$residuals))/mad
plot(mort.lm$fitted.values, rmad,ylab= "Scaled Residuals (MAD)", xlab = "Fitted Values",
     main = "Scaled Residuals (MAD) vs Fitted")

df <- IQR(mort.lm$residuals)
dfresid <- (mort.lm$residuals - Median(mort.lm$residuals))/df
plot(mort.lm$fitted.values, dfresid,ylab= "Scaled Residuals (DF)", xlab = "Fitted Values",
     main = "Scaled Residuals (DF) vs Fitted")

# Robust regression through M estimation.
# Using MASS package. Initial values through argument init may be provided or generated
# via "ls" (default) or "lts". Psi function through argument psi may be either psi.huber
# default), psi.hampel, or psi.bi-square. Parameters of the chosen psi function may be
# specified by plugging in the arguments of psi.huber(u, k=1.345, deriv=0), psi.hampel(u,
# a=2, b=4, c=8, deriv=0), or psi.bisquare(u, c=4.685, deriv=0), where the provided
# values are the defaults.
library(foreign)
library(MASS)
mortrobm<-rlm(Mortality~Doctors+Nursing, mort, init="ls", psi=psi.hampel, scale.est="Huber", method="M",
              model=TRUE, x.ret=TRUE, y.ret=TRUE, contrasts=NULL)
mortrobm
mortrobm$s
mortrobm$w
mortrobm$psi
mortrobm$converged


# Robust regression through MM estimation.
# Using MASS package.
mortrobmm<-rlm(Mortality~Doctors+Nursing, mort, init="ls", method="MM", model=TRUE, x.ret=TRUE,
               y.ret=TRUE, contrasts=NULL)
mortrobmm
mortrobmm$s
mortrobmm$w
mortrobmm$psi
mortrobmm$converged
# Using robustbase package.
library(robustbase)
mortrobmmrb<-lmrob(Mortality~Doctors+Nursing, mort, method="MM", model=TRUE, x=TRUE, y=TRUE,
                 setting="KS2014")
mortrobmmrb
mortrobmmrb$rweights
mortrobmmrb$coefficients
mortrobmmrb$scale
mortrobmmrb$residuals
mortrobmmrb$converged
mortrobmmrb$fitted.values
mortrobmmrb$init.S
mortrobmmrb$cov
mortrobmmrb$model
mortrobmmrb$x
mortrobmmrb$y
# Using robust package.
library(robust)
mortrobmmr<-lmRob(Mortality~Doctors+Nursing, mort, model=TRUE, x=TRUE, y=TRUE,
                 control=lmRob.control(estim="Final", weight=c("Bisquare", "Optimal")))
mortrobmmr
mortrobmmr$M.weights

# Robust regression through LMS estimation.
# Using package MASS.
mortroblms<-lqs(Mortality~Doctors+Nursing, mort, method="lms", model=TRUE, x.ret=FALSE, y.ret=FALSE,
                contrasts=NULL)
mortroblms
mortroblms$coefficients
mortroblms$fitted.values
mortroblms$residuals

# Robust regression through LTS estimation.
# Using package MASS.
mortroblts<-lqs(Mortality~Doctors+Nursing, mort, method="lts", model=TRUE, x.ret=FALSE, y.ret=FALSE,
                contrasts=NULL)
mortroblts
mortroblts$coefficients
mortroblts$fitted.values
mortroblts$residuals

# Robust regression through LQS estimation.
# Using package MASS.
mortroblqs<-lqs(Mortality~Doctors+Nursing, mort, method="lqs", model=TRUE, x.ret=FALSE, y.ret=FALSE,
                contrasts=NULL)
mortroblqs
mortroblqs$coefficients
mortroblqs$fitted.values
mortroblqs$residuals

