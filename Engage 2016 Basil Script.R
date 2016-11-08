setwd("/Users/karensamis/Google Drive/NSERC Engage/Methods and Data/Basil/GrowOuts/Data analysis")
setwd("/Users/mac/Google Drive/NSERC Engage/Methods and Data/Basil/GrowOuts/Data analysis")

EngB <- read.csv("Engage2016_Basil.csv") #all data

library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("lme4", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

str(EngB)

EngB$Rep <- as.factor(EngB$Rep)
EngB$Trtmt <- as.factor(EngB$Trtmt)
EngB$ExpTray <- as.factor(EngB$ExpTray)
EngB$Tray <- as.factor(EngB$Tray)
EngB$Row <- as.factor(EngB$Row)
EngB$Col <- as.factor(EngB$Col)
EngB$Survival <- as.factor(EngB$Survival)
EngB$Branches <- as.numeric(EngB$Branches)

#subset data by analysis column
EngBx <- subset(EngB, Analysis == "3") #by subsetting data by the Analysis column, I have
  #removed any questionable/missing data (1), data from burned bags (2), 
  #and any individuals that did not germinate/survive (0)

#subset EngBx data by rep
EngBx1 <- subset(EngBx, Rep == "1") #subset the analysis data by rep 1
EngBx2 <- subset(EngBx, Rep == "2") #subset the analysis data by rep 2
EngBx3 <- subset(EngBx, Rep == "3") #subset the analysis data by rep 3
EngBx23 <- subset(EngBx, Rep != "1") #subset the analysis data to include only rep 2 and 3


# Set Fuction to Summarize data
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#   data: a data frame.
#   measurevar: the name of a column that contains the variable to be summariezed
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}


##Histograms
#Height
par(mfrow = c(1,1))
hist(EngB$Height) #skew left
EngB$logHeight <- log10(EngB$Height+1)
hist(EngB$logHeight) #better than raw, but few columns *
EngB$sqrtHeight <- sqrt(EngB$Height+0.5)
hist(EngB$sqrtHeight) #better than raw, but not as nice as log
EngB$rankHeight <- rank(EngB$Height)

par(mfrow = c(1,1))
hist(EngBx$Height) #skew left, few columns
EngBx$logHeight <- log10(EngBx$Height+1)
hist(EngBx$logHeight) #better than raw, but few columns *
EngBx$sqrtHeight <- sqrt(EngBx$Height+0.5)
hist(EngBx$sqrtHeight) #better than raw, but not as nice as log
EngBx$rankHeight <- rank(EngBx$Height)

#Branches
par(mfrow = c(1,1))
hist(EngB$Branches) #pretty good... skew left *
EngB$LogBranches <- log10(EngB$Branches+1)
hist(EngB$LogBranches) #not as nice shape as raw
EngB$sqrtBranches <- sqrt(EngB$Branches+0.5)
hist(EngB$sqrtBranches) #not as nice as raw
EngB$rankBranches <- rank(EngB$Branches)

par(mfrow = c(1,1))
hist(EngBx$Branches) #pretty good... skew left *
EngBx$LogBranches <- log10(EngBx$Branches+1)
hist(EngBx$LogBranches) #not as nice as raw, skew right
EngBx$sqrtBranches <- sqrt(EngBx$Branches+0.5)
hist(EngBx$sqrtBranches) #not as nice as raw
EngBx$rankBranches <- rank(EngBx$Branches)

#Lvs
par(mfrow = c(1,1))
hist(EngB$Lvs) #skew left and few columns
EngB$LogLvs<- log10(EngB$Lvs+1)
hist(EngB$LogLvs) #skew right, better shape than raw
EngB$sqrtLvs <- sqrt(EngB$Lvs+0.5)
hist(EngB$sqrtLvs) #same shape as log, but more centred *
EngB$rankLvs <- rank(EngB$Lvs)

par(mfrow = c(1,1))
hist(EngBx$Lvs) #skew left and bad shape
EngBx$LogLvs<- log10(EngBx$Lvs+1)
hist(EngBx$LogLvs) #skew right, better shape than raw (bimodal)
EngBx$sqrtLvs <- sqrt(EngBx$Lvs+0.5)
hist(EngBx$sqrtLvs) #same shape as log, but more centred (bimodal) *
EngBx$rankLvs <- rank(EngBx$Lvs)

#WetWt
par(mfrow = c(1,1))
hist(EngB$WetWt) #skew left and few columns
EngB$LogWetWt<- log10(EngB$WetWt+1)
hist(EngB$LogWetWt) #better shape than raw, slight skew right
EngB$sqrtWetWt <- sqrt(EngB$WetWt+0.5)
hist(EngB$sqrtWetWt) #better shape than raw, fewer columns than log, but more centred *
EngB$rankWetWt <- rank(EngB$WetWt)

par(mfrow = c(1,1))
hist(EngBx$WetWt) #skew left and few columns
EngBx$LogWetWt<- log10(EngBx$WetWt+1)
hist(EngBx$LogWetWt) #better shape than raw, slight skew right
EngBx$sqrtWetWt <- sqrt(EngBx$WetWt+0.5)
hist(EngBx$sqrtWetWt) #better shape than raw, fewer columns than log, but more centred (possible bimodal data) *
EngBx$rankWetWt <- rank(EngBx$WetWt)

#DryWt
par(mfrow = c(1,1))
hist(EngB$DryWt) #skew left and few columns
EngB$LogDryWt<- log10(EngB$DryWt+1)
hist(EngB$LogDryWt) #more columns than raw but not great shape
EngB$sqrtDryWt <- sqrt(EngB$DryWt+0.5)
hist(EngB$sqrtDryWt) #more columns than raw, and a bit better shape than log (still not great) *
EngB$rankDryWt <- rank(EngB$DryWt)

par(mfrow = c(1,1))
hist(EngBx$DryWt) #skew left and few columns
EngBx$LogDryWt<- log10(EngBx$DryWt+1)
hist(EngBx$LogDryWt) #more columns than raw but not great shape
EngBx$sqrtDryWt <- sqrt(EngBx$DryWt+0.5)
hist(EngBx$sqrtDryWt) #more columns than raw, and a bit better shape than log (still not great)
EngBx$rankDryWt <- rank(EngBx$DryWt)

#WetLWt
par(mfrow = c(1,1))
hist(EngB$WetLWt) #few columns, not the worst shape, but not great
EngB$LogWetLWt<- log10(EngB$WetLWt+1)
hist(EngB$LogWetLWt) #good shape and more columns than raw, slight skew right *
EngB$sqrtWetLWt <- sqrt(EngB$WetLWt+0.5)
hist(EngB$sqrtWetLWt) #few columns, but centred and good shape
EngB$rankWetLWt <- rank(EngB$WetLWt) #*

par(mfrow = c(1,1))
hist(EngBx$WetLWt) #few columns, not the worst shape, but not great
EngBx$LogWetLWt<- log10(EngBx$WetLWt+1)
hist(EngBx$LogWetLWt) #good shape and more columns than raw, slight skew right *
EngBx$sqrtWetLWt <- sqrt(EngBx$WetLWt+0.5)
hist(EngBx$sqrtWetLWt) #few columns, but centred and good shape
EngBx$rankWetLWt <- rank(EngBx$WetLWt)

#DryLWt
par(mfrow = c(1,1))
hist(EngB$DryLWt) #few columns and skew left
EngB$LogDryLWt<- log10(EngB$DryLWt+1)
hist(EngB$LogDryLWt) #no better than raw
EngB$sqrtDryLWt <- sqrt(EngB$DryLWt+0.5)
hist(EngB$sqrtDryLWt) #no better than raw
EngB$rankDryLWt <- rank(EngB$DryLWt) #*

par(mfrow = c(1,1))
hist(EngBx$DryLWt) #few columns and skew left
EngBx$LogDryLWt<- log10(EngBx$DryLWt+1)
hist(EngBx$LogDryLWt) #no better than raw
EngBx$sqrtDryLWt <- sqrt(EngBx$DryLWt+0.5)
hist(EngBx$sqrtDryLWt) #no better than raw
EngBx$rankDryLWt <- rank(EngBx$DryLWt) #*

#WaterWt
par(mfrow = c(1,1))
hist(EngB$WaterWt) #few columns but shape is close to okay
EngB$LogWaterWt<- log10(EngB$WaterWt+1)
hist(EngB$LogWaterWt) #worse shape than raw,but more columns
EngB$sqrtWaterWt <- sqrt(EngB$WaterWt+0.5)
hist(EngB$sqrtWaterWt) #good shape *
EngB$rankWaterWt <- rank(EngB$WaterWt)

par(mfrow = c(1,1))
hist(EngBx$WaterWt) #few columns and not great shape
EngBx$LogWaterWt<- log10(EngBx$WaterWt+1)
hist(EngBx$LogWaterWt) #better shape than raw, but skew right and possible bimodal *
EngBx$sqrtWaterWt <- sqrt(EngBx$WaterWt+0.5)
hist(EngBx$sqrtWaterWt) #not as nice shape as log, but centered
EngBx$rankWaterWt <- rank(EngBx$WaterWt)

#LWaterWt
par(mfrow = c(1,1))
hist(EngB$LWaterWt) #shape not too bad
EngB$LogLWaterWt<- log10(EngB$LWaterWt+1)
hist(EngB$LogLWaterWt) #shape much better than raw but skew right
EngB$sqrtLWaterWt <- sqrt(EngB$LWaterWt+0.5)
hist(EngB$sqrtLWaterWt) #fewer columns but good shape *
EngB$rankLWaterWt <- rank(EngB$LWaterWt)

par(mfrow = c(1,1))
hist(EngBx$LWaterWt) #skew left
EngBx$LogLWaterWt<- log10(EngBx$LWaterWt+1)
hist(EngBx$LogLWaterWt) #shape much better than raw but skew right
EngBx$sqrtLWaterWt <- sqrt(EngBx$LWaterWt+0.5)
hist(EngBx$sqrtLWaterWt) #fewer columns but good shape *
EngBx$rankLWaterWt <- rank(EngBx$LWaterWt)

write.table(EngB, file = "Engage2016_Basil_all.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(EngBx, file = "Engage2016_Basil_3.csv", sep = ",", col.names = TRUE, row.names = FALSE)


#******************************************************************************
#Analysis of variation between trtmts while considering rep and tray
#*******************************
#Height
SumEngHx<- summarySE(EngBx, measurevar="logHeight", groupvars=c("Rep", "Trtmt")) 
GGEngHx <- ggplot(data=SumEngHx, aes(x=Trtmt, y=logHeight, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=logHeight-se, ymax=logHeight+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(Log[10]~Stem~Height~(cm))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Stem Height\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBHxTRTr <- lmer(logHeight~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBHxTRTr)
lmeEngBHxTR <- lmer(logHeight~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBHxTR, lmeEngBHxTRTr) #the removal of Tray was significant (p=0.023 Chisq=26.39)
lmeEngBHxTTr <- lmer(logHeight~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBHxTTr, lmeEngBHxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=622.04)
#therefore keep the random effect of Tray within Rep
lmeEngBHxRTr <- update(lmeEngBHxTRTr,~.-Trtmt)
anova(lmeEngBHxRTr, lmeEngBHxTRTr) #the effect of trtmt is significant after considering
#the variation explained by tray within rep (p=0.0017 chisq=15.15)

#check assumptions of best model
RlmeEngBHxTRTr <- resid(lmeEngBHxTRTr) 
FlmeEngBHxTRTr <- fitted(lmeEngBHxTRTr)
plot(FlmeEngBHxTRTr, RlmeEngBHxTRTr) #okay... gap between high and low fitted values
abline(h=0, col=c("red"))
hist(RlmeEngBHxTRTr) #not many columns visible, two very tall groups
qqnorm(RlmeEngBHxTRTr, main="Q-Q plot for residuals") 
qqline(RlmeEngBHxTRTr) #tails at either end

#outliers
RlmeEngBHxTRTr <- resid(lmeEngBHxTRTr)
SDRlmeEngBHxTRTr <- 3*sd(RlmeEngBHxTRTr)
ORlmeEngBHxTRTr <- ifelse(abs(RlmeEngBHxTRTr)>SDRlmeEngBHxTRTr, 1, 0)
plot(RlmeEngBHxTRTr, col=ORlmeEngBHxTRTr+1, pch=16, ylim=c(-1.5,1.5))
EngBxH <- EngBx[!ORlmeEngBHxTRTr,]
nrow(EngBxH) #1237 from 1254

SumEngHx2<- summarySE(EngBxH, measurevar="logHeight", groupvars=c("Rep", "Trtmt")) 
GGEngHx2 <- ggplot(data=SumEngHx2, aes(x=Trtmt, y=logHeight, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=logHeight-se, ymax=logHeight+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Stem~Height~(cm)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Stem Height\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBHxTRTr2 <- lmer(logHeight~Trtmt+(1+Tray|Rep), data=EngBxH)
summary(lmeEngBHxTRTr2)
lmeEngBHxTR2 <- lmer(logHeight~Trtmt+(1|Rep), data=EngBxH)
anova(lmeEngBHxTR2, lmeEngBHxTRTr2) #the removal of Tray was significant (p=<0.0001 Chisq=47.36)
lmeEngBHxTTr2 <- lmer(logHeight~Trtmt+(1|Tray), data=EngBxH)
anova(lmeEngBHxTTr2, lmeEngBHxTRTr2) #the removal of Rep was significant (p=<0.0001 chisq=942.71)
#therefore keep the random effect of Tray within Rep
lmeEngBHxRTr2 <- update(lmeEngBHxTRTr2,~.-Trtmt)
anova(lmeEngBHxRTr2, lmeEngBHxTRTr2) #the effect of trtmt is significant after considering
#the variation explained by tray within rep (p=<0.0001 chisq=24.37)

#check assumptions of best model
RlmeEngBHxTRTr2 <- resid(lmeEngBHxTRTr2) 
FlmeEngBHxTRTr2 <- fitted(lmeEngBHxTRTr2)
plot(FlmeEngBHxTRTr2, RlmeEngBHxTRTr2) #okay... gap between high and low fitted values
abline(h=0, col=c("red"))
hist(RlmeEngBHxTRTr2) #perfect
qqnorm(RlmeEngBHxTRTr2, main="Q-Q plot for residuals") 
qqline(RlmeEngBHxTRTr2) #tails at bottom end, but otherwise good

#*******************************
#Branches
SumEngBx<- summarySE(EngBx, measurevar="Branches", groupvars=c("Rep", "Trtmt")) 
GGEngBx <- ggplot(data=SumEngBx, aes(x=Trtmt, y=Branches, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=Branches-se, ymax=Branches+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab("Number of Branches") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil No. of Branches\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBBxTRTr <- lmer(Branches~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBBxTRTr)
lmeEngBBxTR <- lmer(Branches~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBBxTR, lmeEngBBxTRTr) #the removal of Tray was non-significant (p=0.33 Chisq=15.74)
lmeEngBBxTTr <- lmer(Branches~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBBxTTr, lmeEngBBxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=68.17)
#therefore keep the random effect of Rep
lmeEngBBxR <- update(lmeEngBBxTR,~.-Trtmt)
anova(lmeEngBBxR, lmeEngBBxTR) #the effect of trtmt is non-significant after considering
#the variation explained by rep (p=0.102 chisq=6.21)

#check assumptions of best model
RlmeEngBBxTR <- resid(lmeEngBBxTR) 
FlmeEngBBxTR <- fitted(lmeEngBBxTR)
plot(FlmeEngBBxTR, RlmeEngBBxTR) #pretty good 
abline(h=0, col=c("red"))
hist(RlmeEngBBxTR) #good distribution
qqnorm(RlmeEngBBxTR, main="Q-Q plot for residuals") 
qqline(RlmeEngBBxTR) #tails at either end

#outliers
RlmeEngBBxTR <- resid(lmeEngBBxTR)
SDRlmeEngBBxTR <- 3*sd(RlmeEngBBxTR)
ORlmeEngBBxTR <- ifelse(abs(RlmeEngBBxTR)>SDRlmeEngBBxTR, 1, 0)
plot(RlmeEngBBxTR, col=ORlmeEngBBxTR+1, pch=16, ylim=c(-15,15))
EngBxB <- EngBx[!ORlmeEngBBxTR,]
nrow(EngBxB) #1238 from 1254

SumEngBx2<- summarySE(EngBxB, measurevar="Branches", groupvars=c("Rep", "Trtmt")) 
GGEngBx2 <- ggplot(data=SumEngBx2, aes(x=Trtmt, y=Branches, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=Branches-se, ymax=Branches+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab("Number of Branches") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil No. of Branches\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBBxTRTr2 <- lmer(Branches~Trtmt+(1+Tray|Rep), data=EngBxB)
summary(lmeEngBBxTRTr2)
lmeEngBBxTR2 <- lmer(Branches~Trtmt+(1|Rep), data=EngBxB)
anova(lmeEngBBxTR2, lmeEngBBxTRTr2) #the removal of Tray was non-significant (p=0.28 Chisq=16.47)
lmeEngBBxTTr2 <- lmer(Branches~Trtmt+(1|Tray), data=EngBxB)
anova(lmeEngBBxTTr2, lmeEngBBxTRTr2) #the removal of Rep was significant (p=<0.0001 chisq=103.15)
#therefore keep the random effect of Rep
lmeEngBBxR2 <- update(lmeEngBBxTR2,~.-Trtmt)
anova(lmeEngBBxR2, lmeEngBBxTR2) #the effect of trtmt is significant after considering
#the variation explained by tray within rep (p=0.046 chisq=7.98)

#check assumptions of best model
RlmeEngBBxTR2 <- resid(lmeEngBBxTR2) 
FlmeEngBBxTR2 <- fitted(lmeEngBBxTR2)
plot(FlmeEngBBxTR2, RlmeEngBBxTR2) #pretty good 
abline(h=0, col=c("red"))
hist(RlmeEngBBxTR2) #good distribution
qqnorm(RlmeEngBBxTR2, main="Q-Q plot for residuals") 
qqline(RlmeEngBBxTR2) #small tails at either end, but better than before removing outliers


#*******************************
#Lvs
SumEngLx<- summarySE(EngBx, measurevar="sqrtLvs", groupvars=c("Rep", "Trtmt")) 
GGEngLx <- ggplot(data=SumEngLx, aes(x=Trtmt, y=sqrtLvs, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtLvs-se, ymax=sqrtLvs+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Number~of~Leaves)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil No. of Lvs\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBLxTRTr <- lmer(sqrtLvs~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBLxTRTr)
lmeEngBLxTR <- lmer(sqrtLvs~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBLxTR, lmeEngBLxTRTr) #the removal of Tray was non-significant (p=0.798 Chisq=9.49)
lmeEngBLxTTr <- lmer(sqrtLvs~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBLxTTr, lmeEngBLxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=380.72)
#therefore keep the random effect of Rep
lmeEngBLxR <- update(lmeEngBLxTR,~.-Trtmt)
anova(lmeEngBLxR, lmeEngBLxTR) #the effect of trtmt is non-significant after considering
#the variation explained by rep (p=0.58 chisq=1.98)

#check assumptions of best model
RlmeEngBLxTR <- resid(lmeEngBLxTR) 
FlmeEngBLxTR <- fitted(lmeEngBLxTR)
plot(FlmeEngBLxTR, RlmeEngBLxTR) #pretty good, but gap in middle
abline(h=0, col=c("red"))
hist(RlmeEngBLxTR) #good distribution
qqnorm(RlmeEngBLxTR, main="Q-Q plot for residuals") 
qqline(RlmeEngBLxTR) # small tails at either end

#outliers
RlmeEngBLxTR <- resid(lmeEngBLxTR)
SDRlmeEngBLxTR <- 3*sd(RlmeEngBLxTR)
ORlmeEngBLxTR <- ifelse(abs(RlmeEngBLxTR)>SDRlmeEngBLxTR, 1, 0)
plot(RlmeEngBLxTR, col=ORlmeEngBLxTR+1, pch=16, ylim=c(-5,5))
EngBxL <- EngBx[!ORlmeEngBLxTR,]
nrow(EngBxL) #1247 from 1254

SumEngLx2<- summarySE(EngBxL, measurevar="sqrtLvs", groupvars=c("Rep", "Trtmt")) 
GGEngLx2 <- ggplot(data=SumEngLx2, aes(x=Trtmt, y=sqrtLvs, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtLvs-se, ymax=sqrtLvs+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Number~of~Leaves)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil No. of Lvs\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBLxTRTr2 <- lmer(sqrtLvs~Trtmt+(1+Tray|Rep), data=EngBxL)
summary(lmeEngBLxTRTr2)
lmeEngBLxTR2 <- lmer(sqrtLvs~Trtmt+(1|Rep), data=EngBxL)
anova(lmeEngBLxTR2, lmeEngBLxTRTr2) #the removal of Tray was non-significant (p=0.789 Chisq=9.62)
lmeEngBLxTTr2 <- lmer(sqrtLvs~Trtmt+(1|Tray), data=EngBxL)
anova(lmeEngBLxTTr2, lmeEngBLxTRTr2) #the removal of Rep was significant (p=<0.0001 chisq=417.11)
#therefore keep the random effect of Rep
lmeEngBLxR2 <- update(lmeEngBLxTR2,~.-Trtmt)
anova(lmeEngBLxR2, lmeEngBLxTR2) #the effect of trtmt is non-significant after considering
#the variation explained by rep (p=0.59 chisq=1.91)

#check assumptions of best model
RlmeEngBLxTR2 <- resid(lmeEngBLxTR2) 
FlmeEngBLxTR2 <- fitted(lmeEngBLxTR2)
plot(FlmeEngBLxTR2, RlmeEngBLxTR2) #pretty good, but gap in middle 
abline(h=0, col=c("red"))
hist(RlmeEngBLxTR2) #good distribution
qqnorm(RlmeEngBLxTR2, main="Q-Q plot for residuals") 
qqline(RlmeEngBLxTR2) # better than with outliners included


#*******************************
#WetWt
SumEngWWx<- summarySE(EngBx, measurevar="sqrtWetWt", groupvars=c("Rep", "Trtmt")) 
GGEngWWx <- ggplot(data=SumEngWWx, aes(x=Trtmt, y=sqrtWetWt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtWetWt-se, ymax=sqrtWetWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Harvest~Weight~(g))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil WetWt\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBWWxTRTr <- lmer(sqrtWetWt~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBWWxTRTr)
lmeEngBWWxTR <- lmer(sqrtWetWt~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBWWxTR, lmeEngBWWxTRTr) #the removal of Tray was non-significant (p=0.405 Chisq=14.61)
lmeEngBWWxTTr <- lmer(sqrtWetWt~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBWWxTTr, lmeEngBWWxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=749.97)
#therefore keep the random effect of Rep
lmeEngBWWxR <- update(lmeEngBWWxTR,~.-Trtmt)
anova(lmeEngBWWxR, lmeEngBWWxTR) #the effect of trtmt is significant after considering
#the variation explained by rep (p=0.025 chisq=9.35)

#check assumptions of best model
RlmeEngBWWxTR <- resid(lmeEngBWWxTR) 
FlmeEngBWWxTR <- fitted(lmeEngBWWxTR)
plot(FlmeEngBWWxTR, RlmeEngBWWxTR) #pretty good, but gap in middle
abline(h=0, col=c("red"))
hist(RlmeEngBWWxTR) #good distribution
qqnorm(RlmeEngBWWxTR, main="Q-Q plot for residuals") 
qqline(RlmeEngBWWxTR) # big tails at either end

#outliers
RlmeEngBWWxTR <- resid(lmeEngBWWxTR)
SDRlmeEngBWWxTR <- 3*sd(RlmeEngBWWxTR)
ORlmeEngBWWxTR <- ifelse(abs(RlmeEngBWWxTR)>SDRlmeEngBWWxTR, 1, 0)
plot(RlmeEngBWWxTR, col=ORlmeEngBWWxTR+1, pch=16, ylim=c(-5,5))
EngBxWW <- EngBx[!ORlmeEngBWWxTR,]
nrow(EngBxWW) #1231 from 1254


SumEngWWx2<- summarySE(EngBxWW, measurevar="sqrtWetWt", groupvars=c("Rep", "Trtmt")) 
GGEngWWx2 <- ggplot(data=SumEngWWx2, aes(x=Trtmt, y=sqrtWetWt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtWetWt-se, ymax=sqrtWetWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Harvest~Weight~(g))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil WetWt\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBWWxTRTr2 <- lmer(sqrtWetWt~Trtmt+(1+Tray|Rep), data=EngBxWW)
summary(lmeEngBWWxTRTr2)
lmeEngBWWxTR2 <- lmer(sqrtWetWt~Trtmt+(1|Rep), data=EngBxWW)
anova(lmeEngBWWxTR2, lmeEngBWWxTRTr2) #the removal of Tray was marginally non-significant (p=0.071 Chisq=22.396)
lmeEngBWWxTTr2 <- lmer(sqrtWetWt~Trtmt+(1|Tray), data=EngBxWW)
anova(lmeEngBWWxTTr2, lmeEngBWWxTRTr2) #the removal of Rep was significant (p=<0.0001 chisq=932.37)
#therefore keep the random effect of Rep
lmeEngBWWxR2 <- update(lmeEngBWWxTR2,~.-Trtmt)
anova(lmeEngBWWxR2, lmeEngBWWxTR2) #the effect of trtmt is significant after considering
#the variation explained by rep (p=0.00898 chisq=11.578)

#check assumptions of best model
RlmeEngBWWxTR2 <- resid(lmeEngBWWxTR2) 
FlmeEngBWWxTR2 <- fitted(lmeEngBWWxTR2)
plot(FlmeEngBWWxTR2, RlmeEngBWWxTR2) #pretty good, but gap in middle
abline(h=0, col=c("red"))
hist(RlmeEngBWWxTR2) #good distribution
qqnorm(RlmeEngBWWxTR2, main="Q-Q plot for residuals") 
qqline(RlmeEngBWWxTR2) # large tails at either end but smaller than before removing outliers


#*******************************
#DryWt
SumEngDW <- summarySE(EngBx, measurevar="rankDryWt", groupvars=c("Rep", "Trtmt")) 
GGEngDW <- ggplot(data=SumEngDW, aes(x=Trtmt, y=rankDryWt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=rankDryWt-se, ymax=rankDryWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab("Ranked Dry Weight (g)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil DryWt\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBDWxTRTr <- lmer(rankDryWt~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBDWxTRTr)
lmeEngBDWxTR <- lmer(rankDryWt~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBDWxTR, lmeEngBDWxTRTr) #the removal of Tray was non-significant (p=0.584 Chisq=12.27)
lmeEngBDWxTTr <- lmer(rankDryWt~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBDWxTTr, lmeEngBDWxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=143.7)
#therefore keep the random effect of Rep
lmEngBDWxT <- lm(rankDryWt~Trtmt, data=EngBx)
x <- -2*logLik(lmEngBDWxT, REML=T) +2*logLik(lmeEngBDWxTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=153.04, p=<0.0001, random effect of Rep was sig
AIC(lmEngBDWxT) #=18284.88
AIC(lmeEngBDWxTR) #=18102.54
#Therefore Rep needs to be included in the model as random effect
lmeEngBDWxR <- update(lmeEngBDWxTR,~.-Trtmt)
anova(lmeEngBDWxR, lmeEngBDWxTR) #the effect of trtmt is significant after considering
#the variation explained by rep (p=<0.0001 chisq=64.023)

#check assumptions of best model
RlmeEngBDWxTR <- resid(lmeEngBDWxTR) 
FlmeEngBDWxTR <- fitted(lmeEngBDWxTR)
plot(FlmeEngBDWxTR, RlmeEngBDWxTR) #pretty good, slight downward trend
abline(h=0, col=c("red"))
hist(RlmeEngBDWxTR) #good distribution
qqnorm(RlmeEngBDWxTR, main="Q-Q plot for residuals") 
qqline(RlmeEngBDWxTR) # okay but tails at either end

#outliers
RlmeEngBDWxTR <- resid(lmeEngBDWxTR)
SDRlmeEngBDWxTR <- 3*sd(RlmeEngBDWxTR)
ORlmeEngBDWxTR <- ifelse(abs(RlmeEngBDWxTR)>SDRlmeEngBDWxTR, 1, 0)
plot(RlmeEngBDWxTR, col=ORlmeEngBDWxTR+1, pch=16, ylim=c(-1000,1000))
EngBxDW <- EngBx[!ORlmeEngBDWxTR,]
nrow(EngBxDW) #1254 from 1254... no outliers


#*******************************
#WetLWt
SumEngLWW <- summarySE(EngBx, measurevar="LogWetLWt", groupvars=c("Rep", "Trtmt")) 
GGEngLWW <- ggplot(data=SumEngLWW, aes(x=Trtmt, y=LogWetLWt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=LogWetLWt-se, ymax=LogWetLWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Leaf~Harvest~Weight~(g)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Leaf WetWt\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBLWWxTRTr <- lmer(LogWetLWt~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBLWWxTRTr)
lmeEngBLWWxTR <- lmer(LogWetLWt~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBLWWxTR, lmeEngBLWWxTRTr) #the removal of Tray was non-significant (p=0.764 Chisq=9.97)
lmeEngBLWWxTTr <- lmer(LogWetLWt~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBLWWxTTr, lmeEngBLWWxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=496.15)
#therefore keep the random effect of Rep
lmEngBLWWxT <- lm(LogWetLWt~Trtmt, data=EngBx)
x <- -2*logLik(lmEngBLWWxT, REML=T) +2*logLik(lmeEngBLWWxTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=529.77, p=<0.0001, random effect of Rep was sig
AIC(lmEngBLWWxT) #=-360.02
AIC(lmeEngBLWWxTR) #=-859.63
#Therefore Rep needs to be included in the model as random effect
lmeEngBLWWxR <- update(lmeEngBLWWxTR,~.-Trtmt)
anova(lmeEngBLWWxR, lmeEngBLWWxTR) #the effect of trtmt is marginally non- significant after considering
#the variation explained by rep (p=0.095 chisq=6.38)

#check assumptions of best model
RlmeEngBLWWxTR <- resid(lmeEngBLWWxTR) 
FlmeEngBLWWxTR <- fitted(lmeEngBLWWxTR)
plot(FlmeEngBLWWxTR, RlmeEngBLWWxTR) #pretty good, slight downward trend
abline(h=0, col=c("red"))
hist(RlmeEngBLWWxTR) #good distribution
qqnorm(RlmeEngBLWWxTR, main="Q-Q plot for residuals") 
qqline(RlmeEngBLWWxTR) # okay but tails at either end

#outliers
RlmeEngBLWWxTR <- resid(lmeEngBLWWxTR)
SDRlmeEngBLWWxTR <- 3*sd(RlmeEngBLWWxTR)
ORlmeEngBLWWxTR <- ifelse(abs(RlmeEngBLWWxTR)>SDRlmeEngBLWWxTR, 1, 0)
plot(RlmeEngBLWWxTR, col=ORlmeEngBLWWxTR+1, pch=16, ylim=c(-1000,1000))
EngBxLWW <- EngBx[!ORlmeEngBLWWxTR,]
nrow(EngBxLWW) #1254 from 1254... no outliers


#*******************************
#DryLWt
SumEngLDW <- summarySE(EngBx, measurevar="rankDryLWt", groupvars=c("Rep", "Trtmt")) 
GGEngLDW <- ggplot(data=SumEngLDW, aes(x=Trtmt, y=rankDryLWt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=rankDryLWt-se, ymax=rankDryLWt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab("Ranked Leaf Dry Weight (g)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Leaf DryWt\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBLDWxTRTr <- lmer(rankDryLWt~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBLDWxTRTr)
lmeEngBLDWxTR <- lmer(rankDryLWt~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBLDWxTR, lmeEngBLDWxTRTr) #the removal of Tray was non-significant (p=0.122 Chisq=20.26)
lmeEngBLDWxTTr <- lmer(rankDryLWt~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBLDWxTTr, lmeEngBLDWxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=124.54)
#therefore keep the random effect of Rep
lmEngBLDWxT <- lm(rankDryLWt~Trtmt, data=EngBx)
x <- -2*logLik(lmEngBLDWxT, REML=T) +2*logLik(lmeEngBLDWxTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=118.102, p=<0.0001, random effect of Rep was sig
AIC(lmEngBLDWxT) #=18231.26
AIC(lmeEngBLDWxTR) #=18084.02
#Therefore Rep needs to be included in the model as random effect
lmeEngBLDWxR <- update(lmeEngBLDWxTR,~.-Trtmt)
anova(lmeEngBLDWxR, lmeEngBLDWxTR) #the effect of trtmt is significant after considering
#the variation explained by rep (p=<0.0001 chisq=113.08)

#check assumptions of best model
RlmeEngBLDWxTR <- resid(lmeEngBLDWxTR) 
FlmeEngBLDWxTR <- fitted(lmeEngBLDWxTR)
plot(FlmeEngBLDWxTR, RlmeEngBLDWxTR) #pretty good, slight downward trend
abline(h=0, col=c("red"))
hist(RlmeEngBLDWxTR) #not the best, but ok
qqnorm(RlmeEngBLDWxTR, main="Q-Q plot for residuals") 
qqline(RlmeEngBLDWxTR) # tails at either end

#outliers
RlmeEngBLDWxTR <- resid(lmeEngBLDWxTR)
SDRlmeEngBLDWxTR <- 3*sd(RlmeEngBLDWxTR)
ORlmeEngBLDWxTR <- ifelse(abs(RlmeEngBLDWxTR)>SDRlmeEngBLDWxTR, 1, 0)
plot(RlmeEngBLDWxTR, col=ORlmeEngBLDWxTR+1, pch=16, ylim=c(-1000,1000))
EngBxLDW <- EngBx[!ORlmeEngBLDWxTR,]
nrow(EngBxLDW) #1254 from 1254... no outliers


#*******************************
#WatC
EngBx$WatC <- ((EngBx$WaterWt/EngBx$WetWt)*100)
hist(EngBx$WatC) #few columns and skew right
EngBx$LogWatC <- log10(EngBx$WatC+1)
hist(EngBx$LogWatC) #better but still skew right
EngBx$sqrtWatC <- sqrt(EngBx$WatC+0.5)
hist(EngBx$sqrtWatC) #better than log and raw, but still skew right *
EngBx$rankWatC <- rank(EngBx$WatC)
write.table(EngBx, file = "Engage2016_Basil_AnalysisSet.csv", sep = ",", col.names = TRUE, row.names = FALSE)


SumEngWC <- summarySE(EngBx, measurevar="sqrtWatC", groupvars=c("Rep", "Trtmt")) 
GGEngWC <- ggplot(data=SumEngWC, aes(x=Trtmt, y=sqrtWatC, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtWatC-se, ymax=sqrtWatC+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Water~Content~("%"))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBWCxTRTr <- lmer(sqrtWatC~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBWCxTRTr)
lmeEngBWCxTR <- lmer(sqrtWatC~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBWCxTR, lmeEngBWCxTRTr) #the removal of Tray was significant (p=0.0018 Chisq=34.32)
lmeEngBWCxTTr <- lmer(sqrtWatC~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBWCxTTr, lmeEngBWCxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=361.26)
#therefore keep the random effect of Rep and Tray
lmEngBWCxT <- lm(sqrtWatC~Trtmt, data=EngBx)
x <- -2*logLik(lmEngBWCxT, REML=T) +2*logLik(lmeEngBWCxTRTr, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=372.57, p=<0.0001, random effect of Rep and Tray was sig
AIC(lmEngBWCxT) #=460.04
AIC(lmeEngBWCxTRTr) #=143.02
#Therefore Rep and tray need to be included in the model as random effects
lmeEngBWCxRTr <- update(lmeEngBWCxTRTr,~.-Trtmt)
anova(lmeEngBWCxRTr, lmeEngBWCxTRTr) #the effect of trtmt is significant after considering
#the variation explained by rep (p=<0.0001 chisq=148.05)

#check assumptions of best model
RlmeEngBWCxTRTr <- resid(lmeEngBWCxTRTr) 
FlmeEngBWCxTRTr <- fitted(lmeEngBWCxTRTr)
plot(FlmeEngBWCxTRTr, RlmeEngBWCxTRTr) #pretty good, slight trend
abline(h=0, col=c("red"))
hist(RlmeEngBWCxTRTr) #okay... skewed right as expected
qqnorm(RlmeEngBWCxTRTr, main="Q-Q plot for residuals") 
qqline(RlmeEngBWCxTRTr) # slight tail at bottom end

#outliers
RlmeEngBWCxTRTr <- resid(lmeEngBWCxTRTr)
SDRlmeEngBWCxTRTr <- 3*sd(RlmeEngBWCxTRTr)
ORlmeEngBWCxTRTr <- ifelse(abs(RlmeEngBWCxTRTr)>SDRlmeEngBWCxTRTr, 1, 0)
plot(RlmeEngBWCxTRTr, col=ORlmeEngBWCxTRTr+1, pch=16, ylim=c(-1000,1000))
EngBxWC <- EngBx[!ORlmeEngBWCxTRTr,]
nrow(EngBxWC) #1248 from 1254


SumEngWC2 <- summarySE(EngBxWC, measurevar="sqrtWatC", groupvars=c("Rep", "Trtmt")) 
GGEngWC2 <- ggplot(data=SumEngWC2, aes(x=Trtmt, y=sqrtWatC, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtWatC-se, ymax=sqrtWatC+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Water~Content~("%"))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBWCxTRTr2 <- lmer(sqrtWatC~Trtmt+(1+Tray|Rep), data=EngBxWC)
summary(lmeEngBWCxTRTr2)
lmeEngBWCxTR2 <- lmer(sqrtWatC~Trtmt+(1|Rep), data=EngBxWC)
anova(lmeEngBWCxTR2, lmeEngBWCxTRTr2) #the removal of Tray was significant (p=<0.0001 Chisq=44.701)
lmeEngBWCxTTr2 <- lmer(sqrtWatC~Trtmt+(1|Tray), data=EngBxWC)
anova(lmeEngBWCxTTr2, lmeEngBWCxTRTr2) #the removal of Rep was significant (p=<0.0001 chisq=388.11)
#therefore keep the random effect of Rep and Tray
lmEngBWCxT2 <- lm(sqrtWatC~Trtmt, data=EngBxWC)
x <- -2*logLik(lmEngBWCxT2, REML=T) +2*logLik(lmeEngBWCxTRTr2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=398.053, p=<0.0001, random effect of Rep and Tray was sig
AIC(lmEngBWCxT2) #=363.798
AIC(lmeEngBWCxTRTr2) #=21.58
#Therefore Rep and tray need to be included in the model as random effects
lmeEngBWCxRTr2 <- update(lmeEngBWCxTRTr2,~.-Trtmt)
anova(lmeEngBWCxRTr2, lmeEngBWCxTRTr2) #the effect of trtmt is significant after considering
#the variation explained by rep (p=<0.0001 chisq=157.21)

#check assumptions of best model
RlmeEngBWCxTRTr2 <- resid(lmeEngBWCxTRTr2) 
FlmeEngBWCxTRTr2 <- fitted(lmeEngBWCxTRTr2)
plot(FlmeEngBWCxTRTr2, RlmeEngBWCxTRTr2) #pretty good, slight trend but better than before removing outliers
abline(h=0, col=c("red"))
hist(RlmeEngBWCxTRTr2) #great
qqnorm(RlmeEngBWCxTRTr2, main="Q-Q plot for residuals") 
qqline(RlmeEngBWCxTRTr2) #great


#*******************************
#LWatC
EngBx$LWatC <- ((EngBx$LWaterWt/EngBx$WetLWt)*100)
hist(EngBx$LWatC) #few columns and skew right
EngBx$LogLWatC <- log10(EngBx$LWatC+1)
hist(EngBx$LogLWatC) #better but still skew right
EngBx$sqrtLWatC <- sqrt(EngBx$LWatC+0.5)
hist(EngBx$sqrtLWatC) #bit better than log and raw, but still skew right *
EngBx$rankLWatC <- rank(EngBx$LWatC)
write.table(EngBx, file = "Engage2016_Basil_AnalysisSet.csv", sep = ",", col.names = TRUE, row.names = FALSE)


SumEngLWC <- summarySE(EngBx, measurevar="sqrtLWatC", groupvars=c("Rep", "Trtmt")) 
GGEngLWC <- ggplot(data=SumEngLWC, aes(x=Trtmt, y=sqrtLWatC, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtLWatC-se, ymax=sqrtLWatC+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Leaf~Water~Content~("%"))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Leaf Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBLWCxTRTr <- lmer(sqrtLWatC~Trtmt+(1+Tray|Rep), data=EngBx)
summary(lmeEngBLWCxTRTr)
lmeEngBLWCxTR <- lmer(sqrtLWatC~Trtmt+(1|Rep), data=EngBx)
anova(lmeEngBLWCxTR, lmeEngBLWCxTRTr) #the removal of Tray was significant (p=<0.0001 Chisq=73.87)
lmeEngBLWCxTTr <- lmer(sqrtLWatC~Trtmt+(1|Tray), data=EngBx)
anova(lmeEngBLWCxTTr, lmeEngBLWCxTRTr) #the removal of Rep was significant (p=<0.0001 chisq=400.5)
#therefore keep the random effect of Rep and Tray
lmEngBLWCxT <- lm(sqrtLWatC~Trtmt, data=EngBx)
x <- -2*logLik(lmEngBLWCxT, REML=T) +2*logLik(lmeEngBLWCxTRTr, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=413.027, p=<0.0001, random effect of Rep and Tray was sig
AIC(lmEngBLWCxT) #=976.83
AIC(lmeEngBLWCxTRTr) #=617.701
#Therefore Rep and tray need to be included in the model as random effects
lmeEngBLWCxRTr <- update(lmeEngBLWCxTRTr,~.-Trtmt)
anova(lmeEngBLWCxRTr, lmeEngBLWCxTRTr) #the effect of trtmt is significant after considering
#the variation explained by rep (p=<0.0001 chisq=155.61)

#check assumptions of best model
RlmeEngBLWCxTRTr <- resid(lmeEngBLWCxTRTr) 
FlmeEngBLWCxTRTr <- fitted(lmeEngBLWCxTRTr)
plot(FlmeEngBLWCxTRTr, RlmeEngBLWCxTRTr) #pretty good, slight trend
abline(h=0, col=c("red"))
hist(RlmeEngBLWCxTRTr) #not great... few columns and skew right
qqnorm(RlmeEngBLWCxTRTr, main="Q-Q plot for residuals") 
qqline(RlmeEngBLWCxTRTr) # slight tail at bottom end

#outliers
RlmeEngBLWCxTRTr <- resid(lmeEngBLWCxTRTr)
SDRlmeEngBLWCxTRTr <- 3*sd(RlmeEngBLWCxTRTr)
ORlmeEngBLWCxTRTr <- ifelse(abs(RlmeEngBLWCxTRTr)>SDRlmeEngBLWCxTRTr, 1, 0)
plot(RlmeEngBLWCxTRTr, col=ORlmeEngBLWCxTRTr+1, pch=16, ylim=c(-1000,1000))
EngBxLWC <- EngBx[!ORlmeEngBLWCxTRTr,]
nrow(EngBxLWC) #1248 from 1254


SumEngLWC2 <- summarySE(EngBxLWC, measurevar="sqrtLWatC", groupvars=c("Rep", "Trtmt")) 
GGEngLWC2 <- ggplot(data=SumEngLWC2, aes(x=Trtmt, y=sqrtLWatC, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtLWatC-se, ymax=sqrtLWatC+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Leaf~Water~Content~("%"))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Basil Leaf Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngBLWCxTRTr2 <- lmer(sqrtLWatC~Trtmt+(1+Tray|Rep), data=EngBxLWC)
summary(lmeEngBLWCxTRTr2)
lmeEngBLWCxTR2 <- lmer(sqrtLWatC~Trtmt+(1|Rep), data=EngBxLWC)
anova(lmeEngBLWCxTR2, lmeEngBLWCxTRTr2) #the removal of Tray was significant (p=<0.0001 Chisq=68.55)
lmeEngBLWCxTTr2 <- lmer(sqrtLWatC~Trtmt+(1|Tray), data=EngBxLWC)
anova(lmeEngBLWCxTTr2, lmeEngBLWCxTRTr2) #the removal of Rep was significant (p=<0.0001 chisq=418.82)
#therefore keep the random effect of Rep and Tray
lmEngBLWCxT2 <- lm(sqrtLWatC~Trtmt, data=EngBxLWC)
x <- -2*logLik(lmEngBLWCxT2, REML=T) +2*logLik(lmeEngBLWCxTRTr2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=434.42, p=<0.0001, random effect of Rep and Tray was sig
AIC(lmEngBLWCxT2) #=857.85
AIC(lmeEngBLWCxTRTr2) #=477.68
#Therefore Rep and tray need to be included in the model as random effects
lmeEngBLWCxRTr2 <- update(lmeEngBLWCxTRTr2,~.-Trtmt)
anova(lmeEngBLWCxRTr2, lmeEngBLWCxTRTr2) #the effect of trtmt is significant after considering
#the variation explained by rep (p=<0.0001 chisq=181.63)

#check assumptions of best model
RlmeEngBLWCxTRTr2 <- resid(lmeEngBLWCxTRTr2) 
FlmeEngBLWCxTRTr2 <- fitted(lmeEngBLWCxTRTr2)
plot(FlmeEngBLWCxTRTr2, RlmeEngBLWCxTRTr2) #better than with outliers included
abline(h=0, col=c("red"))
hist(RlmeEngBLWCxTRTr2) #great
qqnorm(RlmeEngBLWCxTRTr2, main="Q-Q plot for residuals") 
qqline(RlmeEngBLWCxTRTr2) # slight tail at bottom end still, but better
