#Official R-code for the perception Data:

#Packages:
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(effects)
require(lattice)

#install.packages("pbkrtest")
library("pbkrtest")

#Possibly not needed
library(Hmisc)

ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue") 
    return(p)
  }
  
  lapply(re, f)
}




perception <- read.csv('Perception_all_Data.csv', h=TRUE)

#Remove all temp, bp and humidity (not using in analysis) and 24 hour counts
#Note: Humidity showed to have some effect but beacues this is only three readings over 48 hours, day effects should incorperate changing humiduty throughout the day

perception <- subset(perception, select = -c(Spider_24, Not_Spider_24, Outside_24, Humidity_start, Humidity_24, Humidity_48, Temp_start, Temp_24, Temp_48, BP_Start, BP_24, BP_48) )

#DGRP as factor

perception$DGRP <- factor(perception$DGRP)

#Total those assorted into vials
perception$spiANDno_48 <- with(perception, Spider_48 + Not_Spider_48)

#Subsample of those with more than six total in vials
perception <- subset(perception, spiANDno_48 >= 6)

#New data frame with needed variables (and change some names):
DGRP_by_counts <- with(perception,
                       data.frame(DGRP, Sex, Spider=Spider_48,
                                  Not_spider=Not_Spider_48, Outside=Outside_48,
                                  Date=Start_date, Vial_total= spiANDno_48, Number=Number))

DGRP_by_counts$proportion_spider <- with(DGRP_by_counts, Spider/Vial_total)

#Data set with all DGRP lines together into one group (by sex also): inclueds mean temperature scales (and humidity) and the mean proportion with spiders and SE

DGRP_sub <-DGRP_by_counts %>%
  group_by(DGRP, Sex) %>%
  summarise(prop_spider=mean(proportion_spider),
            se =((sd(proportion_spider))/sqrt(n())))


###Sex Correlation:
with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

MFcor <- with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

##rcorr:
#??Hmisc
with(DGRP_sub, rcorr(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"]))

line2 <- lm(DGRP_sub$prop_spider[DGRP_sub$Sex == "Female"] ~ DGRP_sub$prop_spider[DGRP_sub$Sex == "Male"])

with(DGRP_sub, plot(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"], xlab = "Females", ylab = "Males", abline(line2), main = "Male Female correlation: proportion with spider"))

legend("bottomleft", bty="y", 
       legend=paste("R = -0.28" , "p-value = 0.0321" ))


#Model:
## DGRP is a random effect

#Remove date or temp,hum,bp:  Redundant to have both?

mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex  + (1|Date) 
              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")
summary(mod1)

rr1 <- ranef(mod1, condVar = TRUE)
head(rr1)

pv <- attr(rr1$DGRP, "postVar")
pv
se_fem <- pv[1, 1, ]
se_ma <- pv[2,2, ]
#Data frame of intercepts
rand.interc<-rr1$DGRP
rand.interc
df<-data.frame(Intercepts=rr1$DGRP[,1:2], DGRP=rownames(rand.interc), se_female=se_fem, se_male=se_ma)
colnames(df) <- c("Female_Int", "Male_Int", "DGRP", "Female_se", "Male_se")
df$m.low <- with(df, Male_Int - 2 * Male_se)
df$m.high <- with(df, Male_Int + 2 * Male_se)
df$f.low <- with(df, Female_Int - 2 * Female_se)
df$f.high <- with(df, Female_Int + 2 * Female_se)

colMeans(ranef(mod1)$DGRP)
df$Male_scale  <- df$Male_Int + (0.08348933)
df$Female_scale  <- df$Female_Int + 0.06116928
df$m.scale.low  <- df$m.low + 0.5
df$m.scale.high  <- df$m.high + 0.5
df$f.scale.low  <- df$f.low + 0.5
df$f.scale.high  <- df$f.high + 0.5


fixef(mod1)
coef(mod1)$DGRP
summary(mod1)
cc1 <- coef(mod1)$DGRP
cc1$DGRP <- df$DGRP
head(cc1)

F1 <- ggplot(df, aes(y=Female_Int, x=reorder(DGRP, Female_Int))) + 
  geom_linerange(aes(ymin=f.low, ymax=f.high), colour="black") + 
  geom_point(colour="red", alpha=.5) + 
  coord_flip() + 
  labs(y="Intercept", x="DGRP Females")
# Error bars == upper and lower 95% confidence intervals
M1 <- ggplot(df, aes(y=Male_Int, x=reorder(DGRP, Male_Int))) + 
  geom_linerange(aes(ymin=m.low, ymax=m.high), colour="black") + 
  geom_point(colour="blue", alpha=.5) + 
  coord_flip() + 
  labs(y="Intercept", x="DGRP Males")


F1 +
  theme(text = element_text(size=15))
M1 +
  theme(text = element_text(size=15))

VarCorr(mod1)
?VarCorr
VarCorr(mod1,comp=c("Variance", "Std.Dev"))
rcorr(as.matrix(df), type="pearson")

line3 <- lm(df$Female_Int ~ df$Male_Int)
line3
with(df, plot(x = Female_Int,y = Male_Int, xlab = "Females", ylab = "Males", abline(line3), main = "Male Female correlation: proportion with spider"))

with(df, plot(x = Female_Int,y = Male_Int, xlab = "Females", ylab = "Males", abline(line3)))

corr <- ggplot(df, aes(x = Female_Int,y=Male_Int))
corr + geom_point() +
  labs(y="Female Intercept", x="Male Intercept") + geom_smooth(method="lm", colour="black") +
  theme(text = element_text(size=15))


#F2 <- ggplot(df, aes(y=Female_scale, 
#                     x=reorder(DGRP, Female_scale))) + 
#  geom_linerange(aes(ymin=f.low, ymax=f.high), colour="black") + 
##  geom_point(colour="red") + 
#  coord_flip() + 
#  labs(y="Intercept", x="DGRP Females")
#F2
# Error bars == upper and lower 95% confidence intervals
#M2 <- ggplot(df, aes(y=Male_scale, x=reorder(DGRP, Male_scale))) + 
#  geom_linerange(aes(ymin=m.low, ymax=m.high), colour="black") + 
#  geom_point(colour="blue", alpha=.5) + 
#  coord_flip() + 
#  labs(y="Intercept", x="DGRP Males")
#M2



#Model comparison: comparing with random DGRP effects and without:

#Large model: mod1

mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + 
                (1|Date) + (0 + Sex|DGRP),
              data = DGRP_by_counts, family = "binomial")
summary(mod1)
Anova(mod1)

mod2 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date) + (1|DGRP),
              data = DGRP_by_counts, family = "binomial")
summary(mod2)
Anova(mod2)

#Small Model:
mod3 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date), 
              data = DGRP_by_counts, family = "binomial")
summary(mod3)
Anova(mod3)

#PB_Mod_1 <- PBmodcomp(mod2, mod3, nsim = 10, ref = NULL, seed = NULL,
#                    cl = NULL, details = 0)
#summary(PB_Mod_1)

#PB_Mod_2 <- PBmodcomp(mod1, mod2, nsim = 10, ref = NULL, seed = NULL,
#                    cl = NULL, details = 0)
#summary(PB_Mod_2)

