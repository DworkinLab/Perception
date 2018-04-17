# With Site Effects
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
#library(Hmisc)

source('Stat_smooth_func.R')

perception <- read.csv('Perception_all_Data.csv', h=TRUE)

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
                       data.frame(Bin, Spi_loc, New_Spi, Row, DGRP, Sex, Spider=Spider_48,
                                  Not_spider=Not_Spider_48, Outside=Outside_48,
                                  Date=Start_date, Vial_total= spiANDno_48, Number=Number))

DGRP_by_counts$proportion_spider <- with(DGRP_by_counts, Spider/Vial_total)
head(DGRP_by_counts)
DGRP_by_counts$Row <- as.factor(DGRP_by_counts$Row)

#mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Spi_loc + Row + New_Spi + Sex
#              + (1|Date)
#              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")
mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + Spi_loc + Row
              + (1|Date)
              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")
summary(mod1) 
Anova(mod1)
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
head(df)

#plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))
df$Female_prob <- exp(df$Female_Int)/(1+exp(df$Female_Int))
df$Male_prob <- exp(df$Male_Int)/(1+exp(df$Male_Int))
#df$Female_se_prob <- exp(df$Female_se)/(1+exp(df$Female_se))
#df$Male_se_prob <- exp(df$Male_se)/(1+exp(df$Male_se))

head(df)
df$m.low <- with(df, Male_prob - 1.96 * Male_se)
df$m.high <- with(df, Male_prob + 1.96 * Male_se)
df$f.low <- with(df, Female_prob - 1.96 * Female_se)
df$f.high <- with(df, Female_prob + 1.96 * Female_se)
#exp(fixef(mod1)[1])
#exp(fixef(mod1)[2])
df2 <- df

F2 <- ggplot(df2, aes(y=Female_prob, x=reorder(DGRP, Female_prob))) + 
  geom_linerange(aes(ymin=f.low, ymax=f.high), colour="black") + 
  geom_point(colour="red", alpha=.5) + 
  coord_flip() + 
  labs(y="Probability with Spider", x="DGRP Females")  +
  theme(text = element_text(size=15)) + 
  geom_hline(yintercept = mean(df$Female_prob))

M2 <- ggplot(df2, aes(y=Male_prob, x=reorder(DGRP, Male_prob))) + 
  geom_linerange(aes(ymin=m.low, ymax=m.high), colour="black") + 
  geom_point(colour="blue", alpha=.5) + 
  coord_flip() + 
  labs(y="Probability with Spider", x="DGRP Males") +
  theme(text = element_text(size=15)) + geom_hline(yintercept =  mean(df$Male_prob))

F2

M2



library(devtools)
#install.packages("ggplot2")

corr <- ggplot(df2, aes(x = Female_prob,y=Male_prob))
corr + geom_point() +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, ) +
  geom_smooth(method="lm",se=FALSE, color="grey34") +
  labs(y="Female Line Probability", x="Male Line Probability") + 
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color="grey34") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15))


with(df2, cor( Female_prob, Male_prob))

