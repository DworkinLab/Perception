#Example Script:
#Packages:
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(effects)

perception <- read.csv('Perception_all_Data.csv', h=TRUE)

#Data cleaning:
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

head(DGRP_by_counts)

#Data layout: Each DGRP line is an inbred DGRP dihybrid line that is expected to behave similarily within each line. The response of interest is the proportion found with spider (Spider) compared to avoiding spider predators (Not_spider) with Data and DGRP Sex effects taken as random.

#Model:
mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex  + (1|Date) 
              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")

summary(mod1)

random1 <- ranef(mod1, condVar = TRUE)

head(random1)
# Variance here: 

pv_attr <- attr(random1$DGRP, "postVar")

se_fem <- pv_attr[1, 1, ]
se_ma <- pv_attr[2,2, ]

#Data frame of intercepts
rand.interc<-random1$DGRP

df<-data.frame(Intercepts=random1$DGRP[,1:2], DGRP=rownames(rand.interc), se_female=se_fem, se_male=se_ma)

colnames(df) <- c("Female_Int", "Male_Int", "DGRP", "Female_se", "Male_se")

df$m.low <- with(df, Male_Int - 2 * Male_se)
df$m.high <- with(df, Male_Int + 2 * Male_se)
df$f.low <- with(df, Female_Int - 2 * Female_se)
df$f.high <- with(df, Female_Int + 2 * Female_se)

F1 <- ggplot(df, aes(y=Female_Int, x=reorder(DGRP, Female_Int))) + 
  geom_linerange(aes(ymin=f.low, ymax=f.high), colour="black") + 
  geom_point(colour="red", alpha=.5) + 
  coord_flip() + 
  labs(x="DGRP Females")

M1 <- ggplot(df, aes(y=Male_Int, x=reorder(DGRP, Male_Int))) + 
  geom_linerange(aes(ymin=m.low, ymax=m.high), colour="black") + 
  geom_point(colour="blue", alpha=.5) + 
  coord_flip() + 
  labs(x="DGRP Males")

print(F1)
print(M1)
