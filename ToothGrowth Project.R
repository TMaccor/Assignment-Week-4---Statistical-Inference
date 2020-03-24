
library(tidyverse)

## load Tooth Growth data

data("ToothGrowth")

str(ToothGrowth)

## The Effect of Vitamin C on Tooth Growth of Guinea Pigs

## 1.- len = tooth lenth (numeric)
## 2.- supp = suplement type (factor) --> VC = ascorbic acid 
##                           (factor) --> OJ = oranje juice
## 3.- dose = mg/day (numeric)   

table(ToothGrowth$dose)

## So there are 3 doses:  0.5   1   2

## How many pigs received each dose and by which method?
Summary_Stats <- ToothGrowth %>% group_by(dose, supp) %>% summarise(n = n())

## 20 pigs received each dose ---> 10 via VC, 10 via OJ


## We obtain a summary of descriptive statistics of the 6 different groups by generating a box & whisker plot

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

dose_supp_plot <- ggplot(ToothGrowth, aes(x = dose, y = len, fill = supp))
dose_supp_plot + geom_boxplot()

## Where we can observe what could be a significant difference in Tooth Growth between the 0.5 mg/day dose
## Versus the 1 & 2 mg/day doses. 
## We need to test for statistical significance to ascertain this.
## It would also appear there is a difference between delivering the Vitamin C via VC vs OJ for the 0.5 and 1mg/day doses

## We now plot only the difference from DOSE for the 3 groups:
dose_plot <- ggplot(ToothGrowth, aes(x = dose, y = len))
dose_plot + geom_boxplot()

ToothGrowth %>% group_by(dose) %>% summarise(Mean_Tooth_Growth = mean(len))

## This time, it could well be that there's statistical difference between all 3 doses (not counting the dose DELIVERY METHOD)

## As we have > 2 groups with what appear similar intergroup variance, we use ANOVA for testing significance (equal variance)

## Start with ANOVA for DOSE:
ANOVA_dose <- aov(len ~ dose, data = ToothGrowth)

## Then we get the ANOVA summary:
summary.aov(ANOVA_dose)

## So, we have a statistically significant difference in Tooth Growth between the DOSES
## We do not know which of the pairwise DOSE comparisons are significant, so now we need to do a TUKEK TEST)
TukeyHSD(x = ANOVA_dose)

## Which results in statistically significant differences between all 3 doses
## The Mean and Confidence interval for ToowthGrowth for the 3 doses are listed(provided in the Tukey Test


### Confidence Intervals: the following info is already provided if we do the TUKEY tests:   
### Summary_Stats <- ToothGrowth %>% group_by(dose) %>% summarise(Mean_Tooth_Growth = mean(len), St.Dev. = sd(len))
### ToothGrowth <- ToothGrowth %>% arrange(dose)
### Obtain vector with Toothh_Growth values at 0.5 mg/day dose
### Dose0.5 <- ToothGrowth[1:20, 1]
### Obtain 95% confidence intervals: t.test(Dose0.5)$conf
### same for the other 2 doses -----

## In relation to TYPE of Vitamin C supplement, not advisable to test solely between VC vs OJ, as DOSE could
## a confounder

## So we do pairwise t-tests for each dose
## First we need to sort the observations by dose  --otherwise pairwise won't work
ToothGrowth <- ToothGrowth %>% arrange(dose)

Dose0.5 <- ToothGrowth[1:20, ]
Dose1 <- ToothGrowth[21:40, ]
Dose2 <- ToothGrowth[41:60, ]

t.test(len ~ supp, data = Dose0.5)
t.test(len ~ supp, data = Dose1)
t.test(len ~ supp, data = Dose2)

## This results ins statistical signifance for SUPPLEMENT TYPE for 0.5 & 1 mg/day doses, but not for 
## the 2mg/day dose. 
## So we have to conclude there is no conclusive data to support that SUPPLEMENT TYPE has a statistically 
## significant effect over Tooth Growth

