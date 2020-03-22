
library(tidyverse)
library(reshape2)
library(BSDA)

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
ToothGrowth %>% group_by(dose, supp) %>% summarise(n = n())

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

## Start with ANOVA for dose and for supp as well:
ANOVA <- aov(len ~ dose+supp, data = ToothGrowth)

## Then we get the ANOVA summary:
summary.aov(ANOVA)

## So, we have a statistically significant difference in Tooth Growth between both the DOSES
## and the 2 delivery methods
## We do not know which of the pairwise DOSE comparisons are significant, so now we need to do a TUKEK TEST)
ANOVA_dose_only <- aov(len ~ dose, data = ToothGrowth)
TukeyHSD(x = ANOVA_dose_only)

## Which results in statistically significant differences between all 3 doses

