### Labour PS 1 week 7

## read in libraries
library(stringr) 
library(ggpubr)
library(stargazer)
library(haven)

# tidyverse
library(dplyr)
library(ggplot2)

library(ggthemes)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## read in data frames & manipulate
setwd("C:/Users/alexa/Documents/GitHub/LSE-labour-PS")

# read in data set
df <- read_dta("ec423ps4/nlsy_2014_data.dta")

## As I will not be in London for most part of the week, I was not able to spend too 
## much time on this problem set. However, with the exception of finding the meaning 
## of the weights, I do not recall any trouble with this exercise. 

### 3
## a
# variables for hourly pay: hrp1, hrp2
df %>% count(hrp1 > 0)
# 5342 have strictly positive values
df %>% count(hrp2 > 0)
# 1194 have strictly positive values
# > as all people working in a second job have to have a first, this is reasonable
## b 
df <- df[df$hrp1 > 0,]
df$hrp1 <- as.numeric(df$hrp1)
summary(df$hrp1)
# units: IF hourly pay, due to the mean being 2634, I assume this is cents, 
# thus the median in dollar should be 19.12 dollar
## c 
df$ln_hr_wage <- log(df$hrp1)
hist(df$ln_hr_wage)
# it looks bell-shapes, although slightly right-leaning

### 4 
## a
summary(df$sampweight)
df$sampweight <- df$sampweight / 100
# interpretation: ?!
## b 
by(df$sampweight, df$id, summary)
# interpretation: ?! 
## c 

### 5 
## a 
table(df$q3_4)
# most common: 12, second most common: 16, 974
# this seems plausible, as most people finish high school after 12 years, 
# while 16 years would translate to some higher education at uni 
## b 
df$yschl <- df$q3_4
df$yschl[df$yschl < 0 | df$yschl > 25] <- 0
table(df$yschl)
# 14, 6 people with neg. values, 9 with 95

### 6 
## a
# as year of observation 2014, formula: 114-dob_year
# however, this is technically only accurate for 31/12/2014
# as then all people could have had their birthday
df$age <- 114 - df$dob_year
table(df$age)
# min: 49, max: 58
# Due to seniorage effects, this might be the age where
# earnings are the highest, thus, I expect comp. high wages
# however, as education increased in the following cohorts, 
# I also expect more heterogeneity in education 
## b 
df$agesq <- (df$age)^2
# it may be sensible to assume that the payoff of education
# decreases in age, thus there is a concave relationship
## c 
df$mom_schl <- df$hgc_mother 
df$mom_schl[df$mom_schl < 0 | df$mom_schl > 25] <- 0
table(df$mom_schl)
df$pop_schl <- df$hgc_father 
df$pop_schl[df$pop_schl < 0 | df$pop_schl > 25] <- 0
table(df$pop_schl)
# weighted output & interpretation: ?! 
## d 
df$female <- ifelse(df$sex == 2,1,0)
# weighted output & interpretation: ?! 
## e
dfNA <- df
dfNA[dfNA < 0] <- NA
cor(dfNA$yschl*dfNA$sampweight,dfNA$mom_schl*dfNA$sampweight)
cor(dfNA$yschl*dfNA$sampweight,dfNA$pop_schl*dfNA$sampweight)
# the results suggest a strong positive correlation between years of schooling
# interestingly, the cor is higher for the mothers years of schooling
## f 
dfNA$afqt <- dfNA$afqt_3 / 1000
# interpretation: 
## g 
dfNA$health_problems <- dfNA$q11_3
summary(dfNA$health_problems)
# 28.4% have health problems

### 5 
## a 
summary(lm(ln_hr_wage~yschl,dfNA,weights=sampweight))
# the estimated returns are 11% per year, this seems in line with literature,
# but quite high in general
## b
summary(lm(ln_hr_wage~yschl+age+agesq+female+mom_schl+pop_schl,dfNA,weights=sampweight))
# the coefficient for yschl is slightly lower, but still significant
# the age is not significant, however, this might relate to the narrow age group
## c
summary(lm(ln_hr_wage~yschl+age+agesq+female+afqt,dfNA,weights=sampweight))
# the coefficient of yschl dropped from 11% to 8%
# if afqt captures ability, then the drop in yschl might be due to an OVB
# of not accounting for the fact that those with more schooling have other forms 
# of helpful skills as well (and might choose more schooling due to that)
## d
summary(lm(ln_hr_wage~yschl+age+agesq+female+health_problems,dfNA,weights=sampweight))
# there is little effect on the coefficient of yschl
# the question is, whether health is associated with years of schooling and wages
# it may be argued that poor health in general will result in less schooling and wage, BUT
# in this data set, we only have grown adults, we do not know when these health problems arose
# those, we only see how their health affects their work when they are around 50, 
# while ideally, we'd want a variable that captures terminal illnesses at a younger age
## e 
dfNA$yschl.f <- factor(dfNA$yschl)
summary(lm(ln_hr_wage~yschl.f+age+agesq+female+afqt,dfNA,weights=sampweight))
# Most of the dummies are not significant, but if only levels are regarded: 
# up until 14 years of schooling, the final years of schooling are negatively associated
# with wage (in contrast to one year of schooling), after that, the relationship is positive. With a few exceptions, the 
# effect size is around |0.3| 
dfNA$yschl2.f <- factor(ifelse(dfNA$yschl > 7, dfNA$yschl, NA))
summary(lm(ln_hr_wage~yschl2.f+age+agesq+female+afqt,dfNA,weights=sampweight))
# relative to those with 8 years of schooling, those with 9 to 11 years had a lower 
# wage, while all schooling year groups after that have a higher wage
# particularily large increases are seen between 15 and 16 years, 18 and 19, 12 and 13
# these might exist due to earned degrees after that specific time
# however, only the effects of years of schooling after 9, 16, 17, 18, 19, 20 are 
# significant to a degree p<.01
## f 
# other factors might include: schooling environment (class-size, teaching quality etc.), 
# peer-effects (how well are other students in the class), parents income (not only
# years of schooling), race, immigration status (first generation, country of birth
# of parents, etc), 
