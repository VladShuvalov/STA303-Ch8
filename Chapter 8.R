library(tidyverse)
library(faraway)
library(ggplot2)
library(lme4)
library(EnvStats) #Using for boxcox transformation
# Q8.1 a and b
###
data(pulp)
data(pulp,package = "faraway")
pulp

#Part a
mod1_pulp = lm (bright ~ operator, data = pulp)
summary(mod1_pulp)
#F-statistic: 4.204 on 3 and 16 DF,  p-value: 0.02261
#The operator is significant at the 5% level

#Part b
mod2_pulp = lmer(bright ~ 1+(1|operator), pulp)
summary(mod2_pulp)
#Estimated variances are: sigma_U = 0.2609, sigma = 0.3260
confint(mod2_pulp)
#CI for sigma_U = (0, 0.61789) (Why is this a one sided CI?)
#CI for sigma = (0.238912, 0.4821845)
lmod_mixed_reml <- lmer(bright ~ 1 + (1|operator),data = pulp,REML = TRUE)
summary(lmod_mixed_reml)
confint(lmod_mixed_reml)
#Q8.2
data(coagulation)
head(coagulation)
dim(coagulation)
boxplot_coag = ggplot(coagulation) +
  aes(x = diet, y = coag) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#Part a
mod1_coag = lm (coag ~ diet, data = coagulation)
summary(mod1_coag)

#Part b
mod2_coag = lmer(coag ~ 1+(1|diet), data = coagulation)
summary(mod2_coag)

#What is the difference between part b and part c?

#Q8.3
data(eggprod)
glimpse(eggprod)
#Part a
mod1_egg = lmer(eggs ~ treat + (1|block), data = eggprod)
summary(mod1_egg)
130.0 / (130.0 + 386.9)
#Treatment O produces on average 36.25 more eggs
#than treatment F
#Variance between blocks accounts for 25.1% of the total variance

#Q9.1

data(ratdrink)
glimpse(ratdrink)

ggplot(ratdrink) +
  aes(x = treat, y = wt) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#See that thyroxine has similar weight to the control group
#thiouracil treated rats overall have lower mean than the other two treatments
ggplot(ratdrink) +
  aes(x = factor(weeks), y = wt) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#Rats gain weight on average over time 
#See higher spread in the later weeks could potentially be
#due to the effect of some treatments

ggplot(ratdrink, aes(sample = wt)) +
  stat_qq()+
  stat_qq_line()
#The left tail looking very off while the rest of the points looking good
#Could potentially do some transformations

rcr = boxcox(ratdrink$wt)
plot(rcr, plot.type = "Q-Q Plots", same.window = TRUE) 
#Lambda of 0.5 looks ok? Few observations off at the tails. 

#Everything below is done without the transformation
#We took a random sample from the population of rats with rats 
#having a different weight from one another take rats to be a random effect
lmer_rats1 = lmer(wt ~ weeks + treat + (1|subject), data = ratdrink)
summary(lmer_rats1)
60.42/(60.42 + 105.15)
#36.5% of the variance is explained by the variance of the rat weight
lmer_rats2 = lmer(wt ~ weeks + treat + weeks:treat + (1|subject), data = ratdrink)
summary(lmer_rats2)
71.21 / (71.21 + 51.22)
#We can see that on average 58% of the variance is explained by the rat weight
#Adding an interaction effect gives a significant effect
#Where did control go?
#On average each rat grew 26.48 grams each week
#Rats treated with thirouracil are 4.78 grams heavier, losing 9.37 grams a week?
#Rats treated with thyroxine are 0.79 grams lighter, gaining 0.66 grams a week?
#Does this make sense? 

#Q9.2
data(hprice)
hprice = as_data_frame(hprice)
glimpse(hprice)
?hprice

ggplot(hprice) +
  aes(x = factor(time), y = narsp) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#General trend upwards over time with a few distict outliers

ggplot(hprice, aes(sample = narsp)) +
  stat_qq()+
  stat_qq_line()
#That does not look good. 
#What if we try to remove the outliers we saw in the boxplot?

hpricefiltered <- hprice %>% filter(narsp <= 5)
glimpse(hpricefiltered)
#Removed the top 32 priced homes in the data. Removing 10% of the data.
#Not great but lets look at the plots

ggplot(hpricefiltered) +
  aes(x = factor(time), y = narsp) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(hpricefiltered, aes(sample = narsp)) +
  stat_qq()+
  stat_qq_line()
#Well the qqplot does look much better but not great.
#Try a transformation?

bcr = boxcox(hprice$narsp)
plot(bcr, plot.type = "Q-Q Plots", same.window = TRUE) 
#None of these are really that great. 

bcfr = boxcox(hpricefiltered$narsp)
plot(bcfr, plot.type = "Q-Q Plots", same.window = TRUE) 
#lambda = -1.5 doesn't look that bad. 
#Again this is with nearly 10% of the data set missing.
#So this is going to be biased.
#Let's try it anyway

hpricefiltered$narsp = ((hpricefiltered$narsp^(-1.5)-1)/-1.5)
#Remember this is now a boxcox transformation of the log of the price

#Metropolitan state areas are a random sample from the population
#Lets add them as a random effect
mod1 = lmer(narsp ~ I(ypc/5000) + perypc + regtest + rcdum + ajwtr + (1|msa) + time, data = hpricefiltered)
summary(mod1)
1.113e-05 / (1.113e-05 + 2.306e-06)

#82.8% of the variance is explained by the variance between the MSAs
#Thats remarkably good but again this is removing the top 10% of our data
#I'm skeptical about accuracy here though.

#Take a look at what we took out?
View(hprice %>% filter(narsp >= 5))
#All of the outliers are coastline properties from 4 MSAs
#Clearly some sort of relationship here
#What to do next?

#9.5
data(sleepstudy)
glimpse(sleepstudy)
?sleepstudy

ggplot(sleepstudy) +
  aes(x = factor(Days), y = Reaction) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(sleepstudy, aes(sample = Reaction)) +
  stat_qq()+
  stat_qq_line()