setwd("~/Desktop/RSession")

install.packages("ggplot2") 
install.packages("tidyverse")
install.packages("GGally")
library(tidyverse)
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

load(file = "DiD.Rda")

data
View(data)

data$post <- ifelse(data$time > 3, 1, 0)
data$treat <- ifelse(data$state > 1, 1, 0)

# ploting teh evolution of time
data_treatment <- data[!(data$state < 2),]
data_treatment
data_control <- data[!(data$state > 1),]
data_control

plot(data_treatment$income, data_treatment$time) #you need go plot the aggregate income 
plot(data_control$income, data_control$time)
# there is no anticipation effect in place this is seen beacuse from state 2 and above tehre is a shoft due to the effect in place
# parallel trend is satisfied: the average change in income compared to the rest looks similar
#estimating the values
data$post <- ifelse(data$time > 3, 1, 0)
data$treat <- ifelse(data$state > 1, 1, 0)
view(data)

modulo <- lm(income ~ post + treat + (post*treat) + age + age2 + age3, data = data)
summary(modulo)

modulo1 <- lm(income_1 ~ post + treat + (post*treat) + age + age2 + age3, data = data)
summary(modulo1)

# in regards to the statistical significance, and which variables can be ruled out.
#talk about economic significance the average income has increased around 1-2% and is economically meaningful,
#which is gonna make some better off (basically also stating the change and its effect on the coefficients)

ggplot(data, aes(income, time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(data, aes(income_1, time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#there is anticipation effect in t=4, however some individuals do respond at this effect in t=3

ggplot(data, aes(income_2, time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#the parallel trend assumption is violated. by comparing the treatment and control group are not similar and dont follow
#the same trend - to support this, we'd have to change control group that is valid counter-factual or have a different test.

modulo2 <- lm(income ~ post + state_man + (post*state_man) + age + age2 + age3, data = data)
summary(modulo2)

object <- ecdf(data)

"natural fluctuation: number of people moving during any given year, and systematical fluctuation: a large number of 
individuals move right before a reform, these are difficuklt to distinguish but if 60% of population is moving then its systematical
with natural fluctuation can be treated easily with an intent to treat estimator"


"Card and Krueger, 1994"
"the paper tlaks about the effect of minimum wage on employment"

"basic idea of DiD  by comparing pre-treatment and post-treatment as different groups from different locations and at different times
the first difference is time and the second is location with only NJ increasing minimum wage"

"by comparing the parallel trend of employment in 2 states pre-treeatment, while for manipulation we use accumulated distribution of restaurant by state changes
its impossible to test with the given data as you need 2 teeatment but was only given 1, only with one period or with just those lication is
very hard to follow them, survey data can easily be manipulated so its not entirley sure if the effect was in place"

"they must have similar products and similar workforce, (perfect competition in the labour market), fastfood have the highest low wage employees
easy to obtain information from restaurants"

"competitive market says increase in minimum wage decreases employment making the model not always true, in monopsony it might increase employment but the extra cost are pushed 
onto the customer"

