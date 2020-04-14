library(Hmisc)
library(ggplot2)
setwd("D:/Experiments/cov_luis")
df<-read.csv("World_COVID-19_BCG_all-LEE.csv", head=T, sep=",", stringsAsFactors = F)
Independents<-c("BCG_policy", "BCG_vaccine_start", "BCG_vaccine_end", "BCG_vaccine_years_range", "BCG_vaccine_years_range_start", 
                "BCG_mean_coverage_end", "BCG_median_coverage_end")

Dependsents<-c("median_age_covid19", "death_per_million", "log_death_per_million", "median_daily_death_per_million",
               "mean_daily_death_per_million", "min_daily_death_per_million", "max_daily_death_per_million",
               "days_deaths_reached_0.1")


#First, we devide the data into two groups, with BCG(1) and without BCG(0)
df$BCG_TF<-1
df[which(df$BCG_policy==3),]$BCG_TF<-0

#after a one-tailed T Test, the group with BCG does have the lower death_per_million when the group without BCG, but not lower on covid19_confirmed
#it seems that the BCG doesn't affect the infection rate, but affect the death rate.
t.test(df$death_per_million ~ df$BCG_TF, paired = F, na.action = na.pass, alternative="greater")
t.test(df$covid19_confirmed ~ df$BCG_TF, paired = F, na.action = na.pass, alternative="greater")

#The boxplot also show the same pattern.
ggplot(df)+geom_boxplot(aes(x=factor(BCG_TF), y=death_per_million))
ggplot(df)+geom_boxplot(aes(x=factor(BCG_TF), y=covid19_confirmed))

#Let's use another grouped way, focus the elders only.
df$death_per_million_elder<-df$ages_65_up/df$population_2018
df$BCG_TF_elder<-0
df[which(df$BCG_policy==1),]$BCG_TF_elder<-1
t.test(df$death_per_million_elder ~ df$BCG_TF_elder, paired = F, na.action = na.pass, alternative="greater")
t.test(df$covid19_confirmed ~ df$BCG_TF_elder, paired = F, na.action = na.pass, alternative="greater")
ggplot(df)+geom_boxplot(aes(x=factor(BCG_TF_elder), y=death_per_million_elder))
ggplot(df)+geom_boxplot(aes(x=factor(BCG_TF_elder), y=covid19_confirmed))
#Great! we got a significant correlation that the elders who have BCG have a lower death ratio.
#Disappoint, right?



#while, is it true?
#Let's look at the GDP in different groups
df$GDP_2018<-as.numeric(df$GDP_2018)
df_com<-df[which(!is.na(df$GDP_2018)),]
ggplot(df_com)+geom_boxplot(aes(x=factor(BCG_TF_elder), y=GDP_2018))
t.test(df_com$GDP_2018 ~ df_com$BCG_TF_elder, paired = F, na.action = na.pass, alternative="greater")
#The figure shows that the groups without BCG has a higher GDP(richer than the group without BCG)

#while GDP has no correlation with death_per_million_elder
cor(df_com$GDP_2018, df_com$death_per_million_elder)

#but from the point plot, we found two tails. obviously, there is a 3rd factor to affect the relation between GDP and elders' death ratio
ggplot(df_com)+geom_point(aes(x=GDP_2018, y=death_per_million_elder))

#here I'd like to use a regression tree to find the 3rd factor
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging



set.seed(123)
df_model<-df_com[, c("death_per_million_elder", "GDP_2018", "BCG_policy", "income_level")]
ames_split <- initial_split(df_model)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)
m1 <- rpart(
  formula = death_per_million_elder ~ .,
  data    = ames_train,
  method  = "anova"
)
rpart.plot(m1)
plotcp(m1)

m2 <- rpart(
  formula = death_per_million_elder ~ .,
  data    = ames_train,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)

plotcp(m2)
abline(v = 12, lty = "dashed")
