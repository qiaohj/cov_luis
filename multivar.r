setwd("D:/Experiments/cov_luis")
library(dplyr)
library(FSA)
library(car)
df<-read.csv("BCG_covid19_DB_unfiltered_10_no-states-USA.csv", sep=",", stringsAsFactors = F)

df$BCG_policy<-as.factor(df$BCG_policy)
df$BCG_policyinterrupted

#make the multivariable linear model
model.full = lm(total_deaths_per_million ~ 
                  population_million+
                  population_density_2018+
                  urban_percentage_2018+
                  HDI_2018+
                  ages_65_up+
                  median_age_2018+
                  BCG_policy,
                data=df
)

#calculate the cor of the variables.
cor(df[, c("total_deaths_per_million", "population_million", "population_density_2018",
                  "urban_percentage_2018",
                  "HDI_2018",
                  "ages_65_up",
                  "median_age_2018")], use="complete.obs")

summary(model.full)

#test the autocorrelations of the variables
durbinWatsonTest(model.full)


crPlots(model.full)
library(leaps)

#tell the importance of the variables, based on the R squ
leaps<-regsubsets(total_deaths_per_million ~ 
                    population_million+
                    population_density_2018+
                    urban_percentage_2018+
                    HDI_2018+
                    ages_65_up+
                    median_age_2018+
                    BCG_policy, data = df,nbest=4)
plot(leaps,scale = 'adjr2')
