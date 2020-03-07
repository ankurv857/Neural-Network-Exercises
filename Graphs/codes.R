
install.packages("MASS")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("gmodels")

library(MASS)         
library(Hmisc)           
library(pastecs)  
library(gmodels)          


str(train)

###------------------------------------------------------------
### Univariate Analysis:
###------------------------------------------------------------

# Generate summary statistics
summary(world)

summary(world$energy_use_percapita)

library(Hmisc)

describe(world$energy_use_percapita)

describe(world)

library(pastecs)

stat.desc(world)

write.csv(stat.desc(world), file = "E:/Work/Documents/Training/R Training 2013/Session 4/Univariates.csv")

###------------------------------------------------------------
### Detach packages which are not required
###------------------------------------------------------------

detach(package:pastecs)
detach(package:Hmisc)

###------------------------------------------------------------
### Frequency distribution for categorical data
###------------------------------------------------------------

table(world$deathCat)

table(world$birthCat, world$deathCat)

# Create Cross tabulations
library(gmodels)

CrossTable( train$target, train$f1,
           prop.chisq = FALSE,
           chisq = TRUE)

detach(package:gmodels)

# Generate histograms to check distributions

attach(world)
#detach(world)

hist(log(train$Count), col = "brown", 
     main = "Histogram on GNI Per Capita",
     xlab = "Gross National Income Per Capita",
     ylab = "Frequency")

colors()

# Histogram of the transformed variable
hs <- hist(log(gni_per_capita), 
           col = "grey", 
           main = "Histogram on GNI Per Capita",
           xlab = "Log of GNI Per Capita",
           ylab = "Frequency")

# Plotting multiple histograms

for(i in 2:9){
  hist(world[,i], col = "darkred", 
       main = colnames(world)[i])
}

plot(density(log(gni_per_capita)), 
     col = "darkred",
     main = "Density Plot")

polygon(density(log(gni_per_capita)), 
        col = "darkred")

qqnorm(gni_per_capita)

qqnorm(log(gni_per_capita))

qqline(log(gni_per_capita))

# Plot boxplot to identify outliers

boxplot(train$f1, 
        range = 1.5, 
        col = "darkred",
        main = "Box Plot on Birth Rate")

boxplot(train$target ~ train$f1, 
        range = 1.5, 
        col = "red",
        main = "Box Plot on Birth Rate",
        xlab = "Birth Rate Category")

#range= 1.5 * (Q3 - Q1)

###------------------------------------------------------------
###  Bivariate Analysis
###------------------------------------------------------------

# Check relationships

plot(birth_rate, 
     gni_per_capita, 
     main = "Correlation",
     xlab = "Birth Rate", 
     ylab = "GNI Per Capita", 
     col = "brown", pch = 20)

plot(train$f1, 
     train$f2, 
     #train$target,
     main = "Correlation",
     xlab = "Birth Rate",
     ylab = "log(GNI Per Capita)", 
     col = "green", pch = 20)

pairs( ~f1 + 
        f2 +f3 + f4 , 
      data = train,
      main = "Scatterplot Matrices", 
      panel = panel.smooth)

pairs(~log(gni_per_capita) + birth_rate + death_rate + 
        fertility_rate+life_expectancy, data = world,
      main = "Scatterplot Matrices", panel = panel.smooth)

cor(train_X[,c(3:16)])
