# Load the data
install.packages("readxl")
library(readxl)
data <- read_excel(file.choose())
attach(data)
names(data)

# Data visualization - 2016
data_2016 <- subset(data, Year == 2016)

# World maps
install.packages("rworldmap")
library(rworldmap)
joinCode <- joinCountryData2Map(data_2016,joinCode = "ISO3",nameJoinColumn = "Code")
mapCountryData(joinCode, nameColumnToPlot = "Child_Mortality", mapTitle = "Child Mortality Rates", catMethod = "fixedWidth", colourPalette = "heat", addLegend = TRUE)

joinCode <- joinCountryData2Map(data_2016,joinCode = "ISO3",nameJoinColumn = "Code")
mapCountryData(joinCode, nameColumnToPlot = "Malaria_Contagions", mapTitle = "Malaria contagions rate", catMethod = "fixedWidth", colourPalette = "terrain", addLegend = TRUE)

# Histogram - not included in the project
library(ggplot2)
ggplot(data_2016, aes(x=Child_Mortality)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") +labs(x = "Child Mortality Rate", y = "Count", title = "Distribution of Child Mortality Rates in 2016")

# Density plot - not included in the project
ggplot(data_2016, aes(x=Child_Mortality)) + geom_density(fill = "blue", alpha = 0.5) +labs(x = "Child Mortality Rate", title = "Density of Child Mortality Rates in 2016")

# Scatter plots - predictors vs Child Mortality Rate
ggplot(data_2016, aes(x = gdp_per_capita, y = Child_Mortality)) +
  geom_point() +  
  labs(x = "GDP per Capita", y = "Child Mortality Rate", 
       title = "Relationship between GDP per Capita and Child Mortality Rate") +
  theme_grey()

ggplot(data_2016, aes(x = Maternal_Deaths, y = Child_Mortality)) +
  geom_point() +
  labs(x = "Maternal Deaths", y = "Child Mortality Rate", 
       title = "Relationship between Maternal Deaths and Child Mortality Rate") +
  theme_grey()

ggplot(data_2016, aes(x = Malaria_Contagions, y = Child_Mortality)) +
  geom_point() +
  labs(x = "Malaria Contagions", y = "Child Mortality Rate", 
       title = "Relationship between Malaria Contagions and Child Mortality Rate") +
  theme_grey()

ggplot(data_2016, aes(x = MCV1, y = Child_Mortality)) +
  geom_point() +
  labs(x = "MCV1 vaccination rate", y = "Child Mortality Rate", 
       title = "Relationship between MCV1 vaccination rate and Child Mortality Rate") +
  theme_grey()

ggplot(data_2016, aes(x = DTP3, y = Child_Mortality)) +
  geom_point() +
  labs(x = "DTP3 vaccination rate", y = "Child Mortality Rate", 
       title = "Relationship between DTP3 vaccination rate and Child Mortality Rate") +
  theme_grey()

# Multivariate linear regression
model <- lm(Child_Mortality ~ Maternal_Deaths + Malaria_Contagions + MCV1 + DTP3 + gdp_per_capita, data=data)
summary(model)

## Validation tests

# Fitted values against residuals
plot(fitted(model), residuals(model), xlab = "Fitted", ylab = "Residuals")
abline(h=0)

plot(fitted(model), rstandard(model), xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h=0)

# Predictors against residuals
plot(data$Maternal_Deaths, residuals(model), xlab = "Maternal deaths", ylab = "Residuals")

plot(data$Malaria_Contagions, residuals(model), xlab = "Malaria Contagions", ylab = "Residuals")

plot(data$MCV1, residuals(model), xlab = "MCV1 vaccination rate", ylab = "Residuals")

plot(data$DTP3, residuals(model), xlab = "DTP3 vaccination rate", ylab = "Residuals")

plot(data$gdp_per_capita, residuals(model), xlab = "GDP per capita", ylab = "Residuals")

# Normality of residuals
qqnorm(residuals(model))
qqline(residuals(model))

# Model testing
summary(step(model, direction="both", trace= 1))

