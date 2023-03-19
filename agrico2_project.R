# Clear directory
rm(list = ls())

# Set working directory
setwd("C:/Users/14087/Documents/master/econ 2509/project")

# Libraries
library(AER)
library(doBy)
library(ggplot2)
library(stargazer)

# SE of regressions
cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

# SE for inverse regressions
ivse=function(reg) {
  rob=robust.se(reg)[, 2]
  return(rob)
}

# load agrico2.csv dataset
agrico2=read.csv("agrico2.csv", header=T, sep=",")
# reading data and the summary data
str(agrico2)
# handle missing data
agrico2 <- na.omit(agrico2)
View(agrico2)

# transform GDP and GDP_per_capita from character to numeric type
agrico2$GDP = as.numeric(as.character(agrico2$GDP)) 
agrico2$GDP_per_capita = as.numeric(as.character(agrico2$GDP_per_capita)) 

# Descriptive Statistics
stargazer(agrico2[c("co2_emission", "land_area", "food_index", "crop_index")], 
          type="text", digits=2, summary.stat=c("n", "mean", "median", "sd", "min", "max"), 
          title="Descriptive Statistics")

## Exploratory Data Analysis
# Line plot
ggplot(agrico2, aes(x=log(co2_emission), y=crop_index)) + 
  geom_point(col="blue") + 
  geom_text(aes(label=code), hjust=1, vjust=1) + 
  labs(title = "Crop production index on Level of CO2", x = "Level of CO2 emissions", y = "Crop production index") +
  stat_smooth(method = "lm", col = "red", se=FALSE)

# Bar plot
ggplot(agrico2, aes(x=crop_index, y=reorder(code, -crop_index), fill = co2_emission)) +
  geom_bar(stat="identity") +
  xlab("Crop Production Index") +
  ylab("Country Code") +
  theme_minimal(base_size = 10)

# Categorical variable named lowcrop
agrico2$lowcrop[agrico2$crop_index <= 103.41] <- "Low value" 
agrico2$lowcrop[agrico2$crop_index > 103.41] <- "Higher value"

# Histogram plot
ggplot(agrico2, aes(x = co2_emission, fill = lowcrop)) + 
  geom_histogram(binwidth=0.25, position="identity", alpha=0.4)

# transform GDP growth rate from character to numeric type
agrico2$GDP = as.numeric(agrico2$GDP) 
agrico2$GDP_growth = as.numeric(agrico2$GDP_growth) 

# Relationship between GDP per capita growth rate and CO2 Emission 
ggplot(agrico2, aes(x = GDP_growth, y=co2_emission, size = GDP, colour = code)) +
  geom_point() +
  theme(legend.position = "right") + 
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_continuous(limits = c(-2, 8)) +
  labs(title = 'GDP per capita growth rate and CO2 Emission', 
       subtitle = 'Year: 2019', x = 'GDP per capita growth rate', y = 'Level of CO2 Emissions')

# transform type of GDP variables from character to numeric type
agrico2$GDP = as.numeric(as.character(agrico2$GDP)) 
agrico2$GDP_per_capita = as.numeric(as.character(agrico2$GDP_per_capita)) 

# logarithm of co2_emission
agrico2$lco2 <- log(agrico2$co2_emission)

# Regressions
# Run 4 regression models using lm command
regr1<-lm(crop_index~lco2, data=agrico2)
regr2<-lm(crop_index~lco2+food_index, data=agrico2)
regr3<-lm(crop_index~lco2+food_index+land_area, data=agrico2)
regr4<-lm(crop_index~lco2+food_index+land_area+log(GDP), data=agrico2)
regr5<-lm(crop_index~lco2+food_index+land_area+log(GDP)+I(lco2*log(GDP)), data=agrico2)

# Using stargazer to display your regression results 
stargazer(regr1, regr2, regr3, regr4, regr5,
          se=list(cse(regr1), cse(regr2), cse(regr3), cse(regr4), cse(regr5)), 
          title="Effect of CO2 emissions on Crop Production Index", type="text", 
          df=FALSE, digits=3, omit.stat=c( "f"))

summary(agrico2$crop_index)
# Create dummy variable of crop index for the vast majority of the countries 
agrico2$medcrop <- ifelse(agrico2$crop_index <= 109.37, 1, 0)

# Run a probit- and logit- models using glm command
p1=glm(medcrop~lco2+food_index+land_area+log(GDP), family=binomial(link="probit"), x=TRUE, data=agrico2)
l1=glm(medcrop~lco2+food_index+land_area+log(GDP), family=binomial, x=TRUE, data=agrico2)

# regression table
stargazer(p1, l1, 
          se=list(NULL, NULL), 
          title="Probit- and Logit- Model of Medium-Sized Countries Crop Index",
          type="text", df=FALSE, digits=3,
          omit.stat=c( "f"))

# calculate marginal error from probit- and logit- regression
library(erer)
fm1=maBina(p1, x.mean=FALSE, rev.dum=TRUE, digits=3)
fm2=maBina(l1, x.mean=FALSE, rev.dum=TRUE, digits=3)

# Using stargazer to display the marginal effects of the probit and logit regressions
stargazer(p1, fm1, l1, fm2,
          se=list(NULL, NULL, NULL, NULL), 
          title="Marginal Effects",
          type="text", star.cutoffs=NA, df=FALSE, digits=3,
          keep.stat=c("n","ll"))

# calculate marginal error from probit- and logit- regression
library(erer)
fm1=maBina(p1, x.mean=FALSE, rev.dum=TRUE, digits=3)
fm2=maBina(l1, x.mean=FALSE, rev.dum=TRUE, digits=3)

# Using stargazer to display the marginal effects of the probit and logit regressions
stargazer(p1, fm1, l1, fm2,
          se=list(NULL, NULL, NULL, NULL), 
          title="Marginal Effects",
          type="text", df=FALSE, digits=3,
          keep.stat=c("n","ll"))

#calculate the pseudo-R2
pseudoR2p1=(p1$null.deviance-p1$deviance)/p1$null.deviance
pseudoR2l1=(l1$null.deviance-l1$deviance)/l1$null.deviance
