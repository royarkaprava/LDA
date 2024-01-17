data <- read.table("H:/Documents/Course to teach/LDA/LDA/Slides/Treatment of Lead Exposed Children Trial.txt", quote="\"", comment.char="")

#Columns represent ID, Treatment Group, Lead Level Week 0, Lead Level Week 1, Lead Level Week 4, Lead Level Week 6. 

#Running ANOVA model for each time point
fit <- lm(V3~V2, data=data)
anova(fit)

fit <- lm(V4~V2, data=data)
anova(fit)

fit <- lm(V5~V2, data=data)
anova(fit)

fit <- lm(V6~V2, data=data)
anova(fit)

#######Checking correlation in placebo group
cor(data[which(data$V2=="P"), 3:6])

##############Cehcking mean and variance of the change in outcome between Week 1 and Week 0
delta <- data[which(data$V2=="A"), 4]-data[which(data$V2=="A"), 3]
mean(delta)

##Correct variance
var(delta)/length(delta)
##Matches with
(var(data[which(data$V2=="A"), 4])+var(data[which(data$V2=="A"), 3]) - 2*cov(data[which(data$V2=="A"), 4],data[which(data$V2=="A"), 3]))/length(delta)

#####But not
(var(data[which(data$V2=="A"), 4])+var(data[which(data$V2=="A"), 3]))/length(delta)


#####Contrast based lme in R
library(lme4)

#Convert Wide to long format
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(data, Week, measurement, V3:V6, factor_key=TRUE)
data_long

data_long <- data_long[order(data_long$V1), ]
data_long

library(lme4)
res <- lmer(measurement ~ factor(Week) + (1|V1), data = data_long)
coef(summary(res))  

#anova(res)

library(emmeans)
library(ggplot2)

res.emm <- emmeans::emmeans(res, "Week")

#If we wanted to compare each visit against every other visit (i.e. all the pairwise comparisons) we can use
emmeans::contrast(res.emm, 'tukey') %>%
  broom::tidy() %>%
  head

library(pander)

#if there was a significant change between any specific visit and baseline:
emmeans::contrast(res.emm, 'trt.vs.ctrl') %>%
  broom::tidy() %>%
  head %>%
  pander


# To check the polynomial contrasts, to see if there was a linear or quadratic change in Lead level over days:
emmeans::contrast(res.emm, 'poly') %>%
  broom::tidy() %>%
  head(3) %>%
  pander(caption="The first three polynomial contrasts. Note you'd have to have quite a fancy theory to warrant looking at any of the higher level polynomial terms.")