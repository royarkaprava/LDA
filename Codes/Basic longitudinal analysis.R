data <- read.table("Treatment of Lead Exposed Children Trial.txt", quote="\"", comment.char="")

#Columns represent ID, Treatment Group, V3=Lead Level Week 0, V4=Lead Level Week 1, V5=Lead Level Week 4, V6=Lead Level Week 6. 

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
cov(data[which(data$V2=="P"), 3:6])

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

#####################ANOVA analysis###################
data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V3", data_long$Week)] = "0"
data_new$Week[grep("V4", data_long$Week)] = "1"
data_new$Week[grep("V5", data_long$Week)] = "4"
data_new$Week[grep("V6", data_long$Week)] = "6"
data_new$Week <- as.numeric(data_new$Week)

ls <- lm(measurement~Week, data=data_new)
model.matrix(ls)

ls <- lm(measurement~as.factor(Week), data=data_new)
model.matrix(ls)

coef(summary(ls)) 
gtsummary::tbl_regression(ls)
anova(ls)

library(lme4)
res <- lme4::lmer(measurement ~ Week + (1|V1), data = data_new)
out <- lFormula(measurement ~ Week + (1|V1), data=data_new)
X <- as.matrix(out$X)
Z <- t(as.matrix(out$reTrms$Zt))

coef(summary(res))  
gtsummary::tbl_regression(res)
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
emmeans::contrast(res.emm, 'change.vs.baseline') %>%
  broom::tidy() %>%
  head %>%
  pander


# To check the polynomial contrasts, to see if there was a linear or quadratic change in Lead level over days:
emmeans::contrast(res.emm, 'poly') %>%
  broom::tidy() %>%
  head(3) %>%
  pander(caption="The first three polynomial contrasts. Note you'd have to have quite a fancy theory to warrant looking at any of the higher level polynomial terms.")