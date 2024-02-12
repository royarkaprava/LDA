data <- read.table("H:/Documents/GitHub/LDA/Codes/Data on seizure counts.txt", quote="\"", comment.char="")

library(tidyr)
data_long <- gather(data, Week, measurement, V5:V8, factor_key=TRUE)
data_long <- data_long[order(data_long$V1), ]

####
colnames(data_long)[4] = "Baseline"
colnames(data_long)[1] = "Subject"
colnames(data_long)[2] = "Treatment"
colnames(data_long)[3] = "Age"

data_new = data_long
data_new$Week <- as.character(data_new$Week)
#data_new$Week[grep("V4", data_long$Week)] = "0"
data_new$Week[grep("V5", data_long$Week)] = "1"
data_new$Week[grep("V6", data_long$Week)] = "2"
data_new$Week[grep("V7", data_long$Week)] = "3"
data_new$Week[grep("V8", data_long$Week)] = "4"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

data_new$Week <- as.factor(data_new$Week)
data_new$measurement <- log(data_new$measurement+1)
data_new$Baseline <- log(data_new$Baseline+1)

contr.poly(4)
contrasts(data_new$Week) = contr.poly(4)
summary(lm(measurement ~ Week, data=data_new))

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))


#######################Contrast############################
res <- lmer(measurement ~ factor(Week) + (1|Subject), data = data_new)
coef(summary(res))  

library(emmeans)
library(ggplot2)

res.emm <- emmeans::emmeans(res, "Week")

#If we wanted to compare each visit against every other visit (i.e. all the pairwise comparisons) we can use
emmeans::contrast(res.emm, 'tukey') %>%
  broom::tidy() %>%
  head


# To check the polynomial contrasts, to see if there was a linear or quadratic change over time:
emmeans::contrast(res.emm, 'poly') %>%
  broom::tidy() %>%
  head(3) 

##################With interaction#######################

res <- lmer(measurement ~ factor(Week)*Treatment + (1|Subject), data = data_new)
coef(summary(res))  

library(emmeans)
library(ggplot2)

res.emm <- emmeans::emmeans(res, "Week")

#If we wanted to compare each time against every other times (i.e. all the pairwise comparisons) we can use
emmeans::contrast(res.emm, 'tukey') %>%
  broom::tidy() %>%
  head


# To check the polynomial contrasts, to see if there was a linear or quadratic change over time:
emmeans::contrast(res.emm, 'poly') %>%
  broom::tidy() %>%
  head(3) 

res.emm <- emmeans::emmeans(res, "Treatment")

#If we wanted to compare the treatments (i.e. all the pairwise comparisons) we can use
emmeans::contrast(res.emm, 'tukey') %>%
  broom::tidy() %>%
  head

#####################For your custom constrast##########################
res <- lmer(measurement ~ factor(Week)*Treatment + (1|Subject), data = data_new)

## set up contrast (linear comb. of coefficients)
ct <- c(0,0,0,0,0,1,1,1/2)   #This is the contrast from AUCMB_1-AUCMB_0 assuming Week 1 as time 1  

m <- sum(fixef(res) * ct)  ## mean of contrast
v <- t(ct) %*% vcov(res) %*% ct   ## variance of contrast
stder <- sqrt(as.numeric(v))      ## standard error
tstat <- m/stder      ## t statistic

##########For large sample##################
2*pnorm(abs(tstat), lower.tail=FALSE)
#############For small sample##############
number_of_parameters_estimated <- length(fixef(res)) + length(res@theta)  #number of parameters for mean + number of parameters for random effect variance
error_df = nrow(data_new) - number_of_parameters_estimated
2*pt(abs(tstat), df=error_df, lower.tail = FALSE)

#Repeat the above for lme#################################################
res <- lme(measurement ~ factor(Week)*Treatment, random = ~1|Subject, data = data_new)
## set up contrast (linear comb. of coefficients)
ct <- c(0,0,0,0,0,1,1,1/2)   #This is the contrast from AUCMB_1-AUCMB_0 assuming Week 1 as time 1  

m <- sum(fixef(res) * ct)  ## mean of contrast
v <- t(ct) %*% vcov(res) %*% ct   ## variance of contrast
stder <- sqrt(as.numeric(v))      ## standard error
tstat <- m/stder      ## t statistic

##########For large sample##################
2*pnorm(abs(tstat), lower.tail=FALSE)
#############For small sample##############
number_of_parameters_estimated <- length(fixef(res)) + sum(upper.tri(getVarCov(res,type = c("random.effects")), diag=T))  #number of parameters for mean + number of parameters for variance
error_df = nrow(data_new) - number_of_parameters_estimated
2*pt(abs(tstat), df=error_df, lower.tail = FALSE)

