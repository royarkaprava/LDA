library(tidyr)

#uploading the data
data <- read.table("H:/Documents/GitHub/LDA/Codes/orthodontic.txt", quote="\"", comment.char="")


#converting the data from wide to long
data_long <- gather(data, age, growth, V3:V6, factor_key=TRUE)
data_long <- data_long[order(data_long$V1), ]
data_long$age <- as.character(data_long$age)
data_long$age[grep("V3", data_long$age)] = "8"
data_long$age[grep("V4", data_long$age)] = "10"
data_long$age[grep("V5", data_long$age)] = "12"
data_long$age[grep("V6", data_long$age)] = "14"

colnames(data) <- c("ID", "Gender", "Age 8", "Age 10",
                    "Age 12", "Age 14")

colnames(data_long) <- c("ID", "Gender", "Age", "growth")

data_long$Age <- as.numeric(data_long$Age)
##########The covariance matrix:

cov_mat <- data.frame(cov(data[,3:6]))
colnames(cov_mat) <- c("Age 8", "Age 10", "Age 12", "Age 14")
rownames(cov_mat) <- c("Age 8", "Age 10", "Age 12", "Age 14")
knitr::kable(cov_mat, digits = 3, caption = "Covariance Matrix")

##########The correlation matrix:
cor_mat <- data.frame(cor(data[,3:6]))
colnames(cor_mat) <- c("Age 8", "Age 10", "Age 12", "Age 14")
rownames(cor_mat) <- c("Age 8", "Age 10", "Age 12", "Age 14")
knitr::kable(cor_mat, digits = 3, caption = "Correlation Matrix")

###################Time as factor##############################

#Unstructured correlation using generalized least square in nlme
library(nlme)
modU <- gls(growth ~ as.factor(Age)*Gender, data = data_long,
           correlation = corSymm(form = ~ 1 | ID))

##########linear mixed model with random intercept#################
mod <- lme(growth ~ as.factor(Age)*Gender, random = ~1|ID, data = data_long) 

mod.AR1 <- update(mod, correlation = corAR1()) #AR1 correlation structure
mod.MA1 <- update(mod, corr = corARMA(p = 0, q = 1)) #MA1 correlation strucutre
mod.ARMA11 <- update(mod, corr = corARMA(p = 1, q = 1)) #ARMA(1,1) correlation structure

anova(modU, mod.AR1)
anova(modU, mod.MA1)
anova(modU, mod.ARMA11)


anova(mod.AR1, mod.ARMA11)
anova(mod.MA1, mod.ARMA11)

###Likelihood comparison is not conclusive
AIC(modU, mod.AR1, mod.MA1, mod.ARMA11)

####mod.AR1 has the smallest AIC, so it is chose. But unstructured GLS estimate is also close.


###################Testing the contrast based on AR1 model
## set up contrast (linear comb. of coefficients)
ct <- c(0,0,0,0,0,1/3,1/3,1/3)   #This is the contrast from E_1-E_0 assuming Week 1 as time 1  

coef <- fixef(mod.AR1)

m <- sum(coef * ct)  ## mean of contrast
v <- t(ct) %*% vcov(mod.AR1) %*% ct   ## variance of contrast
stder <- sqrt(as.numeric(v))      ## standard error
tstat <- m/stder      ## t statistic

##########For large sample##################
2*pnorm(abs(tstat), lower.tail=FALSE)


#############For small sample##############
#number of parameters for mean 
par1 <- length(coef)
#number of parameters for random effect variance
par2 <- sum(upper.tri(getVarCov(mod.AR1,type = c("random.effects")), diag=T)) 

number_of_parameters_estimated <- par1 +par2 #number of parameters for mean + number of parameters for variance
error_df = nrow(data_long) - number_of_parameters_estimated
2*pt(abs(tstat), df=error_df, lower.tail = FALSE)


###################Testing the contrast based on GLS (unstructured covariance). This is added just for reference
##Number of repeated measures
n=4
## set up contrast (linear comb. of coefficients)
ct <- c(0,0,0,0,0,1/3,1/3,1/3)   #This is the contrast from E_1-E_0 assuming Week 1 as time 1    

coef <- fixef(mod.AR1)
coef <- modU$coefficients

m <- sum(coef * ct)  ## mean of contrast
v <- t(ct) %*% vcov(modU) %*% ct   ## variance of contrast
stder <- sqrt(as.numeric(v))      ## standard error
tstat <- m/stder      ## t statistic

##########For large sample##################
2*pnorm(abs(tstat), lower.tail=FALSE)
#############For small sample##############
#number of parameters for mean 
par1 <- length(coef)
#number of parameters for random effect variance
par2 <- n*(n-1)/2

number_of_parameters_estimated <- par1 +  par2 
error_df = nrow(data_long) - number_of_parameters_estimated
2*pt(abs(tstat), df=error_df, lower.tail = FALSE)


###################Time as continuous predictor (not needed for this Qn)##############################
library(nlme)
modU <- gls(growth ~ Age*Gender, data = data_long,
               correlation = corSymm(form = ~ 1 | ID))

##########linear mixed model with random intercept#################
mod <- lme(growth ~ Age*Gender, random = ~1|ID, data = data_long) 

##########linear mixed model with random intercept + slope#################
modA <- lme(growth ~ Age*Gender, random = ~1+Age|ID, data = data_long) 

modExp <- update(mod,correlation = corExp(form = ~ Age))
modGaus <- update(mod,correlation = corGaus(form = ~ Age))

anova(modU, modExp)
anova(modU, modGaus)
anova(modU, modA)



########################Two stage model#################################
res.list <- lmList(growth ~ Age | ID, data=data_long)

# extract the estimated model coefficients (intercepts and slopes) and the corresponding variance-covariance
b <- lapply(res.list, coef)
V <- lapply(res.list, vcov)

estm <- rep(c("intercept","slope"), length(b))
subj <- rep(names(b), each=2)

#one long vector with the model coefficients and the corresponding block-diagonal variance-covariance matrix
library(metafor)
b <- unlist(b)
V <- bldiag(V)


#multivariate meta-analysis with the model coefficients
#The V matrix contains the variances and covariances of the sampling errors. We also allow for heterogeneity in the true outcomes (i.e., coefficients) and allow them to be correlated (by using an unstructured variance-covariance matrix for the true outcomes).
Sex <- dummy(as.factor(rep(data$Gender, each=2)))
estmd <- dummy(as.factor(estm))

Xmat <- cbind("Intercept"=1-estmd, "Slope"=estmd, "Inte_Male"=(1-estmd)*Sex, "Sl_Male"=estmd * Sex)

colnames(Xmat) <- c("Intercept", "Slope", "Intercept_male", "Slope_male")

res2 <- gls(unlist(b) ~ Xmat - 1, correlation = corSymm(form = ~ 1 | subj)) #lme(unlist(b) ~ Xmat - 1, V, random = ~ estm | subj)#, struct="UN")
#res2

summary(res2)