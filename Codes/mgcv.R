data <- read.table("H:/Documents/Course to teach/LDA/LDA/Slides/Treatment of Lead Exposed Children Trial.txt", quote="\"", comment.char="")

library(tidyr)
data_long <- gather(data, Week, measurement, V3:V6, factor_key=TRUE)
data_long <- data_long[order(data_long$V1), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V3", data_long$Week)] = "0"
data_new$Week[grep("V4", data_long$Week)] = "1"
data_new$Week[grep("V5", data_long$Week)] = "4"
data_new$Week[grep("V6", data_long$Week)] = "6"
data_new$Week <- as.numeric(data_new$Week)

library(mgcv)

basic_model4 <- gam(measurement~s(Week, k = 4)+as.factor(V2), data = data_new,
                   method = "REML")
basic_model3 <- gam(measurement~s(Week, k = 3)+as.factor(V2), data = data_new,
                    method = "REML")

#Lower AIC is better
AIC(basic_model3, basic_model4)

basic_summary <- summary(basic_model3)

basic_summary$p.table
basic_summary$s.table

basic_summary <- summary(basic_model4)

basic_summary$p.table
basic_summary$s.table


##########################################################################################
data <- read.table("fev1.txt", quote="\"", comment.char="")

colnames(data) = c("ID", "Height", "Age", "Initial Height", "Initial Age", "LogFEV1")

data_new <- data

data_new$ID <- factor(data_new$ID)

#No 'k' is pecified, gam() sets a default 'k' depending on the number of variables on which the smooth is built.

#One non-linear and one-linear
model1 <- gam(LogFEV1 ~ s(Age)+Height,
                      data = data_new, method = "REML")
sum_model <- summary(model1)

sum_model$s.table

#Two non-linear
model2 <- gam(LogFEV1 ~ s(Age)+s(Height),
             data = data_new, method = "REML")
sum_model <- summary(model2)

sum_model$s.table

#With interaction. Since it is a nonparametric model, s(Age, Height) already includes possible main-effects as well
#Hence, s(Age)+s(Height) part is dropped unlike linear models with iteractions.

model3 <- gam(LogFEV1 ~ s(Age, Height),
              data = data_new, method = "REML")
sum_model <- summary(model3)

sum_model$s.table

plot(model3, page = 1, scheme = 2)

#######Checking residuals
par(mfrow = c(2, 2))
gam.check(model3)

#Lower AIC is better
AIC(model1, model2, model3)


########################GAM with mixed modeling###############
mixed_model1 <-  gamm(LogFEV1 ~ s(Age, Height),data = data_new,random=list(ID=~1))

summary(mixed_model1$gam)
#vis.gam(mixed_model1$gam)

mixed_model2 <-  gamm(LogFEV1 ~ s(Age, Height),data = data_new,random=list(ID=~1+Age))

summary(mixed_model2$gam)
#vis.gam(mixed_model2$gam)

##########Another possiblity with non-linear random-effect as###################
m1_gam <- gamm(LogFEV1 ~ s(Age, Height) +
                s(ID, bs = 're') +
                s(ID, Age, bs = 're'),
              data = data_new, method = 'REML')

########The above model is equivalent to fitting the following linear model#############################
res <- lme(LogFEV1 ~ Age*Height, random = ~1+Age|ID, data = data_new)

summary(AR_model1$gam)
summary(AR_model2$gam)

Exp_model <- gamm(LogFEV1 ~ s(Age, Height),data = data_new,random=list(ID=~1),corExp(form = ~ Age))
Gaus_model <- gamm(LogFEV1 ~ s(Age, Height),data = data_new,random=list(ID=~1),corGaus(form = ~ Age))



######################AR correlations for lead data###########################
data <- read.table("H:/Documents/GitHub/LDA/Codes/Data on seizure counts.txt", quote="\"", comment.char="")

library(tidyr)
library(mgcv)
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
data_new$measurement <- log(data_new$measurement)
data_new$Baseline <- log(data_new$Baseline)

basic_model4 <- gam(measurement~s(Week, k = 4)+as.factor(Treatment), data = data_new,
                    method = "REML")
##################With AR correlation
AR_model1 <- gamm(measurement~s(Week, k = 4)+as.factor(Treatment),data = data_new,correlation=corAR1(form=~1|Subject))
AR_model2 <- gamm(measurement~s(Week, k = 4)+as.factor(Treatment),data = data_new,random=list(Subject=~1),correlation=corAR1())

ARMA_model <- gamm(measurement~s(Week, k = 4)+as.factor(Treatment),data = data_new,random=list(Subject=~1),correlation=corARMA(p = 1, q = 1))

