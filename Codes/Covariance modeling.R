data <- read.table("fev1.txt", quote="\"", comment.char="")

colnames(data) = c("ID", "Height", "Age", "Initial Height", "Initial Age", "LogFEV1")

data_new <- data

data_new$ID <- factor(data_new$ID, ordered = T)

ctrl <- lmeControl(msMaxIter = 1000, msMaxEval = 1000)

library(nlme)
res <- lme(LogFEV1 ~ Age, random = ~1+Age|ID, data = data_new)

###############Exponential covariance##########################
resExp <- update(res,correlation = corExp(form = ~ Age))

anova(res, resExp)

####Example covariance for differnet range parameter
cs1Exp <- corExp(6, form = ~ Age|ID)

cs1Exp <- Initialize(cs1Exp, data_new)
corMatrix(cs1Exp)

##################Gaussian covariance#############################
resGaus <- update(res,correlation = corGaus(form = ~ Age))

anova(res, resGaus)

AIC(resExp, resGaus)
#######################################################################
fm1 <- lme(LogFEV1 ~ Age, random = ~1|ID, data = data_new)
anova(fm1)
fm2 <- update(fm1, random = pdDiag(form=~Age, data=data_new))

fm1Over.lme <- lme(LogFEV1 ~ Age, data=data_new,
                   random=pdDiag(~Age) )

#####################################################################################
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
data_new$V1 <- factor(data_new$V1, order=T)

library(nlme)
res <- lme(measurement ~ Week, random = ~1|V1, data = data_new)

res.AR1 <- update(res, correlation = corAR1())
res.ARMA11 <- update(res, corr = corARMA(p = 1, q = 1))

fm1 <- lme(measurement ~ Week, random = ~1|V1, data = data_new)
ACF(fm1, maxLag = 3)


ctrl <- lmeControl(opt='optim'); #lmeControl(msMaxIter = 1000, msMaxEval = 1000)

res <- lme(measurement ~ Week, random = ~Week|V1,control = ctrl, data = data_new)

##Generalized least square############
fm1 <- gls(measurement ~ Week, data_new,
           correlation = corAR1(form = ~ 1 | V1))
