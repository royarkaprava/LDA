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
cs1Exp <- corExp(1, form = ~ Age|ID)

cs1Exp <- Initialize(cs1Exp, data_new)
corMatrix(cs1Exp)[[1]]

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
data_new$measurement <- log(data_new$measurement+1)
data_new$Baseline <- log(data_new$Baseline+1)


library(nlme)
res <- lme(measurement ~ Week, random = ~1|Subject, data = data_new)

res <- lme(measurement ~ Week, random = ~as.factor(Week)|Subject, data = data_new)

res.AR1 <- update(res, correlation = corAR1())
res.ARMA11 <- update(res, corr = corARMA(p = 1, q = 1))

fm1 <- lme(measurement ~ Week, random = ~1|Subject, data = data_new)
ACF(fm1, maxLag = 3)


ctrl <- lmeControl(opt='optim'); #lmeControl(msMaxIter = 1000, msMaxEval = 1000)

res <- lme(measurement ~ Week, random = ~Week|Subject,control = ctrl, data = data_new)

##Generalized least square############
fm1 <- gls(measurement ~ Week, data_new,
           correlation = corAR1(form = ~ 1 | Subject))

#With unrestricted covariance
fm1 <- gls(measurement ~ Week, data_new,
           correlation = corSymm(form = ~ 1 | Subject))