data <- read.table("fev1.txt", quote="\"", comment.char="")

colnames(data) = c("ID", "Height", "Age", "Initial Height", "Initial Age", "LogFEV1")

data_new <- data

data_new$ID <- factor(data_new$ID)

library(nlme)
library(splines)
M = 4
data_new$Age = data_new$Age/max(data_new$Age)
X <- bs(data_new$Age, df=M, degree = 3) 
res <- lme(LogFEV1 ~ X, random = ~1|ID, data = data_new)


#############################################################################
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
data_new$measurement <- log(data_new$measurement)
data_new$Baseline <- log(data_new$Baseline)


M <- 3
X <- bs(data_new$Week, df=M, degree = 3) 

res <- lme(measurement ~ X, random = ~1|Subject, data = data_new)
res <- lme(measurement ~ X, random = ~X|Subject, data = data_new)

library(splines)

vec <- (1:100)/100

X <- fda::bsplineS(vec, breaks=seq(0,1,length=10), norder = 3) 

plot(vec, X[,1], type='l', col =1, ylim=range(X))
points(vec, X[,2], type='l', col =2)
points(vec, X[,3], type='l', col =3)

vec <- (1:100)/100

X <- splines::bs(vec, df=M, degree = 3) 

plot(vec, X[,1], type='l', col =1, ylim=range(X))
points(vec, X[,2], type='l', col =2)
points(vec, X[,3], type='l', col =3)

#####################Using mgcv###################
gam_model <- gamm(measurement ~ s(Week, k = 4) +
                 s(Subject, bs = 're') +
                 s(Subject, Week, bs = 're'),
               data = data_new, method = 'REML')

########################Polynomial bases
library(nlme)
res1 <- lme(LogFEV1 ~ Age, random = ~1|ID, data = data_new)
res2 <- lme(LogFEV1 ~ Age + Age^2, random = ~1|ID, data = data_new)

anova(res1, res2)
