###########################################################################################################
##########################################Binary case#######################################################
###########################################################################################################

toenail <- read.table("H:/Documents/GitHub/LDA/Codes/toenail.txt", quote="\"", comment.char="")

colnames(toenail) <- c("Subject", "Response", "Treatment", "Month", "Visit")

library(lme4)

#########With interactions############
###Visit code as factor
gm1 <- glmer(Response~Treatment*as.factor(Visit)+(1|Subject),
             data=toenail,
             family=binomial)
summary(gm1)
car::Anova(gm1, type=3)
###Time as continuous
gm2 <- glmer(Response~Treatment*Month+(1|Subject),
             data=toenail,
             family=binomial)
summary(gm2)

car::Anova(gm2, type=3)
###Adding random slope for Time
gm3 <- glmer(Response~Treatment*Month+(1+Month|Subject),
             data=toenail,
             family=binomial)

summary(gm3)

car::Anova(gm3, type=3)

################Without interaction model for cases without significant interaction#######################
gm1p <- glmer(Response~Treatment+as.factor(Visit)+(1|Subject),
             data=toenail,
             family=binomial)
summary(gm1p)

car::Anova(gm1p, type=2)

gm3p <- glmer(Response~Treatment+Month+(1+Month|Subject),
             data=toenail,
             family=binomial)

summary(gm3p)

car::Anova(gm3p, type=2)

###########################################################################################################
##########################################Count case#######################################################
###########################################################################################################

data <- read.table("~/GitHub/LDA/Codes/Data on seizure counts.txt", quote="\"", comment.char="")

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


gm1 <- glmer(measurement ~ Week*Treatment +(1|Subject), data = data_new,
             family=poisson)
summary(gm1)

gm2 <- glmer(measurement ~ Week*Treatment +(1+Week|Subject), data = data_new,
             family=poisson)
summary(gm2)

################No significant interaction observed#############################
gm1p <- glmer(measurement ~ Week+Treatment +(1|Subject), data = data_new,
             family=poisson)
summary(gm1p)

gm2p <- glmer(measurement ~ Week+Treatment +(1+Week|Subject), data = data_new,
             family=poisson)
summary(gm2p)


###############We can also supply Week as factor too

######################Autoregressive fitting#################################
data_newAR <- NULL
for(i in 1:length(unique(data_new$Subject))){
  subi <- which(data_new$Subject==unique(data_new$Subject)[i])
  
  temp <- data_new[subi,]
  
  data_newAR <- rbind(data_newAR, cbind(temp[-1, ], temp$measurement[-length(subi)]))
}
colnames(data_newAR)[7] <- "Yt_1"


gm1 <- glmer(measurement ~ Week+Treatment+Yt_1 +(1|Subject), data = data_newAR,
             family=poisson)
summary(gm1)

gm2 <- glmer(measurement ~ Week+Treatment+Yt_1 +(1+Week|Subject), data = data_newAR,
             family=poisson)
summary(gm2)

################Interestingly autoregressive terms are significant