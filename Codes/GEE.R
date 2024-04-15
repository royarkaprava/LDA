###########################################################################################################
##########################################Binary case#######################################################
###########################################################################################################

toenail <- read.table("H:/Documents/GitHub/LDA/Codes/toenail.txt", quote="\"", comment.char="")

colnames(toenail) <- c("Subject", "Response", "Treatment", "Month", "Visit")

toenail$Subject <- as.factor(toenail$Subject)

range(table(toenail$Subject))

# install.packages("gee")
library(gee) # version 4.13-25
un_gee <- gee(Response~Treatment*Month,
               data=toenail, 
               id = Subject, 
               family = binomial,
               corstr = "unstructured")

###Autoregressive model will not work as the number of visits for one subject is 1.
ar_gee <- gee(Response~Treatment*Month,
              data=toenail, 
              id = Subject, 
              family = binomial,
              corstr = "AR-M",Mv = 1)

##########Dropping the subjects with one visit for AR covariance to work####
vec <- unique(toenail$Subject)[which(table(toenail$Subject)==1)]

inddrop <- which(toenail$Subject %in% vec) 

ar_gee <- gee(Response~Treatment*Month,
              data=toenail[-inddrop,], 
              id = Subject, 
              family = binomial,
              corstr = "AR-M",Mv = 1)


cs_gee <- gee(Response~Treatment*Month,
                  data=toenail, 
                  id = Subject, 
                  family = binomial,
                  corstr = "exchangeable")

library(geepack) # version 1.3.9
cs_gee1 <- geeglm(Response~Treatment*Month,
                 data=toenail, 
                 id = Subject, 
                 family = binomial,
                 corstr = "exchangeable")
summary(cs_gee1)

############This will not match with a mixed model under similar assumptions on the covariance#############
gm2 <- glmer(Response~Treatment*Month+(1|Subject),
             data=toenail,
             family=binomial)
summary(gm2)


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

data_new$Subject <- as.factor(data_new$Subject)

range(table(data_new$Subject))

un_gee <- gee(measurement ~ Week*Treatment,
              data=data_new, 
              id = Subject, 
              family = poisson,
              corstr = "unstructured")

ar_gee <- gee(measurement ~ Week*Treatment,
              data=data_new, 
              id = Subject, 
              family = poisson,
              corstr = "AR-M",Mv = 1, silent = T)

ar_gee2 <- geeglm(measurement ~ Week*Treatment,
              data=data_new, 
              id = Subject, 
              family = poisson,
              corstr = "ar1")

summary(ar_gee2)
############This will not match with a GLMM under similar assumptions on the covariance#############
data_newAR <- NULL
for(i in 1:length(unique(data_new$Subject))){
  subi <- which(data_new$Subject==unique(data_new$Subject)[i])
  
  temp <- data_new[subi,]
  
  data_newAR <- rbind(data_newAR, cbind(temp[-1, ], temp$measurement[-length(subi)]))
}
colnames(data_newAR)[7] <- "Yt_1"


gm1 <- glmer(measurement ~ Week*Treatment+Yt_1 +(1|Subject), data = data_newAR,
             family=poisson)
summary(gm1)



library(geepack) # version 1.3.9
cs_gee <- geeglm(measurement ~ Week*Treatment,
                   data=data_new, 
                   id = Subject, 
                   family = poisson,
                   corstr = "exchangeable")
