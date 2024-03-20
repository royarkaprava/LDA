data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

# colnames(data) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

library(mice)

data$V2[-c(1:62)] <- data$V2[-c(1:62)]+65

data$V3 <- as.numeric(data$V3)
data$V4 <- as.numeric(data$V4)
data$V5 <- as.numeric(data$V5)
data$V6 <- as.numeric(data$V6)
data$V7 <- as.numeric(data$V7)

###Complete case analysis
library(tidyr)
ind <- unique(c(which(is.na(as.numeric(data$V5))),which(is.na(as.numeric(data$V6))),which(is.na(as.numeric(data$V7)))))
data_long <- gather(data[-ind,], Week, measurement, V3:V7, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V3", data_long$Week)] = "0"
data_new$Week[grep("V4", data_long$Week)] = "6"
data_new$Week[grep("V5", data_long$Week)] = "12"
data_new$Week[grep("V6", data_long$Week)] = "20"
data_new$Week[grep("V7", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

data_new$measurement <- as.numeric(data_new$measurement)

library(nlme)
summary(fm1 <- lme(measurement ~ Week, random = ~1|Subject, data = data_new))


###Available-Data Analysis
##Running the analysis dropping the NA's
library(tidyr)
data_long <- gather(data, Week, measurement, V3:V7, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V3", data_long$Week)] = "0"
data_new$Week[grep("V4", data_long$Week)] = "6"
data_new$Week[grep("V5", data_long$Week)] = "12"
data_new$Week[grep("V6", data_long$Week)] = "20"
data_new$Week[grep("V7", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

data_new$measurement <- as.numeric(data_new$measurement)

library(nlme)
summary(fm1 <- lme(measurement ~ Week, random = ~1|Subject, data = data_new, na.action=na.omit))

#################################################
##############################MICE in general#########################################
#################################################
data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

# colnames(data) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

data$V2[-c(1:62)] <- data$V2[-c(1:62)]+65

data$V3 <- as.numeric(data$V3)
data$V4 <- as.numeric(data$V4)
data$V5 <- as.numeric(data$V5)
data$V6 <- as.numeric(data$V6)
data$V7 <- as.numeric(data$V7)

##Extracting the outcome part of the data
library(mice)
dataO <- data[, -c(1:2)]
imp <- mice(dataO, m = 2)
#The follwoing code will give you an imputed matrix averaging all the imputed values
dataN <- mice::complete(imp)

newdataA <- cbind(data[,1:2], dataN)

#If each imputed data is of interest.
dataNF <- mice::complete(imp, action= 'long')

dataN1 <- dataNF[which(dataNF$.imp==1), -c(1:2)] #Dropping first two columns as they are not needed
dataN2 <- dataNF[which(dataNF$.imp==2), -c(1:2)]

newdata1 <- cbind(data[,1:2], dataN1)
newdata2 <- cbind(data[,1:2], dataN2)

# colnames(newdataA) = colnames(newdata1) = colnames(newdata2) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

##Running the analysis for newdata1
library(tidyr)
data_long <- gather(newdata1, Week, measurement, V1:V5, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V1", data_long$Week)] = "0"
data_new$Week[grep("V2", data_long$Week)] = "6"
data_new$Week[grep("V3", data_long$Week)] = "12"
data_new$Week[grep("V4", data_long$Week)] = "20"
data_new$Week[grep("V5", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))


##Running the analysis for newdata2
library(tidyr)
data_long <- gather(newdata2, Week, measurement, V1:V5, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V1", data_long$Week)] = "0"
data_new$Week[grep("V2", data_long$Week)] = "6"
data_new$Week[grep("V3", data_long$Week)] = "12"
data_new$Week[grep("V4", data_long$Week)] = "20"
data_new$Week[grep("V5", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))

##Running the analysis for newdataA
library(tidyr)
data_long <- gather(newdataA, Week, measurement, V1:V5, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V1", data_long$Week)] = "0"
data_new$Week[grep("V2", data_long$Week)] = "6"
data_new$Week[grep("V3", data_long$Week)] = "12"
data_new$Week[grep("V4", data_long$Week)] = "20"
data_new$Week[grep("V5", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))

#################################################
###################MICE with monotine missing###################################
#################################################
data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

# colnames(data) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

data$V2[-c(1:62)] <- data$V2[-c(1:62)]+65

##Extracting the outcome part of the data
library(mice)
dataO <- data[, -c(1:2)] 
imp <- mice(dataO, m = 2)
#The follwoing code will give you an imputed matrix averaging all the imputed values
dataN <- mice::complete(imp, visit="monotone")

newdataA <- cbind(data[,1:2], dataN)

#If each imputed data is of interest.
dataNF <- mice::complete(imp, action= 'long', visit="monotone")

dataN1 <- dataNF[which(dataNF$.imp==1), -c(1:2)] #Dropping first two columns as they are not needed
dataN2 <- dataNF[which(dataNF$.imp==2), -c(1:2)]

newdata1 <- cbind(data[,1:2], dataN1)
newdata2 <- cbind(data[,1:2], dataN2)

library(tidyr)
data_long <- gather(newdata1, Week, measurement, V1:V5, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V1", data_long$Week)] = "0"
data_new$Week[grep("V2", data_long$Week)] = "6"
data_new$Week[grep("V3", data_long$Week)] = "12"
data_new$Week[grep("V4", data_long$Week)] = "20"
data_new$Week[grep("V5", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))


##Running the analysis for newdata2
library(tidyr)
data_long <- gather(newdata2, Week, measurement, V1:V5, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V1", data_long$Week)] = "0"
data_new$Week[grep("V2", data_long$Week)] = "6"
data_new$Week[grep("V3", data_long$Week)] = "12"
data_new$Week[grep("V4", data_long$Week)] = "20"
data_new$Week[grep("V5", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))

##Running the analysis for newdataA
library(tidyr)
data_long <- gather(newdataA, Week, measurement, V1:V5, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V1", data_long$Week)] = "0"
data_new$Week[grep("V2", data_long$Week)] = "6"
data_new$Week[grep("V3", data_long$Week)] = "12"
data_new$Week[grep("V4", data_long$Week)] = "20"
data_new$Week[grep("V5", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))

#################################################
###################LOCF###########################
#################################################

data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

# colnames(data) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

library(mice)

data$V2[-c(1:62)] <- data$V2[-c(1:62)]+62

library(tidyr)
data_long <- gather(data, Week, measurement, V3:V7, factor_key=TRUE)
colnames(data_long) <- c("Treatment", "Subject", "Week", "measurement")

data_long <- data_long[order(data_long$Subject), ]

data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V3", data_long$Week)] = "0"
data_new$Week[grep("V4", data_long$Week)] = "6"
data_new$Week[grep("V5", data_long$Week)] = "12"
data_new$Week[grep("V6", data_long$Week)] = "20"
data_new$Week[grep("V7", data_long$Week)] = "24"
data_new$Week <- as.numeric(data_new$Week)

data_new$Treatment <- as.factor(data_new$Treatment)

data_new$measurement <- as.numeric(data_new$measurement)

data_new <- DescTools::LOCF(data_new)

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))

#################################################
##########################sensitivity#####################
#################################################
data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

# colnames(data) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

data$V2[-c(1:62)] <- data$V2[-c(1:62)]+65

data$V3 <- as.numeric(data$V3)
data$V4 <- as.numeric(data$V4)
data$V5 <- as.numeric(data$V5)
data$V6 <- as.numeric(data$V6)
data$V7 <- as.numeric(data$V7)

delta <- c(-mean(unlist(data[,-c(1:2)]),na.rm=T)*(5:1)/10, mean(unlist(data[,-c(1:2)]),na.rm=T)*(0:5)/10)

ini <- mice(data, maxit=0, print=F)
pred <- ini$pred
#pred[c("sws", "ps"), "ts"] <- 0
post <- ini$post
imp.all.undamped <- vector("list", length(delta))
for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][, i] <- imp[[j]][, i] +", d)
  post["V1"] <- cmd
  imp <- mice(data, pred=pred, post = post, maxit = 10, seed = i * 22, print=FALSE)
  imp.all.undamped[[i]] <- imp
}
output <- sapply(imp.all.undamped, function(x) pool(with(x, lm(cbind(V3,V4,V5,V6,V7)~V1)))$qbar)
cbind(delta, as.data.frame(t(output)))


#Wide format analysis
fit <- lm(cbind(V3,V4,V5,V6,V7)~V1, data=mice::complete(imp.all.undamped[[1]]))

idata <- data.frame(Week = factor(c("V3","V4","V5","V6","V7")))
idata

rmaov <- car::Anova(fit, idata=idata, idesign = ~Week,  type=3)
rmaov

#Wide format analysis
fit <- lm(cbind(V3,V4,V5,V6,V7)~V1, data=mice::complete(imp.all.undamped[[2]]))

idata <- data.frame(Week = factor(c("V3","V4","V5","V6","V7")))
idata

rmaov <- car::Anova(fit, idata=idata, idesign = ~Week,  type=3)
rmaov

#Wide format analysis
fit <- lm(cbind(V3,V4,V5,V6,V7)~V1, data=mice::complete(imp.all.undamped[[3]]))

idata <- data.frame(Week = factor(c("V3","V4","V5","V6","V7")))
idata

rmaov <- car::Anova(fit, idata=idata, idesign = ~Week,  type=3)
rmaov

#Wide format analysis
fit <- lm(cbind(V3,V4,V5,V6,V7)~V1, data=mice::complete(imp.all.undamped[[4]]))

idata <- data.frame(Week = factor(c("V3","V4","V5","V6","V7")))
idata

rmaov <- car::Anova(fit, idata=idata, idesign = ~Week,  type=3)
rmaov

#Wide format analysis
fit <- lm(cbind(V3,V4,V5,V6,V7)~V1, data=mice::complete(imp.all.undamped[[5]]))

idata <- data.frame(Week = factor(c("V3","V4","V5","V6","V7")))
idata

rmaov <- car::Anova(fit, idata=idata, idesign = ~Week,  type=3)
rmaov