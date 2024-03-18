data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

# colnames(data) = c("Treatment Group", "Subject", 
#                    "Baseline", "Month 6", "Month 12", 
#                    "Month 20", "Month 24")

library(mice)

data$V2[-c(1:62)] <- data$V2[-c(1:62)]+62

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

##############################MICE#########################################
data <- matrix(as.numeric(unlist(data)), nrow(data), ncol(data))

##Extracting the outcome part of the data
dataO <- data[, -c(1:2)] 
imp <- mice(dataO, m = 2)
#The follwoing code will give you an imputed matrix averaging all the imputed values
dataN <- complete(imp)

newdataA <- cbind(data[,1:2], dataN)

#If each imputed data is of interest.
dataNF <- complete(imp, action= 'long')

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

