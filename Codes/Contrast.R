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
data_new$measurement <- log(data_new$measurement)
data_new$Baseline <- log(data_new$Baseline)

contr.poly(4)
contrasts(data_new$Week) = contr.poly(4)
summary(lm(measurement ~ Week, data=data_new))

library(nlme)
summary(lme(measurement ~ Week, random = ~1|Subject, data = data_new))
