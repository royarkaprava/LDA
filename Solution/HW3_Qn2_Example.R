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

data_long$Gender <- as.factor(data_long$Gender)


mod <- lmer(growth ~ Age+Gender + (1+Age|ID), data = data_long) 

model1 <- lmer(growth ~ Age+Gender+  (1+Gender+Age|ID), data = data_long) 

model2 <- lmer(growth ~ Age+Gender +(1+Age|ID) + (1|Gender), data = data_long)