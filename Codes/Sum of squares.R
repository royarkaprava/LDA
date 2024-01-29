data <- read.table("Treatment of Lead Exposed Children Trial.txt", quote="\"", comment.char="")

#Columns represent ID, Treatment Group, V3=Lead Level Week 0, V4=Lead Level Week 1, V5=Lead Level Week 4, V6=Lead Level Week 6. 

library(lme4)

#Convert Wide to long format
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(data, Week, measurement, V3:V6, factor_key=TRUE)
data_long

data_long <- data_long[order(data_long$V1), ]
data_long

#####################ANOVA analysis###################
data_new = data_long
data_new$Week <- as.character(data_new$Week)
data_new$Week[grep("V3", data_long$Week)] = "0"
data_new$Week[grep("V4", data_long$Week)] = "1"
data_new$Week[grep("V5", data_long$Week)] = "4"
data_new$Week[grep("V6", data_long$Week)] = "6"
data_new$Week <- as.numeric(data_new$Week)
