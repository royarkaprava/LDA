data <- read.table("~/GitHub/LDA/Codes/cholesterol.txt", quote="\"", comment.char="")

colnames(data) = c("Treatment Group", "Subject", 
                   "Baseline", "Month 6", "Month 12", 
                   "Month 20", "Month 24")

library(mice)

data <- matrix(as.numeric(unlist(data)), nrow(data), ncol(data))

##Full data based imputation
imp <- mice(data, m = 2)
dataN <- complete(imp)

dataN1 <- complete(imp, 1)
dataN2 <- complete(imp, 2)