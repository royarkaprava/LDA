data <- read.table("Treatment of Lead Exposed Children Trial.txt", quote="\"", comment.char="")

#Wide format analysis
fit <- lm(cbind(V3,V4,V5,V6)~V2, data=data)

idata <- data.frame(Week = factor(c("V3","V4","V5","V6")))
idata

rmaov <- car::Anova(fit, idata=idata, idesign = ~Week,  type=3)
rmaov

#By default, because we have used a multivariate DV, this will show a so-called MANOVA (Multivariate ANalysis of VAriance). To obtain an ANOVA, we need to set the multivariate argument in the summary function to

summary(rmaov, multivariate=FALSE)

#In long format analysis in afex package
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(data, Week, measurement, V3:V6, factor_key=TRUE)

data_long <- data_long[order(data_long$V1), ]

#Here Week variable is a factor

afmod <- afex::aov_car(measurement ~ V2*Week + Error(V1/Week), data=data_long)

#Contrast is automatically applied V2, the treatment factor.

summary(afmod)

em_Week <- emmeans::emmeans(afmod, specs = ~ Week)

emmeans::contrast(em_Week, method=list("(Week4 + Week1)/2 - Week0" = c(-1,.5,.5,0), "Week6 - Week4" = c(0,0,1,-1)))

emmeans::contrast(em_Week, 'tukey')

em_pv <- emmeans::emmeans(afmod, specs = ~ Week*V2)
em_pv
