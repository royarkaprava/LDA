library(nlme)
#head(Orthodont)

data <- read.table("fev1.txt", quote="\"", comment.char="")

colnames(data) = c("Subject", "Height", "age", "Initial Height", "Initial Age", "LogFEV1")

data_new <- data#[, c(6, 3, 1, 4)]

data_new$Subject <- factor(data_new$Subject, ordered = T)


res1 <- lme(LogFEV1 ~ age, random = ~ age | Subject, data=data_new)
#summary(res1)

resgls1 <- gls(LogFEV1 ~ age, data_new,
           correlation = corSymm(form = ~ 1 | Subject))

############Two-stage model#####
res.list <- lmList(LogFEV1 ~ age | Subject, data=data_new)

# extract the estimated model coefficients (intercepts and slopes) and the corresponding variance-covariance
b <- lapply(res.list, coef)
V <- lapply(res.list, vcov)

estm <- rep(c("intercept","slope"), length(b))
subj <- rep(names(b), each=2)

#one long vector with the model coefficients and the corresponding block-diagonal variance-covariance matrix
library(metafor)
b <- unlist(b)
V <- bldiag(V)


#multivariate meta-analysis with the model coefficients
#The V matrix contains the variances and covariances of the sampling errors. We also allow for heterogeneity in the true outcomes (i.e., coefficients) and allow them to be correlated (by using an unstructured variance-covariance matrix for the true outcomes).
res2 <- rma.mv(b ~ estm - 1, V, random = ~ estm | subj, struct="UN")
#res2

summary(res1)
summary(res2)

summary(resgls1)
