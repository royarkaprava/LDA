data <- read.table("H:/Documents/GitHub/LDA/Codes/Glycol.txt", quote="\"", comment.char="")

colnames(data) <- c("Litter_ID", "Dose", "Fetal_Weight", "Fetal_Malformations")

# Here the data is not longitudinal, however, there are multiple observations from
# a same subject under different doses. This is thus clustered data and dependence
# among the observations from the same id should be taked into account. We will thus
# use a mixed model to do that.

# Since the data is not longitudinal, not all covariances are appropriate.

data$Litter_ID <- as.factor(data$Litter_ID)

library(nlme)
res <- lme(Fetal_Weight ~ sqrt(Dose), random = ~1|Litter_ID, data = data)

res <- lme4::lmer(Fetal_Weight ~ sqrt(Dose)+ (1|Litter_ID), data = data)

###############3-level############

data <- read.table("H:/Documents/GitHub/LDA/Codes/Cessation.txt", quote="\"", comment.char="")


colnames(data) <- c("School_ID", "Class_ID", "Resistance_Curriculum",
                    "Prevention_Program", "THKS_score", "Post_interventions_THKS")


data$School_ID <- as.factor(data$School_ID)
data$Class_ID <- as.factor(data$Class_ID)

##We need to incorporate two groupings, induced by schools and classes.
res1 <- lme4::lmer(Post_interventions_THKS ~ THKS_score + Resistance_Curriculum+Prevention_Program+Resistance_Curriculum*Prevention_Program +(1|School_ID) +(1|Class_ID), data = data)

summary(res1)

#####If we drop one grouping say the one induced by class_ID, the t-values as well as the estimates change.
res2 <- lme4::lmer(Post_interventions_THKS ~ THKS_score + Resistance_Curriculum+Prevention_Program+Resistance_Curriculum*Prevention_Program +(1|School_ID), data = data)

summary(res2)

# It is thus important to include all groupings into account and choose res1 over res2.