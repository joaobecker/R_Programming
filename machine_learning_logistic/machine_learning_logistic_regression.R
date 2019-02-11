# Uploading NHIS data into R Studio
NH11 <- readRDS("dataSets/NatHealth2011.rds")

# The code below explain the labels for each attribute (variable)
labs <- attributes(NH11)$labels

# Download package for computing & graphing predicted values
library(effects)


##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.



#clean data --> everwrk
levels(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

#logistic regression
working_gen <- glm(everwrk~age_p + r_maritl,
                   data=NH11, family="binomial")

# Understanding how significant each variable is
summary(working_gen)

# Coeffiecient
coef(summary(working_gen))

# Making Coeffiecient easier to understand
working_gen_easy <- coef(summary(working_gen))
working_gen_easy[, "Estimate"] <- exp(coef(working_gen))
working_gen_easy[, "Estimate"] <- exp(coef(working_gen))


# Prediction Models - All marital status

# Setting it up to predict diffferent lvl of marital status for people at age 33, 63

#Does not work
pred_work_0 <- with(NH11,
                    expand.grid(age_p = c(10, 14),
                                r_maritl = "0 Under 14 years"))

pred_work_1 <- with(NH11,
                    expand.grid(age_p = c(33, 63),
                                r_maritl = "1 Married - spouse in household"))

pred_work_2 <- with(NH11,
                    expand.grid(age_p = c(33, 63),
                                r_maritl = "2 Married - spouse not in household"))
#Did not work
pred_work_3 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 63),
                                r_maritl = "3 Married - spouse in household unknown"))

pred_work_4 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 63),
                                r_maritl = "4 Widowed"))

pred_work_5 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 63),
                                r_maritl = "5 Divorced"))

pred_work_6 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 44, 55, 63),
                                r_maritl = "6 Separated"))

pred_work_7 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 44, 55, 63),
                                r_maritl = "7 Never married"))

pred_work_8 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 44, 55, 63),
                                r_maritl = "8 Living with partner"))
# Does not work
pred_work_9 <- with(NH11,
                    expand.grid(age_p = c(24, 33, 44, 55, 63),
                                r_maritl = "9 Unknown marital status"))



# Predictions at different levels

# --- 1 Married - spouse in household
cbind(pred_work_1, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_1))

# --- 2 Married - spouse not in household
cbind(pred_work_2, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_2))

# --- 4 Widowed
cbind(pred_work_4, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_4))

# --- 5 Divorced
cbind(pred_work_5, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_5))
# --- 6 Separated
cbind(pred_work_6, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_6))
# --- 7 Never married
cbind(pred_work_7, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_7))

# --- 8 Living with partner
cbind(pred_work_8, predict(working_gen, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = pred_work_8))



# Predicted Values On A Graph
plot(allEffects(working_gen))
