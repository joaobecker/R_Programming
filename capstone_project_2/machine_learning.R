library("dplyr")
library("tidyr")

# First create Database for machine learning
wp_data_ml <- wp_data

# We want to predict what clients from a paid plan is more likely to unsubscribed from Wishpond's platform.

# Summarize the databse to paid subscribers or unsubscribe

wp_data_ml <- wp_data_ml %>% filter( plan_name == "Basic" | plan_name == "Pro" | plan_name == "Growth" | Unsubscribed == "TRUE")

# Unknowns to NA - In order to use Multiple Imputation (MICE), which as the name says will impute missing values with plausible data values.
wp_data_ml[wp_data_ml == "Unknown"] <- NA
wp_data_ml[wp_data_ml == "Other"] <- NA


# Plan Name - When customers unsubscribe, they are automatically sent to Starter plan, which means that this is not the plan they were subscribed to. 
# Thus, we will turn all the starters plan to NAs and then use mice to use probability and replace with the possible plans the customers used to be subscribed to.
wp_data_ml$plan_name[wp_data_ml$plan_name == "Starter"] <- NA


# Transforming characters into factors
wp_data_ml[c(2:4, 11:21)] <- lapply(wp_data_ml[c(2:4, 11:21)], as.factor)


# Database factored - To see what category does the number represents
wp_data_factored <- wp_data_ml

#Download MICE package
library("mice") 

# New database with variables with missing values only.

simple <- wp_data_ml[c("business_industry", "business_size", "interest", "sales_agent", "plan_name", "website", "support_agent", "cancel_feeling")]
set.seed(98)
imputed <- complete(mice(simple))

# During imputation some values were changed by adding a few spaces between characters, to fix it, it was used the code below:
wp_data_ml$business_size[wp_data_ml$business_size == "6  to 15"] <- "6 to 15"
wp_data_ml$business_size[wp_data_ml$business_size == "16-30"] <- "16 - 30"
wp_data_ml$business_size[wp_data_ml$business_size == "31-100"] <- "31 - 100"


# Now that we do not have any missing data on the variables, we replace the variables from wp_data_ml  with imputed variables
wp_data_ml$business_industry <- imputed$business_industry
wp_data_ml$business_size <- imputed$business_size
wp_data_ml$interest <- imputed$interest
wp_data_ml$sales_agent <- imputed$sales_agent
wp_data_ml$website <- imputed$website
wp_data_ml$support_agent <- imputed$support_agent
wp_data_ml$cancel_feeling <- imputed$cancel_feeling
wp_data_ml$plan_name <- imputed$plan_name

# Split data set into training and test data set.
wp_data_factored <- wp_data_ml

# We dont need pid and selected service on the data, so let's drop form the database
wp_data_ml <- wp_data_ml %>% select(-pid, -selected_service)


# RUNNING LASSO - In order to see what variables may cause overfitting in our model, we will use Lasso, which will indicate us what are the bes variables to use.
library(glmnet)
wp_data_num <- wp_data_ml
wp_data_num[c(1:2, 9:19)] <- lapply(wp_data_num[c(1:2, 9:19)], as.integer)
wp_data_num[c(1:2, 9:19)] <- wp_data_num[c(1:2, 9:19)] - 1
wp_data_num[c(1:2, 9:19)] <- lapply(wp_data_num[c(1:2, 9:19)], as.integer)
wp_data_num <- wp_data_num %>% select(-Unsubscribed)
CV <- cv.glmnet(as.matrix(wp_data_num), unsubscribed_db, family="binomial", type.measure = "class", alpha = 1, nlambda=100)
plot(CV)
fit <- glmnet(as.matrix(wp_data_num), unsubscribed_db, family="binomial", alpha = 1, lambda=CV$lambda.1se)
fit$beta[,1]


# Because it is a large database we can use less data on the training set and more on the test data set to increase confidence on new data
library("caTools")
set.seed(1000)
split <- sample.split(wp_data_ml$Unsubscribed, SplitRatio = 0.65)
train <- subset(wp_data_ml, split == TRUE)
test <- subset(wp_data_ml, split == FALSE)
wp_data_Log <- glm(Unsubscribed ~ business_industry + total_leads + interest + read_blog + sales_agent + sales + affiliated + plan_name + website + support_agent, data = train, family = binomial)
summary(wp_data_Log)


# Using the function above there were a few interesting things

# - If a customer is part of a business that has 6 to 15 employees there is a significant correlation of them staying
# - If a customer subscribe from an affiliated link there is a significant correlation of them staying
# - If a customer subscribe after coming from a demo with one of the sales agent there is a high correlation of them staying
# - There is a high correlation of remaining subscribed if a customer main interest to subscribe to Wishpond is to build a Landing Page or Pop-Up
# - There are high correlations with unsubscribing and a lot of the customer support agents, usually the customer support agents that reply most tickets. 

# Now we will use the predict function to predict how likely customers are from unsubscribing from Wishpond.
predicTest <- predict(wp_data_Log, type="response", newdata = test)

# Accuracy
table(test$Unsubscribed, predicTest > 0.5)

(86+840)/(86+287+68+840)
# We have an accuracy of 72%

# Baseline Method
(68+840)/(86+287+68+840)
# The baseline is of 70%, which indicates that our model is not very strong as it is too close to the baseline.


# Checking AUC, to see if the model can differentiate from high-risk to low-risk to unsubscribe customers. Our model can differentiate with 62% from those who are going to remain subscribed and are going to unsubscribe.
library("ROCR")
ROCRpred <- prediction(predicTest, test$Unsubscribed)
as.numeric(performance(ROCRpred, "auc")@y.values)


# Random Forest

library(randomForest)

wp_data_forest <- randomForest(Unsubscribed ~ business_industry + total_leads + interest + read_blog + sales_agent + sales + affiliated + plan_name + website + support_agent, data = train, nodesize=25, ntree=200)
PredictForest = predict(wp_data_forest, newdata = test)
table(test$Unsubscribed, PredictForest)

# Accuracy from Random Forest Model beats logistic regression by 5%
(108+833)/(265+77+108+831)

# Our model beat baseline by 7%, which means our model did a good job in predicting if a customer is going to unsubscribe or not.
(77+833)/(265+77+108+831)
