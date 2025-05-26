

############################### TASK 2 #######################################


#-----------------------------------------------------------------------------#

########################### TASK 2.1 ##########################################





# Load libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(knitr)
library(gridExtra)
library(corrplot)

#Load the dataset
bank_data<-read.csv("E:/Msc Big Data Analytics/SEMESTER 2/Data Analysis/Bank_Churn.csv", header = TRUE)
head(bank_data)

#Check the structure of the dataset
str(bank_data)
summary(bank_data)

#Check for the null
colSums(is.na(bank_data))


#Check the no.of cols and rows
dim(bank_data)


#Check for duplicates
sum(duplicated(bank_data))



# 1. First I will examine the churn rate
churn_rate<-mean(bank_data$Exited)*100
cat("Overall Churn Rate:",round(churn_rate,2),"%\n")


#Bar plot of churn distribution
ggplot(bank_data, aes(x = factor(Exited), fill = factor(Exited))) +
  geom_bar(aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("green4", "red3")) +  
  labs(title = "Overall Customer Churn Rate", 
       x = "Churned (1 = Yes, 0 = No)", 
       y = "Percentage") +
  theme_minimal()


#2. Examine churn rate by categorical variables


#2.1 Churn Rate by country

country_churn<- bank_data %>%
  group_by(Geography) %>%  
  summarise(Churn_Rate = mean(Exited)*100)  

kable(country_churn,caption = "Churn Rate by Country")



#Plot
ggplot(bank_data,aes(x=Geography,fill = factor(Exited)))+
  geom_bar(position = "fill")+  
  scale_fill_manual(values = c("green4", "red3")) +  
  labs(title = "Churn Rate by Country",
       y="Percentage",fill="Churned")+
  theme_minimal()  




# 2.2 Churn Rate by Gender
gender_churn<- bank_data %>%
  group_by(Gender) %>%  
  summarise(Churn_Rate = mean(Exited)*100)  

kable(gender_churn,caption = "Churn Rate by Gender")




#Plot
ggplot(bank_data,aes(x=Gender,fill = factor(Exited)))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("green4","red3"))+
  labs(title = "Churn Rate by Gender", 
       y="Percentage", fill="Churned")+
  theme_minimal()


# 2.3 Churn Rate by Credit Card Ownership  

# Churn by Credit Card  

card_churn<- bank_data %>%
  group_by(HasCrCard) %>%
  summarise(Churn_Rate=mean(Exited)*100)

kable(card_churn,caption = "Churn Rate by Credit Card Ownership")


#Plot
ggplot(bank_data,aes(x=factor(HasCrCard),fill = factor(Exited)))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("green4","red3"))+
  labs(title = "Churn Rate by Credit Card Ownership",
       x= "Has Credit Card (1=YES, 0=NO)",
       y="Percentage",
       fill="Churned")+
  theme_minimal()



# 2.4 Churn Rate Active Membership

# Churn by Active Status
active_churn<- bank_data %>%
  group_by(IsActiveMember) %>%
  summarise(Churn_Rate = mean(Exited)*100)


kable(active_churn,caption = "Churn Rate by Active Member")


#Plot
ggplot(bank_data,aes(x=factor(IsActiveMember),fill = factor(Exited)))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("green4","red3"))+
  labs(title = "Churn Rate by Active Membership", x= "Is Active Member (1= YES, 0=NO)",
       y= "Percentage",
       fill= "Churned")+
  theme_minimal()




# 3. Churn by Numerical Variables


# 3.1 Churn by Age

#Age distribution by churn

ggplot(bank_data,aes(x=Age, fill = factor(Exited)))+
  geom_histogram(aes(y=..density..),binwidth = 30,alpha=0.5)+
  geom_density(linewidth= 1, color= "black")+
  facet_wrap(~Exited, labeller=labeller(Exited= c("0"="Retained", "1"="Churned")))+
  labs(title = "Age Distribution by Churn Status", x= "Age",
       y= "Density")+
  theme_minimal()


# 3.2 Churn by Credit Score

# Credit Score vs. Churn

ggplot(bank_data,aes(x=CreditScore, fill = factor(Exited)))+
  geom_density(alpha=0.5)+
  labs(title ="Credit Score Distribution by Churn",
       x = "Credit Score",
       y= "Density",
       fill = "Churned")+
  theme_minimal()



# 3.3 Churn by Estimated Salary

# Salary vs. Churn

ggplot(bank_data,aes(x=EstimatedSalary, fill = factor(Exited)))+
  geom_density(alpha=0.5)+
  labs(title = "Salary Distribution by Churn", 
       x= "Estimated Salary",
       y= "Density",
       fill="Churned")+
  theme_minimal()
 


#3.4 Check the multicollinearity

# Select numerical variables

num_data<- bank_data %>%
  select(CreditScore,Age,Tenure,Balance,NumOfProducts,EstimatedSalary,Exited)

#Correlation matrix
cor_matrix<- cor(num_data,use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.8,
         diag = FALSE)









#-----------------------------------------------------------------------------#

##################################  TASK 2.2 ##################################




# load libraries
library(caret)
library(ROCR)
library(DMwR2)
library(xgboost)
library(pROC)
library(dplyr)


#Again check the summary statistic
summary(bank_data)

#Check for the class distribution

table(bank_data$Exited)
prop.table(table(bank_data$Exited))*100

#Summary statistics
summary(bank_data$Exited)

# Calculate the imbalance ratio
imbalance_ratio<- table(bank_data$Exited)[2]/table(bank_data$Exited)[1]
cat("Imbalance Ratio:", imbalance_ratio)



# Step 1: Remove unnecessary columns
bank_data_cleaned <- bank_data %>%
  select(-CustomerId, -Surname)

# Step 2: Convert categorical variables to factors
data_cleaned <- bank_data_cleaned %>%
  mutate(
    Geography = as.factor(Geography),
    Gender = as.factor(Gender),
    Exited = as.factor(Exited)
  )

# Step 3: One-hot encode all variables except Exited
dummy <- dummyVars("~ . -Exited", data = data_cleaned)
data_encoded <- data.frame(predict(dummy, newdata = data_cleaned))

# Step 4: Add the target variable back
data_encoded$Exited <- data_cleaned$Exited

# Step 5: Train-test split
set.seed(42)
index <- createDataPartition(data_encoded$Exited, p = 0.8, list = FALSE)
train_data <- data_encoded[index, ]
test_data  <- data_encoded[-index, ]


table(train_data$Exited)



# Load libraries
library(tidymodels)
library(themis)
library(randomForest)
library(pdp)
library(SHAPforxgboost)
library(caret)


# Step 7: Apply SMOTE

rec <- recipe(Exited ~ ., data = train_data) %>%
  step_smote(Exited)

prep_rec <- prep(rec)
train_data_balanced <- bake(prep_rec, new_data = NULL)

table(train_data_balanced$Exited)  # Check balance

 
# Step 8: Feature Scaling

# Identify numeric columns
numeric_cols <- train_data_balanced %>%
  select(where(is.numeric)) %>%
  names()

# Create preprocessing model
preproc <- preProcess(train_data_balanced[, numeric_cols], method = c("center", "scale"))

# Scale train and test data
train_data_scaled <- predict(preproc, train_data_balanced)
test_data_scaled <- predict(preproc, test_data)

# Add target variable back
train_data_scaled$Exited <- train_data_balanced$Exited


# Step 9: Logistic Regression

logit_model <- glm(Exited ~ ., data = train_data_scaled, family = "binomial")
summary(logit_model)


# Step 10: Random Forest

rf_model <- randomForest(Exited ~ ., data = train_data_scaled, ntree = 500, importance = TRUE)
print(rf_model)
varImpPlot(rf_model)



# Function to calculate multiple metrics
calculate_metrics <- function(predictions, actual) {
  cm <- confusionMatrix(predictions, actual)
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  precision <- cm$byClass['Precision']
  recall <- cm$byClass['Recall']
  f1 <- cm$byClass['F1']
  
  # Calculate AUC
  roc_obj <- roc(as.numeric(actual)-1, as.numeric(predictions)-1)
  auc_score <- auc(roc_obj)
  
  return(data.frame(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    AUC = auc_score
  ))
}

# Make predictions on test data for each model
logit_pred <- predict(logit_model, newdata = test_data_scaled, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, "1", "0") %>% factor(levels = c("0", "1"))

rf_pred <- predict(rf_model, newdata = test_data_scaled)

# Calculate metrics for each model
logit_metrics <- calculate_metrics(logit_pred_class, test_data_scaled$Exited)
rf_metrics <- calculate_metrics(rf_pred, test_data_scaled$Exited)


# Combine all metrics
all_metrics <- rbind(
  cbind(Model = "Logistic Regression", logit_metrics),
  cbind(Model = "Random Forest", rf_metrics)

)

# Print the comparison
print(all_metrics)




library(ggplot2)
library(tidyr)

# Reshape metrics for plotting
metrics_long <- gather(all_metrics, Metric, Value, -Model)

# Plot comparison
ggplot(metrics_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison",
       y = "Score", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate ROC curves
logit_roc <- roc(test_data_scaled$Exited, as.numeric(logit_pred_class))
rf_roc <- roc(test_data_scaled$Exited, as.numeric(rf_pred))


# Plot all ROC curves
plot(logit_roc, col = "blue", main = "ROC Curve Comparison")
plot(rf_roc, col = "green", add = TRUE)

# Add legend
legend(
  "bottomright",
  legend = c(
    paste("Logistic (AUC =", round(auc(logit_roc), 2), ")"),
    paste("Random Forest (AUC =", round(auc(rf_roc), 2), ")")
  ),
  col = c("blue", "green"),
  lwd = 2
)

   
       
           
       
######SAVE THE BEST MODEL###########
# Save the Random Forest model

best_model <- rf_model

# Also save the preprocessing object
preproc_obj <- preproc  

# Define X as the feature matrix excluding the target variable 'Exited'
X <- train_data_scaled %>%
  select(-Exited)  # Remove 'Exited' column to get the feature names

# Save the feature names 
feature_names <- colnames(X) 

# Create a list containing all necessary components
model_bundle <- list(
  model = best_model,
  preprocessor = preproc_obj,
  feature_names = feature_names,
  model_type = "Random Forest",
  version = "1.0",
  timestamp = Sys.time()
)

# Save the entire bundle
saveRDS(model_bundle, file = "best_churn_model.rds")

# Optionally save as .RData if preferred
save(model_bundle, file = "best_churn_model.RData")







#-----------------------------------------------------------------------------#

 ################################### TASK 2.3 #################################



# Ensure test data is a data frame and predictions are probabilities
rf_prob_predictions <- predict(rf_model, test_data_scaled[, feature_names], type = "prob")[, 2]

# Class predictions
rf_class_predictions <- factor(ifelse(rf_prob_predictions > 0.5, 1, 0), levels = c(0, 1))

# Add predictions to test data
test_data_with_preds <- test_data_scaled %>%
  mutate(
    Predicted_Probability = rf_prob_predictions,
    Predicted_Class = rf_class_predictions,
    Actual_Class = factor(Exited, levels = c(0, 1))
  )

# Confusion matrix
confusionMatrix(test_data_with_preds$Predicted_Class, test_data_with_preds$Actual_Class)

# ROC Curve
library(pROC)
roc_obj <- roc(as.numeric(as.character(test_data_with_preds$Actual_Class)), 
               test_data_with_preds$Predicted_Probability)
plot(roc_obj, main = "ROC Curve for Test Set Predictions", col = "darkblue", lwd = 2)
auc_val <- auc(roc_obj)
cat("Area Under the Curve (AUC):", auc_val, "\n")










#-----------------------------------------------------------------------------#

################################### TASK 2.4 #################################



# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)

# Select only relevant columns (excluding churn-related and IDs)
tenure_data <- bank_data %>%
  select(-Exited, -CustomerId, -Surname)

# Convert categorical variables to factors (important for Random Forest)
tenure_data <- tenure_data %>%
  mutate(
    Geography = as.factor(Geography),
    Gender = as.factor(Gender),
    HasCrCard = as.factor(HasCrCard),
    IsActiveMember = as.factor(IsActiveMember)
  )

# Train-test split (80/20)
set.seed(42)
train_index <- createDataPartition(tenure_data$Tenure, p = 0.8, list = FALSE)
train_data <- tenure_data[train_index, ]
test_data <- tenure_data[-train_index, ]


# Train linear regression
lm_model <- train(
  Tenure ~ .,
  data = train_data,
  method = "lm",
  trControl = trainControl(method = "none")  # No cross-validation for simplicity
)

# Check coefficients
summary(lm_model)



# Train Random Forest
rf_model <- train(
  Tenure ~ .,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "none"),  # No CV for simplicity
  ntree = 200,  # Number of trees
  importance = TRUE  # Track variable importance
)

# View variable importance
varImp(rf_model)
plot(varImp(rf_model), main = "Random Forest - Variable Importance")


# Function to calculate regression metrics
calculate_metrics <- function(model, test_data) {
  predictions <- predict(model, test_data)
  actual <- test_data$Tenure
  
  metrics <- data.frame(
    MAE = mean(abs(predictions - actual)),
    RMSE = sqrt(mean((predictions - actual)^2)),
    R2 = cor(predictions, actual)^2
  )
  return(metrics)
}

# Evaluate both models
lm_metrics <- calculate_metrics(lm_model, test_data)
rf_metrics <- calculate_metrics(rf_model, test_data)

# Combine results
results <- rbind(
  cbind(Model = "Linear Regression", lm_metrics),
  cbind(Model = "Random Forest", rf_metrics)
)

print(results)



# Most significant features in regression
coef(lm_model$finalModel) %>% 
  as.data.frame() %>% 
  arrange(desc(abs(.)))




# Top predictors
varImp(rf_model)$importance %>% 
  arrange(desc(Overall))




# Save the model
saveRDS(rf_model, "rf_tenure_model.rds")

# Load and predict function
predict_tenure <- function(new_data) {
  model <- readRDS("rf_tenure_model.rds")
  predict(model, new_data)
}

