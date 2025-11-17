library(data.table)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggcorrplot)

setwd('C:/Users/yongw/OneDrive/Documents/Academics Related/NTU Modules/BC2406 Course Materials/Project Requirements and Guidelines')
diabetes.dt <- fread("diabetes_binary_5050split.csv")

# convert categorical variables to factors
var <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", "Smoker", 
         "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", 
         "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", 
         "DiffWalk", "Sex")

for (i in var) {
  diabetes.dt[[i]] <- as.factor(diabetes.dt[[i]])
}

diabetes_prev <- table(diabetes.dt$Diabetes_binary)
diabetes_prev

## extra visualisations ========================================================
# 1. diabetes prevalence by age
age_diabetes <- diabetes.dt[, .(Count = .N, 
                                Diabetes_Count = sum(as.numeric(as.character(Diabetes_binary)))), 
                            by = Age]
age_diabetes[, Diabetes_Rate := Diabetes_Count / Count]

p_age <- ggplot(age_diabetes, aes(x = Age, y = Diabetes_Rate)) +
  geom_line(color = "lightsalmon", size = 1.2) +
  geom_point(color = "salmon", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Diabetes Risk Increases with Age",
       subtitle = "Clear upward trend shows age as major risk factor",
       x = "Age Group (1=18-24, 13=80+)",
       y = "Diabetes Rate") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
p_age


# 2. diabetes by BMI distribution (with overweight & obese sections)
p_bmi <- ggplot(diabetes.dt, aes(x = BMI, fill = Diabetes_binary)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 25, linetype = "dashed", color = "orange") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red") +
  annotate("text", x = 25, y = 0.06, label = "Overweight", angle = 90, vjust = -0.5, size = 3) +
  annotate("text", x = 30, y = 0.06, label = "Obese", angle = 90, vjust = -0.5, size = 3) +
  labs(title = "BMI Distribution by Diabetes Status",
       subtitle = "Diabetic individuals show higher BMI (shifted right)",
       x = "Body Mass Index (BMI)", y = "Density", fill = "Diabetes Status") +
  scale_fill_manual(values = c("0" = "springgreen3", "1" = "salmon"),
                    labels = c("No Diabetes", "Diabetes")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
p_bmi


# 3. heatmap of diabetes risk by BP and cholesterol
risk_profile <- diabetes.dt[, .(Count = .N,
                                Diabetes_Count = sum(as.numeric(as.character(Diabetes_binary)))),
                            by = .(HighBP, HighChol)]
risk_profile[, Diabetes_Rate := Diabetes_Count / Count]
risk_profile[, BP_Label := ifelse(HighBP == "1", "High BP", "Normal BP")]
risk_profile[, Chol_Label := ifelse(HighChol == "1", "High Chol", "Normal Chol")]

p_risk_heatmap <- ggplot(risk_profile, aes(x = BP_Label, y = Chol_Label, fill = Diabetes_Rate)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(Diabetes_Rate * 100, 1), "%")), 
            size = 6, fontface = "bold") +
  scale_fill_gradient(low = "lightyellow", high = "tomato",
                      labels = scales::percent, name = "Diabetes\nRate") +
  labs(title = "Diabetes Risk by Blood Pressure and Cholesterol",
       subtitle = "Highest risk: Both High BP AND High Cholesterol",
       x = "Blood Pressure Status", y = "Cholesterol Status") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12))
p_risk_heatmap


## correlation analysis & heatmap ==============================================

num_vars <- diabetes.dt[, c("BMI", "GenHlth", "MentHlth", "PhysHlth", "Age", "Education", "Income")]

# Compute correlation matrix (Pearson by default)
cor_matrix <- cor(num_vars, use = "complete.obs")

ggcorrplot(cor_matrix,
           method = "circle",
           type = "lower",        # show only lower triangle
           lab = TRUE,            # add correlation labels
           lab_size = 3,
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix of Numeric Variables",
           ggtheme = ggplot2::theme_minimal)
cor_matrix

# converting factor columns -> char -> numeric type for correlation
diabetes_numeric <- copy(diabetes.dt)
for (col in names(diabetes_numeric)) {
  if (is.factor(diabetes_numeric[[col]])) {
    diabetes_numeric[[col]] <- as.numeric(as.character(diabetes_numeric[[col]]))
  }
}

# correlation matrix
cor_matrix <- cor(diabetes_numeric)
diabetes_cors <- cor_matrix[,"Diabetes_binary"]
diabetes_cors_sorted <- sort(abs(diabetes_cors), decreasing = TRUE) 
round(diabetes_cors[names(diabetes_cors_sorted)], 3) # reordered based on absolute values

# correlation heatmap
cor_matrix2 <- data.table(
  Var1 = rep(rownames(cor_matrix), each = ncol(cor_matrix)),
  Var2 = rep(colnames(cor_matrix), times = nrow(cor_matrix)),
  value = as.vector(cor_matrix)
)

p_heatmap <- ggplot(cor_matrix2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 8),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Correlation Heatmap by All Variables",
       x = "", y = "") +
  coord_fixed()
p_heatmap

# a focused heatmap w key variables with top 9 correlations
key_vars <- c("Diabetes_binary", "HighBP", "HighChol", "BMI", "GenHlth", 
              "Age", "HeartDiseaseorAttack", "DiffWalk", "Income")
cor_matrix_key <- cor(diabetes_numeric[, ..key_vars])

cor_matrix_key2 <- data.table(
  Var1 = rep(rownames(cor_matrix_key), each = ncol(cor_matrix_key)),
  Var2 = rep(colnames(cor_matrix_key), times = nrow(cor_matrix_key)),
  value = as.vector(cor_matrix_key)
)

p_heatmap_key <- ggplot(cor_matrix_key2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Correlation Heatmap - Key Predictors",
       x = "", y = "") +
  coord_fixed()
p_heatmap_key

# -> correlation analysis showing that General Health (0.294), High BP (0.263), and BMI (0.218) 
#    have the strongest associations with diabetes diagnosis. 
# -> heatmap visualisation showing clustering of related health indicators.


## variable analysis ============================================================

## categorical vars (using chi-square test)
categorical_vars <- c("HighBP", "HighChol", "CholCheck", "Stroke",
                      "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
                      "HvyAlcoholConsump", "AnyHealthcare", "DiffWalk", "Sex")

analysis_results_list <- lapply(categorical_vars, function(var) {
  cont_table <- table(diabetes.dt[[var]], diabetes.dt$Diabetes_binary)
  chi_test <- chisq.test(cont_table)
  rates <- prop.table(cont_table, margin = 1)[, 2] # diabetes rate
  list(Variable = var,
       Chi_Square = as.numeric(chi_test$statistic),
       P_Value = chi_test$p.value,
       Diabetes_Rate_Level0 = rates[1],
       # use rates[2] if it exists, otherwise use NA
       Diabetes_Rate_Level1 = if (length(rates) > 1) rates[2] else NA) 
})

categorical_stats <- rbindlist(analysis_results_list)
categorical_stats <- categorical_stats[order(-Chi_Square)] # sort results by decreasing chi-square 

# add significance based on rate difference
categorical_stats[, `:=`(Rate_Difference = abs(Diabetes_Rate_Level1 - Diabetes_Rate_Level0),
                         Significance = ifelse(P_Value < 0.001, "***", 
                                               ifelse(P_Value < 0.01, "**",
                                                      ifelse(P_Value < 0.05, "*", "ns"))))]
# *** = highly significant, ** = very significant, * = significant, ns = not significant

print(categorical_stats[, .(Variable, Chi_Square, P_Value, Significance, 
                            Rate_Difference = round(Rate_Difference, 3))])


## continuous variables (using T-test)
continuous_vars <- c("BMI", "GenHlth", "Age", "MentHlth", "PhysHlth", "Education", "Income")

continuous_results_list <- lapply(continuous_vars, function(var) {
  group0 <- diabetes.dt[Diabetes_binary == "0", get(var)]
  group1 <- diabetes.dt[Diabetes_binary == "1", get(var)]
  t_test <- t.test(group1, group0) 
  
  mean0 <- mean(group0, na.rm = TRUE)
  mean1 <- mean(group1, na.rm = TRUE)
  
  list(Variable = var,
       Mean_NoDiabetes = mean0,
       Mean_Diabetes = mean1,
       Mean_Difference = mean1 - mean0, # calculate difference
       T_Statistic = as.numeric(t_test$statistic),
       P_Value = t_test$p.value)
})

continuous_stats <- rbindlist(continuous_results_list)
continuous_stats <- continuous_stats[order(P_Value)]

# add significance based on p value
continuous_stats[, Significance := ifelse(P_Value < 0.001, "***", 
                                          ifelse(P_Value < 0.01, "**",
                                                 ifelse(P_Value < 0.05, "*", "ns")))]
# *** = highly significant, ** = very significant, * = significant, ns = not significant

print(continuous_stats[, .(Variable, Mean_NoDiabetes = round(Mean_NoDiabetes, 2), 
                           Mean_Diabetes = round(Mean_Diabetes, 2),
                           Mean_Difference = round(Mean_Difference, 2),
                           P_Value, Significance)])


## overall ranking
all_variables_summary <- data.table(Variable = c(categorical_stats$Variable, continuous_stats$Variable),
                                    Type = c(rep("Categorical", nrow(categorical_stats)), 
                                             rep("Continuous", nrow(continuous_stats))),
                                    P_Value = c(categorical_stats$P_Value, continuous_stats$P_Value),
                                    Test_Statistic = c(categorical_stats$Chi_Square, continuous_stats$T_Statistic),
                                    Effect_Size = c(categorical_stats$Rate_Difference, 
                                                    abs(continuous_stats$Mean_Difference)),
                                    Significance = c(categorical_stats$Significance, continuous_stats$Significance))
all_variables_summary <- all_variables_summary[order(P_Value)]
all_variables_summary[, Rank := 1:.N]

print(all_variables_summary[, .(Rank, Variable, Type, P_Value = sprintf("%.2e", P_Value), Significance)])

# higher chi-square indicates a stronger relationship with diabetes diagnosis
top3 <- all_variables_summary[1:3, Variable]
top3 # HighBP, DiffWalk & HighChol has the highest chi-square, hence strongest relationships
# as all variables with p < 0.001 are highly significant predictors
# total 19 strong variables


## logistic regression model diagnosis ==============================================================

set.seed(123)

# 70-30 split
train <- sample.split(Y = diabetes.dt$Diabetes_binary, SplitRatio = 0.7)
trainset <- subset(diabetes.dt, train == T)
testset <- subset(diabetes.dt, train == F)

# Second split: trainset → (train + validation)
train_val_index <- sample.split(Y = trainset$Diabetes_binary, SplitRatio = 0.7)

train_final <- subset(trainset, train_val_index == TRUE)
valset      <- subset(trainset, train_val_index == FALSE)

logres2 <- glm(Diabetes_binary ~ .,
               family = binomial, data = trainset)

threshold <- 0.5
pred_y <- predict(logres2, newdata = testset, type = "response")
predict.test <- ifelse(pred_y > threshold, 1, 0)

# confusion matrix 
con_matrix <- table(Actual = testset$Diabetes_binary, Predicted = predict.test)
con_matrix

TN <- con_matrix[1, 1] # 7770 
FP <- con_matrix[1, 2] # 2834
FN <- con_matrix[2, 1] # 2488 
TP <- con_matrix[2, 2] # 8116

accuracy <- (TP + TN) / sum(con_matrix)
sensitivity <- TP / (TP + FN)  # recall/true positive rate - ability to detect diabetes
specificity <- TN / (TN + FP)  # ability to detect non-diabetes
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

round(accuracy, 4) # 0.7491
round(1 - accuracy, 4) # 0.2509
round(sensitivity, 4) # 0.7654
round(specificity, 4) # 0.7327
round(precision, 4) # 0.7412
round(f1_score, 4) # 0.7531


## threshold optimisation =======================================================
# default threshold = 0.5 -> high accuracy, low sensitivity; want to find a threshold for better sensitivity (reduces FN rate)
# testing different thresholds
logres3 <- glm(Diabetes_binary ~ .,
               family = binomial, data = trainset)

threshold <- 0.5
pred_y <- predict(logres3, newdata = valset, type = "response")

thresholds <- seq(0.1, 0.9, by = 0.05)
threshold_results <- data.table(Threshold = numeric(),
                                Accuracy = numeric(),
                                Sensitivity = numeric(),
                                Specificity = numeric(),
                                F1_Score = numeric(),
                                False_Negatives = numeric())
for (t in thresholds) {
  pred_class <- ifelse(pred_y > t, 1, 0)
  cm <- table(Actual = valset$Diabetes_binary, Predicted = pred_class)
  if (ncol(cm) == 2 && nrow(cm) == 2) {
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    tp <- cm[2, 2]
    
    acc <- (tp + tn) / sum(cm)
    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    prec <- tp / (tp + fp)
    f1 <- 2 * (prec * sens) / (prec + sens)
    
    threshold_results <- rbind(threshold_results, data.table(Threshold = t,
                                                             Accuracy = acc,
                                                             Sensitivity = sens,
                                                             Specificity = spec,
                                                             F1_Score = f1,
                                                             False_Negatives = fn))
  }
}

threshold_results

optimal_thresh <- threshold_results[which.max(F1_Score), Threshold]
optimal_thresh # 0.35 on log res model for 50 50 split dataset

# use optimal threshold on testset
pred_y <- predict(logres3, newdata = testset, type = "response")
pred_improved <- ifelse(pred_y > optimal_thresh, 1, 0)
cm_improved <- table(Actual = testset$Diabetes_binary, Predicted = pred_improved)
cm_improved

TN_imp <- cm_improved[1, 1]
FP_imp <- cm_improved[1, 2]
FN_imp <- cm_improved[2, 1]
TP_imp <- cm_improved[2, 2]

# TN: 6242, FP: 4362, FN: 1151, TP: 9453

FN - FN_imp # 2488 -> 1151 (reduced by 1337)
round(sensitivity, 4) # 0.7654
round(TP_imp / (TP_imp + FN_imp), 4) # 0.7654 -> 0.8915 (increased rate)

accuracy_imp <- (TP_imp + TN_imp) / sum(cm_improved)
sensitivity_imp <- TP_imp / (TP_imp + FN_imp)  # recall/true positive rate - ability to detect diabetes
specificity_imp <- TN_imp / (TN_imp + FP_imp)  # ability to detect non-diabetes
precision_imp <- TP_imp / (TP_imp + FP_imp)
f1_score_imp <- 2 * (precision_imp * sensitivity_imp) / (precision_imp + sensitivity_imp)

round(accuracy_imp, 4) # 0.7401
round(sensitivity_imp, 4) # 0.8915
round(specificity_imp, 4) # 0.5886
round(precision_imp, 4) # 0.6843
round(f1_score_imp, 4) # 0.7742


## CART model ==================================================================

# trainset maximal CART model
m2 <- rpart(Diabetes_binary ~ ., 
            data = trainset, 
            method = "class", 
            control = rpart.control(minsplit = 2, cp = 0))

# compute min CV error + 1 SE in maximal tree m2 
CVerror.cap <- m2$cptable[which.min(m2$cptable[, "xerror"]), "xerror"] + 
  m2$cptable[which.min(m2$cptable[, "xerror"]), "xstd"]

round(CVerror.cap, 4) # cross-validation error cap (min CV error + 1 SE): 0.519

# pruning process - find optimal CP region
i <- 1; j <- 4
while (m2$cptable[i, j] > CVerror.cap) {
  i <- i + 1
}

# geometric mean of cp values
cp.opt <- ifelse(i > 1, sqrt(m2$cptable[i, 1] * m2$cptable[i - 1, 1]), 1)
round(cp.opt, 6) # optimal CP value (geometric mean): 0.000475 

# pruning
m2.1 <- prune(m2, cp = cp.opt)

# visualisation of pruned decision tree
rpart.plot(m2.1, 
           main = "Pruned Decision Tree for Diabetes Prediction",
           extra = 104,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)

# predictions on test set
m2.1_predict <- predict(m2.1, testset, type = "class")
m2.1_predict_prob <- predict(m2.1, testset, type = "prob")[, 2]

# confusion matrix
cart_cm <- table(Actual = testset$Diabetes_binary, Predicted = m2.1_predict)
cart_cm

cart_TN <- cart_cm[1, 1] # 7623   
cart_FP <- cart_cm[1, 2] # 2981
cart_FN <- cart_cm[2, 1] # 2439  
cart_TP <- cart_cm[2, 2] # 8165

cart_accuracy <- mean(m2.1_predict == testset$Diabetes_binary)
cart_error <- 1 - cart_accuracy
cart_sensitivity <- cart_TP / (cart_TP + cart_FN)
cart_specificity <- cart_TN / (cart_TN + cart_FP)
cart_precision <- cart_TP / (cart_TP + cart_FP)
cart_f1 <- 2 * (cart_precision * cart_sensitivity) / (cart_precision + cart_sensitivity)

round(cart_accuracy, 4) # 0.7444
round(cart_error, 4) # 0.2556
round(cart_sensitivity, 4) # 0.77
round(cart_specificity, 4) # 0.7189
round(cart_precision, 4) # 0.7325
round(cart_f1, 4) # 0.7508

# variable importance from CART
cart_importance <- m2.1$variable.importance
print(head(sort(cart_importance, decreasing = TRUE), 10)) # top 10


## ROC curves ==================================================================
# ROC curve = a plot of TPR against FPR at different settings of a decision threshold 
# Area Under the Curve (AUC) summarises the performance. the higher AUC, the better discrimination between positive and negative classes
# AUC > 0.80:  good performance, AUC < 0.70: limited use/worthless 

# for logistic regression:
# calculate AUC 
auc_score <- colAUC(pred_y, testset$Diabetes_binary, plotROC = TRUE)
title(main = "ROC Curve - Logistic Regression Model", 
      sub = paste("AUC =", round(auc_score, 4)))
round(auc_score, 4) # 0.8248
# as AUC > 0.7 indicates good discriminative ability 

# for CART:
cart_auc <- colAUC(m2.1_predict_prob, testset$Diabetes_binary, plotROC = TRUE)
title(main = "ROC Curve - CART Model", 
      sub = paste("AUC =", round(cart_auc, 4)))
round(cart_auc, 4) # 0.7987 


## model comparison: logistic regression vs CART ================================

model_comparison <- data.table(Model = c("Logistic Regression (threshold=0.5)", 
                                         "Logistic Regression (optimal threshold)", 
                                         "CART (pruned)"),
                               Accuracy = c(accuracy, 
                                            (TP_imp + TN_imp) / sum(cm_improved),
                                            cart_accuracy),
                               Sensitivity = c(sensitivity,
                                               TP_imp / (TP_imp + FN_imp),
                                               cart_sensitivity),
                               Specificity = c(specificity,
                                               TN_imp / (TN_imp + FP_imp),
                                               cart_specificity),
                               F1_Score = c(f1_score,
                                            2 * (TP_imp / (TP_imp + FP_imp)) * (TP_imp / (TP_imp + FN_imp)) / 
                                              ((TP_imp / (TP_imp + FP_imp)) + (TP_imp / (TP_imp + FN_imp))),
                                            cart_f1),
                               AUC = c(auc_score, auc_score, cart_auc),
                               False_Negatives = c(FN, FN_imp, cart_FN))
print(model_comparison[, .(Model, 
                           Accuracy = round(Accuracy, 4),
                           Sensitivity = round(Sensitivity, 4),
                           F1_Score = round(F1_Score, 4),
                           AUC = round(AUC, 4),
                           False_Negatives)])

# visualisation of model comparison
comparison_long <- melt(model_comparison[, .(Model, Accuracy, Sensitivity, Specificity, F1_Score)],
                        id.vars = "Model",
                        variable.name = "Metric",
                        value.name = "Score")

p_model_comparison <- ggplot(comparison_long, aes(x = Metric, y = Score, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("skyblue", "palegreen", "salmon")) +
  labs(title = "Model Performance Comparison",
       subtitle = "Comparing Logistic Regression (default & optimized) vs CART",
       x = "Performance Metric",
       y = "Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0)) +
  ylim(0, 1)
p_model_comparison


## facet chart =================================================================

# using sample 
set.seed(123)
sample_indices <- sample(1:nrow(diabetes.dt), 5000)
diabetes_sample <- diabetes.dt[sample_indices]

# as HighBP, Age, BMI, HighChol strongest predictors
# facet chart: Age vs BMI by HighBP and HighChol
p_facet <- ggplot(diabetes_sample, aes(x = BMI, y = Age, color = Diabetes_binary)) +
  geom_point(alpha = 0.5, size = 2) +
  facet_grid(HighBP ~ HighChol, 
             labeller = labeller(HighBP = c("0" = "Normal BP", "1" = "High BP"),
                                 HighChol = c("0" = "Normal Chol", "1" = "High Chol"))) +
  scale_color_manual(values = c("0" = "springgreen3", "1" = "salmon"),
                     labels = c("No Diabetes", "Diabetes"),
                     name = "Status") +
  labs(title = "Diabetes Risk Profiles: Age, BMI, BP & Cholesterol",
       subtitle = "Faceted view shows risk increases with multiple conditions",
       x = "Body Mass Index (BMI)",
       y = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 10))

p_facet



## risk score calculator ========================================================

# using logistic regression coefficients as weights
# extract coefficients from the model
coef_model <- coef(logres2)
round(coef_model, 3)

# odds ratios (exponentiated coefficients) -> shows impact on diabetes risk
odds_ratios <- exp(coef_model)
round(odds_ratios, 3)

calculate_risk_score_v2 <- function(high_bp, high_chol, chol_check, stroke, 
                                    heart_disease, phys_activity, fruits, veggies,
                                    alcohol, healthcare, diffwalk, sex) {
  log_odds <- coef_model["(Intercept)"] +
    coef_model["HighBP1"] * high_bp +
    coef_model["HighChol1"] * high_chol +
    coef_model["CholCheck1"] * chol_check +
    coef_model["Stroke1"] * stroke +
    coef_model["HeartDiseaseorAttack1"] * heart_disease +
    coef_model["PhysActivity1"] * phys_activity +
    coef_model["Fruits1"] * fruits +
    coef_model["Veggies1"] * veggies +
    coef_model["HvyAlcoholConsump1"] * alcohol +
    coef_model["AnyHealthcare1"] * healthcare +
    coef_model["DiffWalk1"] * diffwalk +
    coef_model["Sex1"] * sex
  probability <- 1 / (1 + exp(-log_odds)) # convert log-odds to probability
  risk_score <- probability * 100 # rescaled to 0-100 score
  return(risk_score)
}

# predictions from the model
pred_y <- predict(logres2, newdata = testset, type = "response")
testset[, Risk_Score_Model := pred_y * 100]
summary(testset$Risk_Score_Model)

