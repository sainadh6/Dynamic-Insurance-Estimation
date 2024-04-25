install.packages("scatterplot3d")
install.packages("RColorBrewer")
install.packages("randomForest")
install.packages("readxl")
#remove.packages("dplyr")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("lattice")
install.packages("caret")
install.packages("e1071")
install.packages("Hmisc")
#remove.packages("rlang")
#install.packages("rlang")
install.packages("corrplot")
install.packages("xgboost")
install.packages("nnet")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("MLmetrics")
install.packages("doParallel")
install.packages("scutr")
install.packages("C50")
install.packages("openxlsx")
install.packages("ggcorrplot")


library(openxlsx)
#library("rlang")
library("readxl")
library("dplyr")
library("plyr")
library("ggplot2")
library("lattice")
library("caret")
library("corrplot")
library("randomForest")
library("scatterplot3d")
library("RColorBrewer")
library("e1071")
library("xgboost")
library("nnet")
#library("rpart")
#library("rpart.plot")
#library("MLmetrics")
#library(doParallel)
library(scutr)
library(C50)
library(ggcorrplot)
library("reshape2")

#load file path
path <- "Crop Data copy.xlsx"

#load data
crop_production<- read_excel(path,sheet="crop_production")
rainfall <- read_excel(path,sheet="Rainfall",na="", guess_max = 21474836)
chemicals <- read_excel(path, sheet = "Chemicals")
chemical_damage <- read_excel(path, sheet = "Chemicals and crop damage")

#filtering State TN in Crop Production 
tn_crop_production <- filter(crop_production, State_Name=="Tamil Nadu")

#filtering rainfall for TN and year<=2010
tn_rainfall <- filter(rainfall, state=="TAMIL NADU" & date <= "2010-12-01")


#Rainfall data
tn_rainfall$date <- as.Date(tn_rainfall$date, format = "%d/%m/%Y")
tn_rainfall$date <- format(tn_rainfall$date, "%Y")
dummy<-as.data.frame(tn_rainfall[13:24,])
dummy1 <- dummy[order(as.integer(dummy$rain_in_mm), decreasing = FALSE),]
dummy2 <- dummy1[1:11,]
#Calculating median and filling it in NA in rainfall data
med<- as.character(median(as.integer(dummy2$rain_in_mm)))
tn_rainfall[tn_rainfall == "NA"] <- med
tn_rainfall <- subset(tn_rainfall, select = -c(state))
colnames(tn_rainfall)[1] <- "Crop_Year"
tn_rainfall$rain_in_mm <- as.numeric(tn_rainfall$rain_in_mm)
tn_rainfall <- aggregate(rain_in_mm~Crop_Year, data=tn_rainfall, FUN= sum)
tn_rainfall_dummy <- tn_rainfall
tn_rainfall_dummy <- aggregate(rain_in_mm~Crop_Year, data=tn_rainfall_dummy, FUN= sum)
tn_crop_production_length <- length(tn_crop_production$Crop_Year)

tn_crop_production <- cbind(tn_crop_production, rain_in_mm=0)
tn_rainfall_dummy <- subset(tn_rainfall_dummy, select = -c(rain_in_mm))
for (i in 1:tn_crop_production_length) {
  #print(tn_crop_production$Crop_Year[i])
  yr <- tn_crop_production$Crop_Year[i]
  for(j in 1:14){
    if(yr==tn_rainfall$Crop_Year[j]){
      tn_crop_production$rain_in_mm[i] <- tn_rainfall$rain_in_mm[j]
    }
  }
}

#TN Crop Production df processing

tn_crop_production <- tn_crop_production[!(is.na(tn_crop_production$Production) | tn_crop_production$Production==""), ]
tn_crop_production <- tn_crop_production[sample(nrow(tn_crop_production), nrow(tn_crop_production)*0.1), ]
tn_crop_production_dummy <- tn_crop_production
tn_crop_production_dummy$District_Name[tn_crop_production_dummy$District_Name == "THE NILGIRIS"] <- "NILGIRIS"
#write.xlsx(tn_crop_production_dummy, "tn_crop_production.xlsx")
#tn_crop_production_dummy22 <- tn_crop_production_dummy %>% sample_n(size = nrow(df) * 0.1)
#Ascii encoding 
for(i in seq_along(tn_crop_production_dummy)){
  if(is.character((tn_crop_production_dummy[[i]]))){
    for(j in seq_along(tn_crop_production_dummy[[i]])){
      tn_crop_production_dummy[[i]][j] <- sum(as.numeric(charToRaw(tn_crop_production_dummy[[i]][j])))
    }
  }
}
tn_crop_production_encoded <- tn_crop_production_dummy

char_cols <- which(sapply(tn_crop_production_encoded, is.character))
for (i in char_cols) {
  tn_crop_production_encoded[,i] <- as.numeric(tn_crop_production_encoded[,i])
}



prod_df <- tn_crop_production_encoded[,c("State_Name", "District_Name","Crop_Year","Season","Crop","Area","rain_in_mm","Production")]
#prod_df <- prod_df %>% sample_n(size = nrow(df) * 0.1)

#Outlier Processing
mean_production = mean(prod_df$Production)

std_production = sd(prod_df$Production)

# get threshold values for outliers
Tmin_production = mean_production-(3*std_production)
Tmax_production = mean_production+(3*std_production)

# find outlier
outliers_production<-as.data.frame(prod_df$Production[which(prod_df$Production < Tmin_production | prod_df$Production > Tmax_production)])
colnames(outliers_production) <- c("Production_Outliers")

#remove outliers
no_outliers <- subset(prod_df, prod_df$Production > Tmin_production & prod_df$Production< Tmax_production)

prod_df <- no_outliers

#Data Split
prod_df <- no_outliers
prod_df <- subset(prod_df, select = -c(State_Name))
prod_df_std <- as.data.frame(scale(prod_df))

corrplot(cor(prod_df_std))
# 
# # Create a correlation plot with customized appearance
# corrplot(cor(prod_df_std), is.corr = TRUE, tl.col = "black", col = colorRampPalette(c("white", "blue"))(100))
# 
# ggcorrplot(cor(prod_df_std), type = "lower", outline.color = "white",
#            colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE, lab_size = 3,
#            title = "Correlation Matrix for Crop Production Dataset")
# 
# ggplot(data = melt(cor(prod_df_std)), aes(x=Var1, y=Var2, fill=value)) +
#   geom_tile() +
#   scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson\nCorrelation") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
#   coord_fixed()
# 
# # Add correlation values to plot
# gg_corrplot_with_values <- ggcorrplot(cor(prod_df_std), type = "lower", ggplot = gg_corrplot, tl.cex = 1.5, tl.col = "black")


train <- sample(nrow(prod_df), nrow(prod_df)*0.8)
train_data <- prod_df[train, ]
test_data <- prod_df[-train, ]

#RF Model for Crop Production
rf_model <- randomForest(Production~., data = train_data, ntree = 500, importance = TRUE)
#importance(rf_model)
rf_pred <- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$Production - rf_pred)^2))
rf_r2 <- cor(test_data$Production, rf_pred)^2
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
rf_nrmse <- rf_rmse / (max(test_data$Production) - min(test_data$Production))

#Linear Reg for Crop Production
model_lm <- lm(Production~., data = train_data)
lm_pred <- predict(model_lm, test_data)
lm_rmse <- RMSE(lm_pred, test_data$Production)
lm_rsq <- cor(test_data$Production, lm_pred)^2
lm_nrmse <- lm_rmse / (max(test_data$Production) - min(test_data$Production))

# XGBoost Regression model
xgb_model <- xgboost(data = as.matrix(train_data[, c(1:6)]), label = train_data$Production, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(test_data[, c(1:6)]))
xgb_rmse <- RMSE(xgb_pred, test_data$Production)
xgb_rsq <- cor(xgb_pred, test_data$Production)^2
xgb_nrmse <- xgb_rmse / (max(test_data$Production) - min(test_data$Production))

# Print performance metrics
cat("RF Regression RMSE:", rf_rmse)
cat("RF Regression R-Squared:", rf_r2)
cat("RF NRMSE: ", rf_nrmse)

cat("Linear Regression RMSE: ", lm_rmse)
cat("Linear Regression R-squared: ", lm_rsq)
cat("LM NRMSE: ", lm_nrmse)


cat("XGBoost Regression RMSE: ", xgb_rmse)
cat("XGBoost Regression R-squared: ", xgb_rsq)
cat("XGB NRMSE: ", xgb_nrmse)



#Models without standardization and outlier removal
prod_df<-tn_crop_production_encoded
prod_df <- subset(prod_df, select = -c(State_Name))

train <- sample(nrow(prod_df), nrow(prod_df)*0.8)
train_data <- prod_df[train, ]
test_data <- prod_df[-train, ]

#RF Model for Crop Production
rf_model <- randomForest(Production~., data = train_data, ntree = 500, importance = TRUE)
#importance(rf_model)
rf_pred <- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$Production - rf_pred)^2))
rf_r2 <- cor(test_data$Production, rf_pred)^2
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}

#Linear Reg for Crop Production
model_lm <- lm(Production~., data = train_data)
lm_pred <- predict(model_lm, test_data)
lm_rmse <- RMSE(lm_pred, test_data$Production)
lm_rsq <- cor(test_data$Production, lm_pred)^2

# XGBoost Regression model
xgb_model <- xgboost(data = as.matrix(train_data[, c(1:6)]), label = train_data$Production, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(test_data[, c(1:6)]))
xgb_rmse <- RMSE(xgb_pred, test_data$Production)
xgb_rsq <- cor(xgb_pred, test_data$Production)^2

# Print performance metrics
cat("RF Regression RMSE:", rf_rmse)
cat("RF Regression R-Squared:", rf_r2)

cat("Linear Regression RMSE: ", lm_rmse)
cat("Linear Regression R-squared: ", lm_rsq)


cat("XGBoost Regression RMSE: ", xgb_rmse)
cat("XGBoost Regression R-squared: ", xgb_rsq)




#standardization
prod_df<-tn_crop_production_encoded
prod_df <- subset(prod_df, select = -c(State_Name))

prod_df_std <- as.data.frame(scale(prod_df))

train <- sample(nrow(prod_df_std), nrow(prod_df_std)*0.8)
train_data <- prod_df[train, ]
test_data <- prod_df[-train, ]

#RF Model for Crop Production
rf_model <- randomForest(Production~., data = train_data, ntree = 500, importance = TRUE)
#importance(rf_model)
rf_pred <- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$Production - rf_pred)^2))
rf_r2 <- cor(test_data$Production, rf_pred)^2
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}

#Linear Reg for Crop Production
model_lm <- lm(Production~., data = train_data)
lm_pred <- predict(model_lm, test_data)
lm_rmse <- RMSE(lm_pred, test_data$Production)
lm_rsq <- cor(test_data$Production, lm_pred)^2

# XGBoost Regression model
xgb_model <- xgboost(data = as.matrix(train_data[, c(1:6)]), label = train_data$Production, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(test_data[, c(1:6)]))
xgb_rmse <- RMSE(xgb_pred, test_data$Production)
xgb_rsq <- cor(xgb_pred, test_data$Production)^2

# Print performance metrics
cat("RF Regression RMSE:", rf_rmse)
cat("RF Regression R-Squared:", rf_r2)

cat("Linear Regression RMSE: ", lm_rmse)
cat("Linear Regression R-squared: ", lm_rsq)


cat("XGBoost Regression RMSE: ", xgb_rmse)
cat("XGBoost Regression R-squared: ", xgb_rsq)


#Stadardization and outlier removal
prod_df<-tn_crop_production_encoded
#Outlier Processing
mean_production = mean(prod_df$Production)

std_production = sd(prod_df$Production)

# get threshold values for outliers
Tmin_production = mean_production-(3*std_production)
Tmax_production = mean_production+(3*std_production)

# find outlier
outliers_production<-as.data.frame(prod_df$Production[which(prod_df$Production < Tmin_production | prod_df$Production > Tmax_production)])
colnames(outliers_production) <- c("Production_Outliers")

#remove outliers
no_outliers <- subset(prod_df, prod_df$Production > Tmin_production & prod_df$Production< Tmax_production)

prod_df <- no_outliers

#Data Split
prod_df <- subset(prod_df, select = -c(State_Name))
prod_df_std <- as.data.frame(scale(prod_df))

train <- sample(nrow(prod_df), nrow(prod_df)*0.8)
train_data <- prod_df[train, ]
test_data <- prod_df[-train, ]

#RF Model for Crop Production
rf_model <- randomForest(Production~., data = train_data, ntree = 500, importance = TRUE)
#importance(rf_model)
rf_pred <- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$Production - rf_pred)^2))
rf_r2 <- cor(test_data$Production, rf_pred)^2
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
varImpPlot(rf_model, scale = TRUE, type = 1, main = "Variable Importance Plot for Random Forest Model")

rf_nrmse <- rf_rmse / (max(test_data$Production) - min(test_data$Production))
rf_feature_importance <- varImp(rf_model)

# View the feature importance scores
print(rf_feature_importance)

#Linear Reg for Crop Production
model_lm <- lm(Production~., data = train_data)
lm_pred <- predict(model_lm, test_data)
lm_rmse <- RMSE(lm_pred, test_data$Production)
lm_rsq <- cor(test_data$Production, lm_pred)^2
lm_nrmse <- lm_rmse / (max(test_data$Production) - min(test_data$Production))

# XGBoost Regression model
xgb_model <- xgboost(data = as.matrix(train_data[, c(1:6)]), label = train_data$Production, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(test_data[, c(1:6)]))
xgb_rmse <- RMSE(xgb_pred, test_data$Production)
xgb_rsq <- cor(xgb_pred, test_data$Production)^2
xgb_nrmse <- xgb_rmse / (max(test_data$Production) - min(test_data$Production))

# Print performance metrics
cat("RF Regression RMSE:", rf_rmse)
cat("RF Regression R-Squared:", rf_r2)
cat("RF NRMSE:", rf_nrmse)

cat("Linear Regression RMSE: ", lm_rmse)
cat("Linear Regression R-squared: ", lm_rsq)
cat("LM NRMSE:", lm_nrmse)


cat("XGBoost Regression RMSE: ", xgb_rmse)
cat("XGBoost Regression R-squared: ", xgb_rsq)
cat("XGB NRMSE:", xgb_nrmse)




###########################
#####Start of chemical#####
###########################
chemical_damage_dummy <- chemical_damage



chemical_damage_corr <- chemical_damage_dummy
chemical_damage_corr <- chemical_damage_corr[sample(nrow(chemical_damage_corr), nrow(chemical_damage_corr)*0.1), ]

#chemical_damage_corr <- chemical_damage_dummy
chemical_damage_corr$Season[chemical_damage_corr$Season == "Summer"] <- "summer"
#chemical_damage_corr<-chemical_damage_corr %>% sample_n(size = nrow(df) * 0.1)

#Ascii Encoding
 for(i in seq_along(chemical_damage_corr)){
   if(is.character((chemical_damage_corr[[i]]))){
     for(j in seq_along(chemical_damage_corr[[i]])){
       chemical_damage_corr[[i]][j] <- sum(as.numeric(charToRaw(chemical_damage_corr[[i]][j])))
     }
   }
 }

#chemical_damage_corr <- subset(chemical_damage_corr, select = -c(Number_Doses_Week, Number_Weeks_Used))
chemical_damage_encoded <- chemical_damage_corr

chem_char_cols <- which(sapply(chemical_damage_encoded, is.character))
#chem_char_cols
  
  for (i in chem_char_cols) {
    chemical_damage_encoded[,i] <- as.numeric(unlist(chemical_damage_encoded[,i]))
  }

chem_dmg_reg<-chemical_damage_encoded
corrplot(cor(scale(chem_dmg_reg)))
############################
#############################
chem_dmg_train <- sample(nrow(chem_dmg_reg), nrow(chem_dmg_reg)*0.8)
chem_dmg_train_data <- chem_dmg_reg[chem_dmg_train, ]
chem_dmg_test_data <- chem_dmg_reg[-chem_dmg_train, ]

#Regression Algorithms

rf_model <- randomForest(Number_Weeks_Used~., data = chem_dmg_train_data, ntree = 500, importance = TRUE)
importance(rf_model)
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
# Make predictions on the testing 
setrf_pred <- predict(rf_model, newdata = chem_dmg_test_data)
# Evaluate the performance of the model
rf_rmse <- sqrt(mean((chem_dmg_test_data$Number_Weeks_Used - setrf_pred)^2))
rf_r2 <- cor(chem_dmg_test_data$Number_Weeks_Used, setrf_pred)^2
nrmse <- rf_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))

# Print the performance metrics
print(paste0("Random Forest RMSE: ", rf_rmse))
print(paste0("Random Forest R-squared: ", rf_r2))
print(paste0("Normalised RMSE - RF: ", nrmse))
summary(rf_model)

model_lm <- lm(Number_Weeks_Used~., data = chem_dmg_train_data)
lm_pred <- predict(model_lm, chem_dmg_test_data)
lm_rmse <- RMSE(lm_pred, chem_dmg_test_data$Number_Weeks_Used)
lm_rsq <- cor(chem_dmg_test_data$Number_Weeks_Used, lm_pred)^2
nrmse <- lm_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))
print(lm_rmse)
print(lm_rsq)
print(paste0("Normalised RMSE - LM: ", nrmse))

xgb_model <- xgboost(data = as.matrix(chem_dmg_train_data[, c(1:8)]), label = chem_dmg_train_data$Number_Weeks_Used, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(chem_dmg_test_data[, c(1:8)]))
xgb_rmse <- RMSE(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)
xgb_rsq <- cor(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)^2
nrmse <- xgb_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))

print(paste0("XG Boost RMSE: ", xgb_rmse))
print(paste0("XG Boost R-squared: ", xgb_rsq))
print(paste0("Normalised RMSE - XGBoost: ", nrmse))

########### Undersampling
chem_dmg_train <- sample(nrow(chem_dmg_reg), nrow(chem_dmg_reg)*0.8)
chem_dmg_train_data <- chem_dmg_reg[chem_dmg_train, ]
minority_class <- chem_dmg_train_data[chem_dmg_train_data$Crop_Damage == 1742,]
minority_class1 <- chem_dmg_train_data[chem_dmg_train_data$Crop_Damage == 1324,]
majority_class <- chem_dmg_train_data[chem_dmg_train_data$Crop_Damage == 1318,]
n <- nrow(minority_class)
n1 <- nrow(minority_class1)
n <- n + n1
majority_class_under <- majority_class[sample(nrow(majority_class), n), ]
# Combine minority class with the randomly sampled majority class
data_balanced <- rbind(minority_class, minority_class1, majority_class_under)

chem_dmg_test_data <- chem_dmg_reg[-chem_dmg_train, ]


#Regression Algorithms

rf_model <- randomForest(Number_Doses_Week~., data = data_balanced, ntree = 500, importance = TRUE)
importance(rf_model)
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
# Make predictions on the testing 
setrf_pred <- predict(rf_model, newdata = chem_dmg_test_data)
# Evaluate the performance of the model
rf_rmse <- sqrt(mean((chem_dmg_test_data$Number_Doses_Week - setrf_pred)^2))
rf_r2 <- cor(chem_dmg_test_data$Number_Doses_Week, setrf_pred)^2
nrmse <- rf_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))
# Print the performance metrics
print(paste0("Random Forest RMSE: ", rf_rmse))
print(paste0("Random Forest R-squared: ", rf_r2))
summary(rf_model)

xgb_model <- xgboost(data = as.matrix(data_balanced[, c(1:8)]), label = data_balanced$Number_Weeks_Used, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(chem_dmg_test_data[, c(1:8)]))
xgb_rmse <- RMSE(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)
xgb_rsq <- cor(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)^2
print(paste0("XG Boost RMSE: ", xgb_rmse))
print(paste0("XG Boost R-squared: ", xgb_rsq))

###################################################Ends
######################################################
#outlier handling
chem_char_cols <- which(sapply(chemical_damage_encoded, is.character))
#chem_char_cols

for (i in chem_char_cols) {
  chemical_damage_encoded[,i] <- as.numeric(unlist(chemical_damage_encoded[,i]))
}

chemical_damage_dummy <- chemical_damage_encoded
chem_dmg_reg<- chemical_damage_dummy
mean = mean(chem_dmg_reg$Number_Weeks_Used)
std = sd(chem_dmg_reg$Number_Weeks_Used)
# get threshold values for outliers
Tmin = mean-(3*std)
Tmax = mean+(3*std)
# find outlier
outliers<-as.data.frame(chem_dmg_reg$Number_Weeks_Used[which(chem_dmg_reg$Number_Weeks_Used < Tmin | chem_dmg_reg$Number_Weeks_Used > Tmax)])
colnames(outliers) <- c("Outliers")
#View(outliers)

#remove outliers
no_outliers_chem <- subset(chem_dmg_reg, chem_dmg_reg$Number_Weeks_Used > Tmin & chem_dmg_reg$Number_Weeks_Used < Tmax)
chem_dmg_reg<-no_outliers_chem
chem_dmg_train <- sample(nrow(chem_dmg_reg), nrow(chem_dmg_reg)*0.8)
chem_dmg_train_data <- chem_dmg_reg[chem_dmg_train, ]
#chem_dmg_train_data<-as.data.frame(chem_dmg_train_data)
chem_dmg_test_data <- chem_dmg_reg[-chem_dmg_train, ]
#chem_dmg_test_data<-as.data.frame(chem_dmg_test_data)
rf_model <- randomForest(Number_Weeks_Used~., data = chem_dmg_train_data, ntree = 500, importance = TRUE)
importance(rf_model)
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
# Make predictions on the testing 
setrf_pred <- predict(rf_model, newdata = chem_dmg_test_data)
# Evaluate the performance of the model
rf_rmse <- sqrt(mean((chem_dmg_test_data$Number_Doses_Week - setrf_pred)^2))
rf_r2 <- cor(chem_dmg_test_data$Number_Doses_Week, setrf_pred)^2
nrmse <- rf_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))
# Print the performance metrics
print(paste0("Random Forest RMSE: ", rf_rmse))
print(paste0("Random Forest R-squared: ", rf_r2))
summary(rf_model)


xgb_model <- xgboost(data = as.matrix(chem_dmg_train_data[, c(1:8)]), label = chem_dmg_train_data$Number_Weeks_Used, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(chem_dmg_test_data[, c(1:8)]))
xgb_rmse <- RMSE(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)
xgb_rsq <- cor(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)^2
print(paste0("XG Boost RMSE: ", xgb_rmse))
print(paste0("XG Boost R-squared: ", xgb_rsq))


######################## Oversample

smot_s<-oversample_smote(chem_dmg_train_data, 1742, "Crop_Damage", 5000)
smot_bind <- rbind(smot_s,chem_dmg_train_data)
chem_dmg_test_data <- chem_dmg_reg[-chem_dmg_train, ]
#View(smot_bind)
chem_dmg_train_data$Crop_Damage <- as.numeric(chem_dmg_train_data$Crop_Damage)
chem_dmg_test_data$Crop_Damage <- as.numeric(chem_dmg_test_data$Crop_Damage)
#View(chem_dmg_test_data)


rf_model <- randomForest(Number_Doses_Week~., data = smot_bind, ntree = 500, importance = TRUE)
importance(rf_model)# Make predictions on the testing 
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
setrf_pred <- predict(rf_model, newdata = chem_dmg_test_data)# Evaluate the performance of the model
rf_rmse <- sqrt(mean((chem_dmg_test_data$Number_Doses_Week - setrf_pred)^2))
rf_r2 <- cor(chem_dmg_test_data$Number_Doses_Week, setrf_pred)^2
nrmse <- rf_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))
# Print the performance metrics
print(paste0("Random Forest RMSE: ", rf_rmse))
print(paste0("Random Forest R-squared: ", rf_r2))
summary(rf_model)

xgb_model <- xgboost(data = as.matrix(smot_bind[, c(1:8)]), label = smot_bind$Number_Weeks_Used, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(chem_dmg_test_data[, c(1:8)]))
xgb_rmse <- RMSE(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)
xgb_rsq <- cor(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)^2

print(paste0("XG Boost RMSE: ", xgb_rmse))
print(paste0("XG Boost R-squared: ", xgb_rsq))

###########Scaling
chem_dmg_reg<-chemical_damage_encoded
chem_dmg_scaled <- as.data.frame(scale(chem_dmg_reg))
chem_dmg_train <- sample(nrow(chem_dmg_scaled), nrow(chem_dmg_scaled)*0.8)
chem_dmg_train_data <- chem_dmg_scaled[chem_dmg_train, ]
chem_dmg_test_data <- chem_dmg_scaled[-chem_dmg_train, ]

#Regression Algorithms

rf_model <- randomForest(Number_Weeks_Used~., data = chem_dmg_train_data, ntree = 500, importance = TRUE)
importance(rf_model)
{varImpPlot(rf_model, main = "Variable Importance Plot for Random Forest Model"); options(scipen = 999)}
varImpPlot(rf_model,scale = TRUE, type = 1, main = "Variable Importance Plot for Random Forest Model")
# Make predictions on the testing 
setrf_pred <- predict(rf_model, newdata = chem_dmg_test_data)
# Evaluate the performance of the model
rf_rmse <- sqrt(mean((chem_dmg_test_data$Number_Weeks_Used - setrf_pred)^2))
rf_r2 <- cor(chem_dmg_test_data$Number_Weeks_Used, setrf_pred)^2
rf_nrmse <- rf_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))
rf_feature_importance <- varImp(rf_model)
print(rf_feature_importance)
# Print the performance metrics
print(paste0("Random Forest RMSE: ", rf_rmse))
print(paste0("Random Forest R-squared: ", rf_r2))
print(paste0("Random Forest NRMSE: ", rf_nrmse))
summary(rf_model)

xgb_model <- xgboost(data = as.matrix(chem_dmg_train_data[, c(1:8)]), label = chem_dmg_train_data$Number_Weeks_Used, nrounds = 100, objective = "reg:linear")
xgb_pred <- predict(xgb_model, newdata = as.matrix(chem_dmg_test_data[, c(1:8)]))
xgb_rmse <- RMSE(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)
xgb_rsq <- cor(xgb_pred, chem_dmg_test_data$Number_Weeks_Used)^2
xgb_nrmse <- xgb_rmse / (max(chem_dmg_test_data$Number_Weeks_Used) - min(chem_dmg_test_data$Number_Weeks_Used))
print(paste0("XG Boost RMSE: ", xgb_rmse))
print(paste0("XG Boost R-squared: ", xgb_rsq))
print(paste0("XG Boost NRMSE: ", xgb_nrmse))
# Dump the model to text
model_text <- capture.output(xgb.dump(xgb_model))

# Extract the regression coefficients from the text
coeffs <- as.numeric(strsplit(model_text[grep("f[0-9]*", model_text)], split = "=")[[1]][2:length(model_text)])

# Print regression coefficients
print(coeffs)



########Classification


chemical_damage_dummy1 <- chemical_damage_dummy
#chemical_damage_encoded$Crop_Damage <- chemical_damage$Crop_Damage
chemical_damage_dummy1$Crop_Damage <- factor(chemical_damage_dummy1$Crop_Damage)

chemical_damage_dummy1$Season[chemical_damage_dummy1$Season == "Summer"] <- "summer"
chemical_damage_corr<-chemical_damage_dummy1

#Ascii Encoding
for(i in seq_along(chemical_damage_corr)){
  if(is.character((chemical_damage_corr[[i]]))){
    for(j in seq_along(chemical_damage_corr[[i]])){
      chemical_damage_corr[[i]][j] <- sum(as.numeric(charToRaw(chemical_damage_corr[[i]][j])))
    }
  }
}

#chemical_damage_corr <- subset(chemical_damage_corr, select = -c(Number_Doses_Week, Number_Weeks_Used))
chemical_damage_encoded <- chemical_damage_corr

chem_char_cols <- which(sapply(chemical_damage_encoded, is.character))
#chem_char_cols

for (i in chem_char_cols) {
  chemical_damage_encoded[,i] <- as.numeric(unlist(chemical_damage_encoded[,i]))
}

chem_dmg_clas<-chemical_damage_encoded




set.seed(101)


#attempt 3 by balancing using raw data
chem_dmg_train <- sample(nrow(chem_dmg_clas), nrow(chem_dmg_clas)*0.8)
chem_dmg_train_data <- chem_dmg_clas[chem_dmg_train, ]

minority_class <- chem_dmg_train_data[chem_dmg_train_data$Crop_Damage == 1742,]
minority_class
minority_class1 <- chem_dmg_train_data[chem_dmg_train_data$Crop_Damage == 1324,]
majority_class <- chem_dmg_train_data[chem_dmg_train_data$Crop_Damage == 1318,]

n <- nrow(minority_class)
n1 <- nrow(minority_class1)
n <- n + n1
majority_class_under <- majority_class[sample(nrow(majority_class), n), ]

# Combine minority class with the randomly sampled majority class
data_balanced <- rbind(minority_class, minority_class1, majority_class_under)


chem_dmg_test_data <- chem_dmg_clas[-chem_dmg_train, ]


chem_dmg_knn_model <- train(Crop_Damage~. , data = data_balanced, method = "knn", trControl = trainControl(method = "cv"))
chem_dmg_predictions <- predict(chem_dmg_knn_model, newdata = chem_dmg_test_data)


# Evaluate the performance of the model
confusion_matrix <- table(chem_dmg_predictions, chem_dmg_test_data$Crop_Damage)
confusion_matrix
conf_tab <- as.table(confusion_matrix)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy
conf_res <- confusionMatrix(conf_tab, mode = "prec_recall")

sensitivity <- conf_res$byClass[1]
specificity <- conf_res$byClass[2]

# Print the results
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

##SMOTE
chem_dmg_train1<-chem_dmg_train_data
chem_dmg_train1$Crop_Damage<-as.numeric(chem_dmg_train1$Crop_Damage)
smot_s<-oversample_smote(chem_dmg_train1, 3, "Crop_Damage", 5000)
smot_bind <- rbind(smot_s,chem_dmg_train_data)
chem_dmg_test_data <- chem_dmg_reg[-chem_dmg_train, ]
#View(smot_bind)
chem_dmg_test_data$Crop_Damage <- as.numeric(chem_dmg_test_data$Crop_Damage)
#View(chem_dmg_test_data)

chem_dmg_knn_model <- train(Crop_Damage~. , data = smot_bind, method = "knn", trControl = trainControl(method = "cv"))
chem_dmg_predictions <- predict(chem_dmg_knn_model, newdata = chem_dmg_test_data)


# Evaluate the performance of the model
confusion_matrix <- table(chem_dmg_predictions, chem_dmg_test_data$Crop_Damage)
confusion_matrix
conf_tab <- as.table(confusion_matrix)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy
#conf_res <- confusionMatrix(conf_tab, mode = "prec_recall")

sensitivity <- conf_res$byClass[1]
specificity <- conf_res$byClass[2]

# Print the results
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")



