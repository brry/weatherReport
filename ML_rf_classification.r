# Random Forest Approach for IMU acceleration data classification
# Berry Boessenkool, HPI course Data Science for Wearables, June 2025

# packages, raw data ----
if(!requireNamespace("pacman", quietly=TRUE)) install.packages("pacman")
pacman::p_load("berryFunctions", "randomForest", "caret", "pbapply")

source("ML.R") # load `imu_data` (array as recommended in the paragon presentation)
str(imu_data)
head(imu_data[,,1,"b"])


# extract features from time series data ----

get_stats <- function(matrix)
{
 unlist(lapply(c("x","y","z","m"), function(acc) 
 {
  v <- na.omit(matrix[,acc])
  out <- quantile(v, prob=seq(0,1,0.1))
  names(out) <- paste0("q",sub("%","",names(out)))
  out <- c(out, mean=mean(v), sd=sd(v), range=diff(range(v)))
  names(out) <- paste0(acc,"_",names(out))
  out
 }))
}
get_stats(imu_data[,,1,"b"]) # test run looks good

feature_data <- data.frame(apply(imu_data, 3:4, get_stats))
feature_data <- data.frame(t(feature_data)) # transpose
rnames <- berryFunctions::l2df(strsplit(rownames(feature_data),".", fixed=TRUE))
feature_data <- cbind(rnames, feature_data) ; rm(rnames)
names(feature_data)[1:2] <- c("id", "condition") 
View(feature_data) # n observations should be >> n columns in real life
feature_data$condition <- as.factor(feature_data$condition)

# random forest ----

# Split data into training and testing sets:
set.seed(641) # reproducibility
train_indices <- sample(1:nrow(feature_data), nrow(feature_data)*0.7)
train_data <- feature_data[train_indices, -1]
test_data <- feature_data[-train_indices, -1]

# Train Random Forest model:
rf_model <- randomForest::randomForest(condition ~ ., data=train_data,
                                       ntree=500, mtry=sqrt(ncol(train_data)-1),
                                       importance=TRUE)
# Make predictions:
predictions <- predict(rf_model, test_data)
# Evaluate model performance:
caret::confusionMatrix(predictions, test_data$condition)#$overall['Accuracy']
# Feature importance:
berryFunctions::sortDF(randomForest::importance(rf_model), 3, quiet=TRUE)
randomForest::varImpPlot(rf_model, main="Random Forest Feature Importance", n.var=20)

plot(x_q70~x_q60, data=feature_data)
plot(m_q70~m_q60, data=feature_data)
