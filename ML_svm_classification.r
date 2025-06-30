# Support Vector Machine Approach for IMU acceleration data classification
# Berry Boessenkool, HPI course Data Science for Wearables, June 2025

# before this script, first understand "ML_rf_classification.r"

pacman::p_load("berryFunctions", "e1071", "caret")
source("ML.R") # loads array `imu_data`

# extract advanced features including frequency domain ----
get_stats <- function(matrix) {
 v <- na.omit(matrix[,"m"]) # acceleration magnitude only, not x,y,z
 # Time domain features
 out <- quantile(v, prob=seq(0,1,0.1))
 names(out) <- paste0("q",sub("%","",names(out)))
 out <- c(out, mean=mean(v), sd=sd(v), range=diff(range(v)))
 # Frequency domain features:
 ff <- abs(fft(v))^2
 fn <- length(ff)
 # plot(ff[2:100], type="l")
 out <- c(out,
          dom_freq         = which.max(ff[-1])+1,
          spec_centroid    = sum((1:fn) * ff) / sum(ff),
          low_freq_energy  = sum(ff[ 2:10]), # 1-10 Hz slow movements, postural adjustments
          mid_freq_energy  = sum(ff[11:50]), # Fast movements, quick corrections, fine motor control
          high_freq_energy = sum(ff[51:100]),# Often noise, or very rapid oscillations
          energy           = sum(v^2)  )     # a signal processing feature
 out
}
get_stats(imu_data[,,1,"b"]) # test run looks good

feature_data <- data.frame(apply(imu_data, 3:4, get_stats))
feature_data <- data.frame(t(feature_data)) # transpose
rnames <- berryFunctions::l2df(strsplit(rownames(feature_data),".", fixed=TRUE))
feature_data <- cbind(rnames, feature_data) ; rm(rnames)
names(feature_data)[1:2] <- c("id", "condition") 
feature_data$condition <- as.factor(feature_data$condition)
View(feature_data)
# plot(feature_data$low_freq_energy, feature_data$mid_freq_energy)
berryFunctions::bpairs(feature_data[,14:22])

# Support Vector Machine ----

# Split data into training and testing sets:
set.seed(123)
train_indices <- sample(1:nrow(feature_data), nrow(feature_data)*0.7)
train_data <- feature_data[train_indices, -1]
test_data <- feature_data[-train_indices, -1]
# Scale features for SVM:
preProcValues <- caret::preProcess(train_data[, -1], method=c("center", "scale"))
train_scaled <- predict(preProcValues, train_data)
test_scaled <- predict(preProcValues, test_data)

# Hyperparameter tuning for SVM:
# Stage 1: Coarse grid search
tune_coarse <- e1071::tune(svm, condition ~ ., 
                           data=train_scaled,
                           kernel="radial",
                           ranges=list(cost=10^(-2:4),
                                       gamma=10^(-5:2)))
# Stage 2: Fine-tune around the best values
tune_fine <- e1071::tune(svm, condition ~ ., 
                         data=train_scaled,
                         kernel="radial",
                         ranges=list(
                          cost= tune_coarse$best.parameters$cost *c(.4,.6,.8,1,1.2,1.5,2,3,5),
                          gamma=tune_coarse$best.parameters$gamma*c(.4,.6,.8,1,1.2,1.5,2,3,5)
                         ))
tune_fine
plot(tune_fine, main="SVM Hyperparameter Tuning Results")

# Train SVM with best parameters:
svm_model <- e1071::svm(condition ~ ., 
                        data=train_scaled,
                        kernel="radial",
                        cost=tune_fine$best.parameters$cost,
                        gamma=tune_fine$best.parameters$gamma,
                        probability=TRUE)
# Make predictions:
predictions <- predict(svm_model, test_scaled)
# Evaluate model performance:
caret::confusionMatrix(predictions, test_scaled$condition)#$overall['Accuracy']


# Using caret with built-in tuning:
caret_model <- caret::train(condition ~ ., 
                           data=train_scaled,
                           method="svmRadial",
                           tuneGrid=expand.grid(C=10^(-2:4), sigma=10^(-5:2)),
                           trControl=caret::trainControl(method="cv", number=5))
plot(caret_model)
predictions <- predict(caret_model, test_scaled)
caret::confusionMatrix(predictions, test_scaled$condition)#$overall['Accuracy']

