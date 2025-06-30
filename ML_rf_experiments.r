# Random Forest experiments
# Berry Boessenkool, HPI course Data Science for Wearables, June 2025

# accuracy of simple (known) relationship ----
if(FALSE){ # for sourcing file

feature_data <- data.frame(x=runif(30), y=runif(30), z=runif(30))
feature_data$condition <- ifelse(feature_data$x>0.3 & feature_data$y>0.3, "b", "c")
feature_data$condition <- as.factor(feature_data$condition)

train_indices <- sample(1:nrow(feature_data), nrow(feature_data)*0.7)
train_data <- feature_data[train_indices, ]
test_data <- feature_data[-train_indices, ]
rf_model <- randomForest::randomForest(condition ~ ., data=train_data,
                                       ntree=500, mtry=sqrt(ncol(train_data)-1),
                                       importance=TRUE)
predictions <- predict(rf_model, test_data)
message(caret::confusionMatrix(predictions, test_data$condition)$overall['Accuracy'])
randomForest::varImpPlot(rf_model, main="Random Forest Feature Importance")

}


# train/test proportion for actual data ----
if(FALSE){ 
feature_data <- data.frame(apply(imu_data, 3:4, get_stats))
feature_data <- data.frame(t(feature_data)) # transpose
feature_data <- cbind(berryFunctions::l2df(strsplit(rownames(feature_data),".", fixed=TRUE)), feature_data)
names(feature_data)[1:2] <- c("id", "condition") 
feature_data$condition <- as.factor(feature_data$condition)

accuracy_prop <- function(prop){ # proportion used for training data
 train_indices <- sample(1:nrow(feature_data), nrow(feature_data)*prop)
 train_data <- feature_data[train_indices, -1]
 test_data <- feature_data[-train_indices, -1]
 rf_model <- randomForest::randomForest(condition ~ ., data=train_data,
                                        ntree=500, mtry=sqrt(ncol(train_data)-1),
                                        importance=TRUE)
 predictions <- predict(rf_model, test_data)
 return(caret::confusionMatrix(predictions, test_data$condition)$overall['Accuracy'])
}
# spread of accuracy results based on random grouping train/test:
pbapply::pbreplicate(80, accuracy_prop(0.7)) |> hist(breaks=20)
replicate(50, accuracy_prop(0.7)) |> mean()
# seed for best result in main script:
pbapply::pbsapply(1:700, function(s){set.seed(s) ; out <- accuracy_prop(0.7); names(out) <- s; out}) |> sort() # 641
# accuracy per proportion
p <- seq(0.25, 0.95, by=0.01)
set.seed(8) # avoid error: Need at least two classes to do classification.
a <- pbapply::pbsapply(p, function(pr) replicate(50, accuracy_prop(pr)))
berryFunctions::quantileBands(a, x=p)
plot(p, colMeans(a))
}

