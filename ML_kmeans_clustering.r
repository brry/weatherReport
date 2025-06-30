# K-Means Clustering Approach for IMU acceleration data classification
# Berry Boessenkool, HPI course Data Science for Wearables, June 2025

# before this script, first understand "ML_rf_classification.r" and "ML_svm..."

pacman::p_load("cluster", "caret", "factoextra")
source("ML.R") # loads array `imu_data`

# Features ----

get_stats <- function(matrix)
{
 v <- na.omit(matrix[,"m"])
 out <- quantile(v, prob=seq(0,1,0.1))
 names(out) <- paste0("q",sub("%","",names(out)))
 out <- c(out, mean=mean(v), sd=sd(v), range=diff(range(v)))
 out
}
get_stats(imu_data[,,1,"b"]) # test run looks good
feature_data <- data.frame(apply(imu_data, 3:4, get_stats))
feature_data <- data.frame(t(feature_data)) # transpose
rnames <- berryFunctions::l2df(strsplit(rownames(feature_data),".", fixed=TRUE))
feature_data <- cbind(rnames, feature_data) ; rm(rnames)
names(feature_data)[1:2] <- c("id", "condition") 
feature_data$condition <- as.factor(feature_data$condition)

if(FALSE){
# to check results with known simple example:
feature_data <- data.frame(x=runif(30), y=runif(30), z=runif(30))
feature_data$condition <- ifelse(feature_data$x>0.3 & feature_data$y>0.3, "b", "c")
feature_data$condition <- as.factor(feature_data$condition)
scaled_data <- scale(feature_data[,1:3])
}


# K-means clustering ----

scaled_data <- scale(feature_data[,-(1:2)])
true_labels <- feature_data$condition
kmeans_result <- kmeans(scaled_data, centers=2, nstart=25) # k=2 since we have 2 conditions
# manual confusion matrix to assign (random) cluster number to condition
cm <- table(true_labels, kmeans_result$cluster)
cm
clusterlabels <- if(sum(diag(cm)) < sum(diag(cm[,2:1]))) c("c","b") else  c("b","c")
kmeans_result$pred <- factor(clusterlabels[kmeans_result$cluster])
cm <- table(true_labels, kmeans_result$pred)
cm
sum(diag(cm)) / sum(cm) # accuracy

caret::confusionMatrix(kmeans_result$pred, true_labels)

# Visualize clusters using Principal component analysis (PCA):
pca_result <- prcomp(scaled_data)
pca_data <- data.frame(
 PC1=pca_result$x[, 1],
 PC2=pca_result$x[, 2],
 true_condition=factor(true_labels),
 cluster=factor(kmeans_result$pred)
)
op <- par(mfrow=c(1,2), las=1)
plot(PC2~PC1, data=pca_data, col=true_condition, pch=16, main="True condition")
legend("left", levels(pca_data$true_condition), col=1:2, pch=16)
plot(PC2~PC1, data=pca_data, col=cluster, pch=16, main="K-means Clusters")
par(op) ; rm(op)

factoextra::fviz_cluster(kmeans_result, data=scaled_data)
cluster::clusplot(scaled_data, kmeans_result$cluster, color=TRUE, lines=0)

# Feature importance for clustering (distance from cluster centers):
feature_importance <- apply(kmeans_result$centers, 2, function(x) abs(diff(x)))
sort(feature_importance, decreasing=TRUE)[1:10]
