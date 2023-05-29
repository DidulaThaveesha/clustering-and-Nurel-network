library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(cluster)
library(caret)
library("FactoMineR")
library("factoextra")
library(factoextra)
library(dplyr)
library(stats)
library(cluster)
library(fpc)


datasets = read_xlsx("vehicles.xlsx")
datasets <- subset(datasets, select = -Class)
datasets <- subset(datasets, select = -Samples)
datasets<-scale (datasets)


boxplot(datasets)

for (i in 1:ncol(datasets)) {
  q1 <- quantile(datasets [,i],0.25, na.rm = TRUE)
  q3 <- quantile(datasets [,i],0.75, na.rm = TRUE)
  
  #inter-quartile-range
  iqr <- q3- q1
  
  #upper bound and lower bound
  u_bound <-q3 + 1.5 * iqr
  l_bound <- q1 - 1.5 * iqr
  
  #replacing the outliers data as NA
  datasets[,i][datasets[,i] > u_bound] <- NA
  datasets[,i][datasets[,i] < l_bound] <- NA
}
cleaned_vehicle <- na.omit(datasets)
boxplot(cleaned_vehicle)

#-----------------part B--------------------------
# NBclust method

set.seed(123)
nb <- NbClust(cleaned_vehicle, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
k <- nb$Best.nc

# Elbow method
wss <- (nrow(cleaned_vehicle)-1)*sum(apply(cleaned_vehicle,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(cleaned_vehicle, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#k_optimal <- 3

# Gap statistic method
library(cluster)
set.seed(123)
fviz_nbclust(cleaned_vehicle, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# Silhouette method
fviz_nbclust(cleaned_vehicle, FUN = hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method")
optimal_clusters <- 3 # Replace this with the optimal number you found
kmeans_result <- kmeans(cleaned_vehicle, centers = optimal_clusters)

print(kmeans_result)


#------------- part C--------------------------------------

# Set seed for reproducibility
set.seed(123)

# Perform kmeans clustering with k=3
kmeans_result <- kmeans(cleaned_vehicle, centers = 3)

# Print kmeans output
kmeans_result

# Print centers of each cluster
print(kmeans_result$centers)

# Print cluster assignment for each observation
print(kmeans_result$cluster)

# Calculate and print BSS and WSS
BSS <- sum(kmeans_result$size * apply(kmeans_result$centers, 1, function(x) sum((x - mean(cleaned_vehicle))^2)))
WSS <- sum(kmeans_result$withinss)
TSS <- sum(kmeans_result$tot.withinss) + sum(kmeans_result$betweenss)
BSS_ratio <- BSS / TSS
cat("BSS:", BSS, "\n")
cat("WSS:", WSS, "\n")
cat("BSS/TSS ratio:", BSS_ratio,"\n")



#------------Part D ----------------------------------
fviz_silhouette(silhouette(kmeans_result$cluster, dist(cleaned_vehicle)))
#------------- Part E --------------------------

# Perform PCA on the scaled data
pca <- prcomp(cleaned_vehicle)

# The eigenvalues and eigenvectors should be shown.
summary(pca)

# Calculate the overall score for each principal component.
cumulative <- cumsum(pca$sdev^2 / sum(pca$sdev^2))

# Make a new dataset with the characteristics of the primary components.
X_pca <- predict(pca, cleaned_vehicle)

# Select the main factors that result in a cumulative score of at least 92%.
num_pcs <- sum(cumulative <= 0.92)
X_pca <- X_pca[, 1:num_pcs]

# Display the transformed dataset
head(X_pca)
X_pca

#------------------Part F -------------------


# Perform PCA analysis on the dataset
pca <- prcomp(cleaned_vehicle, center = TRUE, scale. = TRUE)

# Display the summary of PCA analysis
summary(pca)

# Display the scree plot to visualize the eigenvalues
plot(pca, type = "l")

# Display the biplot to visualize the correlation between variables and principal components
biplot(pca, scale = 0)

# Extract the transformed dataset with principal components as attributes
transformedData <- as.data.frame(pca$x)

# Calculate the cumulative score per principal component
cumulative_score <- cumsum(pca$sdev^2 / sum(pca$sdev^2) * 100)

# Display the cumulative score
cumulative_score

# Choose the principal components with at least cumulative score > 92%
chosen_pc <- which(cumulative_score > 92)[1]

# Create a new dataset with the chosen principal components
newData <- as.data.frame(pca$x[, 1:chosen_pc])

# Display the transformed dataset with the chosen principal components
head(newData)

# NBclust method

set.seed(123)
nb <- NbClust(newData, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nb <- NbClust(newData, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans")
k <- nb$Best.nc

# Elbow method
wss <- (nrow(newData)-1)*sum(apply(newData,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(newData, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#k_optimal <- 3

# Gap statistic method
library(cluster)
set.seed(123)
fviz_nbclust(newData, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# Silhouette method
fviz_nbclust(newData, FUN = hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method")
optimal_clusters <- 3 # Replace this with the optimal number you found
kmeans_result <- kmeans(newData, centers = optimal_clusters)

print(kmeans_result)

######################### part 1 g #######################################

# Set seed for reproducibility
set.seed(123)

# Perform kmeans clustering with k=3
kmeans_result <- kmeans(newData, centers = 2)

# Print kmeans output
kmeans_result

# Print centers of each cluster
print(kmeans_result$centers)

# Print cluster assignment for each observation
print(kmeans_result$cluster)

# Calculate and print BSS and WSS
BSS <- sum(kmeans_result$size * apply(kmeans_result$centers, 1, function(x) sum((x - mean(newData))^2)))
WSS <- sum(kmeans_result$withinss)
TSS <- sum(kmeans_result$tot.withinss) + sum(kmeans_result$betweenss)
BSS_ratio <- BSS / TSS
cat("BSS:", BSS, "\n")
cat("WSS:", WSS, "\n")
cat("BSS/TSS ratio:", BSS_ratio,"\n")
fviz_cluster(kmeans_result,data = newData)
#-------------------Part H----------------------------

fviz_silhouette(silhouette(kmeans_result$cluster, dist(newData)))

#-------------------Part I----------------------------
# Calculate Calinski-Harabasz index
ch_index <- calinhara(newData, kmeans_result$cluster)
# Print Calinski-Harabasz index
print(ch_index)


