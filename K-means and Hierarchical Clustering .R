library(corrplot)
library(cluster) 
library(factoextra)
library(mice)
library(VIM)
library(dplyr)
library(DataExplorer)
library(lattice)
library(MASS)
library(nnet)
library(tidyverse)
library(cluster)
library(corrplot)
library(dbscan)

### EDA ###
data <- Wholesale_customers_miss
introduce(data)
plot_str(data)
plot_intro(data)
plot_missing(data)
plot_histogram(data, ncol = 3)

# Identifying outliers
data %>% 
  gather(key = Category, value = Amount) %>%
  ggplot(aes(x = Category, y = Amount)) +
  geom_boxplot(alpha = 0.2)

# Check for correlation
corrmatrix <- cor(data)
corrplot(corrmatrix, method = 'number')

# Checking for missing values
is.na.data.frame(data)
md.pattern(data)
md.pairs(data)

# Visualization of missing values
aggr(data, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(data),
     cex.axis=.6, gap=4, ylab=c("Proportion of missingness","Missingness Pattern"))

### Multiple imputation
imp_data <- mice(data, m=5, maxit=10, method="pmm", seed=123)
summary(imp_data)

xyplot(imp_data, Milk ~ Delicatessen,pch=18,cex=1)
densityplot(imp_data)

#Model fitting
model_final <- with(imp_data,lm(Fresh ~ Milk+Grocery+Frozen+Detergents_Paper+Delicatessen))
model_final

pooled <- pool(model_final)
summary(pooled)

reg_dat <- complete(imp_data )# extract the completed data set
summary(lm(Fresh ~ Milk+Grocery+Frozen+Detergents_Paper+Delicatessen, data = reg_dat))# model fitting and summary

### Clustering ###

# Getting the optimal number of clusters
fviz_nbclust(reg_dat, kmeans, method = "wss")+labs(subtitle = "Elbow method")
fviz_nbclust(reg_dat, kmeans, method="silhouette")+labs(subtitle = "Silhouette method")
fviz_nbclust(reg_dat, kmeans, method="gap_stat")+labs(subtitle = "Gap Stat method")

# Compute k-means with k = 3
set.seed(123)
km.res <- kmeans(reg_dat, 3, nstart = 25)
km.res
# clusplot(reg_dat, km.res$cluster, color = TRUE, shade = TRUE, lines = 0)

# reg_dat$cluster <- km.res$cluster
# head(reg_dat, 6)

# Visualize kmeans clustering
#fviz_cluster(km.res, reg_dat, ellipse.type = "norm")
# clusplot(reg_dat, km.res$cluster, color=TRUE, shade = TRUE, label=2)
# aggregate(reg_dat, by=list(cluster=km.res$cluster), mean)

# # compare K-means clustering to the true class labels
# dd <- cbind(reg_dat, cluster = km.res$cluster)
# table(dd$Category, dd$cluster)

# PCA
pr.out = prcomp(reg_dat) 
labels <- dd$cluster
plot(pr.out$x[,1], pr.out$x[,3], col=labels, pch=19, xlab = "PC1", ylab="PC2")

set.seed(123)
kmean.out = kmeans(pr.out$x[,c(1,3)], centers=3, nstart=25)
fviz_cluster(kmean.out, pr.out$x[,c(1,3)], 
             ellipse.type = "norm", xlab="PC1 (48.3%)", ylab="PC3 (17%)" )
kmean.out


### dbscan clustering
# minPts = 3
model_hdb_3 <- hdbscan(reg_dat, minPts = 3)
print(model_hdb_3)

reg_dat %>% 
  mutate(cluster = model_hdb_3$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean) #cluster 0 is noise

plot(model_hdb_3, show_flat = TRUE)

reg_dat %>% 
  mutate(cluster = model_hdb_3$cluster) %>%
  gather(key = Category, value = Amount, -cluster) %>%
  filter(cluster == 0) %>%
  ggplot(aes(x = Amount)) +
  geom_histogram(bins = 25) +
  facet_wrap(~Category, scales = "free")

# minPts = 4
model_hdb_4 <- hdbscan(reg_dat, minPts = 4)
print(model_hdb_4)

reg_dat %>% 
  mutate(cluster = model_hdb_4$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean) #cluster 0 is noise

plot(model_hdb_4, show_flat = TRUE)

reg_dat %>% 
  mutate(cluster = model_hdb_3$cluster) %>%
  gather(key = Category, value = Amount, -cluster) %>%
  filter(cluster == 0) %>%
  ggplot(aes(x = Amount)) +
  geom_histogram(bins = 25) +
  facet_wrap(~Category, scales = "free")



