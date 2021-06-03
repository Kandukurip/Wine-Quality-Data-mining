white.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
white.raw <- read.csv(white.url, header = TRUE, sep = ";")
white <- white.raw
View(white)
str(white)
library(factoextra)
library(NbClust)
set.seed(1234)
ind <- sample(2, nrow(white), replace=T, prob=c(0.7, 0.3))
white.train <- white[ind==1, ]
white.test <- white[ind==2, ]
# build a decision tree
library(party)
white.formula <- alcohol ~ fixed.acidity + residual.sugar
white.ctree <- ctree(white.formula, data=white.train)
white.ctree
plot(white.ctree)
# predict on test data
pred <- predict(white.ctree, newdata = white.test)
# check prediction result
table(pred, white.test$alcohol)
set.seed(8953)
white2 <- white
# remove class IDs
white2$alcohol <- NULL
# k-means clustering
white.kmeans <- kmeans(white2, 3)
# check result
table(white$alcohol, white.kmeans$cluster)
# plot clusters and their centers
plot(white2[c("fixed.acidity", "residual.sugar")], col=white.kmeans$cluster)
points(white.kmeans$centers[, c("fixed.acidity", "residual.sugar")],
       col=1:3, pch="*", cex=5)
library(fpc)
white2 <- white[-5] # remove class IDs
# DBSCAN clustering
ds <- dbscan(white2, eps = 0.42, MinPts = 5)
# compare clusters with original class IDs
table(ds$cluster, white$alcohol)
# 1-3: clusters; 0: outliers or noise
plotcluster(white2, ds$cluster)
