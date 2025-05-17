#zadanie 1
install.packages("ClusterR")
library(ClusterR)
library(cluster)
head(iris)
iris_1 <- iris[,-5]
iris_2 <- scale(iris_1)
set.seed(245)

kmeans.trzy <- kmeans(iris_2, centers = 3, iter.max = 20, nstart = 30)
kmeans.trzy
kmeans.trzy$cluster

table(iris$Species)

confusionmatrix <- table(iris$Species, kmeans.trzy$cluster)
confusionmatrix

spec <- as.factor(ifelse(kmeans.trzy$cluster == 2, "setosa", 
                  ifelse(kmeans.trzy$cluster == 3, "versicolor", "virginica")))
spec

confusionmatrix.kor <- table(iris$Species, spec)
confusionmatrix.kor

iris_1
plot(iris_1[,c("Sepal.Length", "Sepal.Width")])
plot(iris_1[,c("Sepal.Length", "Sepal.Width")],
     col = kmeans.trzy$cluster,
     main = "K-means with 3 clusters")

kmeans.trzy$centers
centers1 <- kmeans.trzy$centers[, "Sepal.Length"] * sd(iris_1[, "Sepal.Length"]) + mean(iris_1[, "Sepal.Length"])
centers1
centers2 <- kmeans.trzy$centers[, "Sepal.Width"] * sd(iris_1[, "Sepal.Width"]) + mean(iris_1[, "Sepal.Width"])
centers2
center <- cbind(centers1, centers2)
center

# cex is font size, pch is symbol
points(center, 
       col = 1:3, pch = 8, cex = 3) 

y_kmeans <- kmeans.trzy$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')

install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.trzy, data = iris_1, 
             palette = c("blue", "red","green"), 
             geom = c("point", "text"), 
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#zadanie 2
library(cluster)
library(ggplot2)
library(factoextra)

head(diamonds)
str(diamonds)
levels(diamonds$cut)
levels(diamonds$color)
levels(diamonds$clarity)

diamonds
set.seed(23)
dane <- diamonds

dane$cut
dane$cut <- as.numeric(dane$cut)
dane$cut

dane$color
dane$color <- as.numeric(dane$color)
dane$color

dane$clarity
dane$clarity <- as.numeric(dane$clarity)
dane$clarity

rows <- sample(nrow(dane), 1000)
rows
dane <- dane[rows, c(1:10)]
dane

dane <- na.omit(dane)
danes <- scale(dane)
danes

fviz_nbclust(danes, kmeans, method = "wss", k.max = 12) +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Metoda łokcia")

km.diamonds <- kmeans(danes, centers = 3, iter.max = 20, nstart = 30)
km.diamonds
km.diamonds$cluster
km.diamonds$centers

fviz_cluster(km.diamonds, data = dane,
             palette = c("pink", "lightblue", "hotpink"),
             geom = c("point", "text"),
             ellipse.type = "convex",
             ggtheme = theme_bw())
#fviz_cluster(km.diamonds, data = dane, geom = "point")
#fviz_cluster(km.diamonds, data = dane, geom = c("point", "text"))
#fviz_cluster(km.diamonds, data = dane, geom = "point", ellipse.type = "convex")

y_kmeans <- km.diamonds$cluster
y_kmeans

clusplot(dane[, c(1,2)],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster diamonds"),
         xlab = 'Carat',
         ylab = 'cut')

str(diamonds)
dane.n <- diamonds[,c(1,5,6,7,8,9,10)]
num_cols <- sapply(diamonds, is.numeric)
num_cols
dane.n <- na.omit(dane.n)
dane.n.s <- scale(dane.n)
km.diamonds.n <- kmeans(dane.n.s, centers = 2, iter.max = 20, nstart = 30)
km.diamonds.n
km.diamonds.n$cluster
km.diamonds.n$centers

fviz_cluster(km.diamonds.n, data = dane.n, 
             palette = c("purple", "magenta", "hotpink" ),
             geom =c("point", "text"), 
             ellipse.type = "convex",
             ggtheme = theme_bw())

#zadanie 3
library(cluster)
votes.repub
names(votes.repub)
set.seed(123)

vote <- votes.repub[, c("X1960", "X1964", "X1968", "X1972", "X1976")]
vote
names(vote)
str(vote)
vote <- na.omit(vote)
vote.s <- scale(vote)
vote.s

library(factoextra)
fviz_nbclust(vote.s, kmeans, method = "wss") +
  geom_vline(xintercept = 7, linetype = 2) +
  labs(subtitle = "Metoda łokcia")

km.vote.s <- kmeans(vote.s, centers = 7, iter.max = 20, nstart = 30)
km.vote.s$centers
km.vote.s$cluster

fviz_cluster(km.vote.s, data = vote, 
             palette = c("blue", "magenta", "purple", "green", "red", "orange", "yellow"),
             geom = c("point", "text"),
             ellipse.type = "convex",
             ggtheme = theme_bw())

vote <- votes.repub[, c("X1960", "X1976")]
vote <- na.omit(vote)
vote.s <- scale(vote)

fviz_nbclust(vote.s, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Metoda łokcia")

km.vote.s <- kmeans(vote.s, centers = 3, iter.max = 20, nstart = 30)
km.vote.s$centers
km.vote.s$cluster

fviz_cluster(km.vote.s, data = vote,
             palette = c("hotpink", "purple", "blue"),
             geom = c("point", "text"),
             ellipse.type = "convex",
             ggtheme = theme_bw())

#zadanie 4
library(cluster)
pluton
names(pluton)
str(pluton)
pl <- na.omit(pluton)

pls <- scale(pl)
library(factoextra)

fviz_nbclust(pls, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Metoda łokcia")

km.pl <- kmeans(pls, centers = 3, iter.max = 20, nstart = 30)
km.pl
km.pl$cluster
km.pl$centers

fviz_cluster(km.pl, data = pls,
             palette = c("magenta", "blue", "purple"),
             geom = c("point", "text"),
             ellipse.type = "convex",
             ggtheme = theme_bw())

