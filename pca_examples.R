#PCA 1:breakfast cereals
cereals.df <- read.csv("Cereals.csv") 
# compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating)) 
summary(pcs) 
pcs$rot
scores <- pcs$x
head(scores, 5)

pcs <- prcomp(na.omit(cereals.df[,-c(1:3)])) 
summary(pcs)

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)

plot(cereals.df$calories,cereals.df$rating,pch=19,xlab="Calories",ylab="Rating",cex=1.5,cex.lab=1.5,col="skyblue4")

require(graphics)
plot(prcomp(na.omit(cereals.df[,-c(1:3)])))

biplot(prcomp(na.omit(cereals.df[,-c(1:3)]), scale = TRUE))

#PCA 2: IRIS 
#pca - calculated for the first 4 columns of the data set that correspond to biometric measurements ("Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width")
data(iris)

#split data into 2 parts for pca training (75%) and prediction (25%)
set.seed(1)
samp <- sample(nrow(iris), nrow(iris)*0.75)
iris.train <- iris[samp,]
iris.valid <- iris[-samp,]

#conduct PCA on training dataset
pca <- prcomp(iris.train[,1:4], retx=TRUE, center=TRUE, scale=TRUE)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100) # percent explained variance

#prediction of PCs for validation dataset
pred <- predict(pca, newdata=iris.valid[,1:4])

#plot result
COLOR <- c(2:4)
PCH <- c(1,16)

pc <- c(1,2) #principal components to plot

png("pca_pred.png", units="in", width=5, height=4, res=200)
op <- par(mar=c(4,4,1,1), ps=10)
plot(pca$x[,pc], col=COLOR[iris.train$Species], cex=PCH[1], 
 xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"), 
 ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
)
points(pred[,pc], col=COLOR[iris.valid$Species], pch=PCH[2])
legend("topright", legend=levels(iris$Species), fill = COLOR, border=COLOR)
legend("topleft", legend=c("training data", "validation data"), col=1, pch=PCH)