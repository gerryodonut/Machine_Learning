library(caret)
library(naivebayes)
library(e1071)
library(ggplot2)
library(corrplot)
dataku <- read.csv("D:/Documents/1. Okta Toyibah/2. Semester/5. Semester 5/Machine Learning/Lomba/Data_Fix.csv", sep = ",")
View(dataku)
summary(dataku)
str(dataku)
dataku$label <- as.factor(dataku$label)
str(dataku)
datasample <- sample(2, nrow(dataku), replace = T, prob = c(0.7,0.3))
datatrain <- dataku[datasample==1,]
datatest <- dataku[datasample==2,]
nb <- naiveBayes(label~., data = datatrain)
prediksi <- predict(nb, newdata = datatest)
conma <- confusionMatrix(datatest$label,prediksi)
conma
conma$byClass
hist(dataku[,2], xlab="Rating", main="Histogram Rating Restaurant")
boxplot(dataku[,2], xlab="Rating", ylab="frequency", col="orange")
plot(x=dataku[,2], y=dataku[,4], xlab = "Rating", ylab = "Price", col=3, pch=16, main="Plot Data Price & Rating")
qqnorm(dataku$rating)
qqline(dataku$rating)
korelasi <- cor(dataku[,2],dataku[,4])
korelasi
hist(dataku[,2], xlab="Rating", main="Histogram Rating Restaurant", col = "blue")

