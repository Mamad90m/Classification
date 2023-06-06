
library(ISLR2)
library(ggplot2)
library(lattice)
library(caret)
library(MASS)
library(e1071)
library(class)
library(tidyverse)
##checking data structure and visualization
str(Weekly)
names(Weekly)
head(Weekly)
dim(Weekly)
attach(Weekly)
apply(Weekly, 2, function(x) sum(is.na(x)))
summary(Weekly[2:8])
round(prop.table(table(Direction)), digit = 2)
par(mfrow = c(2, 3))
  hist(Weekly[, 2], lwd = 2, col = "dodgerblue", border = "red", 
       xlab = "Lag1", main = "")

  hist(Weekly[, 3], lwd = 2, col = "dodgerblue", border = "red",
       xlab = "Lag2", main = "")
  
  hist(Weekly[, 4], lwd = 2, col = "dodgerblue", border = "red",
       xlab = "Lag3", main = "")
  
  hist(Weekly[, 5], lwd = 2, col = "dodgerblue", border = "red",
       xlab = "Lag4", main = "")
  
  hist(Weekly[, 6], lwd = 2, col = "dodgerblue", border = "red",
       xlab = "Lag5", main = "")
  
  barplot(table(Direction), col = rainbow(2), xlab = "Direction", 
          ylab = "Frequency") 
par(mfrow = c(1, 1))
featurePlot(x = Weekly[, 2:6], y = Weekly$Direction, 
            plot = "ellipse", auto.key = list(column = 2))
featurePlot(x = Weekly[, 2:6], y = Weekly$Direction, 
            plot = "boxplot", auto.key = list(column = 2))
##Fitting a logistic regression model
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, 
                data = Weekly, family = binomial)
summary(glm.fits)
#Since Lag2 and Lag1 have the smallest p-value respectively, 
#we choose these two variables in order to predict Direction.
##Creating test and train
Weekly_train <- (Year < 2007)
Weekly_test <- Weekly[!Weekly_train, ]
dim(Weekly_test)
Direction_test <- Direction[!Weekly_train]
##Fitting a logistic regression on train dataset
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Weekly, 
                family = binomial, subset = Weekly_train)
##Making a prediction
glm.probs <- predict(glm.fits, Weekly_test, type = "response")
glm.pred <- rep("Down", 209)
glm.pred[glm.probs > 0.5] = "Up"
#Confusion matrix
cm.glm <- confusionMatrix(factor(glm.pred), factor(Direction_test))
plt <- as.data.frame(cm.glm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference ,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Down","Up")) +
  scale_y_discrete(labels=c("Up","Down"))
####linear discriminant analysis(lda)
#Fitting a LDA model
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Weekly,
               subset = Weekly_train)
plot(lda.fit)
##making a prediction 
lda.pred <- predict(lda.fit, Weekly_test)
names(lda.pred)
lda.class <- lda.pred$class
#Confusion Matrix
cm.lda <- confusionMatrix(factor(lda.class), factor(Direction_test))
plt <- as.data.frame(cm.lda$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference, Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Down","Up")) +
  scale_y_discrete(labels=c("Up","Down"))
####Quadratic discriminant analysis(qda)
#Fitting a QDA model
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Weekly, 
               family = binomial, subset = Weekly_train)
#making prediction
qda.pred <- predict(qda.fit,  Weekly_test)
qda.class <- qda.pred$class
#Confusion Matrix
cm.qda <- confusionMatrix(factor(qda.class), factor(Direction_test))
plt <- as.data.frame(cm.qda$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference, Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Down","Up")) +
  scale_y_discrete(labels=c("Up","Down"))
####Naive Bayes(nb)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Weekly, 
                     subset = Weekly_train)
mean(Lag1[Weekly_train][Direction[Weekly_train] == "Down"])
sd(Lag1[Weekly_train][Direction[Weekly_train] == "Down"])
##making prediction with nb model
nb.class <- predict(nb.fit, Weekly_test)
#Confusion Matrix
cm.nb <- confusionMatrix(factor(nb.class), factor(Direction_test))
plt <- as.data.frame(cm.nb$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference, Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Down","Up")) +
  scale_y_discrete(labels=c("Up","Down"))
##Using predict function in nb model to generate estimates of the 
#-probability that each observation belongs to a particular class.
nb.preds <- predict(nb.fit, Weekly_test, type = "raw")
nb.preds[1:5, ]
####K-Nearest-Neighbors
##making train and test dataset
train.X <- cbind(Lag1, Lag2)[Weekly_train, ]
test.X <- cbind(Lag1, Lag2)[!Weekly_train, ]
train.Direction <- Direction[Weekly_train]
set.seed(1)
##using knn on dataset with k =5
knn.pred <- knn(train.X, test.X, train.Direction, k = 5)
#Confusion matrix
cm.knn <- confusionMatrix(factor(knn.pred), factor(Direction_test))
plt <- as.data.frame(cm.knn$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference, Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Down","Up")) +
  scale_y_discrete(labels=c("Up","Down"))
cm.knn
attributes(cm.knn)
names(cm.knn)
cm.knn[[3]][1]
cm.knn[[4]][1]
cm.knn[[4]][2]
comp <- data.frame(Model = c("Logistic Regression", "lda", "qda", "Naive Bayes", "knn"),
                   Accuracy = c(cm.glm[[3]][1], cm.lda[[3]][1], cm.qda[[3]][1], 
                     cm.nb[[3]][1], cm.knn[[3]][1]), Sensitivity = 
                     c(cm.glm[[4]][1], cm.lda[[4]][1], cm.qda[[4]][1], 
                       cm.nb[[4]][1], cm.knn[[4]][1]), Specificity = 
                     c(cm.glm[[4]][2], cm.lda[[4]][2], cm.qda[[4]][2], 
                       cm.nb[[4]][2], cm.knn[[4]][2]))
complong <- gather(comp, key = "measure", value = "value", c("Accuracy", "Sensitivity", 
                                                         "Specificity")
  complong %>% 
  ggplot(aes(x = Model, y = value, fill = measure)) +
    geom_bar(stat = 'identity')+
facet_wrap(~ measure)
  
  
  
  
  
  
  
  

  

