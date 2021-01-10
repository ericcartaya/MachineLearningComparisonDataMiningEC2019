#Eric Cartaya
#164006876

library(glmnet)
library(e1071)
library(randomForest)
library(penalizedSVM)
set.seed(164006876)

load("C:/Users/ecart/Desktop/School/Data Mining/geno.r")

sampleSize <- floor(0.8 * nrow(x0))

trainIndex <- sample(seq_len(nrow(x0)), size = sampleSize)

trainers <- x0[trainIndex, ]
testers <- x0[-trainIndex, ]
trainers2 <- t(trainers)
testers2 <- t(testers)

designMatrix = cbind(x0==1,x0==2)*1

svm1 = svm(trainers)
pred1 = predict(svm1, testers)

forest = randomForest(y~trainers2, data = trainers2)

nb = naiveBayes(y~trainers, data = trainers)
pred3 = predict(nb, testers)

g1 = glmnet(trainers2, y)
pred3 = predict(g1, testers)

svm2 = svmfs(trainers2, y)
