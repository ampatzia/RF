library(rpart)
library(caret)
library(rpart.plot)
library(CORElearn)
library(MASS)
library(mlbench)
library(alr3)


data(stagec) #rpart

stagec$pgstat <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))



set.seed(123)
train_ind  <- sample(1:nrow(stagec), 0.75 * nrow(stagec))
train <- stagec[train_ind, ]
test <- stagec[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))




fit.rand.forest = CoreModel(pgstat~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$pgstat)
confusionMatrix(a1)



fit.rand.forest = CoreModel(pgstat~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$pgstat)
confusionMatrix(a1)



fit.rand.forest = CoreModel(pgstat~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$pgstat)
confusionMatrix(a1)


fit.rand.forest = CoreModel(pgstat~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$pgstat)
confusionMatrix(a1)



data(segmentationData) #caret
#rpart

set.seed(123)
train_ind  <- sample(1:nrow(stagec), 0.75 * nrow(stagec))
train <- stagec[train_ind, ]
test <- stagec[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))

test<-filter(segmentationData,Case=="Test")
train<-filter(segmentationData,Case=="Train")

test<-test[,-2]
train<-train[,-2]



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=100)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)


fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)

data(ptitanic) #rpartplot

set.seed(123)
train_ind  <- sample(1:nrow(ptitanic), 0.75 * nrow(ptitanic))
train <- ptitanic[train_ind, ]
test <- ptitanic[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))



fit.rand.forest = CoreModel(survived~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$survived)
confusionMatrix(a1)



fit.rand.forest = CoreModel(survived~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$survived)
confusionMatrix(a1)



fit.rand.forest = CoreModel(survived~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$survived)
confusionMatrix(a1)


fit.rand.forest = CoreModel(survived~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$survived)
confusionMatrix(a1)


data("fgl") #MASS



set.seed(123)
train_ind  <- sample(1:nrow(fgl), 0.75 * nrow(fgl))
train <- fgl[train_ind, ]
test <- fgl[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))


fit.rand.forest = CoreModel(type~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=100)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$type)
confusionMatrix(a1)



fit.rand.forest = CoreModel(type~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$type)
confusionMatrix(a1)



fit.rand.forest = CoreModel(type~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$type)
confusionMatrix(a1)



fit.rand.forest = CoreModel(type~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$type)
confusionMatrix(a1)





data("mammoexp", package = "TH.data")


set.seed(123)
train_ind  <- sample(1:nrow(mammoexp), 0.75 * nrow(mammoexp))
train <- mammoexp[train_ind, ]
test <- mammoexp[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))


fit.rand.forest = CoreModel(DECT~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$DECT)
confusionMatrix(a1)



fit.rand.forest = CoreModel(DECT~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$DECT)
confusionMatrix(a1)



fit.rand.forest = CoreModel(DECT~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$DECT)
confusionMatrix(a1)

fit.rand.forest = CoreModel(DECT~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$DECT)
confusionMatrix(a1)



data(blowdown) #alr


blowdown$y<- as.factor(blowdown$y)

set.seed(123)
train_ind  <- sample(1:nrow(blowdown), 0.75 * nrow(blowdown))
train <- blowdown[train_ind, ]
test <- blowdown[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))


fit.rand.forest = CoreModel(y~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$y)
confusionMatrix(a1)



fit.rand.forest = CoreModel(y~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$y)
confusionMatrix(a1)



fit.rand.forest = CoreModel(y~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$y)
confusionMatrix(a1)



fit.rand.forest = CoreModel(y~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$y)
confusionMatrix(a1)

library(mlbench)
#load Ionosphere
data(Ionosphere)


set.seed(123)
train_ind  <- sample(1:nrow(Ionosphere), 0.75 * nrow(Ionosphere))
train <- Ionosphere[train_ind, ]
test <- Ionosphere[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))

fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=100)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)

fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



data("Vehicle") #mlbench




set.seed(123)
train_ind  <- sample(1:nrow(Vehicle), 0.75 * nrow(Vehicle))
train <- Vehicle[train_ind, ]
test <- Vehicle[-train_ind, ]


#train<-train[complete.cases(train),]

rownames(train)<-c(1:nrow(train))
rownames(test)<-c(1:nrow(test))

fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="InfGain", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="Gini", minNodeWeightRF=5, rfNoTrees=1000)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="GainRatio", minNodeWeightRF=5, rfNoTrees=100)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)


fit.rand.forest = CoreModel(Class~., data=train, model="rf", selectionEstimator="Accuracy", minNodeWeightRF=5, rfNoTrees=1)

a<-predict(fit.rand.forest, newdata=test)
a1<-table(a$class,test$Class)
confusionMatrix(a1)



