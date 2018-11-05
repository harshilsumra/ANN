#ANN on data
#reading the data
library(readxl)
dataset <- read_excel(file.choose())
head(dataset)

#feature scaling
normalize <- function(x) {
                          return((x - min(x)) / (max(x) - min(x)))
}
dataset_norm <- as.data.frame(lapply(dataset, normalize))

# Random sampling
samplesize = 0.80 * nrow(dataset)
set.seed(123)
index = sample( seq_len ( nrow ( dataset_norm ) ), size = samplesize )

# Create training and test set
trainingset = dataset_norm[ index, ]
testset = dataset_norm[ -index, ]

str(dataset_norm)
#developing ANN using h2o

library(h2o)
h2o.init(nthreads = -1)
regressor1=h2o.deeplearning(y='prestige_points',
                           training_frame = as.h2o(trainingset),
                           activation='Tanh',
                           hidden=c(2,2),
                           epochs=1000,
                           train_samples_per_iteration=-2)
#putting the test set in
regressor1
plot(regressor1)
pred=h2o.predict(regressor1,type='numerical',newdata = as.h2o(testset[-4]))
finpred=as.vector(pred)
h2o.shutdown()
y
#comparisons
cor(finpred,testset$prestige_points)
dfinpred=as.data.frame((finpred*(max(dataset$prestige_points)-min(dataset$prestige_points)))+(min(dataset$prestige_points)))
actualprestigepts=as.data.frame(((testset$prestige_points)*(max(dataset$prestige_points)-min(dataset$prestige_points)))+(min(dataset$prestige_points)))
cor(dfinpred,actualprestigepts)

#2nd iteration ----- 

#developing ANN using h2o

library(h2o)
h2o.init(nthreads = -1)
regressor=h2o.deeplearning(y='prestige_points',
                           training_frame = as.h2o(trainingset),
                           activation='Rectifier',
                           hidden=c(2,2),
                           epochs=1000,
                           train_samples_per_iteration=-2)
#putting the test set in
regressor
pred=h2o.predict(regressor,type='numerical',newdata = as.h2o(testset[-4]))
finpred2=as.vector(pred)
h2o.shutdown()
y
#comparisons
dfinpred2=as.data.frame(finpred2)
testset2=cbind.data.frame(testset,dfinpred2)
testset2

#using neural net
library(neuralnet)
regressor3=neuralnet(trainingset$prestige_points~trainingset$firm_age+trainingset$offer_size_cr+trainingset$piph.index.,data=trainingset, hidden=1 )
plot(regressor3)
pred3=compute(regressor3,testset[-4])
finpred3=as.vector(pred3)
dfinpred3=as.data.frame(finpred3)
dfinpred3
model_str=pred3$net.result
cor(model_str,testset$prestige_points)
