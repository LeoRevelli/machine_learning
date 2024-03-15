library(haven)
library(stargazer)
library(rpart)
library(rpart.plot)
library(tree)
library(sjlabelled)

#1#Load Eurobarometer data
data <- read_dta("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/machine learning/Project/Data/ZA7901_v1-0-0.dta")
summary(dat?)
#1.1#Select only data with danish respondant
dataden <- subset(data, q1_2==1)
dataden1 <- dataden %>% drop_na(sd05, d15a, d7, d9a,d10,d25 , d60)
library(labelled)
dataden2 <- one_hot(as.data.table(dataden2))

#we create manually an index of environmental?y awareness (envAware), based on answer to questions qa1_3, qa3_1, qa3_2, qa10_2, qa18
dataden1$envAware3one <- ifelse(dataden1$qa1_3<=2 & dataden1$qa3_1<=2 &
                 dataden1$qa18<=4, 1, 0)
dataden1$envAware3 <- ifelse(dataden1$qa1_3<=2 & dataden?$qa3_1<=2 &
                                  dataden1$qa18<=4, "Yes", "No")
sum(dataden1$envAware == "Yes", na.rm=TRUE) # <- we count number of "environmentally aware individual
#we select sociodemographic variables of interest
dataden2 <- subset(dataden1? select=c("sd05","d15a","d7","d8","d9a","d10","d25","sd14","sd15","d40a","d40b","d40c","d60","d62t","sd16",
                                      "p3","envAware3one","envAware3","uniqid","d11","qa3_2","qa10_2"))
dataden2[!complete.cases(dataden2), ] #no NA? in our dataset

########Predict environmental awareness based on socio-demographic characteristics - Denmark
###We create a training set and a test dataset from dataden2
#make this example reproducible
set.seed(1234564)
#use 70% of dataset as training set?and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(dataden2), replace=TRUE, prob=c(0.7,0.3))
traindf  <- dataden2[sample, ]
testdf   <- dataden2[!sample, ]
#Naive OLS regression in training sample
model1=lm(envAware3 ~. - d8-d9a, data=dataden2) #we ?o not use d8 (age education) and d9a (education level) as we prefer their one-hot encodement (d9a_1 to d9a_9)
stargazer(model1, type='text')
#F stat = 1.688 but Fstat p-value<0.05, so our variables are jointly significant even if we have loww R squared
#Te?t predictive accuracy of such a model
testdf$pl_OLS <- predict(model1, newdata=testdf)
#We have to round predicted label as envAware take value 0 or 1. We choose that if predicted label >0.5, then envAware prediction is 1, and 0 otherwise
testdf$pl_OLS_rou?d=ifelse(testdf$pl_OLS >=0.5, 1,0)
sum(testdf$envAware == testdf$pl_OLS_round, na.rm=TRUE) 
#with this model on this specific sample we have 134 good predictions, this means that around 45% of good predictions
#test error rate
mean((testdf$pl_OLS_round -te?tdf$envAware)^2) #test error rate equal to around 55%
##Training error rate
traindf$pl_OLS <- predict(model1, newdata=traindf)
traindf$pl_OLS_round = 0
traindf$pl_OLS_round[traindf$pl_OLS >=0.5]<-1
sum(traindf$envAware == traindf$pl_OLS_round, na.rm=TRUE) ?#training error rate of 54% (STRANGE ???)
#this model overfit data as with other training and test sample, the test error rate was about 37%

#LASSO & RIDGE
library(glmnet)
#Set a seed for replicability
set.seed(1324564)
#set data
x=model.matrix(envAware ~?.-d8, data=dataden2)[,-1]
#Index for observations going to the training sample
train=sample(1:nrow(dataden2),size=700)
#Index for observations going to the test sample (i.e., those not in training already)
test=(1:nrow(dataden2))[which(1:nrow(dataden2)%in%?rain==FALSE)]
y.test=dataden2$envAware[test]
#lasso/rodge, alpha=0 ridge, alpha=1, lasso
lambda_grid=c(sort(10^seq(5,-2,length=100),decreasing=TRUE),0)
ridge.mod=glmnet(x,dataden2$envAware,alpha=0,lambda=lambda_grid,subset=train)
lasso.mod=glmnet(x,dataden?$envAware,alpha=1,lambda=lambda_grid,subset=train)
#Test error rate RIDGE with no CV
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,]) #with lambda=4
ridge.pred.round=ifelse(ridge.pred >=0.5, 1,0)
mean((ridge.pred.round - y.test)^2)
#Test error rate LASS? with no CV
lasso.pred=predict (lasso.mod ,s=4, newx=x[test ,]) #with lambda=4
lasso.pred.round=ifelse(lasso.pred >=0.5, 1,0)
mean((lasso.pred.round - y.test)^2)
#Test error rate OLS
OLS.pred=predict (ridge.mod ,s=0, newx=x[test ,]) #with lambda=0 we have ?east square coef estimates
OLS.pred.round=ifelse(OLS.pred >=0.5, 1,0)
#COMPARE
mean((ridge.pred.round - y.test)^2)
mean((lasso.pred.round - y.test)^2)
mean((OLS.pred.round - y.test)^2)
mean((1 - y.test)^2) #if we give value 1 to each variable we have same ?rro rate

#10-folds CV MSEs:
CV.ridge.MSE=cv.glmnet(x=x,y=dataden2$envAware,nfolds=10,lambda=lambda_grid,alpha=0)
CV.lasso.MSE=cv.glmnet(x=x,y=dataden2$envAware,nfolds=10,lambda=lambda_grid,alpha=1) #???value of lambda that minimizes MSE is about lambda=ex?(5) (as we have log(lambda)=-5)
plot(CV.ridge.MSE)
plot(CV.lasso.MSE)
#lasso more efficient than ridge


#LOOCV MSEs// same than 10-folds CV:
LOOCV.ridge.MSE=cv.glmnet(x=x,y=dataden2$envAware,nfolds=length(dataden2$envAware),lambda=lambda_grid,alpha=0)
LOO?V.lasso.MSE=cv.glmnet(x=x,y=dataden2$envAware,nfolds=length(dataden2$envAware),lambda=lambda_grid,alpha=1)
plot(LOOCV.lasso.MSE)
plot(LOOCV.ridge.MSE)

dataden2$envaware=ifelse(dataden1$qa1_3==1|dataden1$qa1_3==2 & dataden1$qa3_1==1|dataden1$qa3_1==2 &
   ?               dataden1$qa3_2==1|dataden1$qa3_2==2 & dataden1$qa10_2==1|dataden1$qa10_2==2 &
                   dataden1$qa18==1|dataden1$qa18==2|dataden1$qa18==3|dataden1$qa18==4,"Yes","No")

#CLASSIFICATION TREE
#we start by growing a full tree on our sa?ple
tree1=tree(envAware3one ~., data=dataden2[complete.cases(dataden2),])
plot(tree1)
text(tree1)
summary(tree1)
#Variables actually used in tree construction:
#  [1] "d9a"    "d25"    "qa3_2"  "qa10_2" "sd15"  
#Number of terminal nodes:  7 
#Residual mea? deviance:  0.1884 = 187.8 / 997
#use 70% of dataset as training set and 30% as test set
set.seed(1)
train=sample (1: nrow(dataden2 ), 700)
#use test sample
dataden.test=dataden2[-train ,]
envaware.test=envaware[-train]
tree.dataden =tree(envAware3~. , dat?den2 ,subset=train)
tree.pred=predict(tree.dataden ,dataden.test ,type="class")
table(tree.pred ,envaware.test)



#Fitting a full tree taking all regressors
tree1=tree(envAware ~ .-d8, data=dataden2[complete.cases(dataden2),], type="class")
plot(tree1)
te?t(tree1)
#Optimal pruning
set.seed(1234565)
ptree1=cv.tree(envAware3 ~ .-d8, data=dataden2[complete.cases(dataden2),])
ptree1$size[which(ptree1$dev==min(ptree1$dev))]
#Optimal tree:
prune.tree(tree1,best=5)

library(randomForest)
#make this example reprodu?ible
set.seed(1234564)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(dataden2), replace=TRUE, prob=c(0.7,0.3))
traindf  <- dataden2[sample, ]
testdf   <- dataden2[!sample, ]
#Growing a RF of ... trees, consi?ering one regressor at a time (test sample)
set.seed(12345645)
RF1t=randomForest(envAware ~ .-d8-d9a, data=traindf[complete.cases(traindf),],mtry=1,importance=TRUE)
importance(RF1t,type=1)
varImpPlot(RF1t,type=1)
#test model accuracy for predictions
testdf?predictedlabel <- predict(RF1t, newdata=testdf)
testdf$predictedlabelround=0
testdf$predictedlabelround[testdf$predictedlabel >= 0.5]<-1
sum(testdf$envAware == testdf$predictedlabelround, na.rm=TRUE) #139/249 soit 55% soit correctement classé
testdf$predic?edlabelround45 = 0
testdf$predictedlabelround45[testdf$predictedlabel >=0.45]<-1
sum(testdf$envAware == testdf$predictedlabelround45, na.rm=TRUE)
testdf$predictedlabelround55 = 0
testdf$predictedlabelround55[testdf$predictedlabel >=0.55]<-1
sum(testdf$envA?are == testdf$predictedlabelround55, na.rm=TRUE)

write.dta(testdf,file="C:/Users/leore/OneDrive/Documents/testdf.dta")
#use random forest fitted on danemark data to predict france ones
datafr <- subset(data2, q1_6==1)
datafr$predictedlabel <- predict(RF1,?newdata=datafr)
#use fr as test sample
datafr$envAware=0
datafr$envAware[datafr$qa1_3==1|datafr$qa1_3==2 & datafr$qa3_1==1|datafr$qa3_1==2 &
                    datafr$qa3_2==1|datafr$qa3_2==2 & datafr$qa10_2==1|datafr$qa10_2==2 &
                    dataf?$qa18==1|datafr$qa18==2|datafr$qa18==3|datafr$qa18==4] <- 1
datafr$predictedlabelmajvot = 0
datafr$prediclabelmajvot[datafr$predictedlabel <0.5]<-0
datafr$prediclabelmajvot[datafr$predictedlabel >=0.5]<-1
sum(datafr$envAware == datafr$predictedlabelmajvot,?na.rm=TRUE)

#try on the whole sample
#prepare data
data$envAware=0
data$envAware[data$qa1_3==1|data$qa1_3==2 & data$qa3_1==1|data$qa3_1==2 &
                    data$qa3_2==1|data$qa3_2==2 & data$qa10_2==1|data$qa10_2==2 &
                    data$qa18==1?data$qa18==2|data$qa18==3|data$qa18==4] <- 1
data2 <- subset(data, select=c("sd05","d15a","d7","d8","d9a","d10","d25","sd14","sd15","d40a","d40b","d40c","d60","d62t","sd16",
                                      "p3","p5","d15b","envAware","uniqid"))
data3?<- data2 %>% drop_na()
data4 <- subset(data, select=uniqid:q1_41)
data5 <- merge(data2, data4, by.x = 'uniqid', by.y='uniqid')
#make this example reproducible
set.seed(1234564)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE? FALSE), nrow(data5), replace=TRUE, prob=c(0.7,0.3))
traindfw  <- data5[sample, ]
testdfw   <- data5[!sample, ]
#Growing a RF of ... trees, considering one regressor at a time (test sample)
RF1whole=randomForest(envAware ~ ., data=traindfw[complete.cases(t?aindfw),],mtry=1,importance=TRUE)
importance(RF1whole,type=1)
varImpPlot(RF1whole,type=1)
#test model accuracy for predictions
testdfw$predictedlabel <- predict(RF1whole, newdata=testdfw)
testdfw$predictedlabelround = 0
testdfw$prediclabelround[testdfw$pre?ictedlabel <0.5]<-0
testdfw$prediclabelround[testdfw$predictedlabel >=0.5]<-1
sum(testdfw$envAware == testdfw$predictedlabelround, na.rm=TRUE)
testdfw$predictedlabelround2 = 0
testdfw$prediclabelround2[testdfw$predictedlabel <0.2]<-0
testdfw$prediclabelrou?d2[testdfw$predictedlabel >=0.2]<-1
sum(testdfw$envAware == testdfw$predictedlabelround2, na.rm=TRUE)
#grow a full tree
tree_whole=tree(envAware ~ . -tnscntry -country-isocntry-d11r1-d11r2, data=traindfw[complete.cases(traindfw),])
plot(tree_whole)
text(tr?e_whole)