## ----setup, include=FALSE-------
knitr::opts_chunk$set(echo = FALSE , comment=NA)
#########################
##Cleaning Data and EDA##
#########################
#Install libraries if not installed, else load them-----------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("ggplot2", "ISLR", "DataExplorer", "RColorBrewer", "dplyr", "data.table","rpart","randomForest","xgboost","DescTools","Hmisc","ggcorrplot","MASS","tidyverse","caret","precrec","GGally","corrgram","broom","purrr","glmnet","pls","leaps","ROCR","precrec","kknn","rpart","rpart.plot","gbm", "nnet","caret","neuralnet","radiant","radiant.model")
ipak(packages)

options(scipen=999)

#Set seed and working directory-------------------------------------------------
set.seed(100)
setwd("~/Documents/Intro to ML")

#, base_family="Avenir" -- keeping default font due to latex rendering
theme_custom <- function (text_size=12) { 
    theme_bw(base_size=text_size) %+replace% 
        theme(
            panel.background  = element_blank(),
            plot.background = element_rect(fill="gray96", colour=NA), 
            legend.background = element_rect(fill="transparent", colour=NA),
            legend.key = element_rect(fill="transparent", colour=NA)
        )
}



## -------------------------------
#Attaching data for easier operations
suppressMessages(suppressWarnings(attach(Boston)))

cat("Number of rows and columns are : ",nrow(Boston)," and ",ncol(Boston)," respectively.\n")


## -------------------------------
cat("\n")
#Pair plot
ggpairs(Boston[,c("crim","zn","indus","nox","ptratio","rad","tax","lstat","medv")],progress = F ,aes(alpha=0.6))+theme_custom(text_size =10)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## -------------------------------
#Generate correlations with crim rate
round(cor(Boston),2)[1,]



cat("\nVisualize correlation\n\n")
#Visualize correlation relationships
corrgram::corrgram(cor(Boston),upper.panel = panel.shade,lower.panel = NULL)



## -------------------------------
plotFunction <- function(varname1){            
  plot(suppressMessages(suppressWarnings(ggplot(Boston, aes_string(x=varname1, y="crim")) +ggtitle(paste0("Scatterplot of crim with ",varname1))+ geom_point(color="salmon",size=2.1,alpha=0.35)+theme_custom() + 
geom_smooth(method = "lm",color="blue",linetype=4, size = 0.65))))
}    

list_plots <- sapply(names(Boston)[!names(Boston) %in% c("crim")], plotFunction)



## -------------------------------
cat("Range of crime rate in Boston suburbs : ", range(crim)," \n\n")

cat("\nRange of tax rate in Boston suburbs : ", range(tax)," \n\n")

cat("\nRange of ptratio in Boston suburbs : ", range(ptratio)," \n\n")



## -------------------------------
#Crim variable outliers
ggplot(Boston, aes(x=crim)) + geom_boxplot()+theme_custom()

#Tax rate variable outliers
ggplot(Boston, aes(x=tax)) + geom_boxplot()+theme_custom()

#Ptratio variable outliers
ggplot(Boston, aes(x=ptratio)) + geom_boxplot()+theme_custom()



## -------------------------------
cat("A total of",table(chas)[[2]]," suburbs (roughly ", round(100*prop.table(table(chas))[[2]],2) , "%) are bounded by the Charles river.")
#prop.table(table(chas))


## -------------------------------
cat("Median pupil-teacher ratio in the dataset is :", median(ptratio),". \n")



## -------------------------------

#Median values by variable
Med_df = data.frame(sapply(Boston,function(x) round(median(x),2)))
names(Med_df) <- "ColMedian"
Med_df$Range_l = sapply(Boston,function(x)  round(range(x)[1],2))
Med_df$Range_u = sapply(Boston,function(x)  round(range(x)[2],2))
Med_df


## -------------------------------

#Suburbs with minimum owner-occupied units 
Boston[medv == min(medv),]



## -------------------------------
cat("#Suburbs averaging more than 7 dwellings : ",length(which(rm > 7)),". \n")
cat("#Suburbs averaging more than 8 dwellings : ",length(which(rm > 8)),". \n")

#Characteristics of suburbs with more than 8 dwellings on average
Sub_8 = data.frame(sapply(Boston[rm > 8,] ,function(x) round(mean(x),2)))
names(Sub_8) = "ColMean"
Sub_8


## -------------------------------
#Running regression by variable
m1 = broom::tidy(summary(lm(crim~zn, data = Boston)),conf.int=T)
m2 = broom::tidy(summary(lm(crim~indus, data = Boston)),conf.int=T)
m3 = broom::tidy(summary(lm(crim~chas, data = Boston)),conf.int=T)
m4 = broom::tidy(summary(lm(crim~nox, data = Boston)),conf.int=T)
m5 = broom::tidy(summary(lm(crim~rm, data = Boston)),conf.int=T)
m6 = broom::tidy(summary(lm(crim~age, data = Boston)),conf.int=T)
m7 = broom::tidy(summary(lm(crim~dis, data = Boston)),conf.int=T)
m8 = broom::tidy(summary(lm(crim~rad, data = Boston)),conf.int=T)
m9 = broom::tidy(summary(lm(crim~tax, data = Boston)),conf.int=T)
m10 = broom::tidy(summary(lm(crim~ptratio, data = Boston)),conf.int=T)
m11 = broom::tidy(summary(lm(crim~black, data = Boston)),conf.int=T)
m12 = broom::tidy(summary(lm(crim~lstat, data = Boston)),conf.int=T)
m13 = broom::tidy(summary(lm(crim~medv, data = Boston)),conf.int=T)

coef_df <- as.data.frame(rbind(m1[2,],m2[2,],m3[2,],m4[2,],m5[2,],m6[2,],m7[2,],m8[2,],m9[2,],m10[2,],m11[2,],m12[2,],m13[2,]))
coef_df[!names(coef_df) %in% c("term")] <- sapply(coef_df[!names(coef_df) %in% c("term")] , function(x) round(x,2))
coef_df


## ----results='hide'-------------
#ggplot(data = Boston,aes(x = lstat, y = crim)) + scale_x_continuous(name="lstat", limits=range(lstat)) +
#scale_y_continuous(name="crim", limits=range(crim)) +
#scale_linetype(name="s") + geom_point(color="salmon",size=2.1,alpha=0.35)+theme_custom() + 
#geom_smooth(method = "lm",color="blue",linetype=4, size = 0.65)
plot(lm(crim~zn)$fitted.values , crim)
plot(lm(crim~indus)$fitted.values , crim)
plot(lm(crim~nox)$fitted.values , crim)
plot(lm(crim~rm)$fitted.values , crim)
plot(lm(crim~age)$fitted.values , crim)
plot(lm(crim~dis)$fitted.values , crim)
plot(lm(crim~rad)$fitted.values , crim)
plot(lm(crim~tax)$fitted.values , crim)
plot(lm(crim~ptratio)$fitted.values , crim)
plot(lm(crim~black)$fitted.values , crim)
plot(lm(crim~lstat)$fitted.values , crim)
plot(lm(crim~medv)$fitted.values , crim)




## -------------------------------
lm.model <- lm(crim ~ ., data = Boston)
summary(lm.model)



## -------------------------------
both_coef <- data.frame(Simple = round(coef_df$estimate,2), Multiple = round(lm.model$coefficients[2:length(lm.model$coefficients)],2)  )
both_coef


## -------------------------------
ggplot(data = both_coef,aes(x = Simple, y = Multiple)) + geom_point(color="red",fill="black",size=2.5,alpha=0.5)+theme_custom()


## ----warning=FALSE--------------
#To check for non-linearity -- Approach 1 -- Plot
suppressMessages(ggplot(data = Boston,aes(x = zn, y = crim)) + geom_point(color="salmon",fill="black",size=1.5,alpha=0.65)+theme_custom()+ylim(c(0,10))+
  geom_smooth(method = "lm",se=F,color="black",size=0.8)+geom_smooth(method = "loess",se=F,color="blue",linetype=2,size=0.65)+geom_smooth(method = "gam",se=F,color="darkgreen",linetype=4,size=0.65)) #Only crim values less than 10 for better visual




## -------------------------------
#Approach 2 -- Pattern in residuals
mod_summ <- summary(lm(crim~zn))
plot(zn, mod_summ$residuals  , ylab="Residuals", col="salmon")



## -------------------------------
#Approach 3 -- Running polynomial regression
#Running regression by variable
p1 = broom::tidy(summary(lm(crim~poly(zn,3))),conf.int=T)
p2 = broom::tidy(summary(lm(crim~poly(indus,3))),conf.int=T)
p3 = broom::tidy(summary(lm(crim~poly(chas,1))),conf.int=T)
p4 = broom::tidy(summary(lm(crim~poly(nox,3))),conf.int=T)
p5 = broom::tidy(summary(lm(crim~poly(rm,3))),conf.int=T)
p6 = broom::tidy(summary(lm(crim~poly(age,3))),conf.int=T)
p7 = broom::tidy(summary(lm(crim~poly(dis,3))),conf.int=T)
p8 = broom::tidy(summary(lm(crim~poly(rad,3))),conf.int=T)
p9 = broom::tidy(summary(lm(crim~poly(tax,3))),conf.int=T)
p10 = broom::tidy(summary(lm(crim~poly(ptratio,3))),conf.int=T)
p11 = broom::tidy(summary(lm(crim~poly(black,3))),conf.int=T)
p12 = broom::tidy(summary(lm(crim~poly(lstat,3))),conf.int=T)
p13 = broom::tidy(summary(lm(crim~poly(medv,3))),conf.int=T)

coef_poly_df <- as.data.frame(rbind(p1[2:4,],p2[2:4,],p3[2,],p4[2:4,],p5[2:4,],p6[2:4,],p7[2:4,],p8[2:4,],p9[2:4,],p10[2:4,],p11[2:4,],p12[2:4,],p13[2:4,]))

coef_poly_df[!names(coef_poly_df) %in% c("term")] <- sapply(coef_poly_df[!names(coef_poly_df) %in% c("term")] , function(x) round(x,2))

coef_poly_df


## -------------------------------
#Load data
college <- ISLR::College

#sapply(train,class)
#Since we have only one factor, we coerce it to numeric
college$Private <- as.numeric(as.factor(college$Private)) - 1

#One represents Private = 'Yes' now


#Reproducibility
set.seed(100)

# 75% of the sample size
smp_size <- floor(0.75 * nrow(college))
train_ind <- sample(seq_len(nrow(college)), size = smp_size)

train <- college[train_ind, ]
test <- college[-train_ind, ]

#Dimensions of train and test
cat("Number of rows and columns in train are :",nrow(train)," and ",ncol(train)," respectively.\n")
cat("Number of rows and columns in test are :",nrow(test)," and ",ncol(test)," respectively.\n")

#Number of colleges overlap in train and test
#length(which(rownames(train) %in% rownames(test)))

#Scale original data
#college_scaled = as.data.frame(sapply(college,scale))

#Create copies of train test with scaled data
#train_scaled <- college_scaled[train_ind, ]
#test_scaled <- college_scaled[-train_ind, ]



## -------------------------------

set.seed(101)

linearModel <- lm(Apps~. , data = college)
summary(linearModel)


predApps <- predict(linearModel , test)

#Evaluate deviation from actual y
errorApps = test$Apps - predApps
RMSE_error = sqrt(mean((test$Apps - predApps)^2))

cat("Test Root mean squared error is : ",RMSE_error , "\n")

cat("Train RMSE is :",sqrt(mean((train$Apps - predict(linearModel , train))^2))," \n")



## -------------------------------

#Creating independent and dependent variables in required classes
x.train <- as.matrix(train[names(train)[!names(train) %in% c("Apps")]])
x.test <- as.matrix(test[names(test)[!names(test) %in% c("Apps")]])

y.train <- train$Apps
y.test <- test$Apps

#Using unscaled data since glmnet implementation standardizes data internally as stated on https://www.statology.org/ridge-regression-in-r/

set.seed(100)

cv_Ridge <- cv.glmnet(x = x.train, y = y.train , family = "gaussian" , alpha=0)

plot(cv_Ridge)



## -------------------------------
set.seed(100)

cv_Ridge_1se <- glmnet(x = x.train, y = y.train , family = "gaussian", lambda = cv_Ridge$lambda.1se , alpha = 0)


predApps_Ridge <- predict(cv_Ridge_1se , x.test)

#Evaluate deviation from actual y
errorApps_Ridge = test$Apps - predApps_Ridge
RMSE_error_Ridge = sqrt(mean((errorApps_Ridge)^2))

cat("Test Root mean squared error for Ridge regularized model is : ",RMSE_error_Ridge , "\n")



## -------------------------------
coef_df_Ridge = data.frame(VarName = rownames(as.matrix(coef(cv_Ridge_1se)))  , as.matrix(coef(cv_Ridge_1se)) )
names(coef_df_Ridge)[2] <- "Beta"
coef_df_Ridge$Beta <- round(coef_df_Ridge$Beta,2)
coef_df_Ridge[coef_df_Ridge$Beta != 0,]


## -------------------------------
set.seed(100)

cv_Lasso <- cv.glmnet(x = x.train, y = y.train , family = "gaussian" , alpha=1)

plot(cv_Lasso)


## -------------------------------

cv_Lasso_1se <- glmnet(x = x.train, y = y.train , family = "gaussian", lambda = cv_Lasso$lambda.1se , alpha = 1)


predApps_Lasso <- predict(cv_Lasso_1se , x.test)

#Evaluate deviation from actual y
errorApps_Lasso = test$Apps - predApps_Lasso
RMSE_error_Lasso = sqrt(mean((errorApps_Lasso)^2))

cat("Test Root mean squared error for Lasso regularized model is : ",RMSE_error_Lasso , "\n")

coef_df_Lasso = data.frame(VarName = rownames(as.matrix(coef(cv_Lasso_1se)))  , as.matrix(coef(cv_Lasso_1se)) )

names(coef_df_Lasso)[2] <- "Beta"
coef_df_Lasso$Beta <- round(coef_df_Lasso$Beta,2)

coef_df_Lasso[coef_df_Lasso$Beta != 0,]


## -------------------------------
# PCR model by cross validation
set.seed(100)

pcr.fit = pls::pcr(formula = Apps~. , data=train, scale=TRUE ,validation ="CV")

summary(pcr.fit)

validationplot(pcr.fit ,val.type="MSEP")



## -------------------------------
set.seed(100)

pcr.fit2 = pls::pcr(formula = Apps~. , data=train, scale=TRUE ,ncomp= 9 )

pls.pred = predict (pcr.fit2 , test)

#Evaluate deviation from actual y
errorApps_PCR = test$Apps - pls.pred
RMSE_error_PCR = sqrt(mean((errorApps_PCR)^2))

cat("Test Root mean squared error for PCR  model is : ", RMSE_error_PCR , "\n")



## -------------------------------
set.seed(100)

pls.fit2 = plsr(formula = Apps~. , data=train, scale=TRUE ,validation ="CV")

validationplot(pls.fit2 ,val.type="MSEP")

pls.pred2 = predict(pls.fit2 ,test ,ncomp =5)

#Evaluate deviation from actual y
errorApps_PLS = test$Apps - pls.pred2
RMSE_error_PLS = sqrt(mean((errorApps_PLS)^2))

cat("Test Root mean squared error for PLS model is : ", RMSE_error_PLS , "\n")


## -------------------------------

dt <- data.table(Model = c("LR","Ridge","Lasso","PCR","PLS") , RMSE = c(RMSE_error,RMSE_error_Ridge,RMSE_error_Lasso,RMSE_error_PCR, RMSE_error_PLS))
dt$RMSE <- round(dt$RMSE,2)

dt



## -------------------------------
#Reproducibility
set.seed(100)

# 75% of the sample size
smp_size <- floor(0.75 * nrow(Boston))
train_ind <- sample(seq_len(nrow(Boston)), size = smp_size)

train <- Boston[train_ind, ]
test <- Boston[-train_ind, ]

#Creating independent and dependent variables in required classes
x.train <- as.matrix(train[names(train)[!names(train) %in% c("crim")]])
x.test <- as.matrix(test[names(test)[!names(test) %in% c("crim")]])

y.train <- train$crim
y.test <- test$crim

#Creating independent and dependent variables in required classes
x.train <- as.matrix(train[names(train)[!names(train) %in% c("crim")]])
x.test <- as.matrix(test[names(test)[!names(test) %in% c("crim")]])

y.train <- train$crim
y.test <- test$crim

#Using unscaled data since glmnet implementation standardizes data internally as stated on https://www.statology.org/ridge-regression-in-r/

set.seed(100)

cv_Ridge <- cv.glmnet(x = x.train, y = y.train , family = "gaussian" , alpha=0)

plot(cv_Ridge)

set.seed(100)

cv_Ridge_1se <- glmnet(x = x.train, y = y.train , family = "gaussian", lambda = cv_Ridge$lambda.1se, alpha = 0)

coef_df_Ridge = data.frame(VarName = rownames(as.matrix(coef(cv_Ridge_1se)))  , as.matrix(coef(cv_Ridge_1se)) )
names(coef_df_Ridge)[2] <- "Beta"
coef_df_Ridge$Beta <- round(coef_df_Ridge$Beta,2)
coef_df_Ridge


## -------------------------------

predcrim_Ridge <- predict(cv_Ridge_1se , x.test)

#Evaluate deviation from actual y
errorcrim_Ridge = test$crim - predcrim_Ridge
RMSE_error_Ridge = sqrt(mean((errorcrim_Ridge)^2))

cat("Test Root mean squared error for Ridge regularized model is : ",RMSE_error_Ridge , "\n")


## -------------------------------
set.seed(100)

cv_Lasso <- cv.glmnet(x = x.train, y = y.train , family = "gaussian" , alpha=1)

plot(cv_Lasso)

cv_Lasso_1se <- glmnet(x = x.train, y = y.train , family = "gaussian", lambda = cv_Lasso$lambda.1se , alpha = 1)

coef_df_Lasso = data.frame(VarName = rownames(as.matrix(coef(cv_Lasso_1se)))  , as.matrix(coef(cv_Lasso_1se)) )
names(coef_df_Lasso)[2] <- "Beta"
coef_df_Lasso$Beta <- round(coef_df_Lasso$Beta,2)

coef_df_Lasso[coef_df_Lasso$Beta != 0,]



## -------------------------------
cv_Lasso_1se <- glmnet(x = x.train, y = y.train , family = "gaussian", lambda = 0.25 , alpha = 1)

coef_df_Lasso = data.frame(VarName = rownames(as.matrix(coef(cv_Lasso_1se)))  , as.matrix(coef(cv_Lasso_1se)) )
names(coef_df_Lasso)[2] <- "Beta"
coef_df_Lasso$Beta <- round(coef_df_Lasso$Beta,2)

coef_df_Lasso[coef_df_Lasso$Beta != 0,]



## -------------------------------

predcrim_Lasso <- predict(cv_Lasso_1se , x.test)

#Evaluate deviation from actual y
errorcrim_Lasso = test$crim - predcrim_Lasso
RMSE_error_Lasso = sqrt(mean((errorcrim_Lasso)^2))

cat("Test Root mean squared error for Lasso regularized model is : ",RMSE_error_Lasso , "\n")



## -------------------------------

# PCR model by cross validation
set.seed(100)

pcr.fit = pls::pcr(formula = crim~. , data=train, scale=TRUE ,validation ="CV")

summary(pcr.fit)

validationplot(pcr.fit ,val.type="MSEP")

set.seed(100)

pcr.fit2 = pls::pcr(formula = crim~. , data=train, scale=TRUE ,ncomp= 8 )

pls.pred = predict (pcr.fit2 , test)

#Evaluate deviation from actual y
errorcrim_PCR = test$crim - pls.pred
RMSE_error_PCR = sqrt(mean((errorcrim_PCR)^2))

cat("Test Root mean squared error for PCR  model is : ", RMSE_error_PCR , "\n")


## -------------------------------
#using max number of variables as 15 to provide some regularization 
regFitModel <- regsubsets(crim~. , data = Boston , nvmax=15, method ="forward")

plot(regFitModel)$adjr2

#Summary of regFit provides best subset for the same
summary(regFitModel)



## -------------------------------
#Adjusted R-sq for model subset -- Model removing chas, rm, age and tax
summary(regFitModel)$adjr2[ (which(summary(regFitModel)$adjr2 == max(summary(regFitModel)$adjr2)))]

set.seed(100)

reglinearModel <- lm(crim~zn+indus+nox+dis+rad+ptratio+black+lstat+medv , data = Boston)
summary(reglinearModel)

predcrim <- predict(reglinearModel , test)

#Evaluate deviation from actual y
errorcrim = test$crim - predcrim
RMSE_error = sqrt(mean((test$crim - predcrim)^2))

cat("Test Root mean squared error is : ",RMSE_error , "\n")

data.table(Model = c("Subsetting","Ridge","Lasso","PCR") , RMSE = c(RMSE_error,RMSE_error_Ridge,RMSE_error_Lasso,RMSE_error_PCR))


## -------------------------------

set.seed(100)

pls.fit2 = plsr(formula = crim~. , data=Boston, scale=TRUE ,validation ="CV")

validationplot(pls.fit2 ,val.type="MSEP")

pls.pred2 = predict(pls.fit2 ,test ,ncomp =5)

#Evaluate deviation from actual y
errorApps_PLS = test$crim - pls.pred2
RMSE_error_PLS = sqrt(mean((errorApps_PLS)^2))

cat("Test Root mean squared error for PLS model is : ", RMSE_error_PLS , "\n")


## -------------------------------
df <- ISLR::Weekly

#Descriptive stats and univariate plots
DescTools::Desc(df)



## -------------------------------

#Coerce Direction into numeric
df$Direction <- as.numeric(df$Direction) - 1
#Now 0 indicates down and 1 indicates Up

#Visualize correlation relationships
corrgram::corrgram(cor(df),upper.panel = panel.shade,lower.panel = NULL)



## -------------------------------

#Pair plots to understand scatterplot distributions
ggpairs(df,progress = F ,aes(alpha=0.6))+theme_custom(text_size =10)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## -------------------------------
set.seed(100)

logitModel = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=df, family=binomial)
summary(logitModel)



## -------------------------------

PredAll = predict(logitModel, newdata=df[,!colnames(df) == "Direction"], type="response")
confusionMatrix(as.factor(as.numeric(PredAll>=0.5)),reference = as.factor(df$Direction))

#Zero represents Down, 1 represents Up
logit_pred_obj <- ROCR::prediction(PredAll,df$Direction)
auc_logit_all <- ROCR::performance(logit_pred_obj,"auc")

#AUC of the model
cat("Logit Model regressed using all variables gives AUC of ",auc_logit_all@y.values[[1]][1])

#AUC ROC and PR Curve
precrec_obj_test <- evalmod(scores = PredAll, labels = df$Direction)
autoplot(precrec_obj_test)

logit_pr_all <- ROCR::performance(logit_pred_obj,"rec","prec")
pr_df= data.frame(logit_pr_all@x.values,logit_pr_all@y.values)
names(pr_df) <- c("Precision", "Recall")
pr_df$F1 = 2*pr_df$Precision * pr_df$Recall / (pr_df$Precision + pr_df$Recall)





## -------------------------------
#Create out train and test
train_df = df[(df$Year<2009),]

test_df = df[(!df$Year<2009),]

set.seed(100)

# Logistic regression on train
logitModel_Lag2 = glm(Direction ~ Lag2, data=train_df, family=binomial)

# Predictions on test
probLags2 = predict(logitModel_Lag2,test_df[,!colnames(test_df) == "Direction"], type="response")


## -------------------------------
confusionMatrix(as.factor(as.numeric(probLags2>=0.5)),reference =as.factor(test_df$Direction))

#Zero represents Down, 1 represents Up
logit_pred_obj2 <- ROCR::prediction(probLags2,test_df$Direction)
auc_logit_all2 <- ROCR::performance(logit_pred_obj2,"auc")

#AUC of the model
cat("Logit Model regressed using Lag2 gives AUC of ",auc_logit_all2@y.values[[1]][1])

#AUC ROC and PR Curve
precrec_obj_test2 <- evalmod(scores = probLags2, labels = test_df$Direction)
autoplot(precrec_obj_test2)

logit_pr_all <- ROCR::performance(logit_pred_obj2,"rec","prec")
pr_df= data.frame(logit_pr_all@x.values,logit_pr_all@y.values)
names(pr_df) <- c("Precision", "Recall")
pr_df$F1 = 2*pr_df$Precision * pr_df$Recall / (pr_df$Precision + pr_df$Recall)




## -------------------------------

# Predictions for K=1
set.seed(100)

knnModel = kknn(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train_df, test_df, k=1)

#Confusion matrix for the test set
confusionMatrix(as.factor(knnModel$fitted.values),reference = as.factor(test_df$Direction))

#AUC ROC and PR Curve
#precrec_obj_test3 <- evalmod(scores = knnModel$fitted.values, labels = test_df$Direction)
#autoplot(precrec_obj_test3)
#Zero represents Down, 1 represents Up




## ----warning=FALSE--------------
#Exploring stepwise models to see effect of addition of variables
set.seed(100)


step.model <- glm(Direction ~ ., data=train_df[!names(train_df) %in% c("Today")], family=binomial) %>% stepAIC(trace = T)
cols <- names(coef(step.model))
summary(step.model)

PredT = predict(step.model, newdata=test_df[!names(test_df) %in% c("Today")], type="response")
confusionMatrix(reference = as.factor(test_df$Direction), as.factor(as.numeric(PredT > 0.5)))




## -------------------------------

#Exploring interaction effects
set.seed(100)

interactionModel <- glm(Direction ~ Lag1 + Lag2 + Lag1:Lag2, data=train_df[!names(train_df) %in% c("Today")], family=binomial) 
cols <- names(coef(interactionModel))
summary(interactionModel)

PredT2 = predict(interactionModel, newdata=test_df[!names(test_df) %in% c("Today")], type="response")
confusionMatrix(reference = as.factor(test_df$Direction), as.factor(as.numeric(PredT2 > 0.5)))




## -------------------------------

#Model non-linearity of variables
set.seed(100)

nonLinModel <- glm(Direction ~ Lag1 + Lag2 + I(Lag1^2) + I(Lag2^2), data=train_df[!names(train_df) %in% c("Today")], family=binomial) 
cols <- names(coef(nonLinModel))
summary(nonLinModel)

PredT3 = predict(nonLinModel, newdata=test_df[!names(test_df) %in% c("Today")], type="response")
confusionMatrix(reference = as.factor(test_df$Direction), as.factor(as.numeric(PredT3 > 0.5)))




## -------------------------------
#KNN with k = 5, 10, 20, 50, 100

set.seed(100)

cat("\nModel performance with k = 5\n")
knnModel5 = kknn(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train_df, test_df, k=5)
#Confusion matrix for the test set
confusionMatrix(as.factor(as.numeric(knnModel5$fitted.values > 0.5)),reference = as.factor(test_df$Direction))

set.seed(100)
cat("\nModel performance with k = 10\n")
knnModel10 = kknn(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train_df, test_df, k=10)
#Confusion matrix for the test set
confusionMatrix(as.factor(as.numeric(knnModel10$fitted.values > 0.5)),reference = as.factor(test_df$Direction))

set.seed(100)
cat("\nModel performance with k = 20\n")
knnModel20 = kknn(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train_df, test_df, k=20)
#Confusion matrix for the test set
confusionMatrix(as.factor(as.numeric(knnModel20$fitted.values > 0.5)),reference = as.factor(test_df$Direction))

set.seed(100)
cat("\nModel performance with k = 50\n")
knnModel50 = kknn(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train_df, test_df, k=50)
#Confusion matrix for the test set
confusionMatrix(as.factor(as.numeric(knnModel50$fitted.values > 0.5)),reference = as.factor(test_df$Direction))


set.seed(100)
cat("\nModel performance with k = 100\n")
knnModel100 = kknn(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train_df, test_df, k=100)
#Confusion matrix for the test set
confusionMatrix(as.factor(as.numeric(knnModel100$fitted.values > 0.5)),reference = as.factor(test_df$Direction))





## -------------------------------
cs <- ISLR::Carseats

#Reproducibility
set.seed(100)

# 75% of the sample size
smp_size <- floor(0.75 * nrow(cs))
train_ind <- sample(seq_len(nrow(cs)), size = smp_size)

train <- cs[train_ind, ]
test <- cs[-train_ind, ]

cat("Number of rows of train and test are :" , nrow(train) , " and " , nrow(test)," respectively.\n" )



## -------------------------------
tree1 = rpart(Sales ~ . , data = train , method = "anova")
#tree1

rpart.plot::rpart.plot(tree1, box.palette = "Greens")


## -------------------------------

predTree <- predict(tree1 , test)
cat("Mean Squared Error on the test set is :", mean((predTree - test$Sales)^2))



## -------------------------------

printcp(tree1)
plotcp(tree1)



## -------------------------------
#using cp = 0.038
tree2 = rpart(Sales ~ . , data = train , method = "anova" , cp = 0.02)

rpart.plot::rpart.plot(tree2, box.palette = "Greens")


predTree2 <- predict(tree2 , test)
cat("Mean Squared Error of pruned tree on the test set is :", mean((predTree2 - test$Sales)^2))



## -------------------------------

# Bagging => RF with m = ncol
set.seed(100)

bagged10 = randomForest(Sales~., data=train, mtry=10, importance=T)

#importance Matrix
round(importance(bagged10),2)
varImpPlot(bagged10)

baggedPred10 = predict(bagged10,newdata = test)
cat("Mean Squared Error of RF with m = 10 on the test set is :",mean((baggedPred10-test$Sales)^2),"\n")




## -------------------------------

# RandomForest (m = 2)
set.seed(100)
cat("Randomforest with m = 2 \n")
bagged2 = randomForest(Sales~., data=train, mtry=2, importance=T)

#importance Matrix
round(importance(bagged2),2)
varImpPlot(bagged2)

baggedPred2 = predict(bagged2,newdata = test)
cat("Mean Squared Error of RF with m = 2 on the test set is :",mean((baggedPred2-test$Sales)^2),"\n")



cat("Randomforest with m = 4 \n")
# RandomForest (m = 4)
set.seed(100)

bagged4 = randomForest(Sales~., data=train, mtry=4, importance=T)

#importance Matrix
round(importance(bagged4))
varImpPlot(bagged4)

baggedPred4 = predict(bagged4,newdata = test)
cat("Mean Squared Error of RF with m = 4 on the test set is :",mean((baggedPred4-test$Sales)^2),"\n")


cat("Randomforest with m = 6 \n")

# RandomForest (m = 6)
set.seed(100)

bagged6 = randomForest(Sales~., data=train, mtry=6, importance=T)

#importance Matrix
round(importance(bagged6),2)
varImpPlot(bagged6)

baggedPred6 = predict(bagged6,newdata = test)
cat("Mean Squared Error of RF with m = 6 on the test set is :",mean((baggedPred6-test$Sales)^2),"\n")

cat("Randomforest with m = 8 \n")

# RandomForest (m = 8)
set.seed(100)

bagged = randomForest(Sales~., data=train, mtry=8, importance=T)

#importance Matrix
round(importance(bagged),2)
varImpPlot(bagged)

baggedPred = predict(bagged,newdata = test)
cat("Mean Squared Error of RF with m = 8 on the test set is :",mean((baggedPred-test$Sales)^2),"\n")



## -------------------------------
carv <- ISLR::Caravan

carv$Purchase <- as.numeric(carv$Purchase)-1
#Now No is represented by 0 and Yes by 1

carv.train <- carv[1:1000,]
carv.test <- carv[1001:nrow(carv),]

cat("Number of rows in train and test are : ",nrow(carv.train)," and ",nrow(carv.test)," respectively.\n")


## ----warning=FALSE--------------

set.seed(100)

#Boosting
boostingModel = gbm(Purchase~., data=carv.train,distribution = "bernoulli",n.trees = 1000, shrinkage = 0.01)
summary(boostingModel,cBars = 25,
  method = relative.influence, # also can use permutation.test.gbm
  las = 1)


## -------------------------------
# Predcited probalbilites on test
predCarv = predict(boostingModel,  carv.test[!names(carv.test) %in% c("Purchase")], n.trees = 1000,type="response")

# Confusion matrix
confusionMatrix(reference = as.factor(carv.test$Purchase), as.factor(as.numeric(predCarv >= 0.2)))




## ----warning=FALSE--------------
# Logit
set.seed(100)

logitModel = glm(Purchase~., data=carv.train,family = binomial)
# Predcited probalbilites on test
predCarv2 = predict(logitModel,  carv.test[!names(carv.test) %in% c("Purchase")],type="response")
# Confusion matrix
confusionMatrix(reference = as.factor(carv.test$Purchase), as.factor(as.numeric(predCarv2 >= 0.2)))




## ----warning=FALSE--------------
#Benchmarking with logistic regression
logitModelCarv = glm(Purchase~., data = carv.train, family = binomial)
logitProbs = predict(logitModelCarv, carv.test, type="response")

# Confusion matrix
confusionMatrix(reference = as.factor(carv.test$Purchase), as.factor(as.numeric(logitProbs >= 0.2)))




## -------------------------------
#Read in the data
beauty <- data.table::fread("BeautyData.csv")

#Quick summary stats
DescTools::Desc(beauty)



## -------------------------------
#Modelling beauty variable into course ratings
lmModel_beauty <- lm(data = beauty , formula = CourseEvals ~ BeautyScore)
summary(lmModel_beauty)


## -------------------------------
plot(lmModel_beauty)


## -------------------------------
#Modelling all variables into course ratings
lmModel_beautyall <- lm(data = beauty , formula = CourseEvals ~ .)
summary(lmModel_beautyall)




## -------------------------------
plot(lmModel_beautyall)


## -------------------------------
housing <- read.csv("MidCity.csv", header = T)

#Quick data summary
summary(housing)



## -------------------------------
#Multivariate analysis to model price with respect to other variables

#remove identifier column
housing$Home <- NULL

#Brick = 1 indicates Brick houses
housing$Brick <- as.factor(housing$Brick)

#Convert Neighborhood into factor
housing$Nbhd <- as.factor(housing$Nbhd)


set.seed(100)
linearModel_housing <- lm(Price ~ . , data = housing)
summary(linearModel_housing)



## -------------------------------
#To check for premium for both bricked and neighborhood 3 house, we explore the interaction effects of the two variables
set.seed(100)

linearModel_housing <- lm(Price ~ Offers + SqFt + Bedrooms + Bathrooms + Brick*Nbhd , data = housing)
summary(linearModel_housing)




## -------------------------------

housing$NewNbhd <- ifelse(housing$Nbhd == 3,1,0)

linearModel_housing_new <- lm(Price ~  Offers + SqFt + Bedrooms + Bathrooms + Brick*NewNbhd , data = housing)
summary(linearModel_housing_new)


## -------------------------------

#Read in Boston data
data <- Boston

#Reproducibility
set.seed(100)

scaled_df <- as.data.frame(scale(data, center = TRUE, scale = TRUE))
CV_RMSE <- data.table(Iteration=rep(0,225),Size=rep(0,225),Decay=rep(0,225),Test_RMSE=rep(0,225))

for(i in c(1:75)){
  rmse_vec <- c()
  sz_vec <- c()
  dc_vec <- c()
  temp_df <- data.table(Iteration=rep(0,20),Size=rep(0,20),Decay=rep(0,20),Test_RMSE=rep(0,20))
  train_ind <- sample(seq_len(nrow(scaled_df)), size = 0.75)
  train <- scaled_df[train_ind, ]
  test <- scaled_df[-train_ind, ]
  for(sz in seq(1,8,2)){
    for(dc in c(0,0.1,0.01,0.001,0.0001)){
      
      #For loop CV implementation -- using linout for linear output units -- removing trace = F to reduce clutter
      nnetModel = nnet(crim~.,scaled_df,size=sz,decay=dc,linout=T,trace=F)
      #Predict out full data
      predNN = predict(nnetModel,scaled_df)
      FullRMSE = sqrt(mean((crim-predNN)^2))
      #cat("Neural net for iteration ",i," and size as ",sz," and decay as ",dc,"\n RMSE = ",FullRMSE ,"  \n\n")
      #plot(predNN,scaled_df$crim)
      
      temp_df$Iteration <- i
      sz_vec <- c(sz_vec,sz)
      dc_vec <- c(dc_vec,dc)
      rmse_vec <- c(rmse_vec,FullRMSE)
      
    }
  }
  temp_df$Iteration <- i
  temp_df$Size <- sz_vec
  temp_df$Decay <- dc_vec
  temp_df$Test_RMSE <- rmse_vec
  CV_RMSE <- as.data.table(rbind(CV_RMSE,temp_df))
}

CV_RMSE <- CV_RMSE[CV_RMSE$Iteration != 0]



## ----warning=FALSE--------------
#Best decay and size parameters
cat("Best decay and size parameters with minimum TEST RMSE : ")
CV_RMSE[CV_RMSE$Test_RMSE == min(CV_RMSE$Test_RMSE)]

ggplot(CV_RMSE,aes(x = log(Decay) , y = Test_RMSE)) + geom_point(color="salmon",alpha=0.65) + geom_smooth(method = "lm",se=F, size=0.2) + facet_wrap(facets = vars(Size))+theme_custom()


