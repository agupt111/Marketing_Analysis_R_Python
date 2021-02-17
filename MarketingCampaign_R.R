# Marketing Campaign Case study and we need to analyse whether the customer will respond or not.

#clear environment and workspace
rm(list = ls(all = TRUE))

#Load the required necessary libaries
install.packages(c('ggplot2', 'DMwR', "corrgram", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
                   "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees'))


#set the working directory
setwd('D:/Data Science-edwisor/Case Study/Makrting_Campaign_R_CS')
getwd()

#load the data
marketing_train = read.csv('marketing_train.csv', header = T, na.strings = c("", " ", "NA"))
str(marketing_train)

#univariate analysis and variable consolidation
marketing_train$schooling[marketing_train$schooling %in% c('illiterate')] = "unknown"
marketing_train$schooling[marketing_train$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_train$default[marketing_train$default %in% "yes"] = "unknown"
marketing_train$default= as.factor(as.character(marketing_train$default))
marketing_train$marital[marketing_train$marital %in% "unknown"] = "married"
marketing_train$marital = as.factor(as.character(marketing_train$marital))
marketing_train$month[marketing_train$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_train$month[marketing_train$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_train$month = as.factor(as.character(marketing_train$month))
marketing_train$loan[marketing_train$loan %in% "unknown"] = "no"
marketing_train$loan = as.factor(as.character(marketing_train$loan))
marketing_train$schooling = as.factor(as.character(marketing_train$schooling))
marketing_train$profession[marketing_train$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_train$profession[marketing_train$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_train$profession = as.factor(as.character(marketing_train$profession))

#Exploratory data analysis
#Missing Value Analysis
missing_val = data.frame(apply(marketing_train, 2, function(x){sum(is.na(x))}))
missing_val$columns = row.names(missing_val)
names(missing_val)[1] = 'Missing_Percentage'
missing_val$Missing_Percentage = (missing_val$Missing_Percentage / nrow(marketing_train))*100
missing_val = missing_val[order(-missing_val$Missing_Percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[, c(2,1)]
write.csv(missing_val, 'Missing_val.csv', row.names = FALSE)

#Handle missing values 
#there are 3 ways where we can handle missing value ie mean , median, mode and KNN imputation
#Among these 3's which gave the best results we can fix that method

#Mean Method - for continous variable
#marketing_train$custAge[is.na(marketing_train$custAge)] = mean(marketing_train$custAge, na.rm = T)

#Median method
marketing_train$custAge[is.na(marketing_train$custAge)] = median(marketing_train$custAge, na.rm = T)

#KNN Imputation method 
#need to convert every variable into integer form
str(marketing_train)

#marketing_train$profession = as.numeric(marketing_train$profession)
#marketing_train$marital = as.numeric(marketing_train$marital)
marketing_train$schooling = as.numeric(marketing_train$schooling)
#marketing_train$default = as.numeric(marketing_train$default)
#marketing_train$loan = as.numeric(marketing_train$loan)
#marketing_train$month = as.numeric(marketing_train$month)

#we can convert factor directly into numeric but for char - first change to factor and then change to numeric
#marketing_train$housing=as.numeric(as.factor(marketing_train$housing))
#marketing_train$contact=as.numeric(as.factor(marketing_train$contact))
marketing_train$day_of_week=as.numeric(as.factor(marketing_train$day_of_week))
#marketing_train$poutcome=as.numeric(as.factor(marketing_train$poutcome))
#marketing_train$responded=as.numeric(as.factor(marketing_train$responded))

marketing_train$schooling[is.na(marketing_train$schooling)] = median(marketing_train$schooling, na.rm = T)
marketing_train$day_of_week[is.na(marketing_train$day_of_week)] = median(marketing_train$day_of_week, na.rm = T)

#Now Apply Knn imputation in this dataset and KNN is a slow process
require(DMwR)
#marketing_train = knnImputation(marketing_train$schooling, k=3)
#str(marketing_train)

#Data manipulation and convert string categories into factor numeric
for(i in 1:ncol(marketing_train)){
  if(class(marketing_train[,i]) == 'factor'){
    marketing_train[,i] = factor(marketing_train[,i], labels = (1:length(levels(factor(marketing_train[,i])))))
  }
}

str(marketing_train)

#Box plot Analysis and outliers handle
numeric_index = sapply(marketing_train, is.numeric)   #select numeric index
numeric_data = marketing_train[,numeric_index]

cnames = colnames(numeric_data)

library(ggplot2)
for (i in 1:length(cnames))
   {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "responded"), data = subset(marketing_train))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
              outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="responded")+
              ggtitle(paste("Box plot of responded for",cnames[i])))
   }

#plotting together
gridExtra::grid.arrange(gn1, gn5, gn2,ncol=3)
gridExtra::grid.arrange(gn6, gn7,ncol=2)
gridExtra::grid.arrange(gn8, gn9,ncol=2)

#Remove outliers using boxplot method
df = marketing_train
#marketing_train = df

val = marketing_train$previous[marketing_train$previous %in% boxplot.stats(marketing_train$previous)$out]

marketing_train = marketing_train[which(!marketing_train$previous %in% val),]

#loop to remove from all variables
 for(i in cnames){
   print(i)
   val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
   #print(length(val))
   marketing_train = marketing_train[which(!marketing_train[,i] %in% val),]
 }
final = marketing_train

#replace all outliers with NA and impute  -- 2nd way to handle outliers
#for(i in cnames){
#     val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
#     #print(length(val))
 #    marketing_train[,i][marketing_train[,i] %in% val] = NA
 #  }

#Handle NA with KNN imputatation again
#marketing_train = knnImputation(marketing_train, k=3)

mv = data.frame(apply(marketing_train, 2, function(x){sum(is.na(x))}))

str(marketing_train)


#feature selection
#Checking collinearity between variable and collinearity check
#Correlation plot is for continuos variable

cn = sapply(marketing_train, is.numeric)
cn_data= marketing_train[,cn]

require(corrgram)
install.packages('corrplot')
require(corrplot)
corrplot(cn_data, method = "circle")

corrgram(marketing_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

str(marketing_train)
marketing_train$responded = as.factor(marketing_train$responded)
str(marketing_train)

##chi square test of independence for categorical
factor_index = sapply(marketing_train, is.factor)
factor_index
factor_data = marketing_train[,factor_index]
factor_data


for(i in 1:4){
  print(names(factor_data)[i])
  print(chisq.test(factor_data$responded, factor_data[, i]))
}

# if p<0.05 then reject null hypothesis that two variable are dependent on each other else keep that variable

#Dimension Reduction
marketing_train = subset(marketing_train, select = -c(pdays, emp.var.rate, day_of_week, housing))

#Normality check
qqnorm(marketing_train$custAge)
hist(marketing_train$campaign)

#Normalization
cnames = c('custAge', "campaign","previous","cons.price.idx","cons.conf.idx","euribor3m","nr.employed",
           "pmonths","pastEmail")


for(i in cnames){
  print(i)
  marketing_train[,i] = (marketing_train[,i] - min(marketing_train[,i]))/ (max(marketing_train[,i] - min(marketing_train[,i])))
}

#Standardisation
for(i in cnames){
  print(i)
  marketing_train[,i] = (marketing_train[,i] - mean(marketing_train[,i]))/sd(marketing_train[,i])
}

marketing_train = subset(marketing_train, select = -c(previous, pastEmail, pmonths))

#Sampling
#1. Simple random sampling
data_sample = marketing_train[sample(nrow(marketing_train), 3000, replace = F ),]

#2. stratified sampling
#create stratas
#require(sampling)
#stratas = strata(marketing_train, c('profession'), size = c(100, 199, 10, 5), method = 'srswor')  # size is what proportion of data we want
#stratified_data = getdata(marketing_train, stratas)

#3. systematic sampling
#function to generate Kth index
#sys.sample= function(N,n){
#  k = ceiling(N/n)
#  r=sample(1:k, 1)
#  sys.samp = seq(r, r+k*(n-1), k)
#}

#lis = sys.sample(7414, 400) #select the repective rows

# #Create index variable in the data
# marketing_train$index = 1:7414
# 
# #Extract subset from whole data
# systematic_data = marketing_train[which(marketing_train$index %in% lis),]

##Our data in ready for model now
#Divide the data into train and test using startified sampling method
set.seed(1234)   # it fixes the starting number very time when sampling
require(sampling)
require(caret)   # for splitting data
train.index = createDataPartition(marketing_train$responded, p=0.8, list = FALSE)
train = marketing_train[train.index,]
test = marketing_train[-train.index,]

#first algo-
#Decision Tree for Classification
#Develop model on training data

marketing_train = subset(marketing_train, select = -c(poutcome))

require(C50)
C50_model = C5.0(responded ~., train, trials = 100, rules = TRUE)
C50_model

#Summary of DT model
summary(C50_model)


#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

C50_prediction = predict(C50_model, test[,-15], type = "class")   # or type = "prob" -- for probablity and class will show class 1, 2 etc.

#Evaluate the model
#confmatrix_C50 = table(test$responded, C50_prediction)
#confusionMatrix(confumatrix_C50)

#Visuaize decision tree
#plot(C50_model)


#False negative rate 
#FNR = FN / FN + TP

#Recall
#RC = TP / TP + FN


##Random Forest
require(randomForest)
RF_model = randomForest(responded~., train, importance = TRUE, ntree=500)
RF_model

summary(RF_model)

#extract rules from random forest
#transform rf object to an inTress format
require(inTrees)
treeList = RF2List(RF_model)
treeList

#Extract rules
exec = extractRules(treeList, train[,-15])   #R-executable conditions
exec

#Visualization some rules
exec[1:2,]

#Make it more readable format
readableRules = presentRules(exec, colnames(train))
readableRules
readableRules[1:2,]

#get the rules metrics
rulemetrix = getRuleMetric(exec, train[,-15], train$responded)  # get some rules
rulemetrix
rulemetrix[1:2, ]

#now lets predict the data with RF model
RF_prediction = predict(RF_model, test[,-15])
RF_prediction

#Evaluate the model
confumetrix = table(test$responded, RF_prediction)
confumetrix
confusionMatrix(confumetrix)

#False Negative rate
#FNR = FN/FN+TP 

#Accuracy = 90.62
#FNR = 61.9

#Logistic Regression - for categorical dependent variable
#logit_model = glm(responded~. , data = train, family = "binomial")

#Summary of model
#summary(logit_model)

#predict using logistic regression
#logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
#logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
#ConfMatrix_RF = table(test$responded, logit_Predictions)

#False Negative rate
#FNR = FN/FN+TP 

#Accuracy: 90.89
#FNR: 67.85

##KNN Implementation
#library(class)

#Predict test data
#KNN_Predictions = knn(train[, 1:14], test[, 1:14], train$responded, k = 7)

#Confusion matrix
#Conf_matrix = table(KNN_Predictions, test$responded)

#Accuracy
#sum(diag(Conf_matrix))/nrow(test)

#False Negative rate
#FNR = FN/FN+TP 

#Accuracy = 89.6
#FNR = 41.25

#naive Bayes
#library(e1071)

#Develop model
#NB_model = naiveBayes(responded ~ ., data = train)

#predict on test cases #raw
#NB_Predictions = predict(NB_model, test[,1:14], type = 'class')

#Look at confusion matrix
#Conf_matrix = table(observed = test[,17], predicted = NB_Predictions)
#confusionMatrix(Conf_matrix)

#Accuracy: 85.16
#FNR: 53.57

#statical way
#mean(NB_Predictions == test$responded)

#Kmeans implementation
#irisCluster <- kmeans(train[,1:16], 2, nstart = 20)

#table(irisCluster$cluster, train$responded)

##End of Case study of marketing campaign



