# reading data from the rawDataSet file
rawDataSet <- read.csv("yourpath\\rawDataSet.csv") 
rawDataSet <- rawDataSet[-1,-1]

# Names list for the features
names <- c("IP_Address","URL_Length","Shortining_Service",
           "having_At_Symbol","double_slash_redirecting",
           "Prefix_Suffix","having_Sub_Domain","SSLfinal_State",
           "Domain_registeration_length","Favicon","port",
           "HTTPS_token","Request_URL","URL_of_Anchor",
           "Links_in_tags","SFH","Submitting_to_email",
           "Abnormal_URL","Redirect","on_mouseover",
           "RightClick","popUpWidnow","Iframe","age_of_domain",
           "DNSRecord","web_traffic","Page_Rank","Google_Index",
           "Links_pointing_to_page","Statistical_report", "Result") 

# Add column names to dataset
names(rawDataSet) <- names
#preprocessing
str(rawDataSet)
# cleaning the data set to remove missing values
# Method: Imputing Mode Value for replacing with missing values in data set
#Define a function
modeImputation <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Get modes of each variable, by applying the Mode function to them
#Please use the input data set into lapply function as below:
myData.modes <- lapply(rawDataSet[1:32],modeImputation)
myData.modes  


#Function to replace the missing values with MODE value
cleanDataSet  <- rawDataSet
for (i in 1:32){
  
  cleanDataSet[is.na(rawDataSet[,i]), i] <- myData.modes[i]
}

# Check The Cleaned Data Set
str(cleanDataSet)


############# Plot for rawData set with NA values
heatmap.2(data.matrix(rawDataSet),na.color = "Black",dendrogram = 'none')
#superheat(rawDataSet,scale = TRUE,heat.na.col = "White")

################### plot for cleanData set 
heatmap.2(data.matrix(cleanDataSet),na.color = "Black",dendrogram = 'none')

####### Co-relation matrix
cmatrix <- cor(cleanDataSet)
#plot cmatrix 
heatmap(cmatrix)
boxplot(cmatrix)
hist(cmatrix,xlab = "names")

#######  Histogram of 2 variables
cleanHist <- hist(cor(cleanDataSet))
#######  Partition of dataset for testing and training
index <- createDataPartition(cleanDataSet$Result, p=0.75, list=FALSE)
#training dataset
cleanDataSet.training <- cleanDataSet[index,]
#testing dataset
cleanDataSet.test <- cleanDataSet[-index,]

prc_train_labels <- cleanDataSet[index, 31]
prc_test_labels <- cleanDataSet[-index, 31] 

names(getModelInfo())

#######  Prediction Using KNN Technique
knn2 <- knn(train = cleanDataSet.training, test = cleanDataSet.test,cl = prc_train_labels, k=2)
# Predicting test dataset with trained knn models
merge.knn2 <- data.frame(knn2, prc_test_labels)
names(merge.knn2) <- c("Predicted Result", "Observed Result")
merge.knn2
#Cross table 
CrossTable(x= prc_test_labels,y = knn2,prop.chisq = FALSE)
#######  Knn (without preprocessing)
model_knn <- train(cleanDataSet.training[, 1:30], cleanDataSet.training[, 31], method='knn')
model_knn
plot(model_knn)

# Predict the labels of the test set
predictions<-predict(object=model_knn,cleanDataSet.test[,1:30])

# Evaluate the predictions
table(predictions)
#confusion matrix for model with method knn(without preprocessing)
predictKnn <- factor(predictions)
realKnn <- factor(cleanDataSet.test$Result)

data1 <- data.frame(data = predictKnn, type = "prediction")
data2 <- data.frame(data = realKnn, type = "real")
#data3 <- rbind(data1,data2)
data3 <- rbind(data1,data2)

#confusionMatrix(data1,data2)

# Check if the levels are identical
identical(levels(data3[data3$type == "prediction",1]) , levels(data3[data3$type == "real",1]))

confusionMatrix(data3[data3$type == "prediction",1], data3[data3$type == "real",1],  dnn = c("Prediction", "Reference"))



################### table
# Train the model with preprocessing
model_knn1 <- train(cleanDataSet.training[, 1:30], cleanDataSet.training[, 31], method='knn', preProcess=c("center", "scale"))
model_knn1
plot(model_knn1)
# Predict values
predictions<-predict.train(object=model_knn1,cleanDataSet.test[,1:30], type="raw")
# Confusion matrix for training the model with preprocessing
predictKnn <- factor(predictions)
realKnn <- factor(cleanDataSet.test$Result)

data1 <- data.frame(data = predictKnn, type = "prediction")
data2 <- data.frame(data = realKnn, type = "real")
data3 <- rbind(data1,data2)

# Check if the levels are identical
identical(levels(data3[data3$type == "prediction",1]) , levels(data3[data3$type == "real",1]))

confusionMatrix(data3[data3$type == "prediction",1], data3[data3$type == "real",1],  dnn = c("Prediction", "Reference"))

#######   Prediction Using Neural Network Technique

set.seed(2)
nn<-neuralnet(formula = Result~IP_Address+URL_Length+Shortining_Service+having_At_Symbol+double_slash_redirecting+Prefix_Suffix+having_Sub_Domain+SSLfinal_State+Domain_registeration_length+Favicon+port+HTTPS_token+Request_URL+URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+Redirect+on_mouseover+RightClick+popUpWidnow+Iframe+age_of_domain+DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+Statistical_report, data = cleanDataSet.training, hidden=2, linear.output = FALSE)#names command displays the available neural network properties
plot(nn)

#prediction of nn
predict_testNN = compute(nn, cleanDataSet.test[,c(1:31)])
predict_testNN = (predict_testNN$net.result * (max(cleanDataSet$Result) - min(cleanDataSet$Result))) + min(cleanDataSet$Result)

plot(cleanDataSet.test$Result, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((cleanDataSet.test$Result - predict_testNN)^2) / nrow(cleanDataSet.test)) ^ 0.5
RMSE.NN
#confusion matrix for nn
predictConf <- factor(predict_testNN)
real <- factor(cleanDataSet.test$Result)

my_data1 <- data.frame(data = predictConf, type = "prediction")
my_data2 <- data.frame(data = real, type = "real")
my_data3 <- rbind(my_data1,my_data2)
# Check if the levels are identical
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))

confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"))

