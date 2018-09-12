setwd("F:/AcadGild/workings")

library(readr)
library(Hmisc)
library(dplyr)
library(MASS)
library(corrplot)
library(ggplot2)
library(car)


a. Predict the no of comments in next H hrs
b. Use regression technique
c. Report the training accuracy and test accuracy

# import train data set
Variant_1<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_1.csv", header = FALSE)
Variant_2<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_2.csv", header = FALSE)
Variant_3<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_3.csv", header = FALSE)
Variant_4<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_4.csv", header = FALSE)
Variant_5<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Training/Features_Variant_5.csv", header = FALSE)

fbtrain<-rbind(Variant_1,Variant_2,Variant_3,Variant_4,Variant_5)
dim(fbtrain)


#import test data set
test1<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_1.csv", header = FALSE)
test2<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_2.csv", header = FALSE)
test3<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_3.csv", header = FALSE)
test4<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_4.csv", header = FALSE)
test5<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_5.csv", header = FALSE)
test6<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_6.csv", header = FALSE)
test7<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_7.csv", header = FALSE)
test8<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_8.csv", header = FALSE)
test9<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_9.csv", header = FALSE)
test10<-read.csv("F:/AcadGild/workings/Dataset/Dataset/Testing/TestSet/Test_Case_10.csv", header = FALSE)

fbtest<-rbind(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10)
dim(fbtest)

# Assign variable names to the train and test data set
colnames(fbtrain) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                       "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                       "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                       "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                       "basetue","basewed","basethu","basefri","basesat","target")

colnames(fbtest) <- c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                      "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                      "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                      "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                      "basetue","basewed","basethu","basefri","basesat","target")

dim(fbtrain); dim(fbtest)
str(fbtrain); str(fbtest)
View(fbtrain); View(fbtest)

train<-(fbtrain); test<-(fbtest)
head(train); head(test)

# removing overlapping observations if any
distinct(train)

# list the levels for the class
sapply(train, class)



a. Predict the no of comments in next H hrs


# Outlier Detection

sapply(train[,1:54], function(x) quantile(x, c(.01,.05,.25,.5,.75,.90,.95, .99, 1),na.rm=TRUE) )


# clean dataset, impute missing values and perform exploratory data analysis

distinct(train) # removing overlapping observations if any
dim(train)
sapply(train, function(x) sum(is.na(x))) # no missing values

correlation <- cor(train[,c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
                            "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
                            "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
                            "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
                            "basetue","basewed","basethu","basefri","basesat","target")])
                         
corr <- as.data.frame(reshape::melt(correlation))

corr <- corr%>%filter(X1 == "target" & value != 1 & value > 0.32 & value > -0.32)

corr # good corelations with target variable
corrplot.mixed(cor(train[,c(30:34)]))

#  VIF 

library(usdm)

vif(train[,c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11","d12",
        "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26",
        "d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","postshre",
        "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basemon",
        "basetue","basewed","basethu","basefri","basesat","target")])

library(Information)

create_infotables(data = train, y="target")

# Create training and validation sets 

set.seed(123)
smp_size <- floor(0.7 * nrow(train))

train_ind <- sample(seq_len(nrow(train)), size = smp_size)

training <- train[train_ind,]

validation <- train[-train_ind,]
validation

# Difference between the number of comments in last 24 hrs of base time and 
# comments in first 24 hrs of publish is significant

# e. Create a linear regression model to predict the number of comments in the next 24 hours
# (relative to basetime)


TARGET <- lm(target~., data = train)

step <- stepAIC(TARGET, direction = "both")


final_model <- lm(target ~ checkin + talking + d5 + d6 + d7 + d8 + d9 + d10 + d11 + 
                    d12 + d13 + d16 + d17 + d19 + d20 + d21 + d22 + d23 + d24 + 
                    cc1 + cc2 + cc3 + cc4 + basetime + postshre + Hhrs + wed + 
                    thu + fri + basemon + basewed, data = train)
summary(final_model)

#Check Performance on the Validation Set 
val<-predict(final_model, validation, type="response")

mydf<-cbind(validation, val)

mydf$response <- as.factor(ifelse(mydf$val>0.5, 1, 0))
mydf$response

summary(mydf$response)

#Scoring the Test Data using the model we just created 

pred <- predict(final_model, test, type="response") 
final <- cbind(test,pred)
final
head(final)

final_print<-write.csv(final,"final_probs.csv")
final_print






