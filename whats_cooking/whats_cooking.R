setwd("~/Documents/RPI/R Projects/Working directory") 
#set the working directory

library.loading <- c("rpart", "jsonlite", "randomForest", "ggplot2", "tm", "caret", "rpart.plot", "xgboost", "SnowballC", "dplyr")
lapply(library.loading, require, character.only = T) #load packages 

traincooking <- fromJSON("traincooking.json", flatten=TRUE)
testcooking <- fromJSON("testcooking.json", flatten=TRUE)
#imported train and test json files
#created train and test data frames

recipebar <- table(traincooking$cuisine)
barplot(recipebar, 
        main = "Cuisines by Number of Recipes",
        ylab = "Number of Recipes",
        xlab="Country Cuisine")
#created a bar chart in order to figure out the country that contained...
#...the most recipes in the train data frame
#This country, italian, was used as the baseline for the naïve model

table(traincooking$cuisine)
#displayed a count of the number of recipes per country
#a good way to confirm the previous graph was correct
#italian was indeed the recipe with the largest number of recipes in the train data frame

traincorpus <- Corpus(VectorSource(traincooking$ingredients))
testcorpus <- Corpus(VectorSource(testcooking$ingredients))
#First step of creating a corpus

traincorpus <- tm_map(traincorpus, stemDocument, lazy=TRUE)
traincorpus <- tm_map(traincorpus, stripWhitespace, lazy=TRUE)
traincorpus <- tm_map(traincorpus, tolower, lazy=TRUE)
testcorpus <- tm_map(testcorpus, stemDocument,lazy = TRUE)
testcorpus <- tm_map(testcorpus, stripWhitespace,lazy = TRUE)
testcorpus <- tm_map(testcorpus, tolower,lazy = TRUE)
traincorpus <- tm_map(traincorpus,PlainTextDocument)
testcorpus <- tm_map(testcorpus,PlainTextDocument)
#reduced words to their base form
#trimmed white space
#made all letters lower case in the corpora

ingredientsDTM1 <- DocumentTermMatrix(traincorpus)
ingredientsDTM2 <- DocumentTermMatrix(testcorpus)
#created a document term matrix

sparse1 <- removeSparseTerms(ingredientsDTM1, 0.985)
sparse2 <- removeSparseTerms(ingredientsDTM2, 0.985)
#Removed terms with a frequency of less than 1.5%

ingredientsDTM1df <- as.data.frame(as.matrix(sparse1))
ingredientsDTM2df <- as.data.frame(as.matrix(sparse2))
#changed the document term matrix to a data frame

printout <- sapply(ingredientsDTM1df, class)
printout
#used to view the type of variable of each column

traincol <- names(ingredientsDTM1df)
testcol <- names(ingredientsDTM2df)
#use terms instead of ??

intersect1 <- intersect(traincol,testcol)

ingredientsDTM1df <- ingredientsDTM1df[,c(intersect1)]
ingredientsDTM2df <- ingredientsDTM2df[,c(intersect1)]
#removed some columns??

#ingredientsDTM1df$cuisine <- as.factor(traincooking$cuisine)
#turned cuisine into a factor variable

set.seed(12345)

names(ingredientsDTM1df) <- gsub("-", "", names(ingredientsDTM1df))
names(ingredientsDTM2df) <- gsub("-", "", names(ingredientsDTM2df))
#take "-" out of the column name since random Forest would not run with them in the title

row.names(ingredientsDTM1df) <- rownames(1:39774)
#changed row names from character(0) to a number in the train dtm

row.names(ingredientsDTM2df) <- rownames(1:9944)
#changed row names from character(0) to a number in the test dtm

printout <- sapply(ingredientsDTM1df, class)
printout
#used to view the type of variable of each column 

if(traincooking$ingredients %in% "garam masala") 
{testcooking$cuisine="indian"
} else{rfmodel <- randomForest(as.factor(traincooking$cuisine) ~ . ,data = ingredientsDTM1df, 
                               importance=TRUE, ntree=1)}

rfmodel <- randomForest(as.factor(traincooking$cuisine) ~ . ,data = ingredientsDTM1df, importance=TRUE, ntree=200)
rfpredict <- predict(rfmodel, newdata = ingredientsDTM2df, type = "response")
#random forest model and prediction

submission <- data.frame(id = testcooking$id, cuisine= rfpredict)
write.csv(submission, file = "randomforestspanish.csv", row.names = FALSE)
#created data frame for submission to kaggle
