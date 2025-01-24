#Detecting phishing URLs

# loading the data set
original_data <- read.csv("D:\\Transfered from ssd\\Desktop\\PredictiveModeling\\PhiUSIIL_Phishing_URL_Dataset.csv")
# looking the data set
View(original_data)
dim(original_data)
str(original_data)
names(original_data)
original_data$label<-factor(original_data$label)
original_data$label <- ifelse(original_data$label == 0, "phishing", "legitimate")
str(original_data)

###Data preprocessing
# Checking for missing values
total_missing <- sum(is.na(original_data))

# Print the total number of missing values
total_missing
# distribution of each predictors
# Identify numeric columns
numeric_cols <- sapply(original_data, is.numeric)

# Calculate the frequency of each class
freq <- table(original_data["label"])

# Create barplot
barplot(freq, 
        main = "Frequency distribution of Label", 
        xlab = "Classes", 
        ylab = "Frequency", 
        col = "lightblue", 
        las = 2)  # Rotate x-axis labels

# Calculate percentages
percentages <- round((freq / sum(freq)) * 100, 1)

# Add percentages to the barplot
text(x = seq_along(freq), y = freq, labels = paste(percentages, "%"), pos = 3)

# Define the columns for numeric and character types explicitly
# Categorizing the columns
numeric_cols <- c(
  "URLLength","DomainLength","URLSimilarityIndex","CharContinuationRate", "TLDLegitimateProb",
  "URLCharProb","TLDLength","NoOfSubDomain","NoOfObfuscatedChar","ObfuscationRatio","NoOfLettersInURL",
  "LetterRatioInURL","NoOfDegitsInURL","DegitRatioInURL","NoOfEqualsInURL","NoOfQMarkInURL","NoOfAmpersandInURL",
  "NoOfOtherSpecialCharsInURL","SpacialCharRatioInURL", "LineOfCode","LargestLineLength","DomainTitleMatchScore",
  "URLTitleMatchScore","NoOfURLRedirect","NoOfSelfRedirect","NoOfPopup","NoOfiFrame","NoOfImage","NoOfCSS",
  "NoOfJS","NoOfSelfRef","NoOfEmptyRef","NoOfExternalRef"
)

factor_cols <- c(
  "IsDomainIP","TLD","HasObfuscation","HasTitle","HasFavicon","Robots","IsResponsive",
  "HasDescription","HasExternalFormSubmit","HasSocialNet","HasSubmitButton",
  "HasHiddenFields","HasPasswordField","Bank","Pay","Crypto","HasCopyrightInfo","IsHTTPS","label"
)
# Check the lists
print(numeric_cols)
print(factor_cols)

# Convert specified columns to numeric
original_data[numeric_cols] <- lapply(original_data[numeric_cols], as.numeric)

# Convert specified columns to character
original_data[factor_cols] <- lapply(original_data[factor_cols], as.character)

str(original_data)

# removing uncessary columns
data <- original_data[ , !(names(original_data) %in% c("URL","Domain", "FILENAME", "label", "Title"))]
dim(data)
names(data)

# Adding dummy variables
# Treating "TLD"
unique_values <- unique(data["TLD"])
length(unique_values)
str(data["TLD"])


# adding different classes for TLD
library(dplyr)

classify_tld <- function(tld) {
  tld <- as.character(tld)
  
  us_country_domains <- c("uk", "de", "fr", "ca", "au", "jp", "it", "es", "nl", "ru", "ch", "se", "dk", "no", "fi", "ie", "be", "at", "nz", "pl", "br", "mx", "ar", "cl", "za", "in", "cn", "kr", "sg", "hk", "tw")
  commonly_used_domains <- c("com", "org", "net", "edu", "gov", "mil", "io", "co", "info", "biz", "me", "tv", "app", "dev")
  numeric_domains <- as.character(0:999)
  
  if (tld %in% us_country_domains) {
    return("Country Domain")
  } else if (tld %in% commonly_used_domains) {
    return("Common Domain")
  } else if (tld %in% numeric_domains) {
    return("Numeric Domain")
  } else if (startsWith(tld, "xn--")) {
    return("Internationalized Domain")
  } else {
    return("Other")
  }
}

data <- data %>%
  mutate(Classification = sapply(TLD, classify_tld)) %>%
  mutate(
    Internationalized_Domain = as.integer(Classification == "Internationalized Domain"),
    Common_Domain = as.integer(Classification == "Common Domain"),
    Numeric_Domain = as.integer(Classification == "Numeric Domain"),
    Country_Domain = as.integer(Classification == "Country Domain"),
    Other = as.integer(Classification == "Other")
  ) %>%
  select(-Classification, -TLD)  # Remove the Classification and TLD columns if not needed


#checking for near-zero variance predictors
# considering only catagorical values
cat_data<-data[sapply(data, is.character)] ## for all categorical predictors, need to recall the data
cat_data <- cbind(cat_data, 
                  Internationalized_Domain = data[["Internationalized_Domain"]],
                  Common_Domain = data[["Common_Domain"]],
                  Numeric_Domain = data[["Numeric_Domain"]],
                  Country_Domain = data[["Country_Domain"]],
                  Other = data[["Other"]])

dim(cat_data)

#bargraph

# Set up the plot area for multiple graphs
par(mfrow = c(3,3))  
# Loop through each categorical column
for (col in names(cat_data)) {
  # Calculate frequencies
  freq <- table(cat_data[[col]])
  
  # Create barplot
  barplot(freq, 
          main = paste("Frequency of", col), 
          xlab = "Classes", 
          ylab = "Frequency", 
          col = "lightblue", 
          las = 2)  # Rotate x-axis labels for better readability
}

library(caret)
#near zero variables
nearzero_var<-nearZeroVar(cat_data)

names(cat_data[nearzero_var])

# Remove near-zero variance columns
cat_data_cleaned <- cat_data[, -nearzero_var]
dim(cat_data_cleaned)
names(cat_data_cleaned)

# bargraph after deleting near-zero var

# Set up the plot area for multiple graphs
par(mfrow = c(3,3))  
# Loop through each categorical column
for (col in names(cat_data_cleaned)) {
  # Calculate frequencies
  freq <- table(cat_data_cleaned[[col]])
  
  # Create barplot
  barplot(freq, 
          main = paste("Frequency of", col), 
          xlab = "Classes", 
          ylab = "Frequency", 
          col = "lightblue", 
          las = 2)  # Rotate x-axis labels for better readability
}


# changing all to numeric
all_num_data<- data.frame(lapply(data, function(x) as.numeric(as.character(x))))
str(all_num_data)
dim(all_num_data)

# Remove the specified columns
all_num_data <- all_num_data %>%
  select(-IsDomainIP, -HasObfuscation, -HasExternalFormSubmit, -Crypto, -Internationalized_Domain, -Numeric_Domain)
dim(all_num_data)
#correlation

# Calculate correlation matrix
correlations <- cor(all_num_data, use = "complete.obs")  # Handle missing values

# Print the correlation matrix
print(correlations)



# Convert to a data frame and melt it to long format
cor_df <- as.data.frame(as.table(correlations))


## To visually examine the correlation structure of the data
library(corrplot)
corrplot(correlations, tl.cex = 0.3,order = "hclust")
# Finding the highly correlated predictors recommended for deletion
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
colnames(all_num_data)[highCorr]

#deleting highly correlated data
filtered_data <- all_num_data[, -highCorr]
length(filtered_data)

freq <- table(cat_data[["Numeric_Domain"]])
# Create barplot
barplot(freq, 
        main = paste("Frequency of Numeric Domain"), 
        xlab = "Classes", 
        ylab = "Frequency", 
        col = "lightblue", 
        las = 2)  # Rotate x-axis labels for better readability

## After deletion plot
correlations2 <- cor(filtered_data, use = "complete.obs")  # Handle missing values
library(corrplot)
corrplot(correlations2, tl.cex = 0.3,order = "hclust")

# let's explore the continious preictors
num_filtered <- filtered_data[, !names(filtered_data) %in% c(
  "IsHTTPS", "HasTitle", "HasFavicon", "Robots", "IsResponsive",
  "HasDescription", "HasSocialNet", "HasSubmitButton", "HasHiddenFields", "HasPasswordField",
  "Bank", "Pay", "HasCopyrightInfo", "Common_Domain", "Country_Domain", "Other"
)]

dim(num_filtered)

cont_data <- as.data.frame(num_filtered)

# Display the continuous predictors
print(cont_data)
dim(cont_data)

# Histogram
# Set up the layout for multiple plots
par(mfrow = c(3, 3))  # Adjust the number of rows and columns as needed
for (col in names(cont_data)) {
    x_limits <- range(cont_data[[col]]) 
    hist(cont_data[[col]], 
         main = paste(col), 
         xlab = col, 
         xlim = x_limits,
         col = "lightblue", 
         border = "black",
         breaks = 20)
  }

#skewness
library(e1071)
# Function to calculate skewness for numeric or integer columns
skewness_results <- sapply(cont_data, skewness)

# Print the skewness results
print(skewness_results)


#Center and scale
s_c_data<-scale(cont_data, center = TRUE, scale = TRUE)
# Convert scaled data to a data frame
s_c_data_df <- as.data.frame(s_c_data)
#adding constant
# Find the minimum value in the data frame
min_value <- min(s_c_data_df, na.rm = TRUE)  # Overall minimum value
shift_constant <- abs(min_value) + 1  # Shift constant to ensure positivity

# Shift the data
s_c_data_df_shifted <- s_c_data_df + shift_constant


#Box cox transformation
library(caret)
xx1 <- preProcess(s_c_data_df_shifted, method = c("BoxCox"))
transformed_data <- predict(xx1, s_c_data_df_shifted)
# Convert transformed data to a data frame
transformed_data <- as.data.frame(transformed_data)

# Histogram after boxcox transformation
# Set up the layout for multiple plots
par(mfrow = c(3, 3))  # Adjust the number of rows and columns as needed
for (col in names(transformed_data)) {
    hist(transformed_data[[col]], 
         main = paste(col), 
         xlab = col, 
         col = "lightblue", 
         border = "black",
         breaks = 20)
}

# Function to calculate skewness for numeric or integer columns
skewness_after<- sapply(transformed_data, skewness)

# Print the skewness results after boxcox
print(skewness_after)



#boxplot
# Set up the layout for multiple plots
par(mfrow = c(3, 3))  # Adjust the number of rows and columns as needed

# Loop through each column in the data frame
for (col in names(transformed_data)) {
  boxplot(transformed_data[[col]], 
          main = paste(col), 
          ylab = col, 
          col = "lightblue", 
          border = "black")
}

str(cat_data_cleaned)
cat_data_cleaned <- data.frame(lapply(cat_data_cleaned, function(x) as.numeric(as.character(x))))

View(transformed_data)
View(cat_data_cleaned)
dim(transformed_data)
dim(cat_data_cleaned)


#applying spatialsign
spatialsign_data<-spatialSign(transformed_data)
spatialsign_data <- as.data.frame(spatialsign_data)
View(spatialsign_data)

#boxplot after spatialsign
# Set up the layout for multiple plots
par(mfrow = c(3, 3))  # Adjust the number of rows and columns as needed

# Loop through each column in the data frame
for (col in names(spatialsign_data)) {
  boxplot(spatialsign_data[[col]], 
          main = paste(col), 
          ylab = col, 
          col = "lightblue", 
          border = "black")
}

merged_allfinal<-cbind(spatialsign_data, cat_data_cleaned)
dim(merged_allfinal)
# Step 2: Convert all factor columns in merged_all to numeric
merged_allfinal <- as.data.frame(lapply(merged_allfinal, function(x) {
  if (is.factor(x)) {
    as.numeric(as.factor(x))  # Convert factor to character, then to numeric
  } else {
    x  # Keep other types unchanged
  }
}))

# Step 3: Check the structure of the final data frame
str(merged_allfinal)
View(merged_allfinal)


#PCA
pcaObject_data <- prcomp(merged_allfinal, center = TRUE, scale. = TRUE)
summary(pcaObject_data)


#scree plot
screeplot(pcaObject_data, 
          main = "Scree Plot", 
          xlab = "Principal Component", 
          ylab = "Variance Explained", 
          type = "lines", 
          col = "blue",  # Optional: line color
          pch = 19)      # Optional: point type
# Load necessary library
library(ggplot2)


# Calculate variance explained
variance <- pcaObject_data$sdev^2
variance_explained <- variance / sum(variance)

# Create a data frame for plotting
scree_data <- data.frame(PC = 1:length(variance), Variance = variance_explained)

# Draw the scree plot using ggplot2
ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_line() + 
  geom_point() +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()

original_data$label<-factor(original_data$label)
str(original_data)
# applying spatial sign transformation on all_num_data
spatialsign_data49<-spatialSign(all_num_data)
spatialsign_data49 <- as.data.frame(spatialsign_data49)

no_boxcox_data<- cbind(cont_data,cat_data_cleaned)
# applying spatial sign transformation on no_boxcox_data
spatialsign_data44<-spatialSign(no_boxcox_data)
spatialsign_data44 <- as.data.frame(spatialsign_data44)


###############Model building###################
################Linear Models##################
# Logistic regression
#spliting data
# Set the random number seed so we can reproduce the results
set.seed(476)
library(caret)
# use createDataPartition for stratified sampling, p is percentage of training set
trainingRows <- createDataPartition(original_data$label, p = .80, list= FALSE)

trainPredictors <- spatialsign_data49[trainingRows, ]
trainClasses <- original_data$label[trainingRows]

testPredictors <- spatialsign_data49[-trainingRows, ]
testClasses <- original_data$label[-trainingRows]
dim(trainPredictors)
dim(testPredictors)
library(caret)

ctrl <- trainControl(method = "CV",number=3,
                     summaryFunction =defaultSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(476)
logistic <- train(trainPredictors,
                  y = trainClasses,
                  method = "glm",
                  metric = "Kappa",
                  trControl = ctrl)
logistic
# predicting on test set
predicted<- predict(logistic, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

# LDA
set.seed(476)

# use createDataPartition for stratified sampling, p is percentage of training set
trainingRows <- createDataPartition(original_data$label, p = .80, list= FALSE)

trainPredictors <- spatialsign_data44[trainingRows, ]
trainClasses <- original_data$label[trainingRows]

testPredictors <- spatialsign_data44[-trainingRows, ]
testClasses <- original_data$label[-trainingRows]
dim(trainPredictors)
dim(testPredictors)


LDA <- train(trainPredictors,
             y = trainClasses,
             method = "lda",
             metric = "Kappa",
             trControl = ctrl,
             preProcess = c("center", "scale"))
LDA

# predicting on test set
predicted<- predict(LDA, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

#PLSDA
#spliting data
# Set the random number seed so we can reproduce the results
set.seed(476)

# use createDataPartition for stratified sampling, p is percentage of training set
trainingRows <- createDataPartition(original_data$label, p = .80, list= FALSE)

trainPredictors <- spatialsign_data49[trainingRows, ]
trainClasses <- original_data$label[trainingRows]

testPredictors <- spatialsign_data49[-trainingRows, ]
testClasses <- original_data$label[-trainingRows]
dim(trainPredictors)
dim(testPredictors)

set.seed(476)
plsda <- train(x = trainPredictors,
               y = trainClasses,
               method = "pls",
               tuneGrid = expand.grid(.ncomp = 1:30),
               preProc = c("center","scale"),
               metric = "Kappa",
               trControl = ctrl)
plsda
plot(plsda)

# predicting on test set
predicted<- predict(plsda, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

# Penalized Models


glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
                        .lambda = seq(.01, .2, length = 10))
set.seed(100)
glmnTuned <- train(x = trainPredictors,
                   y = trainClasses,
                   method = "glmnet",
                   tuneGrid = glmnGrid,
                   preProc = c("center", "scale"),
                   metric = "Kappa",
                   trControl = ctrl)
glmnTuned
plot(glmnTuned)

# predicting on test set
predicted<- predict(glmnTuned, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)


#####################Non Linear Models######################

#QDA
set.seed(476)

# use createDataPartition for stratified sampling, p is percentage of training set
trainingRows <- createDataPartition(original_data$label, p = .80, list= FALSE)

trainPredictors <- spatialsign_data44[trainingRows, ]
trainClasses <- original_data$label[trainingRows]

testPredictors <- spatialsign_data44[-trainingRows, ]
testClasses <- original_data$label[-trainingRows]
dim(trainPredictors)
dim(testPredictors)
levels(trainClasses)
library(caret)

ctrl <- trainControl(method = "cv", number = 3,
                     summaryFunction =defaultSummary,
                     classProbs = FALSE,
                     savePredictions = TRUE)

set.seed(476)

qda <- train(x = trainPredictors, 
             y = trainClasses,
             method = "qda",
             metric = "Kappa",
             preProc= c("center","scale"),
             trControl = ctrl)
qda

# predicting on test set
predicted<- predict(qda, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

#RDA
rdaGrid <- expand.grid(.gamma = c(0, .1, .2, .4, .6, .8, 1),
                       .lambda = seq(.01, .9, length = 10))
set.seed(476)

rda <- train(x = trainPredictors, 
             y = trainClasses,
             method = "rda",
             metric = "Kappa",
             preProc= c("center","scale"),
             tuneGrid = rdaGrid,
             trControl = ctrl)
rda
plot(rda)
# predicting on test set
predicted<- predict(rda, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

#MDA
set.seed(476)
mda <- train(x = trainPredictors, 
             y = trainClasses,
             method = "mda",
             metric = "Kappa",
             preProc= c("center","scale"),
             tuneGrid = expand.grid(.subclasses = 1:30),
             trControl = ctrl)
mda
plot(mda)
# predicting on test set
predicted<- predict(mda, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

#Neural Network
nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$.size)
numWts <- (maxSize * (44 + 1) + (maxSize+1)*3) 

set.seed(476)
nnetFit <- train(x = trainPredictors, 
                 y = trainClasses,
                 method = "nnet",
                 metric = "Kappa",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = numWts,
                 trControl = ctrl)
nnetFit
plot(nnetFit)
# predicting on test set
predicted<- predict(nnetFit, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

#FDA
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:20)
set.seed(476)
fda <- train(x = trainPredictors, 
             y = trainClasses,
             method = "fda",
             metric = "Kappa",
             tuneGrid = marsGrid,
             trControl = trainControl(method="CV",number=3))
fda
plot(fda)

# predicting on test set
predicted<- predict(fda, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

set.seed(476)

# use createDataPartition for stratified sampling, p is percentage of training set
trainingRows <- createDataPartition(original_data$label, p = .80, list= FALSE)

trainPredictors <- spatialsign_data49[trainingRows, ]
trainClasses <- original_data$label[trainingRows]

testPredictors <- spatialsign_data49[-trainingRows, ]
testClasses <- original_data$label[-trainingRows]
dim(trainPredictors)
dim(testPredictors)

# SVM
library(kernlab)
sigmaRangeReduced <- sigest(as.matrix(trainPredictors))
svmRGrid<- expand.grid(.sigma = sigmaRangeReduced[1],.C = 2^(seq(-4, 6)))
set.seed(476)
svmRModel <- train(x = trainPredictors, 
                   y = trainClasses,
                   method = "svmRadial",
                   metric = "Kappa",
                   preProc = c("center", "scale"),
                   tuneGrid = svmRGrid,
                   fit = FALSE,
                   trControl = ctrl)
svmRModel
plot(svmRModel)
# predicting on test set
predicted<- predict(svmRModel, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)


#KNN
set.seed(476)
knnFit <- train(x = trainPredictors, 
                y = trainClasses,
                method = "knn",
                metric = "Kappa",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(.k = 2:20),
               trControl = ctrl)
knnFit
plot(knnFit)
# predicting on test set
predicted<- predict(knnFit, newdata= testPredictors)

#confusion matrix
confusionMatrix(predicted, testClasses)

#Top 10 most important variables
ImpSim <- varImp(svmRModel, scale = FALSE)
ImpSim
plot(ImpSim, top = 10, scales = list(y = list(cex = .95)))

