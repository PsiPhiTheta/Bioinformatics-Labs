# Thomas Hollis (BCH441, University of Toronto) -v2.1
#     - Purpose: Implement kNN for cancer detection
#     - Bugs & issues: no bugs, no issues, no warnings
#     - Acknowledgements: thanks to Brett Lantz & Prof. Steipe's learning units which were of great inspiration

# DO NOT SIMPLY "source()" THIS FILE! YOU WILL LOSE (IQ) POINTS FOR DOING SO!

###### 1. Load packages and data ######
if (!require(class, quietly=TRUE)) {
  install.packages("class")
  library(class)
} # used for the kNN function

if (!require(gmodels, quietly=TRUE)) {
  install.packages("gmodels")
  library(gmodels)
} # used for our scoring table

data <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

data <- data[-1] # Remove the 'id' feature as we won't use it (does not bring us any info)

# Our knn package will need the target feature coded as a factor, so let's do this now
data$diagnosis<- factor(data$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

###### 2. Exploratory Data Analysis (EDA) ######

summary(data) # Explore the dataset! 31 features describing the cell being observed.

###### 3. Data Preprocessing ######

# We know that kNN is very sensitive to scale so we will have to normalise some data.
# Let's write a simple function for this

normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dataNorm <- as.data.frame(lapply(data[2:31], normalise)) # Now let's normalise our data, exclude labels

# Time split the data into train, validate and test
dataTrain <- dataNorm[1:398, ] # 70% data for training
dataTrainLab <- data[1:398, 1] # and its labels
dataValidation <- dataNorm[399:484, ] # 15% data for validation
dataValidationLab <- data[399:484, 1] # and its labels
dataTest <- dataNorm[485:569, ] # 15% data for testing
dataTestLab <- data[485:569, 1] # and its labels

###### 4. Running kNN ######

# Now that our data is fully ready to feed into our algo, let's feed it in!
# Here we are going to use the class package's "knn()" function
# ?knn for more info

# Let's first compute an upper limit for k (according to our rule of thumb)
(k <- floor(sqrt(nrow(data))))

# Now let's feed the knn package our training data, training labels and test data (without labels!)
dataValidationPred <- knn(train = dataTrain, test = dataValidation, cl = dataTrainLab, k = k)

# Let's have a look at the output
(dataValidationPred) # These should show all the class predictions for the validation set

# Let's see how well our initial model worked on the validation set (leave the test set alone until the end!)
# ?CrossTable for more info
CrossTable(x = dataValidationLab, y = dataValidationPred, prop.chisq=FALSE)

# TASK 1: What information does this table give you?
# TASK 1: How many: true negative? true positive? false positive? false negative?

# ANSWER 1: top left is true negative, bottom right is true positive
# ANSWER 1: bottom left is false negative, top right is false positive

# TASK 2: From this table, what is your classification error rate?

# ANSWER 2: classification error rate is: err = (true negative + true positive)/total
# ANSWER 2: Hence our classification error rate on validation set with k=23 is = 84/86 = 98%!

###### 5. Tuning kNN ######

# Now let's try to find the best possible value of k.
# In normal scenarios the dataset is much bigger so the values of k can be much bigger
# For a large number of different k, we would useually tune k using a grid search
# However, here to keep this exercise interesting the dataset was kept small
# Hence here we have the luxury of being able to try all possible combinations

# TASK 3: tune the kNN! (don't worry about optimising your code, this isn't the goal here)

# ANSWER 3:

# A sensible approach would be to start by creating a function to return the overall performance

performance <- function(label, predictions) {
  acc <- (sum(label == predictions))/(length(label))
  return (acc)
}

# Now we can iterate through all k and get their validation set performance
perf <- 1:floor(sqrt(nrow(data)))

for (i in 1:floor(sqrt(nrow(data)))) {
  k <- i
  dataValidationPred <- knn(train = dataTrain, test = dataValidation, cl = dataTrainLab, k = k)
  perf[i] <- performance(label = dataValidationLab, predictions = dataValidationPred)
  cat("With a k of", k, "we get a performance of:", perf[i], "\n")
} # Best k for me at k=4, k=15, k=16 (I'll pick 16 because of Occam's Razor)

# Bonus marks if you vectorise this code and push it to GitHub!

###### 6. Evaluate final model performance ######

# Now we have picked our best k, let's see how well we can classify the test data
k <- 16 # insert your best K here
dataTestPred <- knn(train = dataTrain, test = dataTest, cl = dataTrainLab, k = k) # run on test data
(performance(label = dataValidationLab, predictions = dataValidationPred)) # evaluate (for me ~0.98)

# Write a conclusion in your journal about whether you think a 2% error rate is acceptable
# Take the example of a patient who has tested positive for cancer. Think about Bayes' Rule!

# END
