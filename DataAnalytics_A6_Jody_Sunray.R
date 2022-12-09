############################
#### LOAD TRAINING DATA ####
############################

library(caret)
library(psych)
library(dplyr)
library(FNN)

source("training_data_load.r")

# get variables of interest
ems_data <- a[,c("incident_response_seconds_qy",
                 "valid_incident_rspns_time_indc",
                 "initial_severity_level_code",
                 "initial_call_type",
                 "borough",
                 "zipcode",
                 "incident_disposition_code",
                 "policeprecinct",
                 "citycouncildistrict",
                 "communitydistrict",
                 "communityschooldistrict",
                 "congressionaldistrict",
                 "incident_year", "month", "dow")
              ]

####################################
######### DATA PREPARATION #########
####################################

# categorize data based on response time speed
ems_data$response_time <- cut(ems_data$incident_response_seconds_qy, br = c(-1,300,600,100000), labels = c('Fast', 'Medium', 'Slow'))

# remove invalid response times
ems_data <- ems_data[ems_data$valid_incident_rspns_time_indc == 'Y',]

###################################
######### REMOVE OUTLIERS #########
###################################

# determine Q1 and Q3
summary(ems_data$incident_response_seconds_qy)
first_quartile <- 269.0
third_quartile <- 542.0

# get IQR
IQR <- IQR(ems_data$incident_response_seconds_qy, na.rm = TRUE)

# get threshold values for outliers
Tmin = first_quartile - (1.5 * IQR)
Tmax = third_quartile + (1.5 * IQR)

# get outliers
ems_data_outliers <- ems_data$incident_response_seconds_qy[which(ems_data$incident_response_seconds_qy < Tmin | ems_data$incident_response_seconds_qy > Tmax)]

# remove outliers
ems_data_no_outliers <- subset(ems_data, ems_data$incident_response_seconds_qy > Tmin & ems_data$incident_response_seconds_qy < Tmax)

####################
### REMOVE NULLS ###
####################

# remove rows with null community district
ems_data_no_outliers <- ems_data_no_outliers[!(is.na(ems_data_no_outliers$communitydistrict)),]

# remove rows with null incident disposition code
ems_data_no_outliers <- ems_data_no_outliers[!(is.na(ems_data_no_outliers$incident_disposition_code)),]

# remove rows with null congressionaldistrict
ems_data_no_outliers <- ems_data_no_outliers[!(is.na(ems_data_no_outliers$congressionaldistrict)),]

# remove rows with null communityschooldistrict
ems_data_no_outliers <- ems_data_no_outliers[!(is.na(ems_data_no_outliers$communityschooldistrict)),]

# remove rows with null policeprecinct
ems_data_no_outliers <- ems_data_no_outliers[!(is.na(ems_data_no_outliers$policeprecinct)),]

# remove rows with null citycouncildistrict
ems_data_no_outliers <- ems_data_no_outliers[!(is.na(ems_data_no_outliers$citycouncildistrict)),]

##################
### GET SAMPLE ###
##################

# get sample of 100000 rows
ems_data_sample <- ems_data_no_outliers[sample(nrow(ems_data_no_outliers), 10000),]

# convert incident_year to string
ems_data_sample$incident_year <- as.character(ems_data_sample$incident_year)

# convert initial_severity_level_code to string
ems_data_sample$initial_severity_level_code <- as.character(ems_data_sample$initial_severity_level_code)

# convert communitydistrict to string
ems_data_sample$communitydistrict <- as.character(ems_data_sample$communitydistrict)

# convert initial_call_type to string
ems_data_sample$initial_call_type <- as.character(ems_data_sample$initial_call_type)

# convert incident_disposition_code to string
ems_data_sample$incident_disposition_code <- as.character(ems_data_sample$incident_disposition_code)

# convert congressionaldistrict to string
ems_data_sample$congressionaldistrict <- as.character(ems_data_sample$congressionaldistrict)

# convert communityschooldistrict to string
ems_data_sample$communityschooldistrict <- as.character(ems_data_sample$communityschooldistrict)

# convert policeprecinct to string
ems_data_sample$policeprecinct <- as.character(ems_data_sample$policeprecinct)

# convert citycouncildistrict to string
ems_data_sample$citycouncildistrict <- as.character(ems_data_sample$citycouncildistrict)

# remove comma from zipcodes
ems_data_sample$zipcode <- gsub(',', '', ems_data_sample$zipcode)

###################################
#### EXPLORATORY DATA ANALYSIS ####
###################################

# look at data types of all variables
str(ems_data_no_outliers)

# look at summary of response time variable
summary(ems_data_no_outliers$incident_response_seconds_qy)

# summarize variables
summary(ems_data_no_outliers)

################
### BOXPLOTS ###
################

# create boxplot of response time distribution for each borough
ggplot(data = ems_data_sample, aes(x=borough, y=incident_response_seconds_qy, color=borough)) + geom_boxplot()

# create boxplot of response time distribution for each incident_year
ggplot(data = ems_data_sample, aes(x=incident_year, y=incident_response_seconds_qy, color=incident_year)) + geom_boxplot()

# create boxplot of response time distribution for each month
ggplot(data = ems_data_sample, aes(x=month, y=incident_response_seconds_qy, color=month)) + geom_boxplot()

# create boxplot of response time distribution for each dow
ggplot(data = ems_data_sample, aes(x=dow, y=incident_response_seconds_qy, color=dow)) + geom_boxplot()

# create boxplot of response time distribution for each initial_severity_level_code
ggplot(data = ems_data_sample, aes(x=initial_severity_level_code, y=incident_response_seconds_qy, color=initial_severity_level_code)) + geom_boxplot()

# create boxplot of response time distribution for each communitydistrict
ggplot(data = ems_data_sample, aes(x=communitydistrict, y=incident_response_seconds_qy, color=communitydistrict)) + geom_boxplot() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))

# create boxplot of response time distribution for each initial_call_type
ggplot(data = ems_data_sample, aes(x=initial_call_type, y=incident_response_seconds_qy, color=initial_call_type)) + geom_boxplot() + theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust=1, vjust=0.4))

# convert incident_year back to numeric
ems_data_sample$incident_year = as.numeric(ems_data_sample$incident_year)

######################################
#### MODEL 1: ########################
#### MULTIPLE REGRESSION ANALYSIS ####
######################################

# create linear model
ems_data_linear_model <- lm(incident_response_seconds_qy ~
                              initial_severity_level_code +
                              initial_call_type +
                              borough +
                              zipcode +
                              incident_disposition_code +
                              policeprecinct +
                              citycouncildistrict +
                              communitydistrict +
                              communityschooldistrict +
                              congressionaldistrict +
                              incident_year + month + dow, data = ems_data_sample)
anova(ems_data_linear_model)
summary(ems_data_linear_model)

# create second linear model with non-significant variables removed
ems_data_linear_model_2 <- lm(incident_response_seconds_qy ~
                              initial_severity_level_code +
                              initial_call_type +
                              borough +
                              zipcode +
                              incident_disposition_code +
                              policeprecinct +
                              communitydistrict +
                              communityschooldistrict +
                              congressionaldistrict +
                              incident_year + dow, data = ems_data_sample)
anova(ems_data_linear_model_2)
summary(ems_data_linear_model_2)

######################################
#### MODEL 2: ########################
#### KNN FOR REGRESSION ##############
######################################

# create dummy variables for each of the following categorical variables
initial_severity_level_code <- as.data.frame(dummy.code(ems_data_sample$initial_severity_level_code))
initial_call_type <- as.data.frame(dummy.code(ems_data_sample$initial_call_type))
borough <- as.data.frame(dummy.code(ems_data_sample$borough))
dow <- as.data.frame(dummy.code(ems_data_sample$dow))
zipcode <- as.data.frame(dummy.code(ems_data_sample$zipcode))
communitydistrict <- as.data.frame(dummy.code(ems_data_sample$communitydistrict))
incident_disposition_code <- as.data.frame(dummy.code(ems_data_sample$incident_disposition_code))
congressionaldistrict <- as.data.frame(dummy.code(ems_data_sample$congressionaldistrict))
communityschooldistrict <- as.data.frame(dummy.code(ems_data_sample$communityschooldistrict))
policeprecinct <- as.data.frame(dummy.code(ems_data_sample$policeprecinct))
# citycouncildistrict <- as.data.frame(dummy.code(ems_data_sample$citycouncildistrict))

# prefixes to dummy variables to differentiate them better
addprefix_initial_severity_level_code <- function(x) {paste(x, "initial_severity_level_code", sep=".")}
initial_severity_level_code <- rename_with(initial_severity_level_code, addprefix_initial_severity_level_code)

addprefix_initial_call_type <- function(x) {paste(x, "initial_call_type", sep=".")}
initial_call_type <- rename_with(initial_call_type, addprefix_initial_call_type)

addprefix_borough <- function(x) {paste(x, "borough", sep=".")}
borough <- rename_with(borough, addprefix_borough)

addprefix_dow <- function(x) {paste(x, "dow", sep=".")}
dow <- rename_with(dow, addprefix_dow)

addprefix_zipcode <- function(x) {paste(x, "zipcode", sep=".")}
zipcode <- rename_with(zipcode, addprefix_zipcode)

addprefix_communitydistrict <- function(x) {paste(x, "communitydistrict", sep=".")}
communitydistrict <- rename_with(communitydistrict, addprefix_communitydistrict)

addprefix_incident_disposition_code <- function(x) {paste(x, "incident_disposition_code", sep=".")}
incident_disposition_code <- rename_with(incident_disposition_code, addprefix_incident_disposition_code)

addprefix_congressionaldistrict <- function(x) {paste(x, "congressionaldistrict", sep=".")}
congressionaldistrict <- rename_with(congressionaldistrict, addprefix_congressionaldistrict)

addprefix_communityschooldistrict <- function(x) {paste(x, "communityschooldistrict", sep=".")}
communityschooldistrict <- rename_with(communityschooldistrict, addprefix_communityschooldistrict)

addprefix_policeprecinct <- function(x) {paste(x, "policeprecinct", sep=".")}
policeprecinct <- rename_with(policeprecinct, addprefix_policeprecinct)

# addprefix_citycouncildistrict <- function(x) {paste(x, "citycouncildistrict", sep=".")}
# citycouncildistrict <- rename_with(citycouncildistrict, addprefix_citycouncildistrict)

# add the dummy variable columns to the sample data
ems_data_sample_dummy <- cbind(ems_data_sample, initial_severity_level_code, initial_call_type, borough, communitydistrict, congressionaldistrict, communityschooldistrict, policeprecinct, incident_disposition_code, dow)

# remove original non-dummy variables and other unused variables
ems_data_sample_clean <- ems_data_sample_dummy %>% dplyr::select(
  -one_of(c("initial_severity_level_code", "initial_call_type", "borough", "zipcode", "communitydistrict", "communityschooldistrict", "incident_disposition_code", "congressionaldistrict", "policeprecinct", "citycouncildistrict", "valid_incident_rspns_time_indc",
            "month", "dow", "response_time")))

# look at summary of variables
summary(ems_data_sample_clean)

# create random sample of 80% of the data
indices <- sample(nrow(ems_data_sample_clean), nrow(ems_data_sample_clean) * 0.80)

# create training and testing sets
train <- ems_data_sample_clean[indices,]
test <- ems_data_sample_clean[-indices,]

train_x <- train[,-1]
train_y <- train[,1]

test_x <- test[,-1]
test_y <- test[,1]

# create knn model
knnmodel <- knnreg(train_x, train_y, k=10)

# predict test data based on model
pred_y = predict(knnmodel, data.frame(test_x))

# plot predicted response times on top of actual response times
x = 1:length(test_y)
plot(x, test_y, col = "cyan3", type = "l")
lines(x, pred_y, col = "deeppink4")
legend("topright",  legend = c("Actual", "Predicted"), 
       fill = c("cyan3", "deeppink4"), col = 2:3,  adj = c(0, 0.6))
grid()

# print out predicted and actual response times
print(data.frame(test_y, pred_y))

# determine accuracy of KNN model
accuracy <- 100 * sum(abs((test_y - pred_y) / pred_y) <= 0.25) / length(test_y)
accuracy

# initialize variables
predicted_year_period <- NULL
accuracy_rate <- NULL

# calculate accuracy and misclassification rates for k = 1 to 20
for (i in 1:20) {
  set.seed(101)
  knnmodel <- knnreg(train_x, train_y, k=i)
  pred_y = predict(knnmodel, data.frame(test_x))
  accuracy_rate[i] <- 100 * sum(abs((test_y - pred_y) / pred_y) <= 0.25) / length(test_y)
}

# get k values
k_values <- 1:20

# create data frame for accuracy rates
accuracy_df <- data.frame(accuracy_rate, k_values)
accuracy_df
# plot accuracy rates
ggplot(accuracy_df, aes(k_values, accuracy_rate)) + geom_point() + geom_line(lty = 'dotted', color = 'cyan3')

######################################
#### MODEL 3: ########################
#### KNN CLASSIFICATION ##############
######################################

# get variables of interest
ems_data_sample_2 <- ems_data_sample_dummy %>% dplyr::select(
  -one_of(c("initial_severity_level_code", "initial_call_type", "borough", "zipcode", "communitydistrict", "communityschooldistrict", "incident_disposition_code", "congressionaldistrict", "policeprecinct", "citycouncildistrict", "valid_incident_rspns_time_indc",
            "month", "dow", "incident_response_seconds_qy")))

# create random sample of 80% of the data
indices <- sample(nrow(ems_data_sample_2), nrow(ems_data_sample_2) * 0.80)

# creating training and testing sets
train <- ems_data_sample_2[indices,]
test <- ems_data_sample_2[-indices,]

train_x <- train[,-2]
train_y <- train[,2]

test_x <- test[,-2]
test_y <- test[,2]

# perform KNN algorithm
knn_response_time <- knn(train_x, test_x, cl = train_y, k = 10)

# create and display contingency matrix
table <- table(knn_response_time, test_y)
table

# determine accuracy of KNN model
accuracy <- 100 * sum(test_y == knn_response_time) / length(test_y)
accuracy

# determine misclassification rate of KNN model (1 - accuracy)
misclassification_rate <- 100 * mean(test_y != knn_response_time)
misclassification_rate

# initialize variables
predicted_year_period <- NULL
error_rate <- NULL
accuracy_rate <- NULL

# calculate accuracy and misclassification rates for k = 1 to 20
for (i in 1:20) {
  set.seed(101)
  predicted_year_period <- knn(train_x, test_x, cl = train_y, k = i)
  error_rate[i] <- 100 * mean(test_y != predicted_year_period)
  accuracy_rate[i] <- 100 * sum(test_y == predicted_year_period) / length(test_y)
}

# get k values
k_values <- 1:20

# create data frame for error rates
error_df <- data.frame(error_rate, k_values)
error_df
# plot error rates
ggplot(error_df, aes(k_values, error_rate)) + geom_point() + geom_line(lty = 'dotted', color = 'cyan3')

# create data frame for accuracy rates
accuracy_df <- data.frame(accuracy_rate, k_values)
accuracy_df
# plot accuracy rates
ggplot(accuracy_df, aes(k_values, accuracy_rate)) + geom_point() + geom_line(lty = 'dotted', color = 'cyan3')

######################################
#### RESOURCES #######################
######################################

## http://www.sthda.com/english/articles/40-regression-analysis/163-regression-with-categorical-variables-dummy-coding-essentials-in-r/
## https://www.datatechnotes.com/2020/10/knn-regresion-example-in-r.html
## https://towardsdatascience.com/the-use-of-knn-for-missing-values-cf33d935c637#:~:text=Why%20using%20KNN%20%3F,all%20kind%20of%20missing%20data.
## https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html
