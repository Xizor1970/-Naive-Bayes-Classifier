# Author: Hendrik A. Dreyer

#Read mushroom data from web and assign proper names to columns
Mushrooms <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", 
                      header=FALSE, 
                      sep=",", 
                      dec=".", 
                      na.strings=c("?"),
                      col.names = c("class", 
                                    "cap_shape",                # - 1
                                    "cap_surface",              # - 2 
                                    "cap_color",                # - 3
                                    "bruises",                  # - 4
                                    "odor",                     # - 5
                                    "gill_attachment",          # - 6
                                    "gill_spacing",             # - 7
                                    "gill_size",                # - 8
                                    "gill_color",               # - 9
                                    "stalk_shape",              # - 10
                                    "stalk_root",               # - 11
                                    "stalk_surface_above_ring", # - 12
                                    "stalk_surface_below_ring", # - 13
                                    "stalk_color_above_ring",   # - 14
                                    "stalk_color_below_ring",   # - 15
                                    "veil_type",                # - 16
                                    "veil_color",               # - 17
                                    "ring_number",              # - 18
                                    "ring_type",                # - 19
                                    "spore_print_color",        # - 20
                                    "population",               # - 21
                                    "habitat"))                 # - 22

#Summerize
summary(Mushrooms)

#Determine dimensions of data set
set.seed(0) 
no_observations <- dim(Mushrooms)[1]   # No. observations (8124) 
no_predictors <- dim(Mushrooms)[2] - 1 # No. predictors (22) = No. variables (23) - dependent var. (1st column) 
test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE)  # 20% data for test 
training_index <- -test_index          # Remaining 80% data observations for training

#############################################
#Initialise variables and setup error matrix#
#############################################
library(naivebayes) 

error <- 0 
accuracy <- 0
fm_string <- "class ~ "
fm_string_start <- "class "
iteration_idx = 0

features <- subset(Mushrooms, select = -c(class))

df_errors <- data.frame(matrix(ncol = 4, nrow = 0))
x_names <- c("features_no", "features_name", "error", "no")
colnames(df_errors) <- x_names

#############################################################
# Function: calc_classifier_error                           #
# Descrtiption: 1) Takes in a formula string as param       #
#               2) Create 10 random training and test sets  #
#               3) Calculate 10 prediction errors           #
#               4) Returns the average calculation error    #
#############################################################
calc_classifier_error <- function(fm_string){
  error <- 0
  for (i in 10){   
    #Determine test and training indexes
    test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE)  # 20% data for test   
    training_index <- -test_index # Remaining 80% data observations for training   
    
    #Create the formula for the feature in question
    fm <- as.formula(fm_string) 
    
    #Create the classifier based on the training index
    NaiveBayesModel <- naive_bayes(fm , data = Mushrooms[training_index, ])   
    
    #Do the prediction based on the test index
    Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ])   
    
    #Create the prediction table
    tab <- table(Pred_class, Mushrooms[test_index,"class"])   
    
    #Calculate the accuracy numbers
    accuracy <- sum(diag(tab))/sum(tab)   
    error <- error + (1 - accuracy) 
  } 
  #Average out
  error <- error/10
  return (error)
}

#########################################################################################
# 1 - Determine the initial list of the features' individual avg. classification errors #
# Plot the individual avg. erros for each feature                                       #
#########################################################################################
for(j in 1:length(names(features))) {
  print("Feature:") 
  print(names(features)[j])
  print("")
  
  #Add next feature to formula 
  fm_string <- ""
  fm_string <- paste(fm_string_start,names(features)[j], sep = " ~ ")
  
  print("Formula to be applied:")
  print(fm_string)  
  print("")  
  
  error <- calc_classifier_error(fm_string)
  
  print("Avg. Error:")
  print(error)
  
  df_errors[j,1] <- as.numeric(j)           #feature_no
  df_errors[j,2] <- names(features)[j]      #Feature_name
  df_errors[j,3] <- error                   #error

  error <- 0 
  accuracy <- 0
  print("--------------------------------------------")
}

#Plot a bar plot of the error df
library(tidyverse)

#Plot descending order for Avg. Classification Error
ggplot(data = df_errors, aes(y = (df_errors$error*100), x = df_errors$features_no)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(df_errors$error*100,2), "%")), vjust = 0.25, hjust = -0.10, size=3, srt = 90) +
  labs(x = "Predictor # used for NB classifier", y = "Avg. Classification Error (%)") +
  ggtitle("Classification errors for each individual feature") +
  scale_x_continuous(breaks = c(1:22))

################################################################
# 2 - Iterate all 22 features and build subset |S| = 1,2,...,n #
# Start with initial list of ordered feature avg. errors       #
# Plot the rsulting subsets
################################################################

#Order list of features - based on individual avg. classification error
df_errors_ordered <- df_errors[order(df_errors$error),]

#Copy first feature over to final df
df_final <- df_errors_ordered[1,]

#Removed the copied feature
df_errors_ordered <- df_errors_ordered[-1,]

print("Iteration: 1")
subset_string_x <- paste("class ~", df_final$features_name[1])
print(paste("Subset(S): ", subset_string_x))
print(paste("Avg. Error: ", df_final[1,]$error, " (", round(df_final[1,]$error*100, 2), "%)"))
print("----------------------------------")

#Iterate df_errors_ordered and process every feature
counter <- 2
while(length(df_errors_ordered$features_no) > 0){
#for(ii in 1:5){

  for(zz in 1:length(df_errors_ordered$features_no)){  
    #Build base string from df_final
    base_string <- ""
    base_string <- paste("class ~ ", df_final$features_name[1])
    
    if(length(df_final$features_name) > 1){
      for(rr in 2:length(df_final$features_name)){
        base_string <- paste(base_string, " + ", df_final$features_name[rr])    
      }    
    }  
    
    #Add first feature from df_errors_ordered to base_string
    base_string <- paste(base_string, " + ", df_errors_ordered$features_name[zz])
    #print(base_string)
    iter_err <- calc_classifier_error(base_string)
    #print(iter_err)
    df_errors_ordered$error[zz] <- iter_err
  }
  
  #order the newly acquired errors in df_error_ordered
  df_errors_ordered <- df_errors_ordered[order(df_errors_ordered$error),]  
  
  #Copy top element (smallest error) to df_final
  df_dummy <- df_errors_ordered[1,]
  df_final <- rbind(df_final, df_dummy)
  
  #Removed the copied feature
  df_errors_ordered <- df_errors_ordered[-1,]  
  
  #Best subset(S) aftyer iteration k
  print(paste("Iteration: ", counter))
  
  subset_string <- ""
  #if(counter==1){
  #  subset_string <- paste("class ~", df_final$features_name[1])
  #}
  #else {
    subset_string <- paste("class ~", df_final$features_name[1])
    for(tt in 2:counter){
      subset_string <- paste(subset_string, " + ", df_final$features_name[tt])  
    }    
  #}

  print(paste("Subset(S): ", subset_string))
  print(paste("Avg. Error: ", df_dummy$error, " (", round(df_dummy$error*100, 2), "%)"))
  print("----------------------------------")
        
  counter <- counter + 1
}

#Add ordered index to df_final
for(p in 1:length(df_final$features_name)){
  df_final$no[p] = p
}

#Plot descending order for Avg. Classification Error
ggplot(data = df_errors, aes(y = df_final$error*100, x = df_final$no)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(df_final$error*100,2), "%")), vjust = -0.5, size=2) +
  labs(x = "Number of features added to NB classifier", y = "Avg. Classification Error (%)") +
  ggtitle("Classification as function of feature subset size |S| = 1,...,n") +
  scale_x_continuous(breaks = c(1:22))
