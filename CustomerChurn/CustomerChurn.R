# ---
# title: "HarvardX Data Science Program"
# author: "Do Quang Anh"
# date: "`r format(Sys.Date())`"
# output:
#   pdf_document: 
#     number_sections: yes
#     fig_caption: yes
#     toc: yes
#     fig_height: 3
#     includes: null
#     latex_engine: lualatex
#     keep_tex: yes
#   html_document:
#     toc: yes
#     df_print: paged
#   word_document:
#     toc: yes
# subtitle: Predict customer churn in a bank
# email: mr.anhdq@gmail.com
# ---
# 
## ----Install Packages
# List of packages for session
.packages = c("tidyverse",       #tidy alvvays and forever!
              "corrplot",        #correlation plots
              "cowplot",         #solve x-axis misalignment when plotting, and better-looking defaults for ggplots
              "gridExtra",       #combine plots
              "knitr",           #report output
              "kableExtra",      #nice tables
              "lubridate",       #date math!
              "reshape2",        #acast to create matrix
              "scales",          #get rid of scientific notation in charts
              "splitstackshape",  #explode pipe-delimited data to one-hot encoded dummy variables
              "dplyr",
              "tm",
              "tmap",
              "wordcloud",
              "tinytex",
              "kableExtra",
              "tidyr",
              "stringr",
              "ggplot2",
              "gbm",
              "caret",
              "xgboost",
              "e1071",
              "class",
              "ROCR",
              "randomForest",
              "PRROC",
              "reshape2",
              "caTools",
              "Rtsne",
              "data.table",
              "ggmosaic",
              "ggthemes",
              "GGally",
              "vcd",
              "ROSE",
              "vip",
              "devtools"
              )


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)
tinytex::install_tinytex(force = TRUE)


## ----Functions and Hooks, include=FALSE----------------------------------------------------------------------------------
# Customize knitr output
#Set Thousands Separator for inline output
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ prettyNum(round(x,2), big.mark=",") } })
#we've already set the graphic device to "png" in the RMD options. the default device for pdfs draws every point of a scatterplot, creatinvg *very* big files.
#But png is not as crisp, so we will set a higher resolution for pdf output of plots. 
knitr::opts_chunk$set(dpi=150)
#Create Kable wrapper function for thousands separator in table output, and nice formating with kableExtra
niceKable = function(...) {
  knitr::kable(..., format.args = list(decimal.mark = '.', big.mark = ",")) %>% kable_styling(latex_options = "hold_position")
}


knitr::opts_chunk$set(echo = TRUE)

 
# # Introduction
# Customer churn is a significant issue for businesses across industries, and the banking industry is no exception. Customer churn refers to the phenomenon where customers stop doing business with a company, which can result in significant losses for the company. In the banking industry, churn can be caused by a variety of factors, such as poor customer service, high fees, or better offers from competitors.
# 
# In this project, we will explore a dataset of bank customers to predict customer churn using machine learning techniques. The dataset contains information about bank customers, including demographics, account information, and transaction history. Our goal is to use this data to build a model that can predict which customers are likely to churn in the future, allowing the bank to take proactive measures to retain these customers.
# 
# # About Dataset
# 
# The dataset we will use in this project is available on Kaggle. The dataset contains information about bank customers, including demographics, account information, and transaction history. The dataset contains 10,000 observations and 14 variables, including:
# 
# RowNumber: The row number.
# CustomerId: The customer ID.
# Surname: The surname of the customer.
# CreditScore: The credit score of the customer.
# Geography: The country of the customer.
# Gender: The gender of the customer.
# Age: The age of the customer.
# Tenure: The number of years the customer has been with the bank.
# Balance: The balance of the customer.
# NumOfProducts: The number of bank products the customer has.
# HasCrCard: Whether the customer has a credit card (1 = yes, 0 = no).
# IsActiveMember: Whether the customer is an active member (1 = yes, 0 = no).
# EstimatedSalary: The estimated salary of the customer.
# Exited: Whether the customer has churned (1 = yes, 0 = no).

# The dataset also includes a data dictionary that provides more detailed information about each variable. In the next section, we will explore the dataset in more detail and perform data cleaning to prepare it for analysis.

options(timeout = 120)

if(!file.exists("churn.csv"))
  download.file("https://raw.githubusercontent.com/doquanganh/HarvardX-Capstones/main/CustomerChurn/churn.csv", 'churn.csv')

df = read.csv("churn.csv")
tribble(
  ~"Dataset",     ~"Number of Rows",    ~"Number of Columns",
  #--             |--                   |----
  "Customer churn",   nrow(df),            ncol(df),
  
)%>%niceKable

 
# A preview of the data structure is shown below from the first few rows in data.
head(df)%>%niceKable


# show dimension, datatype, content of the data set
str(df)


# # Exploratory data analysis and data cleaning
# Let's see whether there is any missing data.
sapply(df, function(x) sum(is.na(x)))%>% niceKable
# There are no NA values in the data. Let see unique values for each attribute

df %>%  summarise_all(n_distinct)%>%niceKable

# Check for imbalance
impl<-data.frame(table(df$Exited))
colnames(impl) <- c('Exited','Count')
impl%>%niceKable


ggplot(df, aes(Exited, fill = factor(Exited),label = 'Exited')) + geom_bar(position = 'dodge') + ggtitle("Customer Churn") +
    geom_text(aes(label = percent( after_stat(count)/sum(after_stat(count)))), stat = "count", vjust = 1.5, colour = "white")+
    theme(legend.position = 'none')

# ## Distribution of class variables
# ### Gender
Gender_grap1<- ggplot(data = df, aes(x = Gender, fill = factor(Exited)),label = 'Gender') +
       geom_bar()+
       labs(x="Gender", y="Count",fill = "Exited")
      
Gender_grap2<- ggplot(df, aes(Gender, label = 'Gender', fill= Gender  )) +
    geom_bar(position = 'dodge') +
    scale_fill_hue(c = 40) +
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
    theme(legend.position = 'none')
plot_grid(Gender_grap1,Gender_grap2,labels = "Distribution of Gender")


# ### Geography
Geography_grap1<- ggplot(data = df, aes(x = Geography, fill = factor(Exited)),label = 'Geography') +
       geom_bar()+
      geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
       labs(x="Geography", y="Count",fill = "Churned")
      


Geography_grap2<- ggplot(df, aes(Geography, label = 'Gender', fill= Geography  )) +
    geom_bar(position = 'dodge') +
    scale_fill_hue(c = 40) +
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
    theme(legend.position = 'none')
plot_grid(Geography_grap1,Geography_grap2,labels = "Distribution of Geography")

# ### HasCrCard
HasCrCard_grap1<- ggplot(data = df, aes(x = HasCrCard, fill = factor(Exited)),label = 'HasCrCard') +
       geom_bar()+
        scale_x_continuous(breaks = c(0,1),
                       labels = c("0-N", "1-Y")) +
      geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
       labs(x="HasCrCard", y="Count",fill = "Churned")
      
HasCrCard_grap2<- ggplot(df, aes(HasCrCard, label = 'HasCrCard', fill= factor(HasCrCard)  )) +
    geom_bar(position = 'dodge') +
    scale_fill_hue(c = 40) +
    scale_x_continuous(breaks = c(0,1),
                 labels = c("0-N", "1-Y")) +
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
    theme(legend.position = 'none')
plot_grid(HasCrCard_grap1,HasCrCard_grap2,labels = "Distribution of HasCrCard")


# ### Is active member
IsActiveMember_grap1<- ggplot(data = df, aes(x = IsActiveMember, fill = factor(Exited)),label = 'IsActiveMember') +
        geom_bar()+
        geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
        scale_x_continuous(breaks = c(0,1),
                       labels = c("0-N", "1-Y")) +
       labs(x="IsActiveMember", y="Count",fill = "Churned")
      
IsActiveMember_grap2<- ggplot(df, aes(IsActiveMember, label = 'IsActiveMember', fill= factor(IsActiveMember)  )) +
    geom_bar(position = 'dodge') +
    scale_fill_hue(c = 40) +
    scale_x_continuous(breaks = c(0,1),
                   labels = c("0-N", "1-Y")) +  
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white")+
    theme(legend.position = 'none')
plot_grid(IsActiveMember_grap1,IsActiveMember_grap2,labels = "Distribution of IsActiveMember")

# ### Tenure
Tenure_grap1<- ggplot(data = df, aes(x = Tenure, fill = factor(Exited)),label = 'Tenure') +
        geom_bar()+
        scale_x_continuous(breaks = seq(0,10,by=1))+
       labs(x="Tenure", y="Count",fill = "Churned")
      
Tenure_grap2<- ggplot(df, aes(Tenure, label = 'Tenure', fill= factor(Tenure)  )) +
    geom_bar(position = 'dodge') +
    scale_fill_hue(c = 40) +
    
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white", size=2)+
    scale_x_continuous(breaks = seq(0,10,by=1))+
    theme(legend.position = 'none')

Tenure_grap3<-ggplot(data = df, aes(y = Tenure, fill = factor(Exited))) +
                      scale_y_continuous(breaks = seq(0,10,by=1))+
                      #scale_x_continuous(breaks = seq(0,1,by=1)) +
                                        geom_boxplot()

plot_grid(Tenure_grap1,Tenure_grap2,Tenure_grap3,labels = "Distribution of Tenure")


# - Customers at both poles (spending less time with the bank or more time with the bank) are more likely to leave than customers with average tenure

# ### NumOfProducts 
NumOfProducts_grap1<- ggplot(data = df, aes(x = NumOfProducts, fill = factor(Exited)),label = 'Num Of Products') +
        geom_bar()+
        geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white",size=2)+
       labs(x="Num Of Products", y="Count",fill = "Churned")
      
NumOfProducts_grap2<- ggplot(df, aes(NumOfProducts, label = 'Num Of Products', fill= factor(NumOfProducts))) +
    geom_bar(position = 'dodge') +
    scale_fill_hue(c = 40) +
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white",size=2)+
    theme(legend.position = 'none')

plot_grid(NumOfProducts_grap1,NumOfProducts_grap2,labels = "Distribution of NumOfProducts ")


# - Customers mainly use 1,2 products. customer using only one product are the most churned, however, customers using 3.4 products are more likely to leave. Pretty weird

# ## Distribution of Continuous Variables
# ### Age
age_hist1<- ggplot(df,aes(x=Age,fill = factor(Age)))+
          geom_histogram(aes(y=after_stat(density)), colour="black", fill="white",binwidth = 5)+
          geom_density(alpha=.2, fill="#FF6666") +
          theme_minimal() +
          theme(legend.position = 'none')


age_hist <- ggplot(df, aes(x = Age, fill = factor(Exited))) +
    geom_histogram(binwidth = 5,colour="black") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0,100,by=10), labels = comma)
age_boxplot <- ggplot(df, aes(x = Exited, y = Age, fill = factor(Exited))) +
    geom_boxplot() + 
    theme_minimal() +
    theme(legend.position = 'none')


plot_grid(age_hist1,age_boxplot,age_hist)


#  - Older customers are more likely to leave the bank

# ### Balance
Balance_hist1<- ggplot(df,aes(x=Balance,fill = factor(Balance)))+
          geom_histogram(aes(y=after_stat(density)),
                            breaks = seq(0,240000,by=10000),
                            alpha = .2,
                            colour="black", 
                            fill="white",
                            binwidth=30
                            ) +
          geom_density(alpha=.2, fill="#FF6666") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5))    


Balance_hist <- ggplot(df, aes(x = Balance, fill = factor(Exited))) +
                      geom_histogram(binwidth=20000,colour="black") +
                      theme_minimal() +
                      scale_x_continuous(breaks = seq(0,240000,by=20000), labels = comma) +
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  

Balance_boxplot <- ggplot(df, aes(x = Exited, y = Balance, fill = factor(Exited))) +
    geom_boxplot() + 
    theme_minimal() +
    theme(legend.position = 'none')


plot_grid(Balance_hist1,Balance_boxplot,Balance_hist)


# - Customers with large balances tend to leave the bank more than the remaining customers
# ### CreditScore
CreditScore_hist1<- ggplot(df,aes(x=CreditScore,fill = factor(CreditScore)))+
          geom_histogram(aes(y=after_stat(density)),
                            alpha = .2,
                            colour="black", 
                            fill="white",
                            binwidth=30
                            ) +
          geom_density(alpha=.2, fill="#FF6666") +
          theme_minimal() +
          #scale_x_continuous(breaks = seq(0,255000,by=1000), labels = comma) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5))    


CreditScore_hist <- ggplot(df, aes(x = CreditScore, fill = factor(Exited))) +
                      geom_histogram(binwidth=30,colour="black") +
                      theme_minimal() +
                      #scale_x_continuous(breaks = seq(0,240000,by=20000), labels = comma) +
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  

CreditScore_boxplot <- ggplot(df, aes(x = Exited, y = CreditScore, fill = factor(Exited))) +
    geom_boxplot() + 
    theme_minimal() +
    theme(legend.position = 'none')


plot_grid(CreditScore_hist1,CreditScore_hist,CreditScore_boxplot)

# - The majority of customers have credit scores above 600. We find that customers with low credit scores below 400 are customers who leave the bank

# ### EstimatedSalary
EstimatedSalary_hist1<- ggplot(df,aes(x=EstimatedSalary,fill = factor(EstimatedSalary)))+
          geom_histogram(aes(y=after_stat(density)),
                            alpha = .2,
                            colour="black", 
                            fill="white",
                            binwidth=10000
                            ) +
          geom_density(alpha=.2, fill="#FF6666") +
          theme_minimal() +
          #scale_x_continuous(breaks = seq(0,255000,by=1000), labels = comma) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5))    


EstimatedSalary_hist <- ggplot(df, aes(x = EstimatedSalary, fill = factor(Exited))) +
                      geom_histogram(binwidth=10000,colour="black") +
                      theme_minimal() +
                      #scale_x_continuous(breaks = seq(0,240000,by=20000), labels = comma) +
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  

EstimatedSalary_boxplot <- ggplot(df, aes(x = Exited, y = EstimatedSalary, fill = factor(Exited))) +
    geom_boxplot() + 
    theme_minimal() +
    theme(legend.position = 'none')


plot_grid(EstimatedSalary_hist1,EstimatedSalary_hist,EstimatedSalary_boxplot)


# - Looking at the chart, we see the same distribution of wages in both leaving and remaining customers. Therefore, this variable does not have much influence on whether customers leave or stay

# ## Correlations between each variables
#Correlations
dfcorr <- df %>%select_if(is.numeric)

correlations <- cor(dfcorr[,], method='spearman')
round(correlations, 2)

corrplot(correlations, number.cex = .65, type = "full",
              method = "color", 
              tl.cex=0.8,
              tl.col = "black",
              addCoef.col = 'grey',
              cl.cex = 0.8,
              order = 'AOE'
           )

# - Churn has a positive correlation with age, balance. Generally the correlation coefficients are not so high.
# - Balance attribute is negatively correlated with numberofproducts attribute
# 
# # Data Preprocessing & Modeling
# 
# ## Data Cleansing & Engenerring
# 
# ### Remove unused rows with RowNumber, CustomerId, Surname
library(ROSE)
library(caret)

#Remove unused rows with RowNumber, CustomerId, Surname
df_clean <- df[,4:14]
#For all columns in df_clean that are of type character, convert them to factor columns
df_clean <-  df_clean %>% mutate_if(is.character, as.factor)

# Convert the Exited and HasCrCard columns to factor columns
df_clean$Exited <- as.factor(df_clean$Exited)
df_clean$HasCrCard <- as.factor(df_clean$HasCrCard)

# Print a summary of the structure of df_clean
str(df_clean)


# ###  One hot encoding Geography and Gender dataframe
# We saw that Geography and Gender columns housed categorical values, so we need to change that as Machine Learning Models take only numerical values

###  one hot encoding Geography and Gender dataframe
# Create a new data frame called df_clean_onehot that is a copy of df_clean
df_clean_onehot <- df_clean
# Remove the Geography and Gender columns from df_clean_onehot
df_clean_onehot$Geography <- NULL
df_clean_onehot$Gender <- NULL

# Create a new data frame called df_clean_geo that contains only the Geography column from df_clean
df_clean_geo <- data.frame(df_clean$Geography)
# Rename the column in df_clean_geo to "Geography"
colnames(df_clean_geo) <- c("Geography")

# Create a new data frame called df_clean_Gender that contains only the Gender column from df_clean
df_clean_Gender <- data.frame(df_clean$Gender)
# Rename the column in df_clean_Gender to "Gender"
colnames(df_clean_Gender) <- c("Gender")

# Create dummy variables for the Geography column using the dummyVars() and predict() functions
dmy_geo  <- dummyVars("~.", data = df_clean_geo)
df_geo_dummy <- data.frame(predict(dmy_geo, newdata = df_clean_geo))

# Create dummy variables for the Gender column using the dummyVars() and predict() functions
dmy_gender <- dummyVars("~.", data = df_clean_Gender)
df_gender_dummy <- data.frame(predict(dmy_gender, newdata = df_clean_Gender))

# Combine the dummy variables for Geography and Gender with df_clean_onehot to create a new data frame called df_after_dummy
df_after_dummy <- cbind(df_geo_dummy,df_gender_dummy,df_clean_onehot)

# Print a summary of the structure of df_after_dummy
str(df_after_dummy)

# 
# ### Imbalance data handling
# Set the random seed to 1234 for reproducibility
set.seed(1234)

# Create a new data frame called tbl_bf_ibl that shows the frequency count of each value in the Exited column of df_after_dummy
tbl_bf_ibl <-data.frame(table(df_after_dummy$Exited))
# Rename the columns in tbl_bf_ibl to "Exited" and "Count"
colnames(tbl_bf_ibl) <- c('Exited','Count')

# Print tbl_bf_ibl in a nicely formatted table using the kbl(), kable_classic(), and kable_styling() functions
tbl_bf_ibl%>%kbl(caption = "Before imbalance data handling")%>%kable_classic(full_width = T, html_font = "Aria") %>% kable_styling(latex_options = "hold_position")

#The data is not imbalance, need to imbalance data handling
# Use the ovun.sample() function to create a new data set called data_balanced with balanced classes
data_balanced <- ovun.sample(Exited ~ ., data = df_after_dummy, method = "both", p=0.5, N=10000, seed = 1)$data

# Create a new data frame called tbl_at_ibl that shows the frequency count of each value in the Exited column of data_balanced
tbl_at_ibl <-data.frame(table(data_balanced$Exited))
colnames(tbl_at_ibl) <- c('Exited','Count')

# Print tbl_at_ibl in a nicely formatted table using the kbl(), kable_classic(), and kable_styling() functions
tbl_at_ibl%>%kbl(caption = "After imbalance data handling")%>%kable_classic(full_width = T, html_font = "Aria")  %>% kable_styling(latex_options = "hold_position")
      


# 
# ### Training and Testing Split
# calculates the number of rows in the data_balanced data frame and assigns the result to the row variable.
row <- dim(data_balanced)[1]
# randomly samples 70% of the total number of rows in data_balanced and assigns the indices of the selected rows to the variable train_idx.
train_idx <- sample(row, row * 0.7) 
#selects the rows in data_balanced that correspond to the indices stored in train_idx
training_df <- data_balanced[train_idx,]
#Selects all rows in data_balanced except for those with the indices stored in train_idx
testing_df <- data_balanced[-train_idx,]

rm(train_idx)

# ## Modeling
# ### RandomForest
# Select the "Exited" column from the "testing_df" data frame and assign it to "rf_answer" variable.
rf_answer <- testing_df$Exited

# Fit a random forest model using the "randomForest()" function to the "training_df" data frame.
# The formula argument "Exited~." specifies that "Exited" is the response variable and all other columns are the predictor variables.
# Assign the fitted model to "rf".
rf <- randomForest(formula = Exited~., data = training_df)

# Use the "predict()" function to make predictions using the fitted model "rf" on the "testing_df" data frame.
# Assign the predicted values to "pred.rf".
pred.rf <- predict(rf, newdata = testing_df)

#confusionMatrix
# Compute the confusion matrix using the "confusionMatrix()" function from the "caret" package.
# The argument "positive" is set to "1" because we are interested in the accuracy of predicting customers who exit the bank.
cm <- caret::confusionMatrix(pred.rf, testing_df$Exited, positive = "1")

# Extract the overall accuracy from the confusion matrix and assign it to "rf_acc" variable.
rf_acc <- cm$overall['Accuracy']
rf_acc <-  unname(rf_acc)

# Compute the AUC and AUPCR
pred <- prediction(as.numeric(as.character(pred.rf)),as.numeric(as.character(testing_df$Exited)))

#Compute the area under the ROC curve (AUC)
auc_rf <- performance(pred, "auc")
#Compute the area under the precision-recall curve (AUCPR)
aucpr_rf <- pr.curve(scores.class0 = pred.rf[testing_df$Exited == 1], scores.class1 = pred.rf[testing_df$Exited == 0],curve = T,  dg.compute = T)

#Compute the ROC and PR curves
auc_plot_rf <- performance(pred, 'sens', 'spec')
aucpr_plot_rf <- performance(pred, "prec", "rec", curve = T,  dg.compute = T)

# make the ROC and PR plots
plot(auc_plot_rf, main=paste("AUC:", auc_rf@y.values[[1]]))
plot(aucpr_plot_rf, main=paste("AUCPR:", aucpr_rf$auc.integral))
plot(aucpr_rf)

# results comparisons
results_comparisons  <- data.frame(
  Model = "Random Forest",
  Accuracy = rf_acc,
  AUC = auc_rf@y.values[[1]],
  AUCPR = aucpr_rf$auc.integral)
# Show results in a nice table format
results_comparisons %>% niceKable


# ### KNN
# Set seed 1234 for reproducibility
set.seed(1234)
# Build a KNN Model with Exited as Target and all other variables as predictors. k is set to 5
knn <- knn(training_df[,-30], testing_df[,-30], training_df$Exited, k=5, prob = TRUE)

#Compute the confusion matrix
cm_knn <- caret::confusionMatrix(knn, testing_df$Exited, positive = "1")

#Extract the accuracy from the confusion matrix
knn_acc <- cm_knn$overall['Accuracy']
knn_acc <-  unname(knn_acc)


# Compute the AUC and AUCPR for the KNN Model
pred <- prediction(
  as.numeric(as.character(knn)), as.numeric(as.character(testing_df$Exited))
)
auc_knn <- performance(pred, "auc") # calculate AUC from prediction object
auc_p_knn <- performance(pred, 'sens', 'spec') # calculate AUC based on sensitivity and specificity
aucpr_p_knn <- performance(pred, "prec", "rec") # calculate AUCPR (AUC for Precision-Recall curve) based on precision and recall

# calculate AUCPR based on predicted values and test set values, considering class 0 as negative and class 1 as positive
aucpr_knn <- pr.curve(
  scores.class0 = knn[testing_df$Exited == 1], 
  scores.class1 = knn[testing_df$Exited == 0],
  curve = T,  
  dg.compute = T
)
# Make the relative plot
plot(aucpr_knn)
# Make the relative plot for AUC
plot(auc_p_knn, main=paste("AUC:", auc_knn@y.values[[1]]))
# Make the relative plot for AUCPR
plot(aucpr_p_knn, main=paste("AUCPR:", aucpr_knn$auc.integral))
# Adding the respective metrics to the results dataset
results_comparisons <- results_comparisons %>% add_row(
  Model = "K-Nearest Neighbors k=5", 
  Accuracy = knn_acc,
  AUC = auc_knn@y.values[[1]],
  AUCPR = aucpr_knn$auc.integral
)
# Show results
results_comparisons %>% niceKable


# 
# ### Naive Algorithm
# 
# Set seed 1234 for reproducibility
set.seed(1234)
#Builds a Naive Bayes model with the target variable 'Exited' and all other variables in the dataset as predictors. The Laplace smoothing is set to 1 to avoid zero probabilities.
naive <- naiveBayes(Exited ~ ., data = training_df, laplace=1)

#Uses the predict() function to generate predictions for the test dataset using the Naive Bayes model that was trained on the training dataset.
nv_predictions <- predict(naive, newdata=testing_df)

#computes the accuracy of the Naive Bayes model. The confusionMatrix() function from the caret package is used to compute the confusion matrix, and the accuracy is extracted from it.
cm_nv <- caret::confusionMatrix(nv_predictions, testing_df$Exited, positive = "1")
nv_acc <- cm_nv$overall['Accuracy']
nv_acc <-  unname(nv_acc)

# Compute the AUC and AUCPR for the Naive Model
pred <- prediction(as.numeric(nv_predictions), testing_df$Exited)
auc_naive <- performance(pred, "auc")
auc_p_naive <- performance(pred, 'sens', 'spec')
aucpr_p_naive <- performance(pred, "prec", "rec")
aucpr_naive <- pr.curve(
  scores.class0 = nv_predictions[testing_df$Exited == 1], 
  scores.class1 = nv_predictions[testing_df$Exited == 0],
  curve = T,  
  dg.compute = T
)
# Make the relative plot
plot(aucpr_naive)
plot(auc_p_naive, main=paste("AUC:", auc_naive@y.values[[1]]))
# Make the relative plot for AUCPR
plot(aucpr_p_naive, main=paste("AUCPR:", aucpr_naive$auc.integral))
# Create a dataframe 'results' that contains all metrics 
# obtained by the trained models
results_comparisons <- results_comparisons %>% add_row(
  Model = "Naive Bayes", 
  Accuracy = nv_acc,
  AUC = auc_naive@y.values[[1]],
  AUCPR = aucpr_naive$auc.integral
)
# Show results
results_comparisons %>% niceKable

# ### XGBoost
## Create a control object
#Create a control object using 10-fold cross-validation
ctrl <-
    trainControl(method = "cv",
                 number = 10,
                 selectionFunction = "best") # selects the best model based on a given performance metric
#Check the parameters available for the xgbTree model
modelLookup("xgbTree")

#Create a grid of hyperparameters to search over
grid <- expand.grid(
    nrounds = 40, # number of iterations (or trees)
    max_depth = c(4,5,6,7,8), # maximum depth of each tree
    eta =  c(0.1,0.2,0.3,0.4,0.5), # learning rate, controls the shrinkage of the weights
    gamma = 0.01, # minimum loss reduction to make a split
    colsample_bytree = 1, # subsampling of columns used per tree
    min_child_weight = 1,  # minimum sum of instance weight needed in a child 
    subsample = c(0.5, 1) # subsampling of the training set
)


##Train an XGBoost model using the training data
set.seed(1234) # set a seed to make the results reproducible
xgb.mod <-
    train(
        Exited ~ ., # formula to be trained
        data = training_df, # data to be used
        method = "xgbTree", # specify the XGBoost tree method
        metric = "Kappa", # performance metric
        trControl = ctrl, # cross-validation control object
        tuneGrid = grid # grid of hyperparameters to be searched
    )

#Make predictions using the XGBoost model
xgb.pred <- predict(xgb.mod, testing_df, type = "raw") # predict the test set using the XGBoost model
xgb.pred.prob <- predict(xgb.mod, testing_df, type = "prob") # predict the test set probabilities using the XGBoost model



#Compute the confusion matrix for the XGBoost model using the predicted labels and the actual labels in the testing data. 'positive = "1"' specifies the positive class as 1 (Exited).
cm_xgb <- caret::confusionMatrix(xgb.pred, testing_df$Exited, positive = "1")
#Compute the overall accuracy of the XGBoost model by extracting the "Accuracy" metric from the confusion matrix.
xgb_acc <- cm_xgb$overall['Accuracy']
xgb_acc <-  unname(xgb_acc)

#Computing the area under the ROC curve (AUC) and area under the precision-recall curve (AUCPR) using the predicted probabilities and actual target variable values
pred <- prediction(as.numeric(as.character(xgb.pred)),as.numeric(as.character(testing_df$Exited))) # creating a prediction object using the predicted values and actual values

auc_val_xgb <- performance(pred, "auc") # computing the AUC
auc_plot_xgb <- performance(pred, 'sens', 'spec') # creating a ROC curve
aucpr_plot_xgb <- performance(pred, "prec", "rec") # creating a precision-recall curve
aucpr_val_xgb <- pr.curve(
  scores.class0 = xgb.pred[testing_df$Exited == 1], 
  scores.class1 = xgb.pred[testing_df$Exited == 0],
  curve = T,  
  dg.compute = T
) # computing the AUCPR

# Make the relative plot
plot(auc_plot_xgb, main=paste("AUC:", auc_val_xgb@y.values[[1]]))
plot(aucpr_plot_xgb, main=paste("AUCPR:", aucpr_val_xgb$auc.integral))
plot(aucpr_val_xgb)

# Add the respective metrics to the results dataset
results_comparisons <- results_comparisons %>% add_row(
  Model = "XGBoost",
  Accuracy = xgb_acc,
  AUC = auc_val_xgb@y.values[[1]],
  AUCPR = aucpr_val_xgb$auc.integral)
#Show the results on a table
results_comparisons %>% niceKable



# # Result
# ## Compare model perfomance
# Formatted table
results_comparisons %>%
    kbl(caption = "Comparison Table") %>%
    kable_classic(html_font = "Aria") %>%
    row_spec(4:4, bold = T, color = "white", background = "green")

# This is a summary table of customer churn prediction results of different models. We see that the XGBoost model has the highest results with Accuracy = 0.918, AUC = 0.9182 , AUCPR= 0.8882
# 
# ## Feature Importance
# Feature Importance

#First, calculate the feature importance using the vip package for the random forest model and store it in feature_imp_rf
feature_imp_rf<- vip::vip(rf, aesthetics = list(colour="black", fill="lightblue"))+ ggtitle("Random Forest")

#Second, calculate the feature importance using the vip package for the XGBoost model and store it in feature_imp_xgb
feature_imp_xgb<-vip::vip(xgb.mod, aesthetics = list(colour="black", fill="green"))+ ggtitle("XGBoost")

#Arrange the two plots side by side using grid.arrange and store it in a variable called "grid"
grid.arrange(feature_imp_rf, feature_imp_xgb, ncol = 2)


# - We only compare the feature important in the two models with the highest scores, Random Forest and XGBoost
# - The features Importance  of the two models are quite similar
# - Although there are more customers from France but customers from Germany have more influence on the model
# 
# # Conclusion
# During our data analysis, we discovered that female customers are the most likely to leave, customers in Germany are most likely to leave, and customers who use only one product are also most likely to leave.After building some models, we find that the Random Forest and XGBoost models are more accurate than others. XGBoost is the model with the highest accuracy of ~92% and AUCPR~90%.
# However, the data from Kaggle is only a small set, so the model can achieve better performance when providing more historical data for the training phase.
# 
