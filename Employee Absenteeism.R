#Clear R environment of all the variables
rm(list = ls())

#Set working directory

setwd("D:/Mohit")

 
getwd()

####Load required libraries
required_lib = c("rJava","xlsx","ggplot2","gridExtra","corrplot","tree","caret","randomForest","usdm")
install.packages(required_lib)
lapply(required_lib,require,character.only = TRUE)

#load data 
df = read.csv("Absenteeism_at_work_Project.csv",header = T)
View(df)

# Checking Data types and converting appropriate ones
str(df)

# Changing data types of following variable into category and rest as numerical
numeric = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
            'Work.load.Average.day', 'Transportation.expense',
            'Hit.target', 'Weight', 'Height', 
            'Body.mass.index', 'Absenteeism.time.in.hours')
category = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
             'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
             'Social.smoker', 'Son', 'Pet')
df$ID  = as.factor(df$ID)
df$Reason.for.absence = as.factor(df$Reason.for.absence)
df$Month.of.absence = as.factor(df$Month.of.absence)
df$Day.of.the.week = as.factor(df$Day.of.the.week)
df$Seasons = as.factor(df$Seasons)
df$Disciplinary.failure = as.factor(df$Disciplinary.failure)
df$Education = as.factor(df$Education)
df$Social.drinker = as.factor(df$Social.drinker)
df$Social.smoker = as.factor(df$Social.smoker)
df$Son = as.factor(df$Son)
df$Pet = as.factor(df$Pet)
df$Work.load.Average.day = as.integer(df$Work.load.Average.day)

str(df)
############## Missing Value Analysis #######################
## On viewing the dataset ,we see certain entries have 0 value which is not an acceptable entry logically .Hence we replace them as NA in those entries
for(i in c("Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Education","ID","Age","Weight","Height","Body.mass.index")){
  #print(i)
  df[i][(df[i] == 0)] = NA
}

## Calculate total sum of missing values column wise
missing_value = data.frame(apply(df,2,function(x){sum(is.na(x))}))
names(missing_value)[1] = "NA_Sum"
missing_value$NA_Percent = (missing_value$NA_Sum/nrow(df))*100



## We will check which imputation method suits best in our data set model
# Here we will use MODE method for imputation of categorical variable and Mean/Median method for continous variables
#data_check = df[11,]
#df[11,] = NA

## Let us impute missing values 
## Mean method for numerical data 
## Mode method for categorical data 
getmode = function(x){
  unique_x = unique(x)
  mode_val = which.max(tabulate(match(x,unique(x))))
}
## Mean method 
#Function : impute_mean_mode
#Parameter : dataset 
impute_mean_mode = function(data_set){
  for(i in colnames(data_set)){
    if(sum(is.na(data_set[,i]))!=0){
      if(is.numeric(data_set[,i])){
        
        data_set[is.na(data_set[,i]),i] = round(mean(data_set[,i],na.rm = TRUE))
      }
      else if(is.factor(data_set[,i])){
        
        data_set[is.na(data_set[,i]),i] = getmode(data_set[,i])
      }
      
    }
  }
  return(data_set)
}
## Median Method for numerical data
#Function : impute_mode_mode
#Parameter : dataset 
impute_median_mode = function(data_set){
  for(i in colnames(data_set)){
    if(sum(is.na(data_set[,i]))!=0){
      if(is.numeric(data_set[,i])){
        
        data_set[is.na(data_set[,i]),i] = median(data_set[,i],na.rm = TRUE)
      }
      
      
      else if(is.factor(data_set[,i])){
        
        data_set[is.na(data_set[,i]),i] = getmode(data_set[,i])
      }
      
    }
  }
  #print(data_set)
  return(data_set)
}


## Select best method 
#df = impute_mean_mode(df)
df = impute_median_mode(df)

# Re checking if missign Value is Present or not
missing_value = data.frame(apply(df,2,function(x){sum(is.na(x))}))
names(missing_value)[1] = "NA_Sum"
missing_value$NA_Percent = (missing_value$NA_Sum/nrow(df))*100


# Outlier Analysis
# BoxPlots - Distribution and Outlier Check

# Boxplot for continuous variables
for (i in 1:length(numeric))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (numeric[i]), x = "Absenteeism.time.in.hours"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteeism for",numeric[i])))
}

#Arrange the plots in grids
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)

# # here outliers are detected in continous variables, only 'height' and 'body mass index' are not having outliers
# other variable sare having outliers but we wont remove them ya substitute any value bcoz this outliers may be representing some extreme values of employees data which is important in deciding absenteeism time
# Now we wil analyse frequency distribution of a continous variable/ or we will plot histogram of a continous variable

bar1 = ggplot(data = df, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_bw()
bar2 = ggplot(data = df, aes(x = Reason.for.absence)) + geom_bar() + 
  ggtitle("Count of Reason for absence") + theme_bw()
bar3 = ggplot(data = df, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Count of Month") + theme_bw()
bar4 = ggplot(data = df, aes(x = Disciplinary.failure)) + geom_bar() + 
  ggtitle("Count of Disciplinary failure") + theme_bw()
bar5 = ggplot(data = df, aes(x = Education)) + geom_bar() + ggtitle("Count of Education") + theme_bw()
bar6 = ggplot(data = df, aes(x = Son)) + geom_bar() + ggtitle("Count of Son") + theme_bw()
bar7 = ggplot(data = df, aes(x = Social.smoker)) + geom_bar() + 
  ggtitle("Count of Social smoker") + theme_bw()
bar8 = ggplot(data = df, aes(x = Day.of.the.week)) + geom_bar() + 
  ggtitle("Count of Day of Week") + theme_bw()
bar9 = ggplot(data = df, aes(x = Seasons)) + geom_bar() + 
  ggtitle("Count of Season") + theme_bw()
bar10 = ggplot(data = df, aes(x = Social.drinker)) + geom_bar() + 
  ggtitle("Count of Social drinker") + theme_bw()
bar11 = ggplot(data = df, aes(x = Pet)) + geom_bar() + 
  ggtitle("Count of Pet") + theme_bw()
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)
gridExtra::grid.arrange(bar5,bar6,bar7,bar8,ncol=2)
gridExtra::grid.arrange(bar9,bar10,bar11,ncol=2)


# Analysis drawn from the factorplot
1.  Reason for absence 23 has highest count : Modical Consultation and then no. 28 : Dental Consultation
2.  Employee ID 3, 28, 34, 22 and 11 has highest number of absent counts : Company can give warning to this employee or terminate them to reduce absenteeism time
3.  March month has highest count which can be attributed to companies increment/review policy usually which results are declared in february and so many employees might be unhappy with review annual bonus and so they are coming late to office to show their anger/unhappiness
4. Monday has highest records in absenteeism time; which shows after spending weekend people feel lazy to come on monday and so they come late to office
5.  Person who is social drinker has more chances of absenteeism
6. Person who has only studies till high school is absent for most of time as compared to graduate/post graduate
7. Company in future must hire at least graduate candidate - who are responsible towards their work
8. Rest other category feature behaviour are random cant be explained by hyphothesis

# Now drawing Histogram for Continous variables

hist1 = ggplot(data = df, aes(x =Transportation.expense)) + 
  ggtitle("Transportation.expense") + geom_histogram(bins = 25)
hist2 = ggplot(data = df, aes(x =Height)) + 
  ggtitle("Distribution of Height") + geom_histogram(bins = 25)
hist3 = ggplot(data = df, aes(x =Body.mass.index)) + 
  ggtitle("Distribution of Body.mass.index") + geom_histogram(bins = 25)
hist4 = ggplot(data = df, aes(x =Absenteeism.time.in.hours)) + 
  ggtitle("Distribution of Absenteeism.time.in.hours") + geom_histogram(bins = 25)

hist5 = ggplot(data = df, aes(x =Weight)) + 
  ggtitle("Distribution of Weight") + geom_histogram(bins = 25)
hist6 = ggplot(data = df, aes(x =Hit.target)) + 
  ggtitle("Distribution of Hit.target") + geom_histogram(bins = 25)
hist7 = ggplot(data = df, aes(x = Work.load.Average.day)) + 
  ggtitle("Distribution of Work.load.Average.day") + geom_histogram(bins = 25)

gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)
gridExtra::grid.arrange(hist5,hist6,hist7,ncol=2)

## Correlation Plot
install.packages("corrgram")
library(corrgram)
corrgram(df[,numeric], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


# # We will remove weight variable from our dataset since it is highly correlated with BMI
df$Weight = NULL
numeric = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
            'Work.load.Average.day', 'Transportation.expense',
            'Hit.target', 'Height', 
            'Body.mass.index', 'Absenteeism.time.in.hours')
category = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
             'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
             'Social.smoker', 'Son', 'Pet')


# Copying the dataset into new dataframe and then going ahead with data normalisation
df1 = df


#Normalization of continuous variables
for(i in numeric){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i]) - min(df[,i]))
}


#Create dummy variables of factor variables
library(dummies)
library(mlr)
df = dummy.data.frame(df, category)



############## Model Building ###########
#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = sample(1:nrow(df), 0.7 * nrow(df))
train = df[ train.index,]
test  = df[-train.index,]


### Linear Regression

#Develop Model on training data
fit_LR = lm(Absenteeism.time.in.hours ~ ., data = train)

#Lets predict for training data
pred_LR_train = predict(fit_LR, train[,names(test) != "Absenteeism.time.in.hours"])

#Lets predict for testing data
pred_LR_test = predict(fit_LR,test[,names(test) != "Absenteeism.time.in.hours"])

# For training data 
print(postResample(pred = pred_LR_train, obs = train[,114]))

# For testing data 
print(postResample(pred = pred_LR_test, obs = test[,114]))



### Random Forest Regressor

set.seed(123)

#Develop Model on training data
fit_RF = randomForest(Absenteeism.time.in.hours~., data = train)

#Lets predict for training data
pred_RF_train = predict(fit_RF, train[,names(test) != "Absenteeism.time.in.hours"])

#Lets predict for testing data
pred_RF_test = predict(fit_RF,test[,names(test) != "Absenteeism.time.in.hours"])

# For training data 
print(postResample(pred = pred_RF_train, obs = train[,114]))

# For testing data 
print(postResample(pred = pred_RF_test, obs = test[,114]))



###################### Monthly loss for the Company####
loss_data = df1[,c("Month.of.absence","Work.load.Average.day","Service.time","Absenteeism.time.in.hours")]

loss_data$WorkLoss = round((loss_data$Work.load.Average.day/loss_data$Service.time)*loss_data$Absenteeism.time.in.hours)
View(loss_data)
monthly_loss = aggregate(loss_data$WorkLoss,by = list(Category = loss_data$Month.of.absence),FUN = sum)
names(monthly_loss) = c("Month","WorkLoss")








