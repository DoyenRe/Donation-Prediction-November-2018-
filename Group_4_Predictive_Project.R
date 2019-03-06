#Goal: Which donors are most likely to donate more than EUR 35 for a re-activation campaign in March 2015?
#Target: Donors likely to donate more than 35 EUR on a future reactivation campaign

install.packages("sqldf")
library(sqldf)
install.packages("lubridate")
library(lubridate)
install.packages("pROC")
library(pROC)
install.packages("randomForest")
library(randomForest)
install.packages("ROCR")
library(ROCR)
#for discretization, uncomment to change that commented code on lines 641, 661, 676
#install.packages("arules")
#library(arules)

######################### Set Working directory for the project ############################################
setwd("C:/Users/ZashtotoMoga/Documents/IESEG/Descriptive and Predictive/Group Project Data/")

####################################### Read data sources###################################################

# Read the data:
gifts = read.table("gifts.csv",sep=";",header = TRUE)
# Inspect the data:
# Show the first lines
# Show the fields
names(gifts)
head(gifts)

# Read the data:
donors = read.table("donors.csv",sep=";",header = TRUE)
# Inspect the data:
# Show the first lines
head(donors)
# Show the fields
names(donors)

# Read the train data:
C2013_train = read.table("Campaign20130411.csv",sep=";",header = TRUE)
# Inspect the data:
# Show the first lines
head(C2013_train)
# Show the fields
names(C2013_train)

# Read the test data:
C2014_test = read.table("campaign20140115.csv",sep=";",header = TRUE)
# Inspect the data:
# Show the first lines
head(C2014_test)
# Show the fields
names(C2014_test)

####################################### End of reading data sources########################################

# Custom function to calculate AUC (from class): 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

##################################### Data Manipulation for Dummy Feature creation ########################################

#Remove five donors who have not donated (not present in gifts data thus not enough info about them)

# sqldf('SELECT donorID
#       FROM donors
#       EXCEPT
#       SELECT donorID
#       FROM gifts')

donors <- donors[-c(462, 2021, 4130, 9562, 26862),]

#Initiate a basetable dataframe with the variables to test
basetable <- data.frame(donorID = donors$donorID)

#Create male dummy variable
basetable$d_Male[donors$gender == "M"] <- 1
basetable$d_Male[donors$gender != "M"] <- 0

#Create female dummy variable
basetable$d_Female[donors$gender == "F"] <- 1
basetable$d_Female[donors$gender != "F"] <- 0

#Create company dummy variable
basetable$d_Company[donors$gender == "C"] <- 1
basetable$d_Company[donors$gender != "C"] <- 0

#Create gender_U dummy variable
basetable$d_gender_u[donors$gender == "U"] <- 1
basetable$d_gender_u[donors$gender != "U"] <- 0

#Create language dummy variable
basetable$d_French[donors$language == "F"] <- 1
basetable$d_French[donors$language != "F"] <- 0

#Create region dummy variable
basetable$d_region[donors$region == "Missing"] <- 0
basetable$d_region[donors$region != "Missing"] <- 1


############################################################################################################################################################

#Add dummy variable to group by district

zipcode <- sqldf("SELECT distinct donorID, zipcode
                 FROM donors
                 GROUP BY 1")

#Extract the first two numbers from the zipcode to group the zipcodes into provinces
zipcode$twodigitscode <- substr(zipcode$zipcode,0,2)
basetable <- merge(x = basetable, y = zipcode, by = "donorID", all.x = TRUE)

basetable$twodigitscode <- as.numeric(basetable$twodigitscode)
str(basetable)

#Assign district based on the zipcodes
basetable$District <- ifelse((basetable$twodigitscode>=10)&(basetable$twodigitscode<13),'Brussels',
                             ifelse((basetable$twodigitscode>=13)&(basetable$twodigitscode<15),'Brabant Wallon',
                                    ifelse(((basetable$twodigitscode>=15)&(basetable$twodigitscode<20)|(basetable$twodigitscode>=30)&(basetable$twodigitscode<35)),'Vlaams Brabant',
                                           ifelse((basetable$twodigitscode>=20)&(basetable$twodigitscode<30),'Antwerpen',
                                                  ifelse((basetable$twodigitscode>=30)&(basetable$twodigitscode<40),'Limburg',
                                                         ifelse((basetable$twodigitscode>=40)&(basetable$twodigitscode<50),'Liège',
                                                                ifelse((basetable$twodigitscode>=50)&(basetable$twodigitscode<60),'Namur',
                                                                       ifelse((basetable$twodigitscode>=66)&(basetable$twodigitscode<70),'Luxembourg',
                                                                              ifelse((basetable$twodigitscode>=80)&(basetable$twodigitscode<90),'Oost Vlaanderen',
                                                                                     ifelse((basetable$twodigitscode>=90)&(basetable$twodigitscode<100),'West Vlaanderen',
                                                                                            'Hainaut'))))))))))
table(basetable$District)

#Assign missing and zeros to the most popular district (Antwerpen) district
which(is.na(basetable$twodigitscode)) #Three obs have problems with ZIPCODE, 2 have NA and 1 zipcode = 0.
basetable$District[is.na(basetable$twodigitscode)] <- 25
basetable$District[basetable$twodigitscode == 0] <- 25
which(is.na(basetable$District))

#Assign dummy variables to the districts
basetable$d_Brussels[basetable$District == 'Brussels'] <- 1
basetable$d_Brussels[basetable$District != 'Brussels'] <- 0

basetable$d_BrabantWallon[basetable$District == 'Brabant Wallon'] <- 1
basetable$d_BrabantWallon[basetable$District != 'Brabant Wallon'] <- 0

basetable$d_VlaamsBrabant[basetable$District == 'Vlaams Brabant'] <- 1
basetable$d_VlaamsBrabant[basetable$District != 'Vlaams Brabant'] <- 0

basetable$d_Antwerpen[basetable$District == 'Antwerpen'] <- 1
basetable$d_Antwerpen[basetable$District != 'Antwerpen'] <- 0

basetable$d_Limburg[basetable$District == 'Limburg'] <- 1
basetable$d_Limburg[basetable$District != 'Limburg'] <- 0

basetable$d_Liege[basetable$District == 'Liège'] <- 1
basetable$d_Liege[basetable$District != 'Liège'] <- 0

basetable$d_Namur[basetable$District == 'Namur'] <- 1
basetable$d_Namur[basetable$District != 'Namur'] <- 0

basetable$d_Luxembourg[basetable$District == 'Luxembourg'] <- 1
basetable$d_Luxembourg[basetable$District != 'Luxembourg'] <- 0

basetable$d_OostVlanderen[basetable$District == 'Oost Vlaanderen'] <- 1
basetable$d_OostVlanderen[basetable$District != 'Oost Vlaanderen'] <- 0

basetable$d_WestVlaanderen[basetable$District == 'West Vlaanderen'] <- 1
basetable$d_WestVlaanderen[basetable$District != 'West Vlaanderen'] <- 0


#Remove intermediate variables
basetable[,c("zipcode", "District", "twodigitscode")] <- NULL

head(basetable)

############################################################################################################################################################

#Filter the gifts data to exclude the dates outside the 2013 campaign date
#First convert date variable from factor to date
class(gifts$date)
gifts$date_date <- as.Date(as.character(gifts$date), format="%d/%m/%Y")
class(gifts$date_date)

#Fill a column in the dataframes to be able to compare with SQL
gifts$c2013_date <- as.integer(as.Date("11/04/2013", format="%d/%m/%Y"))
gifts$c2014_date <- as.integer(as.Date("15/01/2014", format="%d/%m/%Y"))

gifts_c2013 <- sqldf("SELECT *
                    FROM gifts
                     WHERE date_date < c2013_date")

gifts_c2014 <- sqldf("SELECT *
                     FROM gifts
                     WHERE date_date < c2014_date")

#Check that the max date is in fact before the campaign date
#April 11th, 2013 date is 15806 in R
#Januray 15th, 2014 date is 16085 in R
#max date in gifts is 16254
sqldf("SELECT max(date_date) FROM gifts_c2013")
sqldf("SELECT max(date_date) FROM gifts_c2014")

############################################################################################################################################################

#Create total donation times
counts_c2013 <- data.frame(table(gifts_c2013$donorID))
counts_c2014 <- data.frame(table(gifts_c2014$donorID))
names(counts_c2013)[c(1,2)]<- c("donorID", "Donation_count")
names(counts_c2014)[c(1,2)]<- c("donorID", "Donation_count")
head(counts_c2013)

basetable_c2013 <- merge(x = basetable, y = counts_c2013, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable, y = counts_c2014, by = "donorID", all.x = TRUE)

############################################################################################################################################################

#Add variable for the frequency of donation (i.e. the mean donation occurence per year)

Donations_yr_2013 <- sqldf("SELECT donorID, min(date_date) as 'first_donation'
                      FROM gifts_c2013
                      GROUP BY 1")
Donations_yr_2013$c2013_date <- as.integer(as.Date("11/04/2013", format="%d/%m/%Y"))
Donations_yr_2013$Years_dif <- (Donations_yr_2013$c2013_date - Donations_yr_2013$first_donation)/365.25
Donations_yr_2013$Freq <- basetable_c2013$Donation_count/Donations_yr_2013$Years_dif

Donations_yr_2014 <- sqldf("SELECT donorID, min(date_date) as 'first_donation'
                      FROM gifts_c2014
                      GROUP BY 1")
Donations_yr_2014$c2014_date <- as.integer(as.Date("15/01/2014", format="%d/%m/%Y"))
Donations_yr_2014$Years_dif <- (Donations_yr_2014$c2014_date - Donations_yr_2014$first_donation)/365.25
Donations_yr_2014$Freq <- basetable_c2014$Donation_count/Donations_yr_2014$Years_dif

#Remove intermediate variables
Donations_yr_2013[,c("first_donation", "Years_dif", "c2013_date")] <- NULL
Donations_yr_2014[,c("first_donation", "Years_dif", "c2014_date")] <- NULL

#Add new variables to basetable
basetable_c2013 <- merge(x = basetable_c2013, y = Donations_yr_2013, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = Donations_yr_2014, by = "donorID", all.x = TRUE)


############################################################################################################################################################

#Add variable for the aggregate donations

Donation_rate_2013 <- sqldf("SELECT donorID, sum(amount) as TotalDonated
               FROM gifts_c2013
               GROUP BY 1")

basetable_c2013 <- merge(x = basetable_c2013, y = Donation_rate_2013, by = "donorID", all.x = TRUE)

Donation_rate_2014 <- sqldf("SELECT donorID, sum(amount) as TotalDonated
               FROM gifts_c2014
               GROUP BY 1")

basetable_c2014 <- merge(x = basetable_c2014, y = Donation_rate_2014, by = "donorID", all.x = TRUE)

############################################################################################################################################################

#Calculate the max and min donation given by donor

Max_min_given_2013 <- sqldf("SELECT donorID, max(amount) as MaxAmountGiven, min(amount) as MinAmountGiven
               FROM gifts_c2013
               GROUP BY 1")

basetable_c2013 <- merge(x = basetable_c2013, y = Max_min_given_2013, by = "donorID", all.x = TRUE)


Max_min_given_2014 <- sqldf("SELECT donorID, max(amount) as MaxAmountGiven, min(amount) as MinAmountGiven
               FROM gifts_c2014
               GROUP BY 1")

basetable_c2014 <- merge(x = basetable_c2014, y = Max_min_given_2014, by = "donorID", all.x = TRUE)

############################################################################################################################################################

#Find the length of the donations by finding the difference between the first and most recent date of donation
gifts_c2013_week <- sqldf("SELECT donorID, (max(date_date) - min(date_date))/7 as 'active_weeks'
                    FROM gifts_c2013
                     GROUP BY 1")

gifts_c2014_week <- sqldf("SELECT donorID, (max(date_date) - min(date_date))/7 as 'active_weeks'
                    FROM gifts_c2014
                     GROUP BY 1")

#Then change values for those who only donated once from 0 to the median to normalize the histogram
gifts_c2013_week$active_weeks[gifts_c2013_week$active_weeks == 0] <- median(gifts_c2013_week$active_weeks)
gifts_c2014_week$active_weeks[gifts_c2014_week$active_weeks == 0] <- median(gifts_c2014_week$active_weeks)

#Add the variable to the basetable
basetable_c2013 <- merge(x = basetable_c2013, y = gifts_c2013_week, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = gifts_c2014_week, by = "donorID", all.x = TRUE)

#hist(basetable_c2013$active_weeks, breaks=seq(0,1000,l=10))

############################################################################################################################################################

#Find the average donation per donor
gifts_avg_c2013 <- sqldf("SELECT donorID, avg(amount) as 'avg_donation'
                    FROM gifts_c2013
                    GROUP BY 1")

gifts_avg_c2014 <- sqldf("SELECT donorID, avg(amount) as 'avg_donation'
                    FROM gifts_c2014
                         GROUP BY 1")

basetable_c2013 <- merge(x = basetable_c2013, y = gifts_avg_c2013, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = gifts_avg_c2014, by = "donorID", all.x = TRUE)

#Change NA values to zero to show that those donors have never donated
basetable_c2013$avg_donation[which(is.na(basetable_c2013$avg_donation))] <- 0
basetable_c2014$avg_donation[which(is.na(basetable_c2014$avg_donation))] <- 0

############################################################################################################################################################

#Add variable for the average amount of donations per quarter for likelihood of each donor to donate at a particular time over each year
gifts_c2013$quarter <- quarter(gifts_c2013$date_date)

AvgDonatedQ1<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ1
                     FROM gifts_c2013
                     WHERE quarter = 1
                     GROUP BY 1")
AvgDonatedQ2<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ2
                     FROM gifts_c2013
                     WHERE quarter = 2
                     GROUP BY 1")
AvgDonatedQ3<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ3
                     FROM gifts_c2013
                     WHERE quarter = 3
                     GROUP BY 1")
AvgDonatedQ4<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ4
                     FROM gifts_c2013
                     WHERE quarter = 4
                     GROUP BY 1")

basetable_c2013 <- merge(x = basetable_c2013, y = AvgDonatedQ1, by = "donorID", all.x= TRUE)
basetable_c2013 <- merge(x = basetable_c2013, y = AvgDonatedQ2, by = "donorID", all.x = TRUE)
basetable_c2013 <- merge(x = basetable_c2013, y = AvgDonatedQ3, by = "donorID", all.x = TRUE)
basetable_c2013 <- merge(x = basetable_c2013, y = AvgDonatedQ4, by = "donorID", all.x = TRUE)

head(basetable_c2013)

#Create Avg Quarter value Yes/No variable
basetable_c2013$DonatedQ1_Y_N <- ifelse(is.na(basetable_c2013$AvgDonatedQ1),0,1)
basetable_c2013$DonatedQ2_Y_N <- ifelse(is.na(basetable_c2013$AvgDonatedQ2),0,1)
basetable_c2013$DonatedQ3_Y_N <- ifelse(is.na(basetable_c2013$AvgDonatedQ2),0,1)
basetable_c2013$DonatedQ4_Y_N <- ifelse(is.na(basetable_c2013$AvgDonatedQ2),0,1)

#Change NAs to zeros for amount
basetable_c2013$AvgDonatedQ1[is.na(basetable_c2013$AvgDonatedQ1)] <- 0
basetable_c2013$AvgDonatedQ2[is.na(basetable_c2013$AvgDonatedQ2)] <- 0
basetable_c2013$AvgDonatedQ3[is.na(basetable_c2013$AvgDonatedQ3)] <- 0
basetable_c2013$AvgDonatedQ4[is.na(basetable_c2013$AvgDonatedQ4)] <- 0

###FOR 2014
#Add variable for the average amount of donations per quarter for likelihood of each donor to donate at a particular time over each year
gifts_c2014$quarter <- quarter(gifts_c2014$date_date)

AvgDonatedQ1<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ1
                     FROM gifts_c2014
                     WHERE quarter = 1
                     GROUP BY 1")
AvgDonatedQ2<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ2
                     FROM gifts_c2014
                     WHERE quarter = 2
                     GROUP BY 1")
AvgDonatedQ3<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ3
                     FROM gifts_c2014
                     WHERE quarter = 3
                     GROUP BY 1")
AvgDonatedQ4<- sqldf("SELECT donorID, avg(amount) as AvgDonatedQ4
                     FROM gifts_c2014
                     WHERE quarter = 4
                     GROUP BY 1")

basetable_c2014 <- merge(x = basetable_c2014, y = AvgDonatedQ1, by = "donorID", all.x= TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = AvgDonatedQ2, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = AvgDonatedQ3, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = AvgDonatedQ4, by = "donorID", all.x = TRUE)

head(basetable_c2014)

#Create Avg Quarter value dummy variable
basetable_c2014$DonatedQ1_Y_N <- ifelse(is.na(basetable_c2014$AvgDonatedQ1),0,1)
basetable_c2014$DonatedQ2_Y_N <- ifelse(is.na(basetable_c2014$AvgDonatedQ2),0,1)
basetable_c2014$DonatedQ3_Y_N <- ifelse(is.na(basetable_c2014$AvgDonatedQ2),0,1)
basetable_c2014$DonatedQ4_Y_N <- ifelse(is.na(basetable_c2014$AvgDonatedQ2),0,1)

#Change NAs to zeros for amount
basetable_c2014$AvgDonatedQ1[is.na(basetable_c2014$AvgDonatedQ1)] <- 0
basetable_c2014$AvgDonatedQ2[is.na(basetable_c2014$AvgDonatedQ2)] <- 0
basetable_c2014$AvgDonatedQ3[is.na(basetable_c2014$AvgDonatedQ3)] <- 0
basetable_c2014$AvgDonatedQ4[is.na(basetable_c2014$AvgDonatedQ4)] <- 0

############################################################################################################################################################
#Add variable to calculate the evolution of a customer, i.e. are they donating more in the past 1 year as compared to the last 5 years?

gifts_c2013$april_2012 <- as.Date('11/04/2012', format='%d/%m/%Y')

Evol_2012 <- sqldf("SELECT donorID, sum(amount) as Donation_2012
                    FROM gifts_c2013
                     WHERE date_date > april_2012
                     GROUP BY 1")

gifts_c2013$april_2008 <- as.Date('11/04/2008', format='%d/%m/%Y')

Evol_2008 <- sqldf("SELECT donorID, sum(amount) as Donation_2008
                 FROM gifts_c2013
                 WHERE date_date > april_2008
                 GROUP BY 1")

Evolution_c2013 <- merge(x = Evol_2008, y = Evol_2012, by = "donorID", all.x = TRUE)
Evolution_c2013[is.na(Evolution_c2013)] <- 0

Evolution_c2013$Evolution <- Evolution_c2013$Donation_2012/Evolution_c2013$Donation_2008


###FOR 2014 dataset
gifts_c2014$april_2013 <- as.Date('11/04/2013', format='%d/%m/%Y')

Evol_2013 <- sqldf("SELECT donorID, sum(amount) as Donation_2013
                    FROM gifts_c2014
                     WHERE date_date > april_2013
                     GROUP BY 1")

gifts_c2014$april_2009 <- as.Date('11/04/2009', format='%d/%m/%Y')

Evol_2009 <- sqldf("SELECT donorID, sum(amount) as Donation_2009
                 FROM gifts_c2014
                 WHERE date_date > april_2009
                 GROUP BY 1")

Evolution_c2014 <- merge(x = Evol_2009, y = Evol_2013, by = "donorID", all.x = TRUE)
Evolution_c2014[is.na(Evolution_c2014)] <- 0

Evolution_c2014$Evolution <- Evolution_c2014$Donation_2013/Evolution_c2014$Donation_2009

#Remove intermediate columns
Evolution_c2013[,c("Donation_2012", "Donation_2008")] <- NULL
Evolution_c2014[,c("Donation_2013", "Donation_2009")] <- NULL

#Merge evolution variables with basetables
basetable_c2013 <- merge(x = basetable_c2013, y = Evolution_c2013, by = "donorID", all.x = TRUE)
basetable_c2014 <- merge(x = basetable_c2014, y = Evolution_c2014, by = "donorID", all.x = TRUE)

#Convert NA variables to median to ignore the effect of donors who have not donated since 2008
basetable_c2013$Evolution[is.na(basetable_c2013$Evolution)] <- median(basetable_c2013$Evolution, na.rm = TRUE)
basetable_c2014$Evolution[is.na(basetable_c2014$Evolution)] <- median(basetable_c2014$Evolution, na.rm = TRUE)

############################################################################################################################################################

#Check for duplicate rows in the test and train datasets
C2013_train[duplicated(C2013_train$donorID),]
C2014_test[duplicated(C2014_test$donorID),]

#Combine duplicates in train dataset
C2013_train <- sqldf("SELECT DISTINCT donorID, sum(amount) as amount
                      FROM C2013_train
                      GROUP BY donorID")

#Create test and train target columns
C2013_train$d_amount[C2013_train$amount < 35] <- 0
C2013_train$d_amount[C2013_train$amount >= 35] <- 1

C2014_test$d_amount[C2014_test$amount < 35] <- 0
C2014_test$d_amount[C2014_test$amount >= 35] <- 1

#Add the basetable to the target values. Left merge to ensure that variables from donors with no target are not included.
train <- merge(x = C2013_train, y = basetable_c2013, by = "donorID", all.x = TRUE)

#Add the basetable to the target values
test <- merge(x = C2014_test, y = basetable_c2014, by = "donorID", all.x = TRUE)


#Remove donorID and amount so that they are not used as one of the variables in the regression
train[,c("donorID", "amount")] <- NULL
test[,c("donorID", "amount")] <- NULL

##################################### Check for Outliers in the Base table and perform Winsorization ########################################

#Winsorize nondummy variables in basetables of the data
variables_winsorize <- c("Donation_count", "Freq", "TotalDonated", "Freq", "active_weeks", "avg_donation", "AvgDonatedQ1", "AvgDonatedQ2",
               "AvgDonatedQ3", "AvgDonatedQ4")

for(var in variables_winsorize)
{
  LL = mean(basetable_c2013[,var])-3*sd(basetable_c2013[,var])
  UL = mean(basetable_c2013[,var])+3*sd(basetable_c2013[,var])
  toreplaceLL = which(train[,var]<LL)
  train[toreplaceLL ,var] = LL
  toreplaceUL = which(train[,var]>UL)
  train[toreplaceUL ,var] = UL
}


for(var in variables_winsorize)
{
  LL = mean(basetable_c2014[,var])-3*sd(basetable_c2014[,var])
  UL = mean(basetable_c2014[,var])+3*sd(basetable_c2014[,var])
  toreplaceLL = which(test[,var]<LL)
  test[toreplaceLL ,var] = LL
  toreplaceUL = which(test[,var]>UL)
  test[toreplaceUL ,var] = UL
}

#lapply(basetable_c2013[variables_winsorize], hist)
#lapply(train[variables_winsorize], hist)

train <- train[complete.cases(train),]
test <- test[complete.cases(test),]

##################################### Modelling Methods ########################################

#Method #1: Run forward stepwise logistic regression to pick the best predictors

variables = names(train)[c(2:length(train))]
variablesorder = c()

model = glm(d_amount ~ 1,data=train,family=binomial)

formula<-formula(paste("d_amount","~",paste(variables,collapse="+")))


for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=train)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("d_amount","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=train,family=binomial)
  print(v)
}

auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)

for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("d_amount","~",paste(vars,collapse="+"))
  model<-glm(formula,data=train,family="binomial")	
  predicttrain<-predict(model,newdata=train,type="response")
  predicttest<-predict(model,newdata=test,type="response")
  auctrain[i] = auc(train$d_amount,predicttrain)
  auctest[i] = auc(test$d_amount,predicttest)
} 
  
plot(auctrain, main="AUC", col="red", type='l')
lines(auctest,col="blue",type='l')
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

finalvariables = variablesorder[c(0:5)]
formula<-paste("d_amount","~",paste(finalvariables,collapse="+"))
model<-glm(formula,data=train,family="binomial")	
predicttrain<-predict(model,newdata=train,type="response")
predicttest<-predict(model,newdata=test,type="response")
auctrain = auc(train$d_amount,predicttrain)
auctest = auc(test$d_amount,predicttest)

print(model)
print(auctrain)
print(auctest)

############################################################################################################################################################

#Method #2: Run random forest to compare to logistic regression

train$d_amount = as.factor(train$d_amount)
modelrf = randomForest(d_amount ~., data=train, importance=TRUE, ntree=100, maxnodes = 10)
predictions_train = predict(modelrf,newdata = train, type = "prob")
predictions_test = predict(modelrf,newdata = test, type = "prob")
auc(train$d_amount,predictions_train)
auc(test$d_amount,predictions_test)

##################################### Model Evaluation Methods ########################################

###Business Case (from evaluation)
variables = c("d_region", "avg_donation", "active_weeks", "Donation_count", "d_gender_u") 

formula<-paste("d_amount","~",paste(variables,collapse="+"))
model<-glm(formula,data=train,family="binomial")	
predicttrain<-predict(model,newdata=train,type="response")
predicttest<-predict(model,newdata=test,type="response")

# Plot the cumulative gains curve
#Test
pred <- prediction(predicttest,test$d_amount)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="Cumulative Gains", col="blue")
#Train
pred <- prediction(predicttrain,train$d_amount)
perf <- performance(pred,"tpr","fpr")
par(new=TRUE)
plot(perf, main="Cumulative Gains", col="red")
abline(0,1)
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

# Plot the lift curve for train and test data
pred <- prediction(predicttest,test$d_amount)
perf <- performance(pred,"lift","rpp")
plot(perf, main="Lift Curve", col="blue", ylim=c(0,5))
pred <- prediction(predicttrain,train$d_amount)
perf <- performance(pred,"lift","rpp")
par(new=TRUE)
plot(perf, main="Lift Curve", col="red",ylim=c(0,5))
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

# Plot cumulative response curve
predtest <- prediction(predicttest,test$d_amount)
perftest <- performance(predtest,"lift","rpp")
predtrain <- prediction(predicttrain,train$d_amount)
perftrain <- performance(predtrain,"lift","rpp")
averagetargettest = sum(test$d_amount)/nrow(test)
averagetargettrain = sum(as.numeric(train$d_amount))/nrow(train)
perftest@y.values[[1]] = perftest@y.values[[1]] * averagetargettest
perftrain@y.values[[1]] = perftrain@y.values[[1]] * averagetargettrain
plot(perftest,main="Cumulative Response",col="blue")
par(new=TRUE)
plot(perftrain, main="Cumulative Response", col="red")
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
        col=c("red", "blue"), lwd = 2)

##################################### Predictor Insight Graphs (PIG) of Target Variables ########################################

#First, create the PIG tables
#PIG for d_region
PIG_region = data.frame("Region" = c("Available", "Missing"), 
                          "Target_Incidence" = c(mean(test$d_amount[test$d_region == 1]), mean(test$d_amount[test$d_region == 0])), 
                          "Size" = c(sum(test$d_region == 1), sum(test$d_region == 0)))


#PIG for avg_donation
#Find the group ranges
#discretize(test$avg_donation, method = "interval", breaks = 8)
PIG_avg_donation = data.frame("Average_Donation" = c("[0,35)","[35,70)", "[70,105)", "[105,140)", "[140,175)", "[175,210]","[210,245]"),
                              "Target_Incidence" = c(mean(test$d_amount[test$avg_donation < 35]), 
                                                     mean(test$d_amount[(test$avg_donation >= 35 )&(test$avg_donation < 70)]),
                                                     mean(test$d_amount[(test$avg_donation >= 70)&(test$avg_donation < 105)]), 
                                                     mean(test$d_amount[(test$avg_donation >= 105)&(test$avg_donation < 140)]), 
                                                     mean(test$d_amount[(test$avg_donation >= 140)&(test$avg_donation < 175)]), 
                                                     mean(test$d_amount[(test$avg_donation >= 175)&(test$avg_donation <= 210)]),
                                                     mean(test$d_amount[(test$avg_donation >= 210)&(test$avg_donation <= 245)])),
                              "Size" = c(sum((test$avg_donation > 0)&(test$avg_donation < 35)), 
                                         sum((test$avg_donation >= 35)&(test$avg_donation < 70)),
                                         sum((test$avg_donation >= 70)&(test$avg_donation < 105)),
                                         sum((test$avg_donation >= 105)&(test$avg_donation < 140)),
                                         sum((test$avg_donation >= 140)&(test$avg_donation < 175)),
                                         sum((test$avg_donation >= 175)&(test$avg_donation < 210)), 
                                         sum((test$avg_donation >= 210)&(test$avg_donation <= 245))))


#PIG for active_weeks
#Find the group ranges
#discretize(test$active_weeks, method = "interval", breaks = 5)
PIG_active_weeks = data.frame("Active_weeks" = c("(0,174)", "[174,348)", "[348,522)", "[522,696)", "[696,870]"),
                              "Target_Incidence" = c(mean(test$d_amount[(test$active_weeks > 0)&(test$active_weeks < 174)]), 
                                                     mean(test$d_amount[(test$active_weeks >= 174)&(test$active_weeks < 348)]), 
                                                     mean(test$d_amount[(test$active_weeks >= 348)&(test$active_weeks < 522)]), 
                                                     mean(test$d_amount[(test$active_weeks >= 522)&(test$active_weeks < 696)]), 
                                                     mean(test$d_amount[(test$active_weeks >= 696)&(test$active_weeks <= 870)])),
                              "Size" = c(sum((test$active_weeks > 0)&(test$active_weeks < 174)),
                                         sum((test$active_weeks >= 174)&(test$active_weeks < 348)), 
                                         sum((test$active_weeks >= 348)&(test$active_weeks < 522)),
                                         sum((test$active_weeks >= 522)&(test$active_weeks < 696)), 
                                         sum((test$active_weeks >= 696)&(test$active_weeks <= 870))))

#PIG for Donation_count
#Find the group ranges
#discretize(test$Donation_count, method = "interval", breaks = 5)
PIG_Donation_count = data.frame("Donation_count" = c("[1,6.9)", "[6.9,12.8)", "[12.8,18.6)", "[18.6,24.5)", "[24.5,30.4]"),
                              "Target_Incidence" = c(mean(test$d_amount[test$Donation_count < 6.9]), 
                                                     mean(test$d_amount[(test$Donation_count >= 6.9)&(test$Donation_count < 12.8)]), 
                                                     mean(test$d_amount[(test$Donation_count >= 12.8)&(test$Donation_count < 18.6)]), 
                                                     mean(test$d_amount[(test$Donation_count >= 18.6)&(test$Donation_count < 24.5)]), 
                                                     mean(test$d_amount[(test$Donation_count >= 24.5)&(test$Donation_count <= 30.4)])),
                              "Size" = c(sum(test$Donation_count < 6.9), 
                                         sum((test$Donation_count >= 6.9)&(test$Donation_count < 12.8)), 
                                         sum((test$Donation_count >= 12.8)&(test$Donation_count < 18.6)),
                                         sum((test$Donation_count >= 18.6)&(test$Donation_count < 24.5)), 
                                         sum((test$Donation_count >= 24.5)&(test$Donation_count <= 30.4))))

#PIG for d_gender_u
PIG_gender_u = data.frame("Gender" = c("Gender_U", "Other Gender"), 
                          "Target_Incidence" = c(mean(test$d_amount[test$d_gender_u == 1]), mean(test$d_amount[test$d_gender_u == 0])), 
                          "Size" = c(sum(test$d_gender_u == 1), sum(test$d_gender_u == 0)))
PIG_region
PIG_avg_donation
PIG_active_weeks
PIG_Donation_count
PIG_gender_u

####################################################################################

#Then, plot the graphs

#PIG Region
#Add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 1)

## Plot first set of data and draw its axis
barplot(PIG_region$Size, ylim=c(0,25000), las = 1, xlab="", names.arg = c("Region Available", "Region Missing"),
     main="Predictor Incidence Graphs: Info Given") ## las=1 makes horizontal labels
mtext("Number of Donors", side = 2,line = 4)
axis(2, las = 1)

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(PIG_region$Target_Incidence, pch=16,  xlab="", xlim=c(0.5,2.5), ylab="", ylim=c(0,0.015),
     axes=FALSE, type="b", col="blue")
## a little farther out (line=4) to make room for labels
mtext("Target Incidence",side=4, col="blue", line = 4)
axis(4, col = "blue", col.axis = "blue", las = 1)

####################################################################################

#PIG Average Donation
#Add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 1)

## Plot first set of data and draw its axis
d=barplot(PIG_avg_donation$Size, ylim=c(0,25000), las = 1,
          main="Predictor Incidence Graphs: Average Donation") ## las=1 makes horizontal labels
text(d[,1],-4,offset=1,srt = 45, adj= 1.15, xpd = TRUE, labels = c("[0 - $35[","[$35 - $70[ ", "[$70 - $105[","[$105 - $1140[", "[$140 - $175[","[$175 - $210[","[$210 - 245]") , cex=1)
mtext("Number of Donors", side = 2,line = 4)
axis(2, las = 1)
mtext("Average Donation", side=1, line=4.6)

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(PIG_avg_donation$Target_Incidence, pch=16,  xlab="", xlim=c(0.5,7.5), ylab="", ylim=c(0,0.015),
     axes=FALSE, type="b", col="blue")
## a little farther out (line=4) to make room for labels
mtext("Target Incidence",side=4, col="blue", line = 4)
axis(4, col = "blue", col.axis = "blue", las = 1)

####################################################################################

#PIG Active Weeks
#Add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 1)

## Plot first set of data and draw its axis
c=barplot(PIG_active_weeks$Size, ylim=c(0,25000), las = 1,
        main="Predictor Incidence Graphs: Length of Relationship") ## las=1 makes horizontal labels
text(c[,1],-4,offset=1,srt = 45, adj= 1.15, xpd = TRUE, labels = c("0 to 174",  "174 to 348", "348 to 522", "522 to 696", "696 to 870") , cex=1)
mtext("Number of Donors", side = 2,line = 4)
axis(2, las = 1)
mtext('Number of weeks', side=1, line=4)

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(PIG_active_weeks$Target_Incidence, pch=16,  xlab="", xlim=c(0.5,5.5), ylab="", ylim=c(0,0.015),
     axes=FALSE, type="b", col="blue")
## a little farther out (line=4) to make room for labels
mtext("Target Incidence",side=4, col="blue", line = 4)
axis(4, col = "blue", col.axis = "blue", las = 1)

####################################################################################

#PIG Donation Count
#Add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 1)

## Plot first set of data and draw its axis
b=barplot(PIG_Donation_count$Size, ylim=c(0,25000), las = 1,
        main="Predictor Incidence Graphs: Number of Donations") ## las=1 makes horizontal labels
text(b[,1],-3.7,offset=1,srt = 45, adj= 1.15, xpd = TRUE, labels = c("[1,6.9[", "[6.9,12.8[", "[12.8,18.6[", "[18.6,24.5[", "[24.5,30.4]"), cex=1)
mtext("Number of Donors", side = 2,line = 4)
axis(2, las = 1)
mtext("Number of donations", side=1, line=4.3)

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(PIG_Donation_count$Target_Incidence, pch=16,  xlab="", xlim=c(0.5,5.5), ylab="", ylim=c(0,0.015),
     axes=FALSE, type="b", col="blue")
## a little farther out (line=4) to make room for labels
mtext("Target Incidence",side=4, col="blue", line = 4)
axis(4, col = "blue", col.axis = "blue", las = 1)

####################################################################################

#PIG Gender U
#Add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 1)

## Plot first set of data and draw its axis
a=barplot(PIG_gender_u$Size, ylim=c(0,25000), las = 1, xlab="",names.arg = c("Gender U", "Other Genders"),
        main="Predictor Incidence Graphs: Gender Type") ## las=1 makes horizontal labels
mtext("Number of donors", side = 2,line = 4)

axis(2, las = 1)

barplot(PIG_gender_u$Size,names.arg = c("Gender U", "Other Genders"), order = NA, axes = FALSE,
           plot=FALSE)

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(PIG_gender_u$Target_Incidence, pch=16,  xlab="", xlim=c(0.5,2.5), ylab="", ylim=c(0,0.015),
     axes=FALSE, type="b", col="blue")
## a little farther out (line=4) to make room for labels
mtext("Target Incidence",side=4, col="blue", line = 4)
axis(4, col = "blue", col.axis = "blue", las = 1)

##################################### Minimum Profit Estimation ##################################################

population_size <- 44691
target_incidence <- 0.01
reward_target <- 35
cost_campaign <- 0.5

profitcampaign <- function(perc_target, perc_selected,population_size, reward_target, cost_campaign) {
  cost=cost_campaign*perc_selected*population_size
  reward=reward_target*perc_target*perc_selected*population_size
  return(reward-cost)
}

#If we assume that we address the top 20% donors with the highest probability to donate
#According to the model
perc_selected=0.2
#From the lift curve
lift=1.8
perc_target=lift*target_incidence
profitcampaign(perc_target,perc_selected,population_size, reward_target, cost_campaign)

#Perc_selected - Lift - Profit
#0.1 - 2 - 893.82
#0.15 - 1.85 - 988.78
#0.2 - 1.8 - 1161.97
#0.3 - 1.6 - 804,44
#0.4 - 1.45 - 134.08
#0.5 - 1.4 - -223.455
#0.6 - 1.38 - -455


perc_selected_graph=c(0.1,0.15,0.2,0.3,0.4,0.5,0.6)
profit_graph=c(893.82,988.78,1161.97,804.44,134.08,-223.46,-455.85)
plot(perc_selected_graph,profit_graph, type='l',main='Profit per percentage of the population selected',xlab='Percentage of the population selected',
     ylab='Profit ($)', ylim=c(-300,1200))


