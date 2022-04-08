library(dplyr)
library(readr)
library(stats)
library(chron)
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(ggpubr)
library(psych)
library(lubridate)




##consuming in the 2018 Data
UberData <- read.csv("UberData.csv",header = TRUE, stringsAsFactors = FALSE)

colnames(New.UberData)%in%colnames(UberData)


##cleaning and processing the Data my data
New.UberData <- 
  UberData%>%
    mutate(Duration = as.numeric(Duration..min.),
         GST = as.numeric(GST.amount),
         datetime = paste(Request.Date..Local.,Request.Time..Local.),
         "Request Time" = strptime(datetime, format = "%m/%d/%Y %H:%M"),
          "Date Requested" = as.IDate(`Request Time`),
          dayofweek = weekdays(`Date Requested`),
          "Time Requested" = format(strptime(`Request.Time..Local.`,"%I:%M %p"), 
                               format = "%H:%M"),
           hours = as.numeric(format(strptime(`Request.Time..Local.`,"%I:%M %p"), 
                                         format = "%H")),
         Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name))%>%
    select(-c(1,2,9,10,20,21))%>%
    rename("First Name" = First.Name,
           "Last Name" = Last.Name,
           "Employee ID" = Employee.ID,
           "Distance" = Distance..mi.,
           "Service and Tech Fees"= Service.and.Technology.Fee,
           "Dropoff Address"= Dropoff.Address,
           "Pickup Address" = Pickup.Address,
           "Expense Code"= Expense.Code,
           "Expense Memo"= Expense.Memo,
           "Fare" = Fare.in.KES,
           "Charges" = Total.Charge.in.KES)

##handling missing values by replacing them with 0
New.UberData$GST[which(is.na(New.UberData$GST))] <- 0


##Exploratory Data Analysis
##analysis by year
YearlyData <- 
  New.UberData%>%
    mutate(Year = as.numeric(format(`Date Requested`, "%Y")))%>%
    select(Charges,Months,Year)%>%
    group_by(Year)%>%
    summarise(summedTotals = sum(Charges))


##Visualization using pie charts
pie(YearlyData$summedTotals,labels = YearlyData$Year, 
    main = "Yearly Expenditure on Uber", col = c("red","blue"))
legend("bottomleft",YearlyData$Year,fill = c("red","blue"))




###analysis of expenditure trends by months
MonthlyData <- 
  New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name))%>%
  select(Charges,Months,Year)%>%
  group_by(Months, Year)%>%
  summarise(MonthlyTotals = sum(Charges))



##monthly expenditure trends show no seasonality.
ggplot(data = MonthlyData, aes(x = Months, y = MonthlyTotals)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  facet_grid(. ~Year)+
  labs(x = "Months", y = "Uber Expediture", title = "Monthly Expenditure on Uber") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
        legend.title = element_text(face="bold", size = 10),
        strip.background = element_rect(fill="lightblue", colour="black", size=1),
        strip.text = element_text(face="bold", size=rel(1.2)))


##compare against 2017**

##Employee STATS Analysis
##Total Number of employee uber orders by Months
EmployeeMonthlyFreq <- 
  New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name),
         FullNames = paste(`First Name`,`Last Name`))%>%
  select(9,12,17,19,20,21,22)%>%
  group_by(Months,Year)%>%
  count()%>%
  rename(Freq = n)
 

##Visualizing Employee stats with a histogram
ggplot(EmployeeMonthlyFreq,aes(fill = Months,x = Months,y = Freq))+
  geom_histogram(position = "dodge",stat = "identity")+
  facet_grid(.~Year)+
  ggtitle("Frequency of Uber orders")

#there is no evidence of seasonality from 2017 to 2018


##individual employee total orders
EmployeeYearlyExpense <- 
  New.UberData%>%
    mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
           Months = factor(months(as.Date(`Date Requested`)),
                           levels = month.name),
           FullNames = paste(`First Name`,`Last Name`))%>%
    select(9,10,13,12,17,19,20,21,22)%>%
    group_by(FullNames,Year)%>%
    count()%>%
    arrange(desc(n))

##individual employee expenses
EmployeeYearlyExpense <- 
  New.UberData%>%
  mutate(Year = as.numeric(format(`Date Requested`, "%Y")),
         Months = factor(months(as.Date(`Date Requested`)),
                         levels = month.name),
         FullNames = paste(`First Name`,`Last Name`))%>%
  select(9,10,13,12,17,19,20,21,22)%>%
  group_by(FullNames,Year)%>%
  summarise(Individual_Charges = sum(Charges))%>%
  arrange(desc(Individual_Charges))


###analysis by time of order
AnalysisByTime <- 
  New.UberData%>%
  mutate(FullNames = paste(`First Name`,`Last Name`))%>%
  select(9,10,13,12,17,19,20,21,22)%>%
  group_by(hours)%>%
  summarise(No.OfOrders = n()) %>%
  arrange(desc(No.OfOrders))

##Visualizing analysis by time of order using a bar chart
ggplot(data = AnalysisByTime, aes(x = hours, y = No.OfOrders))+
  geom_bar(stat = 'identity', aes(fill = hours))+
  ggtitle("Analysis by time of order")

##Most of the orders are done at 10 am and 9 pm

New.UberData%>%
  mutate(FullNames = paste(`First Name`,`Last Name`))%>%
  select(9,10,13,12,17,19,20,21,22)%>%
  filter(Year == "2018")%>%
  group_by(dayofweek)%>%
  summarise(No.OfOrders = n()) %>%
  arrange(desc(No.OfOrders))

##Employee analysis by time of order and amount paid
Order_Amount <- 
  New.UberData%>%
    select(Charges,hours)


cor(Order_Amount)
##there is very little correlation between the time of order an the amount paid
  

###consuming in the departments data
EmployeeData <- read.csv("EmployeeNames.csv",header=TRUE)
EmployeeYearlyExpense$FullNames


##creating a forloop that matches each name to the department.
codes = EmployeeData$Employee.Name
Department <- vector(mode="character", length=nrow(EmployeeYearlyExpense))

for (i in 1:nrow(EmployeeYearlyExpense)) {
  idx = match(EmployeeYearlyExpense$FullNames[i], codes)
      Department[i] <- EmployeeData$Department[idx]
  }


###analysis of Total spending by department and expense code
DepartExpense <- 
  cbind(EmployeeYearlyExpense,Department)%>%
    rename(Departments = ...5)%>%
    group_by(Departments,Year)%>%
    summarise(TotalSpending = sum(Individual_Charges))%>%
    arrange(desc(TotalSpending))


##visualising spending by expense code
ggplot(data = DepartExpense, aes(x = Departments, y = TotalSpending))+
  geom_bar(stat = 'identity', aes(fill = Departments))+
  facet_grid(.~Year)+
  theme_grey()+
  labs(title = 'spending by expense code',
       y = 'TotalSpending', x = 'Departments')
##The quality assurance department spends alot on uber throughout both years


##analysis of frequency of order by expense code
AnalysisByexpenseCode <- 
  New.UberData%>%
    mutate(CAPS = toupper(`Expense Code`),
        Replaced = str_replace_all(CAPS,fixed(" "), ""),
         Replaced2 = str_replace_all(Replaced,
                                     fixed("working late"), "WORKLATE"),
         New.ExpenseCode = str_replace_all(Replaced2, 
                                     fixed(c("NATIONALBANKKENYATTAAVENUE" = "OFFICEADMIN",
                                             "NSSFSETTLEMENT"   = "OFFICEADMIN",
                                             "NSSF" = "OFFICEADMIN","1517" = "OFFICEADMIN",
                                             "NAKUMATTLIFESTYLE" = "OFFICEADMIN",
                                             "NSSFFOLLOWUP" = "OFFICEADMIN",
                                             "PAYMENTOFPAYEANDNHIF" = "OFFICEADMIN",
                                             "FROMBANKTOOFFICE" = "OFFICEADMIN",
                                             "OFFICEADMINFOLLOWUP" = "OFFICEADMIN",
                                             "FOLLOWUPONOFFICEADMIN" = "OFFICEADMIN",
                                             "CLIENTSUPPORT" = "SUPPORT",
                                             "WORKINGLATE" = "WORKLATE"))))%>%
    select(9,20,21,25)%>%
    group_by(New.ExpenseCode,Year)%>%
    count()
    
  
ggplot(data = AnalysisByexpenseCode, aes(x = New.ExpenseCode, y = n))+
  geom_bar(stat = 'identity', aes(fill = New.ExpenseCode))+
  facet_grid(.~Year)+
  theme_grey()+
  labs(title = 'frequency of order by expense code',
       y = 'Frequency of order', x = 'Expense code')    




##evaluating correlation between distance,Fare and Duration
## Our hypothesis - they are correlated
Data.f <- 
  New.UberData%>%
    select(Distance,Fare,Duration)
    
cor(Data.f)

##they have a perfect positive correlation because the results are above zero


##predicting annual expense by expense code
model <- lm(formula =  TotalSpending~`Expense Code`, data = DepartExpense)
summary(model)







