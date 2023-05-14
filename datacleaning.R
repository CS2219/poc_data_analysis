library(ggplot2)
library(inspectdf)
library(tidyverse)
library(reader)

#set the work directory
setwd('')

#compile the offence csv files into one dataset for analysis 
df3 <- list.files(full.names = TRUE, recursive = TRUE, path="./Dataset") %>% 
  lapply(read_csv,  col_types = cols( .default = col_character())) %>% 
  bind_rows

#Removed percentage columns and renamed them
df3_new = df3[,!grepl("^Percentage",names(df3))]

df3_new <- df3_new %>% 
  rename(
    'Town' ='...1',
    'Homicide_us' ='Number of Homicide Unsuccessful',
    'Homicide' ='Number of Homicide Convictions',
    'Offences_Against_Person' ='Number of Offences Against The Person Convictions',
    'Offences_Against_Person_us' ='Number of Offences Against The Person Unsuccessful',
    'Sexual' ='Number of Sexual Offences Convictions',
    'Sexual_us' ='Number of Sexual Offences Unsuccessful',
    'Burglary' ='Number of Burglary Convictions',
    'Burglary_us' ='Number of Burglary Unsuccessful',
    'Robbery' ='Number of Robbery Convictions',
    'Robbery_us' ='Number of Robbery Unsuccessful',
    'Theft' ='Number of Theft And Handling Convictions',
    'Theft_us' ='Number of Theft And Handling Unsuccessful',
    'Fraud_Forgery' ='Number of Fraud And Forgery Convictions',
    'Fraud_Forgery_us' ='Number of Fraud And Forgery Unsuccessful',
    'Criminal' ='Number of Criminal Damage Convictions',
    'Criminal_us' ='Number of Criminal Damage Unsuccessful',
    'Drug' ='Number of Drugs Offences Convictions',
    'Drug_us' ='Number of Drugs Offences Unsuccessful',
    'Public_order' ='Number of Public Order Offences Convictions',
    'Public_order_us' ='Number of Public Order Offences Unsuccessful',
    'other' ='Number of All Other Offences (excluding Motoring) Convictions',
    'other_us' ='Number of All Other Offences (excluding Motoring) Unsuccessful',
    'Motoring' ='Number of Motoring Offences Convictions',
    'Motoring_us' ='Number of Motoring Offences Unsuccessful'
    )

#DataStructure
glimpse(df3_new)

library(skimr)
skim(df3_new)

library(visdat)
vis_miss(df3_new)
vis_dat(df3_new)

library(inspectdf)
inspect_types(df3_new) %>% show_plot()

#Descriptive data analysis

#average calculation 2014----------------------
crime_avg_2014<- df3_new %>%
  filter(crime_year=='2014' & Town !='National')
crime_avg_2014 = crime_avg_2014[,!grepl("_us$",names(crime_avg_2014))]
crime_avg_2014 <-crime_avg_2014 %>% select(-c('Number of Admin Finalised Unsuccessful','Town'))


agg_crime_year_2014 <- crime_avg_2014 %>% group_by(crime_month)%>%
  summarise(homicide = mean(Homicide),
            Offences_Against_Person = mean(Offences_Against_Person),
            Sexual = mean(Sexual),
            Burglary = mean(Burglary),
            Robbery = mean(Robbery),
            Theft = mean(Theft),
            Fraud_Forgery = mean(Fraud_Forgery),
            Criminal = mean(Criminal),
            Drug = mean(Drug),
            Public_order = mean(Public_order),
            other = mean(other),
            Motoring = mean(Motoring))

#plotting the mean value

agg_crime_year_2014%>% 
  gather("key", "value", - c(crime_month)) %>%
  ggplot(aes(x = crime_month, y = value, group = key, fill = key))+
  ggtitle("Average crime vs crime type in 2014")+
  labs(y= "Average crime", x = "crime type")+
  geom_bar(position="stack", stat="identity")


#average calculation 2014 unsuccessful----------------------
crime_us_avg_2014<- df3_new %>%
  filter(crime_year=='2014' & Town !='National')


agg_crime_us_year_2014 <- crime_us_avg_2014 %>% group_by(crime_month)%>%
  summarise(homicide = mean(Homicide_us),
            Offences_Against_Person = mean(Offences_Against_Person_us),
            Sexual = mean(Sexual_us),
            Burglary = mean(Burglary_us),
            Robbery = mean(Robbery_us),
            Theft = mean(Theft_us),
            Fraud_Forgery = mean(Fraud_Forgery_us),
            Criminal = mean(Criminal_us),
            Drug = mean(Drug_us),
            Public_order = mean(Public_order_us),
            other = mean(other_us),
            Motoring = mean(Motoring_us))

#plotting the mean value

agg_crime_us_year_2014%>% 
  gather("key", "value", - c(crime_month)) %>%
  ggplot(aes(x = crime_month, y = value, group = key, fill = key))+
  ggtitle("Average unsuccessful crime vs crime type in 2014")+
  labs(y= "Average unsuccessful crime", x = "crime type")+
  geom_bar(position="stack", stat="identity")
#----------------avg cal 2015------------
crime_avg_2015<- df3_new %>%
  filter(crime_year=='2015' & Town !='National')


agg_crime_year_2015 <- crime_avg_2015 %>% group_by(crime_month)%>%
  summarise(homicide = mean(Homicide),
            Offences_Against_Person = mean(Offences_Against_Person),
            Sexual = mean(Sexual),
            Burglary = mean(Burglary),
            Robbery = mean(Robbery),
            Theft = mean(Theft),
            Fraud_Forgery = mean(Fraud_Forgery),
            Criminal = mean(Criminal),
            Drug = mean(Drug),
            Public_order = mean(Public_order),
            other = mean(other),
            Motoring = mean(Motoring))

#plotting the mean value
agg_crime_year_2015%>% 
  gather("key", "value", - c(crime_month)) %>%
  ggplot(aes(x = crime_month, y = value, group = key, fill = key))+
  ggtitle("Average crime vs crime type in 2015")+
  labs(y= "Average crime", x = "crime type")+
  geom_bar(position="stack", stat="identity")

#----------------avg cal 2015 unsuccessful------------
crime_us_avg_2015<- df3_new %>%
  filter(crime_year=='2015' & Town !='National')


agg_crime_us_year_2015 <- crime_us_avg_2015 %>% group_by(crime_month)%>%
  summarise(homicide = mean(Homicide_us),
            Offences_Against_Person = mean(Offences_Against_Person_us),
            Sexual = mean(Sexual_us),
            Burglary = mean(Burglary_us),
            Robbery = mean(Robbery_us),
            Theft = mean(Theft_us),
            Fraud_Forgery = mean(Fraud_Forgery_us),
            Criminal = mean(Criminal_us),
            Drug = mean(Drug_us),
            Public_order = mean(Public_order_us),
            other = mean(other_us),
            Motoring = mean(Motoring_us))

#plotting the mean value
agg_crime_us_year_2015%>% 
  gather("key", "value", - c(crime_month)) %>%
  ggplot(aes(x = crime_month, y = value, group = key, fill = key))+
  ggtitle("Average unsuccessful crime vs crime type in 2015")+
  labs(y= "Average unsuccessful crime", x = "crime type")+
  geom_bar(position="stack", stat="identity")

#---------------avg cal 2018-------
crime_avg_2018<- df3_new %>%
  filter(crime_year=='2018' & Town !='National')
crime_avg_2018 = crime_avg_2018[,!grepl("_us$",names(crime_avg_2018))]


agg_crime_year_2018 <- crime_avg_2018 %>% group_by(crime_month)%>%
  summarise(homicide = mean(Homicide),
            Offences_Against_Person = mean(Offences_Against_Person),
            Sexual = mean(Sexual),
            Burglary = mean(Burglary),
            Robbery = mean(Robbery),
            Theft = mean(Theft),
            Fraud_Forgery = mean(Fraud_Forgery),
            Criminal = mean(Criminal),
            Drug = mean(Drug),
            Public_order = mean(Public_order),
            other = mean(other),
            Motoring = mean(Motoring))

#plotting the mean value

agg_crime_year_2018%>% 
  gather("key", "value", - c(crime_month)) %>%
  ggplot(aes(x = crime_month, y = value, group = key, fill = key))+
  ggtitle("Average crime vs crime type in 2018")+
  labs(y= "Average crime", x = "crime type")+
  geom_bar(position="stack", stat="identity")

#unsuccessful avg in 2018
crime_us_avg_2018<- df3_new %>%
  filter(crime_year=='2018' & Town !='National')


agg_crime_us_year_2018 <- crime_us_avg_2018 %>% group_by(crime_month)%>%
  summarise(homicide = mean(Homicide_us),
            Offences_Against_Person = mean(Offences_Against_Person_us),
            Sexual = mean(Sexual_us),
            Burglary = mean(Burglary_us),
            Robbery = mean(Robbery_us),
            Theft = mean(Theft_us),
            Fraud_Forgery = mean(Fraud_Forgery_us),
            Criminal = mean(Criminal_us),
            Drug = mean(Drug_us),
            Public_order = mean(Public_order_us),
            other = mean(other_us),
            Motoring = mean(Motoring_us))

#plotting the mean value

agg_crime_us_year_2018%>% 
  gather("key", "value", - c(crime_month)) %>%
  ggplot(aes(x = crime_month, y = value, group = key, fill = key))+
  ggtitle("Average unsuccessful crime vs crime type in 2018")+
  labs(y= "Average unsuccessful crime", x = "crime type")+
  geom_bar(position="stack", stat="identity")

#calculating the sum of crime type yearly and plotting the graph

crime_rate_2014<- df3_new %>%
  filter(crime_year=='2014' & Town !='National')


agg_town_month_2014 <- crime_rate_2014 %>% group_by(Town,crime_month) %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
agg_town_month_2014%>% 
  gather("key", "value", -c(crime_month,Town)) %>%
  ggplot(aes(x = Town, y = value, group = key, fill = key),color='Blue')+
  ggtitle("Cities vs Sum of successful crimes in year 2014")+
  labs(y= "Sum of crimes", x = "Cities")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()

#unsuccessful crime in 2014
crime_rate_us_2014<- df3_new %>%
  filter(crime_year=='2014' & Town !='National')


agg_town_month_us_2014 <- crime_rate_us_2014 %>% group_by(Town,crime_month) %>% 
  summarise(sum_homicide_us = sum(Homicide_us),
            sum_Offences_Against_Person_us = sum(Offences_Against_Person_us),
            sum_Sexual_us = sum(Sexual_us),
            sum_Burglary_us = sum(Burglary_us),
            sum_Robbery_us = sum(Robbery_us),
            sum_Theft_us = sum(Theft_us),
            sum_Fraud_Forgery_us = sum(Fraud_Forgery_us),
            sum_Criminal_us = sum(Criminal_us),
            sum_Drug_us = sum(Drug_us),
            sum_Public_order_us = sum(Public_order_us),
            sum_other_us = sum(other_us),
            sum_Motoring_us = sum(Motoring_us))

library(forcats)
agg_town_month_us_2014%>% 
  gather("key", "value", -c(crime_month,Town)) %>%
  ggplot(aes(x = Town, y = value, group = key, fill = key),color='Blue')+
  ggtitle("Cities vs Sum of unsuccessful crimes in year 2014")+
  labs(y= "Sum of crimes", x = "Cities")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()

#successful crime in 2015
crime_rate_2015<- df3_new %>%
  filter(crime_year=='2015' & Town !='National')


agg_tbl_town_month_2015<- crime_rate_2015 %>% group_by(Town,crime_month) %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

#Analysis of crime types in few countries

crime_rate_avon<- df3_new %>%
  filter(crime_year=='2014' & Town =='Avon and Somerset')


crime_rate_avon_2014 <- crime_rate_avon %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
crime_rate_avon_2014%>% 
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value))+
  ggtitle("Crime in Avon and Somerset in year 2014")+
  labs(y= "Sum of crimes", x = "Crime_types")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()


crime_rate_avon_2018<- df3_new %>%
  filter(crime_year=='2018' & Town =='Avon and Somerset')


crime_rate_avon_2018 <- crime_rate_avon %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
crime_rate_avon_2018%>% 
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value))+
  ggtitle("Crime in Avon and Somerset in year 2018")+
  labs(y= "Sum of crimes", x = "Crime_types")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()


#-----------

crime_rate_avon<- df3_new %>%
  filter(crime_year=='2014' & Town =='Metropolitan and City')


crime_rate_avon_2014 <- crime_rate_avon %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
crime_rate_avon_2014%>% 
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value))+
  ggtitle("Crime in Metropolitan and City in year 2014")+
  labs(y= "Sum of crimes", x = "Crime_types")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()


crime_rate_avon_2018<- df3_new %>%
  filter(crime_year=='2018' & Town =='Metropolitan and City')


crime_rate_avon_2018 <- crime_rate_avon %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
crime_rate_avon_2018%>% 
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value))+
  ggtitle("Crime in Metropolitan and City in year 2018")+
  labs(y= "Sum of crimes", x = "Crime_types")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()


#-----------

crime_rate_glos_2014<- df3_new %>%
  filter(crime_year=='2014' & Town =='Gloucestershire')


crime_rate_glos_2014 <- crime_rate_glos_2014 %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
crime_rate_glos_2014%>% 
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value))+
  ggtitle("Crime in Gloucestershire in year 2014")+
  labs(y= "Sum of crimes", x = "Crime_types")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()


crime_rate_glos_2018<- df3_new %>%
  filter(crime_year=='2018' & Town =='Gloucestershire')


crime_rate_glos_2018 <- crime_rate_glos_2018 %>% 
  summarise(homicide = sum(Homicide),
            Offences_Against_Person = sum(Offences_Against_Person),
            Sexual = sum(Sexual),
            Burglary = sum(Burglary),
            Robbery = sum(Robbery),
            Theft = sum(Theft),
            Fraud_Forgery = sum(Fraud_Forgery),
            Criminal = sum(Criminal),
            Drug = sum(Drug),
            Public_order = sum(Public_order),
            other = sum(other),
            Motoring = sum(Motoring))

library(forcats)
crime_rate_glos_2018%>% 
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value))+
  ggtitle("Crime in Gloucestershire in year 2018")+
  labs(y= "Sum of crimes", x = "Crime_types")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()



