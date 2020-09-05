# Set up

library(tidyverse)
library(lubridate)
library(readxl)
library(knitr)
library(data.table)
library(dplyr)


# Import data
Fuel_Jul20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_july2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")

Fuel_Jun20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_june2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")

Fuel_May20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_may2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")

NSWPublicHoliday<- read.csv("Public_Holiday_2019-2020\\australian_public_holidays_2020.csv")%>% filter(Jurisdiction=="nsw")
#might consider to change this table to a wide format and split the data in period, such 3 dayas prior public holiday

# format raw data
NSWPublicHoliday$Date<- ymd(NSWPublicHoliday$Date)
NSWPublicHoliday<- NSWPublicHoliday%>% rename(date=Date)
str(NSWPublicHoliday)

## Combine Fuel May to Jul 20
Fuel_RAW<- rbind(Fuel_May20,Fuel_Jun20,Fuel_Jul20)
Fuel_RAW$PriceUpdatedDate<- dmy_hms(Fuel_RAW$PriceUpdatedDate)
Fuel_RAW$date <- as.Date(format(Fuel_RAW$PriceUpdatedDate, "%Y-%m-%d"))

## merge Fuel_RAW with public holiday
Fuel_All<- left_join(Fuel_RAW,NSWPublicHoliday,by="date")

## sample of the merge data
head(Fuel_All)

## inspect the Fuel_All
names(Fuel_All)
str(BasicEda)
describe(Fuel_All)

min(Fuel_All$date)
max(Fuel_All$date)
# we have a total of 154,980 price records from May to Jul 20 for a period of 92 days (2020-05-01 to 2020-07-31) across 2071 unique service station names with 2189 different addresses.
# from above we can tell the some of the stations shared a same name but with different locations. 

# To confirm, we first create a list service station list
ServiceStationList<-Fuel_All%>%
  select("ServiceStationName","Address", "Suburb","Postcode")%>%
  unique()

group_by(ServiceStationList,ServiceStationName)%>%count()%>%arrange(-n)
# we can see some of the ServiceStationName has multiple addresses, for example, service station Caltex Armidale has 4 different street addresses, as a result, we will need to build a primary key in this table

group_by(ServiceStationList,Address)%>%count()%>%arrange(-n)
# we can see some of the address share the same name.

DuplicateAddress <- ServiceStationList[duplicated(ServiceStationList$Address),]%>%select(Address)
data_list <- as.vector(DuplicateAddress[,1])
StationsToChk <- Fuel_All[Fuel_All$Address %in% data_list,]




# as a result, in order to find a primary key, we need to build a primary key by combining service station name and address.
ServiceStationList$LocationKey <- paste(ServiceStationList$ServiceStationName,ServiceStationList$Address)
group_by(ServiceStationList,LocationKey)%>%count()%>%arrange(-n)



## plot all price points by day by brand, we use brand (24) instead of 
ggplot(Fuel_All)+ geom_point(aes(y=Price, x=date,colour=Brand))+
  labs(title="Fuel Price By Day",
       subtitle = "From May 2020 to July 2020",
       y = "Fuel $ per L", X= "Price Updated Date")
