install.packages("writexl")
library(writexl)
#install.packages("xlsx")
#library(xlsx)
#install.packages("openxlsx")
library(openxlsx)


# eda
library(funModeling)

basic_eda <- function(data){
  glimpse(data)
  print(status(data))
  freq(data)
  print(profiling_num(data))
  plot_num(data)
  describe(data)}

BasicEda <-describe(Fuel_All)
write.xlsx(BasicEda,'Basic Eda on Fuel_All.xlxs')

##glimpse(carPrice)
glimpse(Fuel_All)


# get unique value of each column

freq(Fuel_All$Address)
freq(Fuel_All$date)
length(unique(Fuel_All$Address))

#save as csv
write.csv(ServiceStationList,"ServiceStationList.csv")


StationsTOChk <- ServiceStationList[ServiceStationList$Address ==(ServiceStationList[duplicated(ServiceStationList$Address),]%>%select(Address))]

                                   