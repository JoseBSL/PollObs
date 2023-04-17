
#Install packages
install.packages("rdwd")

#Load library
library(rdwd)
library(dplyr)

#Retrieve data link with variables of interest
link <- selectDWD("Leipzig-Holzhausen", res="hourly",  per="recent",
                  var=c("air_temperature","precipitation", "wind"))
#Dowload data
file <- dataDWD(link, read=FALSE)
#Generate climatic file
clim <- readDWD(file, varnames = T)

#Note: Try to find a way to get the dates of interest

str(clim)
data(metaIndex)
str(metaIndex, vec.len=2)


clim %>% colnames()



