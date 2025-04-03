
#Install packages
#devtools::install_github("liamkendall/pollimetry")
#devtools::install_github("liamkendall/pollimetrydata")
#Load library
library(pollimetry)
library(pollimetrydata)

#Load pollinator trait data
polltraits = readRDS("Data/Trait_data/Processed/PollTraits.rds")




(example <- cbind.data.frame(IT = c(1.2, 2.3), 
                             Sex = c("Female","Male"), 
                             Family = c("Apidae","Andrenidae"),
                             Region = c("NorthAmerica","Europe"),
                             Species = c("Ceratina_dupla","Andrena_flavipes")))


example=cbind.data.frame(IT=c(1.3,2.3),
                         Species = c("Ceratina_dupla","Andrena_flavipes"))




tonguelength(example,mouthpart="all")

bodysize(x = example, taxa = "bee", type = "taxo")
?bodysize

foragedist(c(10,5,2), type = "GreenleafAll") 

