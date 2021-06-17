
######===============WELCOME TO LETS TALK R=======================######

#Make sure you have created a branch and you are working on that branch!!


###-------------------------LOADING PACKAGES------------------------#####

#1)Download the packages that we are going to use today, 
#if you don't have them

install.packages("tidyverse")
install.packages("readxl")
install.packages("here")


#2) Load the packages

library(tidyverse)
library(readxl)

###-------------------------02-LOADING DATASET------------------------#####

#Source of data (1) - see README for citation

#Checking the working sheets in our data set before loading

excel_sheets(here::here('2019_WAFCT.xlsx'))

##Loading WAFCT, skip 2 first rows to use tagnames for components

WAFCT <- readxl::read_excel(here::here('2019_WAFCT.xlsx'),
                            sheet = 5, skip = 2) %>%
  mutate(FCT = 'WAFCT') %>% 
  glimpse()

#Instead of glimpse you can try str(), head(), tail()


###-------------------------02-CLEANING DATASET-----------------################


#rename variables 

WAFCT <- WAFCT %>% rename(code = '...1', 
                          fooditem = '...2', 
                          fooditemFR = '...3',
                          scientificName = '...4',
                          ref = '...5' ,
                          ENERC2 = 'ENERC...9', 
                          ENERC1 = 'ENERC...10') 


#Converting into numeric numeric variables  

WAFCT<- WAFCT %>% mutate_at(vars(8:69), funs(as.numeric)) 

#We can use the 'variables name' instead

#Using mutate_if

#The following f(x) removes []

no_brackets <- function(i){
  case_when(
    str_detect(i, '\\[.*?\\]') == TRUE ~ str_extract(i, '(?<=\\[).*?(?=\\])'), 
    TRUE ~ i)
}

WAFCT <- WAFCT %>% mutate_if(is.character, no_brackets)


###-------------------------03-VISUALIZATION------------------------#####
#Source of data (2) - see README for citation

##-------load-data

fbs <- read.csv(here::here('FAOSTAT-region-africa-2014-2018_2021-02-12.csv'))

##-------Create-a-plot

ggplot(data = fbs) +
  geom_point(mapping = aes(x = Year, y = Value, colour = Item.Code))


fbs$Item.Code <- as.factor(fbs$Item.Code)

fbs %>% dplyr::filter(Year %in% c(2014, 2018)) %>% 
ggplot() +
  geom_point(mapping = aes(x = Year, y = Value, colour = Item.Code))


fbs %>% dplyr::filter(Value > 0) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = Area))

fbs$Year <- as.factor(fbs$Year)

fbs %>% dplyr::filter(Value > 1) %>% 
  ggplot(aes(x = Value, y = Year)) +
  geom_boxplot() + 
  geom_point(aes(colour = Item, alpha = 1/999999999)) +
  theme(legend.position = 'none') 

###-------------------------04-CREATE A LOOP!----

#We need to rename multiple variables in multiple dataset
#but, not all variables are present in all dataset

#Here's the dataset with the new variables: 

variables <- read.csv(here::here( "fct-variable-names.csv"))

#You can find the dataset to be modified in Teams. 

#We can start with one data set

WAFCT <- read.csv(here::here('data', 'MAPS_WAFCT.csv'))

#Here's the *manual* solution:

WAFCT<- WAFCT %>% rename(
  original_food_id = "code",
  original_food_name = "fooditem",
  fct_name = "FCT",
  data_reference_original_id = "ref",
  moisture_in_g = "WATER",
  energy_in_kcal = "ENERC1",
  energy_in_kj = "ENERC2", 
  totalprotein_in_g = "PROTCNT",
  totalfats_in_g = "FAT",
  saturatedfa_in_g = "FASAT", 
  monounsaturatedfa_in_g = "FAMS", 
  polyunsaturatedfa_in_g = "FAPU", 
  cholesterol_in_mg = "CHOLE",
  carbohydrates_in_g = "CHOAVLDF", 
  fibre_in_g = "FIBTG", 
  ash_in_g = "ASH",
  ca_in_mg = "CA", 
  fe_in_mg = "FE",
  mg_in_mg = "MG",
  p_in_mg = "P",
  k_in_mg = "K",
  na_in_mg = "NA", 
  zn_in_mg = "ZN", 
  cu_in_mg = "CU", 
  vitamina_in_rae_in_mcg = "VITA_RAE", 
  thiamin_in_mg = "THIA",
  riboflavin_in_mg = "RIBF", 
  niacin_in_mg = "NIA", 
  vitaminb6_in_mg = "VITB6C", 
  folicacid_in_mcg = "FOLAC", 
  folate_in_mcg = "FOL",
  vitaminb12_in_mcg = "VITB12", 
  vitaminc_in_mg = "VITC",
  vitamind_in_mcg = "VITD",
  vitamine_in_mg = "VITE", 
  phyticacid_in_mg = "PHYTCPP")


#The idea is to create a loop that would do the same but in one line ;) 



#Now, we need to loop that over multiple dataset :D




#Finally, we need to ensure that all dataset has the same variables and the same
#structure
#Can we use the above loop to do this final step?

#Here's how it was again *manually* done..

#getting the names of all the standard variables names, to filter them afterward
var.name <- variables %>% select(Column.Name) %>% pull

#getting all the MAPS-standard variables included in the dataset.

var.dat <- variables %>% spread(Column.Name, Description) %>% 
  mutate_all(as.numeric) %>%                               #fixing the type of
  mutate_at(c("original_food_id", "original_food_name",
              "data_reference_original_id",
              "fct_name"),   #variables so I can
            as.character)     #merge the two dataset

WAFCT<- WAFCT %>% left_join(., var.dat) %>% select(var.name)


#Here's the dataset with the new variables: 

variables <- read.csv(here::here( "fct-variable-names.csv"))

#You can find the dataset to be modified in Teams. 

#We can start with one data set

WAFCT <- read.csv(here::here('data', 'MAPS_WAFCT.csv'))

#Here's the *manual* solution:

WAFCT<- WAFCT %>% rename(
  original_food_id = "code",
  original_food_name = "fooditem",
  fct_name = "FCT",
  data_reference_original_id = "ref",
  moisture_in_g = "WATER",
  energy_in_kcal = "ENERC1",
  energy_in_kj = "ENERC2", 
  totalprotein_in_g = "PROTCNT",
  totalfats_in_g = "FAT",
  saturatedfa_in_g = "FASAT", 
  monounsaturatedfa_in_g = "FAMS", 
  polyunsaturatedfa_in_g = "FAPU", 
  cholesterol_in_mg = "CHOLE",
  carbohydrates_in_g = "CHOAVLDF", 
  fibre_in_g = "FIBTG", 
  ash_in_g = "ASH",
  ca_in_mg = "CA", 
  fe_in_mg = "FE",
  mg_in_mg = "MG",
  p_in_mg = "P",
  k_in_mg = "K",
  na_in_mg = "NA", 
  zn_in_mg = "ZN", 
  cu_in_mg = "CU", 
  vitamina_in_rae_in_mcg = "VITA_RAE", 
  thiamin_in_mg = "THIA",
  riboflavin_in_mg = "RIBF", 
  niacin_in_mg = "NIA", 
  vitaminb6_in_mg = "VITB6C", 
  folicacid_in_mcg = "FOLAC", 
  folate_in_mcg = "FOL",
  vitaminb12_in_mcg = "VITB12", 
  vitaminc_in_mg = "VITC",
  vitamind_in_mcg = "VITD",
  vitamine_in_mg = "VITE", 
  phyticacid_in_mg = "PHYTCPP")


#The idea is to create a loop that would do the same but in one line ;) 



#Now, we need to loop that over multiple dataset :D




#Finally, we need to ensure that all dataset has the same variables and the same
#structure
#Can we use the above loop to do this final step?

#Here's how it was again *manually* done..

#getting the names of all the standard variables names, to filter them afterward
var.name <- variables %>% select(Column.Name) %>% pull

#getting all the MAPS-standard variables included in the dataset.

var.dat <- variables %>% spread(Column.Name, Description) %>% 
  mutate_all(as.numeric) %>%                               #fixing the type of
  mutate_at(c("original_food_id", "original_food_name",
              "data_reference_original_id",
              "fct_name"),   #variables so I can
            as.character)     #merge the two dataset

WAFCT<- WAFCT %>% left_join(., var.dat) %>% select(var.name)

###-------------------------05-WEBSCRAPING!----

#We need to extract some information from a website
#we are using the rvest package

install.packages("rvest")

library(rvest)
library(tidyverse)

#I want to get the list of countries/locations and the records underneath 
#http://www.fao.org/infoods/infoods/tables-and-databases/africa/en/
#Ideally as a list or as a table

#I have extracted a list with the each records 


infoods <- read_html("http://www.fao.org/infoods/infoods/tables-and-databases/africa/en/")


infoods %>%  html_nodes("ul") %>%
  .[[30]]

#Here's the list of the records

fct.list <- infoods %>%  html_nodes("ul") %>% 
  .[c(6:30)] 

#I would like to have an object like that but for all the records

p <- fct.list[[8]]  


#I try looping but it doesn't work...#

r <- list()

for(i in 25) {
  
  r[i] <-  fct.list[[i]] 
  
}





