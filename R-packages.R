
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

###-------------------------LOADING DATASET------------------------#####

#Checking the working sheets in our data set before loading

excel_sheets(here::here('2019_WAFCT.xlsx'))

##Loading WAFCT, skip 2 first rows to use tagnames for components

WAFCT <- readxl::read_excel(here::here('2019_WAFCT.xlsx'),
                            sheet = 5, skip = 2) %>%
  mutate(FCT = 'WAFCT') %>% 
  glimpse()

#Instead of glimpse you can try str(), head(), tail()


######----------------------------------------------------------################


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



