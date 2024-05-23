
# missing species in FB and present in IPNI and POWO ----------------------

##libraries
devtools::install_github("barnabywalker/kewr")
library(kewr)
library(tidyverse)
library(readr)
library(readxl)
library(plyr)
library(writexl)


##calling dataset
angio <- read_excel("Data/Raw/datasheet_angiosperms.xlsx")

##running function of missing species in Flora de Brasil
x1 <- missing_FFB_function(df = angio)

##replacing Leguminosae by Fabaceae
x1 <- x1 %>% mutate(family = case_when(family == "Leguminosae" ~ "Fabaceae", 
                                       .default = as.character(family)))
##family present
family_x <- sort(unique(x1$family))

##list for each family
list_abs <- vector("list", length = length(family_x))

##recovering ID
angio_id <- angio %>% select(ID, Família) %>% drop_na %>%
                    filter(Família %in% family_x)

##for loop for generating the list and the csv file for each family 
for(i in seq_along(family_x)){
  
  list_abs[[i]] <- x1 %>% filter(family == family_x[i])
  
  write_csv(list_abs[[i]],
  file =paste0("Data/Processed/Angiosperms/final_family_2/",
                     family_x[i], "_FB",
                 angio_absent$ID[i], ".csv"))
}

