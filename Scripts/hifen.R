
# Hifen manipulation ------------------------------------------------------

##libraries
library(vctrs)
library(tidyverse)
library(kewr)
library(plyr)


##Calling dataset
angio = read_excel("Data/Raw/datasheet_angiosperms_07Nov2023.xlsx")


##Applying function
x.1 <- hifen_incompatibility(df = angio)

##First inconsistency (Hifen in Flora but not in IPNI/POWO)
hifen_first = x.1[[1]]

##saving
write_csv(hifen_first,
      file = "Data/Processed/Angiosperms/hifen_first_inconsistency.csv")

#####Second inconsistency (Hifen in IPNI/POWO but not in Flora) 
hifen_second = x.1[[2]]

##saving
write_csv(hifen_second,
          file = "Data/Processed/Angiosperms/hifen_2nd_inconsistency.csv")

##Third inconsistency (species with hifen in IPNI/POWO but not present in any form in Flora)
hifen_third = x.1[[3]]

##saving
write_csv(hifen_third,
          file = "Data/Processed/Angiosperms/hifen_3rd_inconsistency.csv")
