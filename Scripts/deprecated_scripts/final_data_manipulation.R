library(tidyverse)
library(readxl)
library(writexl)

##calling original dataset
angio_df <- read_excel("Data/Raw/datasheet_angiosperms_07Nov2023.xlsx") 

##filtering only families
angio_fam <- angio_df %>% filter(Rank == "FAMILIA")

##calling excel files
ex_files <- list.files(path = "Data/Processed/Angiosperms/missing_powo_ipni")

##removing csv files
ex_files <- ex_files[-c(111,61,67)]

##excel names
ex_names = str_remove_all(ex_files, ".xlsx")

##calling csv files
csv_files <- list.files(path = "Data/Processed/Angiosperms/missing_powo_ipni/csv")

##csv names
csv_names = str_remove_all(csv_files, ".csv")


##First excel files
for(i in seq_along(ex_files)){
  
  ##reading datasets
  x <- read_excel(path = paste0("Data/Processed/Angiosperms/missing_powo_ipni/",
                                ex_files[i]))
  tryCatch({
  ##modifying the requested columns
  x <- x %>% filter(hybrid == FALSE) %>% 
    mutate(status = case_when(source == "POWO" ~ "Accepted",
                              source == "IPNI" ~ status))
  if(nrow(x) == 0){
    print(paste0("No missing species for", " ", ex_names[i]))
  } else {
    write_csv(x, file = paste0("Data/Processed/Angiosperms/final_family/",
                               ex_names[i], "_", "FB",
                               angio_fam[which(angio_fam[,"Família"] == ex_names[i]), "ID"],
                               ".csv"))
  }  
  }, error = function(e){
    if(nrow(x) == 0){
      print(paste0("No missing species for", " ", ex_names[i]))
    } else {
    x <- x %>% filter(hybrid == FALSE) %>% mutate(status = "Accepted")
    
    write_csv(x, file = paste0("Data/Processed/Angiosperms/final_family/",
                               ex_names[i], "_","FB",
                               angio_fam[which(angio_fam[,"Família"] == ex_names[i]), "ID"], 
                               ".csv"))
    }
    
    
  })
}

###Now the csv files

##First excel files
for(i in seq_along(csv_files)){
  
  ##reading datasets
  x <- read_csv(file = paste0("Data/Processed/Angiosperms/missing_powo_ipni/csv/",
                                csv_files[i]))
  ##modifying the requested columns
    x <- x %>% filter(hybrid == FALSE) %>% 
      mutate(status = case_when(source == "POWO" ~ "Accepted",
                                source == "IPNI" ~ status))
    if(nrow(x) == 0){
      print(paste0("No missing species for", " ", csv_names[i]))
    } else {
    write_csv(x, file = paste0("Data/Processed/Angiosperms/final_family/",
                               csv_names[i], "_", "FB",
                               angio_fam[which(angio_fam[,"Família"] == csv_names[i]), "ID"],
                               ".csv"))
      }
  }



