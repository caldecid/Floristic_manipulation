library(tidyverse)
library(readxl)
library(writexl)

ex_files <- list.files(path = "Data/Processed/Angiosperms/missing_powo_ipni")

##removing csv files
ex_files <- ex_files[-c(111,61,67)]

for(i in seq_along(ex_files)){
  tryCatch({
    x <- read_excel(path = paste0("Data/Processed/Angiosperms/missing_powo_ipni/",
                                  ex_files[i]))
    
    x <- x %>% mutate(status = as.factor(status),
                      status = if_else(status == 'Accepted', 'Unknown', status),
                      status = as.factor(status)) 
    
    write_xlsx(x, path = paste0("Data/Processed/Angiosperms/missing_powo_ipni/",
                                ex_files[i]))
    print(i)
  }, error = function(e){
    ##enumerating the csv files
    
    print(ex_files[i])
    
  })
}

###now the csv files
csv_files <- list.files(path = "Data/Processed/Angiosperms/missing_powo_ipni/csv")

for(i in seq_along(csv_files)){
  tryCatch({
    x <- read_csv(file = paste0("Data/Processed/Angiosperms/missing_powo_ipni/csv/",
                                  csv_files[3]))
    
    x <- x %>% mutate(status = as.factor(status),
                      status = if_else(status == 'Accepted', 'Unknown', status),
                      status = as.factor(status)) 
    
    write_csv(x, file = paste0("Data/Processed/Angiosperms/missing_powo_ipni/csv/",
                                csv_files[i]))
    print(i)
  }, error = function(e){
    ##enumerating the csv files
    
    print(csv_files[i])
    
  })
}
