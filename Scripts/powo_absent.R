##libraries
library(tidyverse)
devtools::install_github("DBOSlab/expowo")
library(expowo)
library(readxl)
library(stringr)
library(plyr)
library(writexl)

#####calling dataframes

##angiosperms, filling information, dropping NA, and creating tax_name

angiosperms_bra <- read_excel("Data/Raw/datasheet_angiosperms_07Nov2023.xlsx") %>% 
  fill(Família, Gênero) %>% 
  drop_na(Espécie) %>% 
  unite("taxon_name", Gênero:Espécie, sep = "_", remove = FALSE)


##family names
fam_names <- unique(angiosperms_bra$Família)


######creating list for keeping the missing species within the families
list_fam_ang <- vector("list", length = length(fam_names))

##naming
names(list_fam_ang) <- fam_names


##loop for finding missing species in the Flora de Brasil
for(i in 200){
  
  ##subseting the angiosperm df
  angio_fam = angiosperms_bra %>% filter(Família == fam_names[i])
  
  ###tryCatch for handling the missing families in POWO
  tryCatch({
    ##calling the species within families in POWO
    pow_df = powoSpecies(family = fam_names[i],
                         hybrid = TRUE,
                         synonyms = TRUE,
                         country = "Brazil")
    return(pow_df)
  }, error = function(e){
    message("Absent family in POWO")
    print(e)
  })
  
  ##arranging taxa names
  pow_df$taxon_name = str_replace(pow_df$taxon_name, " ", "_")
  
  ##POWO species absent in Flora de Brasil
  pow_abs = pow_df[-which(pow_df$taxon_name %in% angio_fam$taxon_name), ]
  
  ##if else statement for not saving empty dfs
    if(dim(pow_abs)[1] == 0){
    print("No absent species in Flora de Brasil")
  } else{
    tryCatch({
    ##Saving automatically in the metadata folder
      write_xlsx(pow_abs, path = paste0("Data/Metadata/Angiosperms/expowo_abs/",
                                        fam_names[i], ".csv"))
      
      ##inserting a df inside each list with the missing species in the Flora Brazil  
      list_fam_ang[[i]] = pow_abs
    }, error = function(e){
      ##Saving automatically in the metadata folder
      write_csv(pow_abs, file = paste0("Data/Metadata/Angiosperms/expowo_abs/",
                                        fam_names[i], ".csv"))
      
      ##inserting a df inside each list with the missing species in the Flora Brazil  
      list_fam_ang[[i]] = pow_abs
    })
   
  }
}



