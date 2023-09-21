##libraries
library(tidyverse)
devtools::install_github("DBOSlab/expowo")
library(expowo)
library(readxl)


#####calling dataframes

##angiosperms, filling information, dropping NA, and creating tax_name

angiosperms_bra <- read_excel("Data/Angiospermas_07 08 2023.xlsx") %>% 
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
for(i in seq_along(fam_names)){
  ##calling the species within families in POWO
  pow_df = powoSpecies(family = fam_names[i], country = "Brazil")
  
  ##arranging taxa names
  pow_df$taxon_name = str_replace(pow_df$taxon_name, " ", "_")
  
  ##subseting the angiosperm df
  angio_fam = angiosperms_bra %>% filter(Família == fam_names[i])
  
  ##POWO species absent in Flora de Brasil
  pow_abs = pow_df[-which(pow_df$taxon_name %in% angio_fam$taxon_name), ]
  
  ##if else statement for not saving empty dfs
    if(dim(pow_abs)[1] == 0){
    print("No absent species in Flora de Brasil")
  } else{
    ##Saving automatically in the metadata folder
   write_csv(pow_abs, file = "Data/Metadata/Angiosperms/", fam_names[i], ".csv")
    
  ##inserting a df inside each list with the missing species in the Flora Brazil  
    list_fam_ang[[i]] = pow_abs
  }
}


