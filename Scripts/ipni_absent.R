######IPNI search
devtools::install_github("barnabywalker/kewr")
library(kewr)
library(tidyverse)
library(readr)
library(readxl)
library(plyr)



##angiosperms, filling information, dropping NA, and creating tax_name

angiosperms_bra <- read_excel("Data/Raw/Angiospermas_07 08 2023.xlsx") %>% 
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
  
  ##subseting the angiosperm df
  angio_fam = angiosperms_bra %>% filter(Família == fam_names[i])
  
  ###tryCatch for handling the missing families in IPNI
  tryCatch({
    ##calling the species within families in IPNI
    ipni_df = tidy(search_ipni(list(family = fam_names[i],
                                    distribution = "Brazil"),
                               limit = 1000))
    ##filtering for only considering species
    ipni_df = ipni_df %>% filter(rank == "spec.")
                   return(ipni_df)
  }, error = function(e){
    message("Absent family in IPNI")
    print(e)
  })
    
    #arranging taxa names
    ipni_df$name = str_replace(ipni_df$name, " ", "_")
    
    ##POWO species absent in Flora de Brasil
    ipni_abs = ipni_df[-which(ipni_df$name %in% angio_fam$taxon_name), ]
    
    ##if else statement for not saving empty dfs
    if(dim(ipni_abs)[1] == 0){
      print("No absent species in Flora de Brasil")
    } else{
      ##Saving automatically in the metadata folder
      write.csv(ipni_abs, file = paste0("Data/Metadata/Angiosperms/ipni/",
                                        fam_names[i], ".csv"))
      
      ##inserting a df inside each list with the missing species in the Flora Brazil  
      list_fam_ang[[i]] = ipni_abs
    }
}


###removing null elements
list_fam_ipni = list_fam_ang[-which(sapply(list_fam_ang, is.null))]


##saving list
save(list_fam_ipni, file = "Data/Metadata/Angiosperms/ipni/ipni_fam_abs.RData")

##collapsing the list in a df
df_ipni_families <- do.call("rbind.fill", list_fam_ipni) 

rownames(df_ipni_families) <- NULL


##writing
write_csv(df_ipni_families, file = "Data/Metadata/Angiosperms/ipni/ipni_fam_abs.csv")



