
# POWO search -------------------------------------------------------------

library(kewr)
library(tidyverse)
library(writexl)
library(plyr)
library(stringr)


##creating new folder
path <- "Data/Metadata/Angiosperms"
newdir <- "powo"
dir.create(file.path(path, newdir))


##angiosperms, filling information, dropping NA, and creating tax_name

angiosperms_bra <- read_excel("Data/Raw/Angiospermas_07 08 2023.xlsx") %>% 
  fill(Família, Gênero) %>% 
  drop_na(Espécie) %>% 
  unite("taxon_name", Gênero:Espécie, sep = "_", remove = FALSE)


##family names
fam_names <- unique(angiosperms_bra$Família)


######creating list for keeping the missing species within the families
list_fam_ang_powo <- vector("list", length = length(fam_names))

##naming
names(list_fam_ang_powo) <- fam_names


##loop for finding missing species in the Flora de Brasil
for(i in seq_along(fam_names)){
  
  ###tryCatch for handling the missing families in POWO
  tryCatch({
    ##calling the species within families in POWO
    powo_df = tidy(search_powo(list(family = fam_names[i],
                                    distribution = "Brazil"),
                               limit = 1000))
    ##filtering for only considering species
    powo_df = powo_df %>% filter(rank == "Species")
    return(powo_df)
  }, error = function(e){
    message("Absent family in POWO")
    print(e)
  })
  
  #arranging taxa names
  powo_df$name = str_replace(powo_df$name, " ", "_")
  
  ##POWO species absent in Flora de Brasil
  powo_abs = powo_df[-which(powo_df$name %in% angiosperms_bra$taxon_name), ]
  
  
  ##if else statement for not saving empty dfs
  if(dim(powo_abs)[1] == 0){
    print("No absent species in Flora de Brasil")
  } else{
    powo_abs[,c('genus', 'species')] = str_split_fixed(powo_abs$name, '_', 2) 
    
    powo_abs = powo_abs %>% relocate(family, genus, species, author)
    ##inserting a df inside each list with the missing species in the 
    list_fam_ang_powo[[i]] = powo_abs
    
    write_xlsx(powo_abs, path = paste0("Data/Metadata/Angiosperms/powo/",
                                 fam_names[i],
                                 ".xlsx"))
    
  }
}


###removing null elements
list_fam_powo = list_fam_ang_powo[-which(sapply(list_fam_ang_powo, is.null))]


##saving list
save(list_fam_powo, file = "Data/Metadata/Angiosperms/powo/powo_fam_abs.RData")

##collapsing the list in a df
df_powo_families <- do.call("rbind", list_fam_powo) 

rownames(df_powo_families) <- NULL


##writing
write_csv(df_powo_families,
          file = "Data/Metadata/Angiosperms/powo/powo_fam_abs.csv")
