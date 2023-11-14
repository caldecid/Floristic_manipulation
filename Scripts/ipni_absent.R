
# IPNI search -------------------------------------------------------------


devtools::install_github("barnabywalker/kewr")
library(kewr)
library(tidyverse)
library(readr)
library(readxl)
library(plyr)
library(writexl)


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
for(i in seq_along(fam_names)){
  
 
  
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
    
    ##IPNI species absent in Flora de Brasil
    ipni_abs = ipni_df[-which(ipni_df$name %in% angiosperms_bra$taxon_name), ]
    
    ##if else statement for not saving empty dfs
    if(dim(ipni_abs)[1] == 0){
      print("No absent species in Flora de Brasil")
    } else{
      
      ##inserting a df inside each list with the missing species in the Flora Brazil  
      list_fam_ang[[i]] = ipni_abs
    }
}


###removing null elements
list_fam_ipni = list_fam_ang[-which(sapply(list_fam_ang, is.null))]


##saving list
save(list_fam_ipni,
     file = "Data/Metadata/Angiosperms/ipni_abs/ipni_fam_abs_2.RData")


##for loop for saving as xlsx each family
for(i in seq_along(list_fam_ipni)){
  df = list_fam_ipni[[i]] %>% select(any_of(c("family", "genus", "species",
                                              "authors", "citationType",
                                              "rank", "hybrid", "reference",
                                              "publication", "publicationYear",
                                              "referenceCollation",
                                              "publicationId", "suppressed",
                                              "typeLocations", "collectorTeam",
                                              "collectionNumber", "collectionDate1",
                                              "distribution", "locality", "id",
                                              "fqld", "inPowo", "wfold", "bhlLink",
                                              "publicationYearNote", "remarks",
                                              "referenceRemarks"))) %>% 
                 mutate(url = paste0("www.ipni.org/n/", list_fam_ipni[[i]]$id))
  
  df$citationType <- "tax_nov"
  df$source <- "ipni"
  
  write_xlsx(df, path = paste0("Data/Metadata/Angiosperms/ipni_abs/",
                               unique(list_fam_ipni[[i]]$family),
                               ".xlsx"))                                            
  
}



##collapsing the list in a df
df_ipni_families <- do.call("rbind.fill", list_fam_ipni) %>% 
                           select(any_of(c("family", "genus", "species",
                                         "authors", "citationType",
                                         "rank", "hybrid", "reference",
                                         "publication", "publicationYear",
                                         "referenceCollation",
                                         "publicationId", "suppressed",
                                         "typeLocations", "collectorTeam",
                                         "collectionNumber", "collectionDate1",
                                         "distribution", "locality", "id",
                                         "fqld", "inPowo", "wfold", "bhlLink",
                                         "publicationYearNote", "remarks",
                                         "referenceRemarks")))%>% 
  mutate(url = paste0("www.ipni.org/n/", id))

df_ipni_families$citationType <- "tax_nov"
df_ipni_families$source <- "ipni"

rownames(df_ipni_families) <- NULL

df_ipni_families$name <- str_c(df_ipni_families$genus,
                               df_ipni_families$species, sep = "_")

##eliminating duplicated species
df_ipni_families <- df_ipni_families[-which(duplicated(df_ipni_families$name)),]

##writing
write_csv(df_ipni_families,
          file = "Data/Metadata/Angiosperms/ipni_abs/ipni_fam_abs.csv")
