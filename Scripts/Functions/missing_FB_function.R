# Obtaining missing species in FFB ----------------------------------------

##github repository
devtools::install_github("barnabywalker/kewr")

#' @title Missing species from Flora de Brasil (FB) but present in international repositories

#' @description This function obtains the missing brazilian species from FB but present in the POWO and IPNI repositories
 
#' @param df A dataframe containing the latest version of Flora de Brasil Angiosperm species. In can be with a subset of families

#' @returns A dataframe containing the missing species in Flora de Brasil and present in POWO and IPNI
#' 
#' @import kewr
#' @import tidyverse
#' @import readr
#' @import readxl
#' @import plyr
#' @import writexl
 

missing_FFB_function <- function(df){
  
  ##completing dataframe information (auxiliary function)
  
  plants_bra = complete_df(df = df)
  
  ##dataset with only species with hifen
  plants_hifen =
         plants_bra[which(stringr::str_detect(plants_bra$taxon_name,
                                               "-")), ]
  ##removing hifen
  plants_hifen$taxon_name =  stringr::str_replace(plants_hifen$taxon_name, 
                                                          "-", "")
  ## joining datasets 
  plants_bra <- rbind(plants_bra, plants_hifen)
  
  ##family names
  fam_names <- unique(plants_bra$Family)
  
  ######creating list for keeping the missing species within each family according to IPNI
  list_fam_IPNI <- vector("list", length = length(fam_names))
  
  ######creating list for keeping the missing species within each family according to POWO
  list_fam_POWO <- vector("list", length = length(fam_names))
  
  ##naming
  names(list_fam_IPNI) <- fam_names
  
  names(list_fam_POWO) <- fam_names
  
###################IPNI######################################
  
##loop for finding missing species in the Flora de Brasil
  for(i in seq_along(fam_names)){
    
    ###tryCatch for handling the missing families in IPNI
    tryCatch({
      ##calling the species within families in IPNI
      ipni_df = tidy(kewr::search_ipni(list(family = fam_names[i],
                                      distribution = "Brazil"),
                                 limit = 1000,
                                 filters = c("species")))
     ipni_df
    }, error = function(e){
      message("Absent family in IPNI")
      print(e)
    })
    
    ##IPNI species absent in Flora de Brasil
    ipni_abs = ipni_df[-which(ipni_df$name %in% plants_bra$taxon_name), ]
    
   
    ##if else statement for not saving empty dfs
    if(dim(ipni_abs)[1] == 0){
      print("No absent species in Flora de Brasil")
    } else{
      
      ##inserting a df inside each list with the missing species in the Flora Brazil  
      list_fam_IPNI[[i]] = ipni_abs
    }
  }
  
###removing null elements
  list_fam_IPNI = list_fam_IPNI[-which(sapply(list_fam_IPNI, is.null))]

#####collapsing the list in a df
  df_ipni_families <- do.call("rbind.fill", list_fam_IPNI) %>% 
           dplyr::select(tidyselect::any_of(c("name", "family", "genus", "species",
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
    dplyr::mutate(url = paste0("www.ipni.org/n/", id))
  ##working with the columns
  df_ipni_families$citationType <- "tax_nov"
  df_ipni_families$source <- "IPNI"
  
  rownames(df_ipni_families) <- NULL

  ##eliminating duplicated species
  df_ipni_families <- df_ipni_families[-which(duplicated(df_ipni_families$name)),]
  
###########################POWO###############################################
  ##loop for finding missing species in the Flora de Brasil
  for(i in seq_along(fam_names)){
    
    ###tryCatch for handling the missing families in POWO
    tryCatch({
      ##calling the species within families in POWO
      powo_df = tidy(kewr::search_powo(list(family = fam_names[i],
                                      distribution = "Brazil"),
                                 limit = 1000,
                                 filters = c("species", "accepted")))
     powo_df
    }, error = function(e){
      message("Absent family in POWO")
      print(e)
    })
    
    
    ##POWO species absent in Flora de Brasil
    powo_abs = powo_df[-which(powo_df$name %in% plants_bra$taxon_name), ]
    
    ##if else statement for not saving empty dfs
    if(dim(powo_abs)[1] == 0){
      print("No absent species in Flora de Brasil")
    } else{
      powo_abs[,c('genus', 'species')] = stringr::str_split_fixed(powo_abs$name, ' ', 2) 
      
      powo_abs = dplyr::rename(powo_abs, "authors" = "author")
      
      powo_abs = powo_abs %>% dplyr::relocate(name, family, genus, species, authors)
      ##inserting a df inside each list with the missing species in the 
      list_fam_POWO[[i]] = powo_abs
  
    }
  }
  
###removing null elements
list_fam_POWO = list_fam_POWO[-which(sapply(list_fam_POWO, is.null))]
  
##collapsing the list in a df
df_powo_families <- do.call("rbind", list_fam_POWO) 

rownames(df_powo_families) <- NULL

##Source
df_powo_families$source <- "POWO"

##merging dataframes, not repeating species from both repositories
df_missing_flora <- plyr::rbind.fill(df_ipni_families,
          df_powo_families[!df_powo_families$name %in% df_ipni_families$name,])

return(df_missing_flora)

}

