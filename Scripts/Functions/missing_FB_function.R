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
     
    }, error = function(e){
      message("Absent family in IPNI")
      print(e)
    })
    
    ##IPNI species absent in Flora de Brasil
    ipni_abs = ipni_df[-which(ipni_df$name %in% plants_bra$taxon_name), ]
    
    ##looking and removing for hifens
    ipni_abs$hifen_name = stringr::str_replace(ipni_abs$name, 
                                               "-", "")
    
    ##IPNI species with hifen absent in FB
    if(sum(ipni_abs$hifen_name %in% plants_bra$taxon_name) != 0){
      ipni_abs <- ipni_abs[-which(ipni_abs$hifen_name %in% plants_bra$taxon_name), ]
    } else {
      ipni_abs
    }
    
    ##removing column
    ipni_abs <- ipni_abs %>% dplyr::select(-hifen_name)
    
    
    ##if else statement for not saving empty dfs
    if(dim(ipni_abs)[1] == 0){
      print("No absent species in Flora de Brasil")
      
      list_fam_IPNI[[i]] = NULL
      
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
                    "authors", "citationType", "hybrid",
                    "rank", "reference", 
                    "publication", "publicationYear",
                    "referenceCollation",
                    "publicationId",
                    "typeLocations", "collectorTeam",
                    "collectionNumber", "collectionDate1",
                    "distribution", "locality", "id", "bhlLink",
                    "publicationYearNote", "remarks")))%>% 
    dplyr::mutate(url = paste0("www.ipni.org/n/", id))
  ##working with the columns
  df_ipni_families$citationType <- "tax_nov"
  df_ipni_families$source <- "IPNI"
  
  ##obtaining the doi
  df_ipni_families$doi <- ifelse(str_detect(df_ipni_families$remarks,
                                            "doi:[^\\s]+"), 
                            paste0("https://doi.org/",
                                   str_extract(df_ipni_families$remarks,
                                               "(?<=doi:)[^\\s]+")), NA)
                               
  
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
    
    ##looking and removing for hifens
    powo_abs$hifen_name = stringr::str_replace(powo_abs$name, 
                                               "-", "")
    
    ##Powo species with hifen absent in FB
    if(sum(powo_abs$hifen_name %in% plants_bra$taxon_name) != 0){
      
      powo_abs <- powo_abs[-which(powo_abs$hifen_name %in% plants_bra$taxon_name), ]
      
    } else {
      powo_abs
    }
    
    ##removing column
    powo_abs <- powo_abs %>% dplyr::select(-hifen_name)
    
    
    ##if else statement for not saving empty dfs
    if(dim(powo_abs)[1] == 0){
      print("No absent species in Flora de Brasil")
      
      list_fam_POWO[[i]] = NULL
      
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

##id
df_powo_families$id <- gsub("[^0-9-]", "", df_powo_families$fqId)

##url
df_powo_families$url <- paste0("https://powo.science.kew.org/taxon/",
                           df_powo_families$id)

##removing species that appear both in IPNI and POWO
df_powo_families <- df_powo_families[!df_powo_families$name %in% df_ipni_families$name,]

##generating reference and publicationYear for merging
df_powo_families$reference <- NA

df_powo_families$publicationYear <- NA

df_powo_families$hybrid <- NA

##loop for generating the reference and publicationYear from lookup_powo
for(i in 1:nrow(df_powo_families)) {
  
  ##temporal dataset
  tem_df <- tidy(lookup_powo(df_powo_families$id[i]))
  
  #if else statements due some powo df dont have this info
  if(is.null(tem_df$reference)){
    df_powo_families$reference[i] = NA
  }else {
    df_powo_families$reference[i] <- tem_df$reference
  }
  if(is.null(tem_df$namePublishedInYear)){
    df_powo_families$publicationYear[i] = NA
  }else {
    df_powo_families$publicationYear[i] <- tem_df$namePublishedInYear
  }
  if(is.null(tem_df$hybrid)){
    df_powo_families$hybrid[i] = NA
  }else {
    df_powo_families$hybrid[i] <- tem_df$hybrid
  }
  
}

df_powo_families <- df_powo_families %>%
                select(any_of(c("name", "family", "genus", "species",
                               "authors", "rank", "reference", "hybrid",
                               "publicationYear", "id", "url", "source")))
##merging dataframes
df_missing_flora <- plyr::rbind.fill(df_ipni_families, df_powo_families)

##removing hybrid species
df_missing_flora <- df_missing_flora %>% dplyr::filter(hybrid == "FALSE")

##removing hybrid column
df_missing_flora <- df_missing_flora %>% dplyr::select(-hybrid)

return(df_missing_flora)

}

