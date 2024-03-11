
##libraries
library(vctrs)
library(tidyverse)
library(kewr)
library(plyr)

# Function for assessing "hifen" incompatibilities ------------------------

hifen_incompatibility <- function(df){}
  
  ###FFB the original dataset
  angiosperms_bra =   angiosperms_bra %>% 
    unite("taxon_name", Gênero:Espécie, sep = " ",
          remove = FALSE) 
  
  ##family names
  fam_names <- unique(angiosperms_bra$Família)
  
  
  ######creating list for keeping the species with hifen present in powo
  #but absent in FFB
  
  list_powo_hifen_absent_in_FFB <- vector("list", length = length(fam_names))
  
  ######creating list for keeping the species with hifen present in IPNI
  #but absent in FFB
  
  list_IPNI_hifen_absent_in_FFB <- vector("list", length = length(fam_names))
  
  ######creating list for keeping the species with hifen present in FFB
  #but absent in POWO
  
  list_FFB_hifen_absent_in_powo <- vector("list", length = length(fam_names))
  
  ######creating list for keeping the species with hifen present in FFB
  #but absent in IPNI
  
  list_FFB_hifen_absent_in_IPNI <- vector("list", length = length(fam_names))
  
  ##Naming lists
  
  ##naming
  names(list_powo_hifen_absent_in_FFB) <- fam_names
  names(list_IPNI_hifen_absent_in_FFB) <- fam_names
  names(list_FFB_hifen_absent_in_powo) <- fam_names
  names(list_FFB_hifen_absent_in_IPNI) <- fam_names
  
  ##only species with hifen
  FFB_hifen = angiosperms_bra$taxon_name[which(str_detect(angiosperms_bra$taxon_name,
                                                          "-"))]
  
  ##filtering 
  FFB_hifen_df = angiosperms_bra %>% filter(taxon_name %in% FFB_hifen) 
  

##mergin genus and species for taxon name due to some hifens between both in the ipni and powo
for(i in seq_along(fam_names)){
  
  

  
  tryCatch({ 
  ##powo dataframe filtered by family
  df_powo = tidy(search_powo(list(family = fam_names[i],
                                           distribution = "Brazil"),
                                      limit = 1000,
                                      filters = c("species", "accepted")))
  
  ##species with hifen
  powo_hifen = df_powo$name[which(str_detect(df_powo$name,
                                                      "-"))]
  
  #filtering
  powo_hifen_df = df_powo %>% filter(name %in% powo_hifen)
  
  ########IPNI dataframe filtered by family
  df_IPNI = tidy(search_ipni(list(family = fam_names[i],
                                           distribution = "Brazil"),
                                      limit = 1000,
                                      filters = c("species")))
  
  ##which species have hifen
  IPNI_hifen = df_IPNI$name[which(str_detect(df_IPNI$name,
                                                      "-"))]
  
  #filtering
  IPNI_hifen_df = df_IPNI %>% filter(name %in% IPNI_hifen)
  }, error = function(e){
    message("Absent family in IPNI or POWO")
    print(e)
  })
  
  
  ############Obtaining dataframes############
##which is in the powo df but it is not in the FFB df
 list_powo_hifen_absent_in_FFB[[i]] <- powo_hifen_df[which(powo_hifen_df$name %in%
                         setdiff(powo_hifen_df$name,FFB_hifen_df$taxon_name)),]
  
 ##which is in the FFB but it is not in the powo df
 list_FFB_hifen_absent_in_powo[[i]] <- FFB_hifen_df[which(FFB_hifen_df$taxon_name %in%
                         setdiff(FFB_hifen_df$taxon_name,powo_hifen_df$name)),]
  
  ##which is in the IPNI df but it is not in the FFB df
 list_IPNI_hifen_absent_in_FFB[[i]] <- IPNI_hifen_df[which(IPNI_hifen_df$name %in%
                        setdiff(IPNI_hifen_df$name,FFB_hifen_df$taxon_name)),]
  
  ###Which in the FFB but it is not in the IPNI DF
 list_FFB_hifen_absent_in_IPNI[[i]] <- FFB_hifen_df[which(FFB_hifen_df$taxon_name %in%
                       setdiff(FFB_hifen_df$taxon_name,IPNI_hifen_df$name)),]
 
  
  
}

  ##eliminating empty elements
  list_powo_hifen_absent_in_FFB = list_drop_empty(list_powo_hifen_absent_in_FFB)
  list_FFB_hifen_absent_in_powo = list_drop_empty(list_FFB_hifen_absent_in_powo) 
  list_IPNI_hifen_absent_in_FFB = list_drop_empty(list_IPNI_hifen_absent_in_FFB)
  list_FFB_hifen_absent_in_IPNI = list_drop_empty(list_FFB_hifen_absent_in_IPNI)
  
  ##transforming in a dataset
  POWO_hifen_absent_FFB = do.call("rbind", list_powo_hifen_absent_in_FFB)
  FFB_hifen_absent_POWO = do.call("rbind", list_FFB_hifen_absent_in_powo)
  IPNI_hifen_absent_FFB = do.call("rbind.fill", list_IPNI_hifen_absent_in_FFB)
  FFB_hifen_absent_IPNI = do.call("rbind", list_FFB_hifen_absent_in_IPNI)
  
  ##eliminating rownames
  rownames(POWO_hifen_absent_FFB) = NULL
  rownames(FFB_hifen_absent_POWO) = NULL
  rownames(IPNI_hifen_absent_FFB) = NULL
  rownames(FFB_hifen_absent_IPNI) = NULL
  
  ##source 
  POWO_hifen_absent_FFB$source = "POWO"
  
  IPNI_hifen_absent_FFB$source = "IPNI"

  ##merging POWO and IPNI dataframes
  hifen_absent_FFB = rbind.fill(POWO_hifen_absent_FFB, IPNI_hifen_absent_FFB)
  
  ##removing duplicated lines
  hifen_absent_FFB_distinct = distinct(hifen_absent_FFB, name,
                                       .keep_all = TRUE)

  ##saving
  write_csv(hifen_absent_FFB_distinct,
                  file = "Data/Processed/Angiosperms/hifen_absent_in_FFB.csv")
 
  ###merging FFB datasets
  FFB_hifen_absent_POWO$absent = "POWO"
  FFB_hifen_absent_IPNI$absent = "IPNI"
  
  hifen_absent_powo_and_ipni = rbind.fill(FFB_hifen_absent_POWO,
                                          FFB_hifen_absent_IPNI)
  
  ##removing duplicated lines
  hifen_absent_powo_and_ipni  = distinct(hifen_absent_powo_and_ipni, taxon_name,
                                         .keep_all = TRUE)
  
  ##saving
  write_csv(hifen_absent_powo_and_ipni,
            file = "Data/Processed/Angiosperms/hifen_absent_in_Powo_and_IPNI.csv")
  
  
 