
###########expowo##########

powo_fam <- unique(df_powo_families$family)

list_expowo <- vector("list", length = length(powo_fam))

names(list_expowo) = powo_fam

for(i in 18:length(powo_fam)){
  ##filetring family
  fam_powo = df_powo_families %>% filter(family == powo_fam[i])
  
  ##extracting genus
  powo_genus = unique(fam_powo$genus)
  
  list_genus <- vector("list", length = length(powo_genus))
  
  for(j in seq_along(powo_genus)){
    
    
    ##implementing expowo
    tryCatch({
      ##calling the species within families and genus in POWO
      expowo_df = powoSpecies(family = powo_fam[i],
                                  genus = powo_genus[j],
                                  hybrid = TRUE,
                                  synonyms = TRUE,
                                  country = "Brazil")
      
      #arranging taxa names
      expowo_df$taxon_name = str_replace(expowo_df$taxon_name, " ", "_")
      
      #POWO species absent in Flora de Brasil
      list_genus[[j]]= 
        expowo_df[-which(expowo_df$taxon_name %in% angiosperms_bra$taxon_name), ]
      return(list_genus[[j]])
    }, error = function(e){
      message("Absent genus in POWO")
      print(e)
    })
   
   
    
  
  }
  
 
  list_expowo[[i]] = do.call("rbind", list_genus)
}
