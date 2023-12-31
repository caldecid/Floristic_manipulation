library(tidyverse)
library(expowo)
library(plyr)

###########expowo##########

powo_fam <- unique(angiosperms_bra$Família)

##second round
powo_fam_1 = powo_fam[1:50]
list_expowo <- vector("list", length = length(powo_fam_1))

names(list_expowo) = powo_fam_1


for(i in seq_along(powo_fam_1)){
  ##filtering family
  fam_powo = angiosperms_bra %>% filter(Família == powo_fam[i])
  
  ##extracting genus
  powo_genus = unique(fam_powo$genus)
  
  list_genus <- vector("list", length = length(powo_genus))
  
  for(j in seq_along(powo_genus)){
    
    
    ##implementing expowo
    tryCatch({
      ##calling the species within families and genus in POWO
      expowo_df = powoSpecies(family = "Acanthaceae",
                                  hybrid = TRUE,
                                  synonyms = TRUE,
                                  country = "Brazil")
      
      #arranging taxa names
      expowo_df$taxon_name = str_replace(expowo_df$taxon_name, " ", "_")
      
      #POWO species absent in Flora de Brasil
      list_genus[[j]]= 
        x = expowo_df[-which(expowo_df$taxon_name %in% angiosperms_bra$taxon_name), ]
      return(list_genus[[j]])
    }, error = function(e){
      message("Absent genus in POWO")
      print(e)
    })
   
   
    
  
  }
  
   
  list_expowo[[i]] = do.call("rbind.fill", list_genus)
 
}



list_expowo <- list_expowo[-which(sapply(list_expowo, is.null))]


list_expowo <- list_expowo[-which(sapply(list_expowo,
                                         function(x) dim(x)[1] == 0))]


save(list_expowo, file = "Data/list_expowo.RData")

write_csv(list_expowo[[i]], 
          file = paste0("Data/Metadata/Angiosperms/expowo/expowo_syn/",
                        powo_fam[i], ".csv"))
for(i in 21:length(list_expowo)) {
  write_xlsx(list_expowo[[i]], 
            path = paste0("Data/Metadata/Angiosperms/expowo/expowo_syn/",
                          unique(list_expowo[[i]]$family), ".xlsx"))
  print(unique(list_expowo[[i]]$family))
}

##xlsx has some limits
write_csv(list_expowo[[20]], 
                     file = paste0("Data/Metadata/Angiosperms/expowo/expowo_syn/",
                                   unique(list_expowo[[20]]$family), ".csv"))

x = vector(mode = "character", length = length(list_expowo))

for(i in seq_along(list_expowo)){
  x[i] = unique(list_expowo[[i]]$family)
}


