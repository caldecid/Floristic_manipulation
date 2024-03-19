load("list_expowo.RData")


expowo_files <- list.files(path = paste0(getwd(),
                                "/Data/Metadata/Angiosperms/expowo_abs"))

##list for storing excel
list_exp = vector("list", length = length(expowo_files))

##naming families
names(list_exp) = str_remove_all(expowo_files, ".csv")


for(i in seq_along(expowo_files)){
  ##try catch due to some csv files
  tryCatch({
   ##reading excel files
    list_exp[[i]] <- read_excel(path = paste0(getwd(),
                                          "/Data/Metadata/Angiosperms/expowo_abs/",
                                              expowo_files[i]))
    }, error = function(e){
    ##enumerating the csv files
     
      print(i)
      
  })
  
}

##reading csv
csv_files <- c(12, 24,27,95,159)


for(i in csv_files){
  
  list_exp[[i]] = read_csv(file = paste0(getwd(),
                                      "/Data/Metadata/Angiosperms/expowo_abs/",
                                             expowo_files[i]))
}



# Modifying datasets for merging ------------------------------------------

##EXPOWO

list_expowo = vector("list", length = length(list_exp))

for(i in seq_along(list_expowo)){
  
  list_expowo[[i]] = list_exp[[i]]
  ##create the source column
  list_expowo[[i]]$source = "POWO"
  
  ##renaming and creating columns
  list_expowo[[i]] = list_expowo[[i]] %>% 
    rename(reference = publication,
                   distribution = native_to_country,
                   url = powo_uri) %>% 
    mutate(publicationYear = stri_extract_last_regex(reference,
                                                     "\\d{4}"),
           publicationYear = as.integer(publicationYear),
           citation_type = case_when(is.na(authors) ~ NA, 
 !is.na(authors) & grepl("(", authors, fixed = TRUE) == TRUE ~ "comb_nova",
 !is.na(authors) & grepl("(", authors, fixed = TRUE) == FALSE ~ "taxon_novo"),
 hybrid = if_else(hybrid == "no",
                                "FALSE", hybrid),
 hybrid = as.logical(hybrid))
}

##Naming
names(list_expowo) = str_remove_all(expowo_files, ".csv")

##calling IPNI

load("C:/Users/OD32382/Desktop/Post doc/Domingos/Floristic_manipulation/Data/Metadata/Angiosperms/ipni_abs/ipni_fam_abs_2.RData")


for(i in seq_along(list_fam_ipni)) {
  list_fam_ipni[[i]]$status = "Accepted"
  
  list_fam_ipni[[i]]$scientific_name = paste(list_fam_ipni[[i]]$genus,
                                             list_fam_ipni[[i]]$species, 
                                             list_fam_ipni[[i]]$authors)
  
  list_fam_ipni[[i]]$accepted_name = list_fam_ipni[[i]]$scientific_name
  
  list_fam_ipni[[i]]$source = "IPNI"
  
  list_fam_ipni[[i]]$url = paste0("www.ipni.org", list_fam_ipni[[i]]$url)
  
  list_fam_ipni[[i]]$native_to_botanical_countries = NA
  
  list_fam_ipni[[i]]$introduced_to_country = NA
  
  list_fam_ipni[[i]]$introduced_to_botanical_countries = NA
  
    list_fam_ipni[[i]] = list_fam_ipni[[i]] %>% rename(taxon_name = name,
                   kew_id = id,
                   citation_type = citationType) %>% 
    select(family, genus, species, taxon_name, authors,
           scientific_name, status, accepted_name,
           reference, hybrid, distribution,
           native_to_botanical_countries, introduced_to_country,
           introduced_to_botanical_countries,
           kew_id, url, source, publicationYear, citation_type)%>% 
    mutate(citation_type = if_else(citation_type == "tax. nov.",
                                   "taxon_novo", citation_type))
}


##binding dataframes

##obtaining names
names_ipni = names(list_fam_ipni)

names_expowo = names(list_expowo)

##looking for common family names in both dataframes
expo_com = which(names_expowo %in% names_ipni)

ipni_com = which(names_ipni %in% names_expowo)

##subseting lists
list_expo_com = list_expowo[expo_com]

list_ipni_com = list_fam_ipni[ipni_com]

##empty list
list_combined = vector("list", length = length(list_ipni_com))

##binding dataframes and removing duplicates
for(i in seq_along(list_expo_com)){
  list_combined[[i]] = bind_rows(list_expo_com[[i]], list_ipni_com[[i]])
  
  list_combined[[i]] = list_combined[[i]][!duplicated(list_combined[[i]]$taxon_name), ]  
  
}

names(list_combined) = names(list_expo_com)

##families only present in expowo
list_expo_only = list_expowo[-expo_com]

##families only present in IPNI 
list_ipni_only = list_fam_ipni[-ipni_com]

###combining list
list_missing_data = c(list_combined, list_expo_only, list_ipni_only)

##saving
save(list_missing_data, file = "C:/Users/OD32382/Desktop/Post doc/Domingos/Floristic_manipulation/Data/Processed/Angiosperms/missing_powo_ipni/list_missing.RData" )

##now as dataframe
df_missing <- do.call(bind_rows, list_missing_data)

write_csv(df_missing, file = "C:/Users/OD32382/Desktop/Post doc/Domingos/Floristic_manipulation/Data/Processed/Angiosperms/missing_powo_ipni/df_missing.csv")

##names missing
names_missing = names(list_missing_data)

##now each family (xlsx)

for(i in seq_along(list_missing_data)){
  tryCatch({
    ##reading excel files
    write_xlsx(list_missing_data[[i]],
               path = paste0(getwd(),
                "/Data/Processed/Angiosperms/missing_powo_ipni/",
                names_missing[i], ".xlsx"))
  }, error = function(e){
    ##enumerating the csv files
    
    print(names_missing[i])
    
  })
  
}

##some did not were converted to xlsx
csv_names = c("Apocynaceae", "Bignoniaceae", "Boraginaceae", 
              "Iridaceae","Rubiaceae")

for(i in seq_along(csv_names)) {
  write_csv(list_missing_data[[csv_names[i]]],
             file = paste0(getwd(),
                           "/Data/Processed/Angiosperms/missing_powo_ipni/csv/",
                           csv_names[i], ".csv"))
  
}

