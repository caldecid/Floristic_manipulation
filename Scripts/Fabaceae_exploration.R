#########calling Fabacea sp, 2008 to 2020, absent in Flora

fabaceae_missing <- readxl::read_excel("Data/Metadata/Angiosperms/ipni/Leguminosae.xlsx")


###Fabaceae missing in Flora
fabaceae_2008_2020_faltantes <- fabaceae_missing %>% 
                         filter(publicationYear %in% 2008:2020)

##writing fabacea 2008 to 2020 absent in Flora
write_xlsx(fabaceae_2008_2020_faltantes, 
           path = "Data/Processed/Angiosperms/fabaceae_2008_2020_faltantes.xlsx")

##calling Fabacea  2008 to 2020, in Brazil according to IPNI
x <- 2008:2020

fabacea_list <- vector("list", length(x))


for(i in seq_along(x)){
  
  fabacea_list[[i]] <- tidy(search_ipni(list(family = "Fabaceae",
                                             published = x[i],
                                             distribution = "Brazil"),
                                        filters=c("species")))
  
}

fabacea_2008_2020_brasil <- do.call("rbind.fill", fabacea_list) %>% 
                    relocate(family, genus, species)

write_xlsx(fabacea_2008_2020_brasil, 
           path = "Data/Processed/Angiosperms/fabaceae_2008_2020_brasil_ipni.xlsx")

