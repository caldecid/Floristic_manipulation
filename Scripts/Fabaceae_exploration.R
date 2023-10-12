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

# Asteraceae -------------------------------------------------------------

#########calling Asteraceae sp, 2008 to 2020, absent in Flora

Asteraceae_missing <- read_excel("Data/Metadata/Angiosperms/ipni/Asteraceae.xlsx")


###Asteraceae missing in Flora
Asteraceae_2008_2020_faltantes <- Asteraceae_missing %>% 
                               filter(publicationYear %in% 2008:2020)

##writing fabacea 2008 to 2020 absent in Flora
write_xlsx(Asteraceae_2008_2020_faltantes, 
           path = "Data/Processed/Angiosperms/Asteraceae_2008_2020_faltantes.xlsx")

##calling Fabacea  2008 to 2020, in Brazil according to IPNI
x <- 2008:2020

Asteraceae_list <- vector("list", length(x))


for(i in seq_along(x)){
  
  Asteraceae_list[[i]] <- tidy(search_ipni(list(family = "Asteraceae",
                                             published = x[i],
                                             distribution = "Brazil"),
                                        filters=c("species")))
  
}

Asteraceae_2008_2020_brasil <- do.call("rbind.fill", Asteraceae_list) %>% 
  relocate(family, genus, species)

write_xlsx(Asteraceae_2008_2020_brasil, 
      path = "Data/Processed/Angiosperms/Asteraceae_2008_2020_brasil_ipni.xlsx")


# Asteraceae --------------------------------------------------------------

#########calling Asteraceae sp, 2008 to 2020, absent in Flora

Asteraceae_missing <- read_excel("Data/Metadata/Angiosperms/ipni/Asteraceae.xlsx")


###Asteraceae missing in Flora
Asteraceae_2008_2020_faltantes <- Asteraceae_missing %>% 
  filter(publicationYear %in% 2008:2020)

##writing fabacea 2008 to 2020 absent in Flora
write_xlsx(Asteraceae_2008_2020_faltantes, 
           path = "Data/Processed/Angiosperms/Asteraceae_2008_2020_faltantes.xlsx")

##calling Fabacea  2008 to 2020, in Brazil according to IPNI
x <- 2008:2020

Asteraceae_list <- vector("list", length(x))


for(i in seq_along(x)){
  
  Asteraceae_list[[i]] <- tidy(search_ipni(list(family = "Asteraceae",
                                                 published = x[i],
                                                 distribution = "Brazil"),
                                            filters=c("species")))
  
}

Asteraceae_2008_2020_brasil <- do.call("rbind.fill", Asteraceae_list) %>% 
  relocate(family, genus, species)

write_xlsx(Asteraceae_2008_2020_brasil, 
           path = "Data/Processed/Angiosperms/Asteraceae_2008_2020_brasil_ipni.xlsx")


# Rubiaceae ---------------------------------------------------------------

#########calling Rubiaceae sp, 2008 to 2020, absent in Flora

Rubiaceae_missing <- read_excel("Data/Metadata/Angiosperms/ipni/Rubiaceae.xlsx")


###Rubiaceae missing in Flora
Rubiaceae_2008_2020_faltantes <- Rubiaceae_missing %>% 
  filter(publicationYear %in% 2008:2020)

##writing fabacea 2008 to 2020 absent in Flora
write_xlsx(Rubiaceae_2008_2020_faltantes, 
           path = "Data/Processed/Angiosperms/Rubiaceae_2008_2020_faltantes.xlsx")

##calling Rubiaceae  2008 to 2020, in Brazil according to IPNI
x <- 2008:2020

Rubiaceae_list <- vector("list", length(x))


for(i in seq_along(x)){
  
  Rubiaceae_list[[i]] <- tidy(search_ipni(list(family = "Rubiaceae",
                                                published = x[i],
                                                distribution = "Brazil"),
                                           filters=c("species")))
  
}

Rubiaceae_2008_2020_brasil <- do.call("rbind.fill", Rubiaceae_list) %>% 
  relocate(family, genus, species)

write_xlsx(Rubiaceae_2008_2020_brasil, 
           path = "Data/Processed/Angiosperms/Rubiaceae_2008_2020_brasil_ipni.xlsx")


# Melastomataceae ---------------------------------------------------------

#########calling Melastomataceae sp, 2008 to 2020, absent in Flora

Melastomataceae_missing <- read_excel("Data/Metadata/Angiosperms/ipni/Melastomataceae.xlsx")


###Melastomataceae missing in Flora
Melastomataceae_2008_2020_faltantes <- Melastomataceae_missing %>% 
  filter(publicationYear %in% 2008:2020)

##writing fabacea 2008 to 2020 absent in Flora
write_xlsx(Melastomataceae_2008_2020_faltantes, 
           path = "Data/Processed/Angiosperms/Melastomataceae_2008_2020_faltantes.xlsx")

##calling Melastomataceae  2008 to 2020, in Brazil according to IPNI
x <- 2008:2020

Melastomataceae_list <- vector("list", length(x))


for(i in seq_along(x)){
  
  Melastomataceae_list[[i]] <- tidy(search_ipni(list(family = "Melastomataceae",
                                               published = x[i],
                                               distribution = "Brazil"),
                                          filters=c("species")))
  
}

Melastomataceae_2008_2020_brasil <- do.call("rbind.fill", Melastomataceae_list) %>% 
  relocate(family, genus, species)

write_xlsx(Melastomataceae_2008_2020_brasil, 
           path = "Data/Processed/Angiosperms/Melastomataceae_2008_2020_brasil_ipni.xlsx")
