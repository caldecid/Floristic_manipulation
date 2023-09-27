
# working with the HV exsicatas -------------------------------------------


devtools::install_github("barnabywalker/kewr")
library(kewr)
library(dplyr)
library(readr)
library(readxl)
library(plyr)


#######reading first family

fam1 <- read_excel(path = "Data/Raw/exsicatas_HV/Acanthaceae_HV.xlsx")

fam1 <- fam1 %>% filter(`Rank:` == "Espécie")

##using the df_ipni_families

mis_fam_1 <- df_ipni_families %>% filter(family == "Acanthaceae")

fam_ex = fam1[fam1$Gênero %in% mis_fam_1$genus, ]

fam_ex = fam_ex[fam_ex$Espécie %in% mis_fam_1$species,]

length(unique(fam_ex$Espécie))
