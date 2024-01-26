library("tidyverse")
trait <- read_tsv("./Files/Trait data.txt", guess_max = Inf)
trait <- filter(trait, is.na(Remark)) # removed two Brachypodium samples that were too dry
trait$ID <- paste(str_sub(trait$Genus, start = 0, end = 3), str_sub(trait$Species, start = 0, end = 3), trait$`Plot ID`, trait$`Individual Nr`, sep = ".") #Identifier column to link data

LA <- read_tsv("./Files/Leaf scans.txt", guess_max = Inf)
trait_LA <- trait %>% select(c(8,10,14)) %>% pivot_longer(names_to = "Var", values_to = "Code", cols = c(1:2)) # selecting table of bag IDs and leaf scan codes
trait_LA <- trait_LA %>% left_join(LA, by = "Code") %>% select(-c(Var, Code)) # joining scan codes and total leaf areas
trait_LA <- trait_LA %>% na.omit %>% group_by(ID) %>% summarise(`Total LA [cm2]` = sum(Total_area)) # calculating leaf area per sample (or ID)
trait <- left_join(trait, trait_LA, by = "ID")
rm(LA, trait_LA)
trait$`LA [cm2]` <- trait$`Total LA [cm2]`/trait$`Bulk Nr of leaves` # calculating leaf area per leaf
trait$`LMA [mg/cm2]` <- trait$`Dry mass [g]`*1000/trait$`LA [cm2]` # calculating leaf mass per area [mg/cm2]

trait$sp_name <- paste(trait$Genus, trait$Species, sep = " ") # combined species name
sp_traits <- trait %>% group_by(sp_name) %>% summarise(`mean LDMC [g/g]` = mean(`LDMC [g/g]`), `std LDMC [g/g]` = sqrt(var(`LDMC [g/g]`)),
                                                        `mean LA [cm2]` = mean(`LA [cm2]`), `std LA [cm2]` = sqrt(var(`LA [cm2]`)),
                                                        `mean LMA [mg/cm2]` = mean(`LMA [mg/cm2]`), `std LMA [mg/cm2]` = sqrt(var(`LMA [mg/cm2]`)), n_obs = length(sp_name))

trait <- left_join(trait, sp_traits, by = "sp_name")
trait$out <- case_when(trait$LDMC > (trait$LDMC_mean + trait$LDMC_std*2) ~ "up",
                       trait$LDMC < (trait$LDMC_mean - trait$LDMC_std*2) ~ "down") 
# determines outliers that are below (down) or above (up) mean LDMC +- 2* standard deviaton

