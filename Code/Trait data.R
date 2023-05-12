library("tidyverse")
trait <- read_tsv("./Files/Trait data.txt", guess_max = Inf)
trait$sp_name <- paste(trait$Genus, trait$Species, sep = " ") # combined species name
LDMC <- trait %>% group_by(sp_name) %>% summarise(LDMC_mean = mean(LDMC), LDMC_std = sqrt(var(LDMC)))
trait <- left_join(trait, LDMC, by = "sp_name")
trait$out <- case_when(trait$LDMC > (trait$LDMC_mean + trait$LDMC_std*2) ~ "up",
                       trait$LDMC < (trait$LDMC_mean - trait$LDMC_std*2) ~ "down") 
# determines outliers that are below (down) or above (up) mean LDMC +- 2* standard deviaton

123