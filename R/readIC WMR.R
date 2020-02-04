##################
# Create IC files 
# ARU - 2019
##################

rm(list=(ls()))

# Required packages
library(tidyverse)
# Parameters
ind_year <- "2018"
output_year   <- "2019"
datacall <- "WGDEEP"
species <- "ARU"
stock <- "aru.27.123a4"
# Load dataset 
path <- ""
output_path <- ""
# 1. Effort
load(file = paste0(path, "ARU_effort.Rdata"))
# 2. Landings
load(file = paste0(path, "ARU_catch.Rdata"))


# Effort - HI FILES
# a. format
deffs <- deffk %>%
  mutate(AreaType = "Div", 
         RecordType = "HI", 
         Country = "NL", 
         Year = as.numeric(ind_year), 
         SeasonType = ifelse(Datacall %in% "WGCEPH" , "Month", "Quarter"), 
         Season = ifelse(Datacall %in% "WGCEPH", MONTH, QUARTER), 
         FishingArea = paste0("27.", DIVISION), 
         UnitEffort = "kwd", 
         DepthRange = NA, 
         AreaQualifier = NA, 
         kwd = round(kwd, digits = 0))%>%
  rename(Fleet = METIER, Effort = kwd)%>%
  select(2, 31:35, 26, 30, 36, 38, 37, 29, 39)

# b. aggregate 
eff_fin <- deffs %>% 
  group_by(RecordType, Country,  Year, SeasonType, Season, Fleet, AreaType, FishingArea,  DepthRange,  UnitEffort, AreaQualifier) %>% 
  summarize(Effort = sum(Effort, na.rm = TRUE)) %>% 
  select(1:10, 12, 11)

# Landings - SI FILES 
# a. format

cats <- cats %>%
  mutate(RecordType = "SI", 
         Country = "NL", 
         Year = as.numeric(ind_year), 
         SeasonType = ifelse(Datacall %in% "WGCEPH", "Month", "Quarter"), 
         Season = ifelse(Datacall %in% "WGCEPH", MONTH, QUARTER), 
         AreaType = "Div",
         FishingArea = paste0("27.", DIVISION),
         DepthRange = NA,
         Species = Species,
         Stock = Stock,
         CatchCategory = "L",
         ReportingCategory = "R",
         DataToFrom = NA,
         Usage = "H",
         SamplesOrigin = "O",
         QualityFlag = NA,
         UnitCATON = "kg",
         CATON = round(WEIGHT, digits = 0),
         OffLandings = -9,
         varCATON = -9,
         InfoFleet = "No particular",
         InfoStockCoordinator = "No particular",
         InfoGeneral = "No particular")%>%
  rename(Fleet = METIER)%>%
  select(25:29, 19, 30:32, 3, 2, 33:45)

# b. Aggregate

cat_fin <- cats %>%
  group_by(RecordType, Country,  Year, SeasonType, Season, Fleet, AreaType, FishingArea,  DepthRange,  Species, Stock, CatchCategory, ReportingCategory, DataToFrom, Usage, SamplesOrigin, QualityFlag, UnitCATON, OffLandings, varCATON, InfoFleet, InfoStockCoordinator, InfoGeneral) %>% 
  summarize(CATON = sum(CATON, na.rm = TRUE)) %>% 
  select(1:18, 24, 19:23) %>% 
  ungroup(Stock) %>% 
  mutate(Stock = NA)

# Match effort and landings (nrow(SI FILES) = nrow(HI FILES))

index_landing <- paste0(cat_fin$Season, cat_fin$Fleet, cat_fin$AreaType, cat_fin$FishingArea)
index_effort <- paste0(eff_fin$Season, eff_fin$Fleet, eff_fin$AreaType, eff_fin$FishingArea)
ind <- which(index_effort %in% index_landing)
eff_match <- eff_fin[ind,]



# Create IC file 

write.table(eff_match, sep=",", paste(output_path, datacall, "_Intercatch_output_HI_SI_", species, "_", ind_year, "_", stock, ".csv", sep=""), row.names=FALSE, col.names=FALSE, na = "NA")
write.table(cat_fin, sep=",", paste(output_path, datacall, "_Intercatch_output_HI_SI_", species, "_", ind_year, "_", stock , ".csv", sep=""), row.names=FALSE, append = TRUE, col.names=FALSE, na = "NA")