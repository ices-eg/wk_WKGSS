
#catch by area

#install.packages('mapplots')
library(mar)
library(icesSAG)
library(fs)

tyr <- lubridate::year(Sys.Date())
species_number <- 19
mar <- connect_mar()



landings <- 
  tbl_mar(mar,'ops$bthe."ices_landed_catch"') %>% 
  filter((trim(area) == "5" & trim(subarea) %like% 'a%' | trim(area) == "14") , 
         species_code == 'ARU',
         (country != 'Iceland' & year>1981 )| year < 1982) %>% 
  group_by(year,area,country) %>% 
  summarise(c = sum(landings,na.rm = TRUE)) %>% 
  collect(n=Inf) %>% 
  bind_rows(fiskifelag_oslaegt(mar) %>% 
              filter(fteg == species_number) %>% 
              left_join(tbl(mar,'gear_mapping')) %>%
              mutate(gear = ifelse(nvl(gear,' ') %in% c('BMT'),gear, 'Other')) %>% 
              group_by(gear,ar) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3) %>% 
              mutate(country='Iceland') %>% 
              collect(n=Inf) %>% 
              rename(year=ar) %>% 
              mutate(area = '5.a')) %>%  
  bind_rows(lods_oslaegt(mar) %>% 
              filter(fteg == species_number,veidisvaedi == 'I',ar>1993) %>% 
              left_join(lesa_skipaskra(mar)) %>% 
              mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
              left_join(tbl(mar,'gear_mapping')) %>%
              mutate(gear = ifelse(nvl(gear,' ') %in% c('BMT'),gear, 'Other')) %>% 
              group_by(gear,ar,country) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3)%>% 
              collect(n=Inf) %>% 
              rename(year=ar) %>% 
              mutate(area = '5.a')) %>% 
  filter(year < tyr,c>0) %>% 
  group_by(year,area, gear,country) %>% 
  summarise(c=sum(c)) %>% 
  rename(landings = c) %>% 
  mutate(species = species_number,
         country = forcats::fct_recode(country,Norway = 'NO',
                                       `Faroe Islands` = 'FO',
                                       Belgium = 'BE', 
                                       Greenland = "GR",
                                       Greenland = 'GL',
                                       Germany = 'DE',
                                       Russia = 'RU'),
         country = ifelse(grepl('UK',country)|country=='GB','UK',country)) %>% 
  select(year,species,country,area,gear,landings) %>% 
  #this is a hack that will improve next year when landings tables are requested
  bind_rows(data_frame(year = 2014:2018, species = 19, country = 'Greenland', area = '14', gear = 'BMT', landings = c(4,12,16,666,425))) %>%  
  rename(Year = year, Country = country) %>% 
  mutate(Species = 'ARU',
         Stock = 'ARU',
         Subarea = ifelse(area=='14', '27.14', '27.5'), 
         Division = ifelse(area=='14', '27.14', '27.5a'),
         CatchCategory = 'landings') %>%
  group_by(Species, Stock, Year, Country, CatchCategory, Subarea, Division) %>% 
  summarise(Caton = sum(landings)) %>% 
  write_csv('data_to_share/CatchAreaCountry ARU.27.5a14.csv')


  #ices_rectangle	quarter	year	species	catch_kg

gearlist<-NULL
imp_gears<-gearlist[[19]]<-c('BMT','LLN', 'GIL')

## position of samples from the catches

#spatial data combined with Greenland
greenland_prop <-
  readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
  select(YEAR, MD, Lat, Long, Amount) %>% 
  setNames(.,c('year', 'month', 'lat', 'lon','catch')) %>% 
  mutate(towtime = 0, section = '14') %>%
  filter(year %in% c(2017)) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  left_join(
    readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
      select(YEAR, MD, Lat, Long, Amount) %>% 
      setNames(.,c('year', 'month', 'lat', 'lon','catch')) %>% 
      mutate(towtime = 0, section = '14') %>%
      filter(year %in% c(2017)) %>% 
      group_by(year) %>% 
      summarise(tot = sum(catch))
  ) %>% 
  group_by(year,rect) %>% 
  summarise(prop = sum(catch / tot)) %>% 
  ungroup %>% 
  mutate(year = 2018) %>% 
  full_join(readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
              select(YEAR, MD, Lat, Long, Amount) %>% 
              setNames(.,c('year', 'month', 'lat', 'lon','catch')) %>% 
              filter(is.na(lat)) %>% 
              mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                                      ifelse(month %in% c(4,5,6), 2, 
                                             ifelse(month %in% c(7,8,9), 3, 4)))) %>% 
              group_by(year, quarter) %>% 
              summarise(tot_catch = sum(catch))) %>% 
  mutate(catch_kg = tot_catch*prop)


catch_dist <-
  tbl(mar,paste0('GSS','_catch')) %>% 
  filter(year > 1987) %>% 
  select(towtime, year, month, lat, lon, catch) %>% 
  collect(n=Inf) %>% 
  mutate(section = '5a') %>% 
  bind_rows(readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
              select(YEAR, MD, Lat, Long, Amount) %>% 
              setNames(.,c('year', 'month', 'lat', 'lon','catch')) %>% 
              mutate(towtime = 0, section = '14')) %>% 
  mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                          ifelse(month %in% c(4,5,6), 2, 
                                 ifelse(month %in% c(7,8,9), 3, 4)))) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  group_by(year,quarter,rect) %>% 
  summarise(catch_kg=sum(catch)) %>% 
  left_join(tbl(mar,paste0('GSS','_catch')) %>% 
              #filter(year > 2016, gear %in% imp_gears) %>% 
              select(towtime, year, month, lat, lon, catch) %>% 
              collect(n=Inf) %>% 
              mutate(section = '5a') %>% 
              bind_rows(readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
                          select(YEAR, MD, Lat, Long, Amount) %>% 
                          setNames(.,c('year', 'month', 'lat', 'lon','catch')) %>% 
                          mutate(towtime = 0, section = '14')) %>% 
              mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                                      ifelse(month %in% c(4,5,6), 2, 
                                             ifelse(month %in% c(7,8,9), 3, 4)))) %>% 
              group_by(year,quarter) %>% 
              summarise(tot_catch_ton = sum(catch)/1000)
  ) %>% 
  left_join(fiskifelag_oslaegt(mar) %>% 
                          filter(fteg == species_number) %>% 
                          group_by(ar,man) %>% 
                          summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3) %>% 
                          collect(n=Inf) %>% 
                          rename(year=ar, month = man) %>%   
              bind_rows(lods_oslaegt(mar) %>% 
                          filter(fteg == species_number,veidisvaedi == 'I',ar>1993) %>% 
                          left_join(lesa_skipaskra(mar)) %>% 
                          mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
                          filter(country == 'Iceland') %>% 
                          group_by(ar,man) %>% 
                          summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3)%>% 
                          collect(n=Inf) %>% 
                          rename(year=ar, month = man)) %>%
              mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                                      ifelse(month %in% c(4,5,6), 2, 
                                             ifelse(month %in% c(7,8,9), 3, 4)))) %>% 
              filter(year < tyr,c>0) %>% 
              group_by(year,quarter) %>% 
              summarise(landings_ton=sum(c))
            ) %>% 
  mutate(raising_factor = landings_ton/tot_catch_ton,
         raised_catch = catch_kg*ifelse(is.na(raising_factor), 1, raising_factor)) %>% 
  filter(!grepl('NA', rect)) %>% 
  bind_rows(greenland_prop %>% 
              select(year, quarter, rect, raised_catch = catch_kg)) %>% 
  group_by(year, quarter, rect) %>% 
  summarise(catch_kg = sum(raised_catch)) %>% 
  rename(ices_rectangle = rect) %>%
  mutate(species = 'ARU') %>% 
  write_csv('data_to_share/CatchByRect ARU.27.5a14.csv')
  #dbWriteTable(mar,paste0('GSS','temp_spat_catch_byQ'),.,overwrite =TRUE)

#biological data
biol_data <-
  lesa_kvarnir(mar) %>% 
  filter(tegund==19) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(synaflokkur==35) %>%
  rename(haul_id = synis_id, length_cm = lengd, age = aldur, weight_g = oslaegt, maturity_stage = kynthroski,) %>% 
  mutate(gender = ifelse(kyn==1, 'M',
                         ifelse(kyn==2, 'F', NA)),
         maturity = ifelse(maturity_stage==1, 'immature',
                           ifelse(maturity_stage %in% c(2:4), 'mature',NA)),
         source = 'IAGS',
         person = 'Pamela J. Woods'
         )  %>% 
  select(person, source, haul_id, age, length_cm, weight_g, gender, maturity_stage, maturity) %>% 
  filter(!(is.na(age) & is.na(length_cm) & is.na(weight_g) & is.na(gender) & is.na(maturity_stage))) %>% 
  write_csv('data_to_share/BiolData_ARU.27.5a14.csv')

  
#stations

biol_data <-
  lesa_kvarnir(mar) %>% 
  filter(tegund==19) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(synaflokkur==35) %>%
  mutate(person = 'Pamela J. Woods',
         source = 'IAGS',
         country = 'Iceland',
         division = '27.5.a'
         ) %>% 
  rename(haul_id = synis_id, depth = togdypi_hift) %>% 
  #convert dags to day month year
  select(person, source, country, division, haul_id, day, month, year, lat lon, depth)
  

#Add code here

#length distributions
  
  
