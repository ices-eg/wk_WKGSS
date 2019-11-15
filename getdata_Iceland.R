
#catch by area

#install.packages('mapplots')
library(mar)
library(icesSAG)
library(fs)

tyr <- lubridate::year(Sys.Date())
species_number <- 19
mar <- connect_mar()
global_regions <- 101:108 #for haddock
ldist_bins <- NULL
ldist_bins[[19]] <- c(seq(10, 58,by=1),60)


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
              mutate(area = '5.a') ) %>% 
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
  bind_rows(readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
              select(NATION_KODE,YEAR, Amount) %>% 
              setNames(.,c('country', 'year', 'catch')) %>% 
              mutate(area = '27.14.b', 
                     country = ifelse(country %in% c('EU', 'DEU'), 'Germany',
                                      ifelse(country=='ISL', 'Iceland',
                                             ifelse(country=='NOR', 'Norway',
                                                    ifelse(country=='GRL', 'Greenland',
                                                           country))))
                           ) %>% 
              group_by(country,year) %>% 
              summarise(landings = sum(catch)) %>% 
              mutate(species = 19, gear = 'BMT')) %>%  
  rename(Year = year, Country = country) %>% 
  mutate(Species = 'ARU',
         Stock = 'ARU',
         Subarea = ifelse(area=='14', '27.14', '27.5'), 
         Division = ifelse(area=='14', '27.14.b', '27.5a'),
         CatchCategory = 'landings') %>%
  group_by(Species, Stock, Year, Country, CatchCategory, Subarea, Division) %>% 
  summarise(Caton = sum(landings)) %>% 
  write_csv('data_to_share/CatchAreaCountry ARU.27.5a14.csv')


#ices_rectangle	quarter	year	species	catch_kg

gearlist<-NULL
imp_gears<-gearlist[[19]]<-c('BMT','LLN', 'GIL')

## position of samples from the catches

#spatial data combined with Greenland

#approximation for 2018 data with missing lat / lon
greenland_prop <-
  readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
  select(NATION_KODE,YEAR, MD, Lat, Long, Amount) %>% 
  setNames(.,c('country', 'year', 'month', 'lat', 'lon','catch')) %>% 
  mutate(section = '27.14.b', 
         country = ifelse(country %in% c('EU', 'DEU'), 'Germany',
                          ifelse(country=='ISL', 'Iceland',
                                 ifelse(country=='NOR', 'Norway',
                                        ifelse(country=='GRL', 'Greenland',
                                               country))))
         ) %>%
  filter(year %in% c(2017)) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  left_join(
    readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
      select(NATION_KODE,YEAR, MD, Lat, Long, Amount) %>% 
      setNames(.,c('country', 'year', 'month', 'lat', 'lon','catch')) %>% 
      mutate(section = '27.14.b', 
             country = ifelse(country %in% c('EU', 'DEU'), 'Germany',
                              ifelse(country=='ISL', 'Iceland',
                                     ifelse(country=='NOR', 'Norway',
                                            ifelse(country=='GRL', 'Greenland',
                                                   country))))
      ) %>%
      filter(year %in% c(2017)) %>% 
      group_by(year, country) %>% 
      summarise(tot = sum(catch))
  ) %>% 
  group_by(year,country,rect) %>% 
  summarise(prop = sum(catch / tot)) %>% 
  ungroup %>% 
  mutate(year = 2018) %>% 
  full_join(
    readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
      select(NATION_KODE,YEAR, MD, Lat, Long, Amount) %>% 
      setNames(.,c('country', 'year', 'month', 'lat', 'lon','catch')) %>% 
      mutate(section = '27.14.b', 
             country = ifelse(country %in% c('EU', 'DEU'), 'Germany',
                              ifelse(country=='ISL', 'Iceland',
                                     ifelse(country=='NOR', 'Norway',
                                            ifelse(country=='GRL', 'Greenland',
                                                   country))))
      ) %>%
      
      filter(is.na(lat)) %>% 
      mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                              ifelse(month %in% c(4,5,6), 2, 
                                     ifelse(month %in% c(7,8,9), 3, 
                                            ifelse(month %in% c(10,11,12), 4, NA))))) %>% 
      group_by(country, year, quarter) %>% 
      summarise(tot_catch = sum(catch))
    )%>% 
  mutate(catch_kg = tot_catch*prop)


catch_dist <-
  tbl(mar,paste0('GSS','_catch')) %>% 
  filter(year > 1987) %>% 
  select(year, month, lat, lon, catch) %>%
  mutate(country ='Iceland') %>% 
  collect(n=Inf) %>% 
  mutate(section = '27.5.a') %>% 
  bind_rows(
    readxl::read_xlsx('../../data/Catches_ARGandARS_Greenland_logbooks (to PW, Iceland) (1).xlsx') %>% 
      select(NATION_KODE,YEAR, MD, Lat, Long, Amount) %>% 
      setNames(.,c('country', 'year', 'month', 'lat', 'lon','catch')) %>% 
      mutate(section = '27.14.b', 
             country = ifelse(country %in% c('EU', 'DEU'), 'Germany',
                              ifelse(country=='ISL', 'Iceland',
                                     ifelse(country=='NOR', 'Norway',
                                            ifelse(country=='GRL', 'Greenland',
                                                   country))))) 
    ) %>% 
  mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                          ifelse(month %in% c(4,5,6), 2, 
                                 ifelse(month %in% c(7,8,9), 3, 
                                        ifelse(month %in% c(10,11,12), 4, NA))))) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  group_by(country, year,quarter,rect,section) %>% 
  summarise(catch_kg=sum(catch)) %>% 
  left_join(tbl(mar,paste0('GSS','_catch')) %>% 
                  #filter(year > 2016, gear %in% imp_gears) %>% 
                  select(year, month, lat, lon, catch) %>% 
                  mutate(country = 'Iceland') %>% 
                  collect(n=Inf) %>% 
                  mutate(section = '27.5.a') %>% 
                  mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                                          ifelse(month %in% c(4,5,6), 2, 
                                                 ifelse(month %in% c(7,8,9), 3, 
                                                        ifelse(month %in% c(10,11,12), 4, NA))))) %>% 
                  group_by(country,section, year,quarter) %>% 
                  summarise(tot_catch_ton = sum(catch)/1000)
      ) %>% 
  left_join(fiskifelag_oslaegt(mar) %>% 
              filter(fteg == species_number) %>% 
              mutate(country = 'Iceland',section = '27.5.a') %>% 
              group_by(country, section, ar,man) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3) %>% 
              collect(n=Inf) %>% 
              rename(year=ar, month = man) %>%   
              bind_rows(lods_oslaegt(mar) %>% 
                          filter(fteg == species_number,veidisvaedi == 'I',ar>1993) %>% 
                          left_join(lesa_skipaskra(mar)) %>% 
                          mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst), section = '27.5.a') %>% 
                          filter(country == 'Iceland') %>% 
                          group_by(country,section,ar,man) %>% 
                          summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3)%>% 
                          collect(n=Inf) %>% 
                          rename(year=ar, month = man)) %>%
              mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                                      ifelse(month %in% c(4,5,6), 2, 
                                             ifelse(month %in% c(7,8,9), 3, 
                                                    ifelse(month %in% c(10,11,12), 4, NA))))) %>% 
              filter(year < tyr,c>0) %>% 
              group_by(country, section,year,quarter) %>% 
              summarise(landings_ton=sum(c))
      ) %>% 
      mutate(raising_factor = landings_ton/tot_catch_ton,
             raised_catch = catch_kg*ifelse(is.na(raising_factor), 1, raising_factor)) %>% 
      filter(!grepl('NA', rect)) %>% 
      bind_rows(greenland_prop %>%
                  mutate(section = '27.14.b') %>% 
                  select(country, section, year, quarter, rect, raised_catch = catch_kg)) %>% 
      group_by(country, section, year, quarter, rect) %>% 
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
  filter(synaflokkur %in% c(1,2,4,8,30,35)) %>%
  mutate(GRIDCELL = 10*reitur + smareitur) %>%
  left_join(tbl(mar,'reitmapping_original'), by = "GRIDCELL") %>%
  rename(region = DIVISION) %>%
  mutate(region = ifelse(is.na(region) & !(synaflokkur %in% c(30,35)), 101, region)) %>% 
  filter(!(synaflokkur %in% c(1,2,4,8) & !(region %in% local(global_regions))),
         !is.na(region)) %>% 
  rename(haul_id = synis_id, length_cm = lengd, age = aldur, weight_g = oslaegt, maturity_stage = kynthroski,) %>% 
  mutate(gender = ifelse(kyn==1, 'M',
                         ifelse(kyn==2, 'F', NA)),
         maturity = ifelse(maturity_stage==1, 'immature',
                           ifelse(maturity_stage %in% c(2:4), 'mature',NA)),
         spawning = ifelse(maturity_stage==3, 'yes', 'no'),
         source = ifelse(synaflokkur==35, 'IAGS', 'commercial'),
         person = 'Pamela J. Woods'
         )  %>% 
  select(person, source, haul_id, age, length_cm, weight_g, gender, maturity_stage, maturity) %>% 
  filter(!(is.na(age) & is.na(length_cm) & is.na(weight_g) & is.na(gender) & is.na(maturity_stage))) %>% 
  collect(n=Inf) %>% 
  write_csv('data_to_share/BiolData_ARU.27.5a14.csv')

  
#stations

stations_data <-
  lesa_kvarnir(mar) %>% 
  filter(tegund==19) %>% 
  select(synis_id) %>% 
  full_join(lesa_lengdir(mar) %>%
            filter(tegund==19) %>% 
            select(synis_id)) %>% 
  distinct() %>% 
  left_join(lesa_stodvar(mar)) %>%
  filter(synaflokkur %in% c(1,2,4,8, 35)) %>%
  mutate(GRIDCELL = 10*reitur + smareitur) %>%
  left_join(tbl(mar,'reitmapping_original'), by = "GRIDCELL") %>%
  rename(region = DIVISION) %>%
  mutate(region = ifelse(is.na(region) & !(synaflokkur %in% c(30,35)), 101, region)) %>% 
  filter(!(synaflokkur %in% c(1,2,4,8) & !(region %in% local(global_regions))),
         !is.na(region)) %>% 
  mutate(person = 'Pamela J. Woods',
         source = ifelse(synaflokkur==35, 'IAGS', 'commercial'),
         country = 'Iceland',
         division = '27.5.a'
         ) %>% 
  rename(haul_id = synis_id, depth = togdypi_hift) %>% 
  mutate(day = day(dags), month = month(dags), year = year(dags)) %>%
  select(person, source, country, division, haul_id,day,month,year, lat = lat.x, lon = lon.x, depth) %>% 
  collect(n = Inf) %>% 
  write_csv('data_to_share/StationsData_ARU.27.5a14.csv')

#links for trying to connect to ICES network  
  # https://community.ices.dk/ExpertGroups/benchmarks/2020/wkdeep/_layouts/15/PickerTreeView.aspx?title=CbqPickerSelectFolderTitle&text=CbqPickerSelectFolderText&filter=websListsFolders&root=SPList:6d86768d-bc32-44d4-a68d-a3218109e204?SPWeb:6c7dbae6-f94a-462f-8bc2-b852d8ef1b84:&selectionUrl=/ExpertGroups/benchmarks/2020/wkdeep/2014%20Meeting%20docs/06.%20Data/combined/&featureId=&errorString=&iconUrl=/_layouts/15/images/smt_icon.gif?rev=40&scopeToWeb=true&requireCT=&sourceId=/06. Data/combined/
  # https://community.ices.dk/ExpertGroups/benchmarks/2020/wkdeep/_layouts/15/start.aspx#/SitePages/HomePage.aspx?RootFolder=%2FExpertGroups%2Fbenchmarks%2F2020%2Fwkdeep%2F2014%20Meeting%20docs%2F06%2E%20Data%2Fcombined&FolderCTID=0x01200086119F7A284DDE4D825348277F2A1A7A&View=%7B6C506B9A%2D6D58%2D468E%2D9DB4%2DBBF47E5FC264%7D
  # http://community.ices.dk/ExpertGroups/benchmarks/2020/wkdeep/2014%20Meeting%20docs/06.%20Data/combined
  # 
  # list.files('http://community.ices.dk/ExpertGroups/benchmarks/2020/wkdeep/2014%20Meeting%20docs/06.%20Data/combined')
  
#length distributions

  
ldist1<-
  lesa_lengdir(mar) %>%
  filter(tegund==19) %>%
  left_join(lesa_stodvar(mar)) %>% 
  filter(synaflokkur %in% c(1,2,4,8, 35)) %>%
  mutate(GRIDCELL = 10*reitur + smareitur) %>%
  left_join(tbl(mar,'reitmapping_original'), by = "GRIDCELL") %>%
  rename(region = DIVISION) %>%
  mutate(region = ifelse(is.na(region) & !(synaflokkur %in% c(30,35)), 101, region)) %>% 
  filter(!(synaflokkur %in% c(1,2,4,8) & !(region %in% local(global_regions))),
         !is.na(region)) %>% 
  mutate(source = ifelse(synaflokkur==35,'IAGS','commercial'),
         country = 'Iceland',
         division = '27.5.a')

ldist.surv <-
  ldist1 %>%
  filter(synaflokkur==35) %>% 
  skala_med_toldum() %>% 
  bind_
  rename(month = man) %>% 
  mutate(quarter = ifelse(month %in% c(1,2,3), 1, 
                          ifelse(month %in% c(4,5,6), 2, 
                                 ifelse(month %in% c(7,8,9), 3, 4))),
         n_1000s = fjoldi/1000) %>%
  select(source, country, division, year = ar, quarter, length_cm = lengd, n_1000s) %>% 
  collect(n=Inf)
  
  #catch data....
  catch <- 
    afli_stofn(mar) %>% 
    inner_join(afli_afli(mar)) %>% 
    filter(tegund == Species, ar == tyr) %>% 
    mutate(GRIDCELL = 10*reitur + smareitur) %>% 
    inner_join(tbl(mar,'husky_gearlist'),
               by =c('veidarf'='veidarfaeri')) %>% 
    rename(vf = geartext) %>% 
    inner_join(tbl(mar,'reitmapping')) %>% 
    rename(region = DIVISION) %>% 
    filter(region %in% global_regions) #%>% 
  
  if(dim(catch %>% collect(n=Inf))[1]==0){
    print('WARNING: No catch data - using landings instead')
    catch <- 
      landings_caa %>% 
      mutate(afli = landings_caa, man = 1, GRIDCELL = 1811, region = 101) #arbitrarily chosen so that the rest of the code works... 
  }
  
  #.... by gear and compared with landings_caa
  sc <- 
    catch %>% 
    group_by(vf) %>% 
    summarise(catch = sum(afli)) %>% 
    left_join(landings_caa)
  
  #.... corrected by the discrepancy between landings_caa and catch specific to gear types...
  commcatch <- 
    catch %>% 
    left_join(sc) %>% 
    mutate(landings_caa = ifelse(is.na(landings_caa), catch, landings_caa),
           catch = afli*landings_caa/catch) %>% #discrepancy correction
    mutate(synaflokkur_group = 's1') %>% # 'commercial' synaflokkur - helps with later groupings - 
    #group_by(vf, man) %>% 
    filter(!is.na(catch)) %>% 
    #mutate(catch = afli) %>% 
    collect(n=Inf) #%>% 
  
  #catch for the ind grouping - used as a weight when combining catch data
  afli_ind <- 
    commcatch %>% 
    rename(month = man) %>% 
    left_join(month_group) %>% 
    left_join(GRIDCELL_group) %>% 
    left_join(region_group) %>% 
    left_join(vf_group) %>% 
    ungroup() %>% 
    unite(index,c(month_group,synaflokkur_group,region_group,GRIDCELL_group,vf_group),sep = '',remove = FALSE) %>% 
    filter(!is.na(month_group), !is.na(GRIDCELL_group), !is.na(region_group), vf_group != 'NA' | !is.na(vf_group), !is.na(synaflokkur_group)) %>% 
    filter(index == ind) %>% 
    summarise(catch = sum(catch,na.rm=TRUE)) %>% 
    mutate(catch = catch/1000) %>% 
    .$catch
  
  
  
distributions <- MakeLdist(Species,
                           lengd=ldist_bins[[19]],
                           Stodvar=ldist1 %>% 
                             filter(synaflokkur!=35) %>% 
                             select(-c(tegund, lengd, fjoldi, kyn, kynthroski)),
                           lengdir=ldist1 %>% 
                             filter(synaflokkur!=35) %>% 
                             select(c(tegund, lengd, fjoldi, kyn, kynthroski)),
                           lengd.thyngd.data=cond_ind,
                           talid=F,afli=afli_ind)
    