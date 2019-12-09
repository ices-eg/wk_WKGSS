library(tidyverse)
library(fishmethods)#needed for "alternative growth moedels" and "age-length key" part of the code

### Get data ###

Nor <- 
  readxl::read_excel("data_received/Template_data_sharing_NOR.xls", sheet = "biological data") %>% 
  rename(gender = gender...10, species = specie) %>% 
  dplyr::select(-c(gender...7, number)) 

Nor_st <- 
  readxl::read_excel("data_received/Template_data_sharing_NOR.xls", sheet = "station") %>% 
  rename(species = specie, lon = long) %>%
  mutate(division = as.character(division)) 

Ice <- 
  read_csv("data_received/BiolData_ARU.27.5a14.csv", col_types = cols(weight_g = 'd')) %>% 
  mutate(maturity = ifelse(maturity == 'mature', 'Mature', 
                ifelse(maturity== 'immature', 'Immature', maturity)))

Ice_st <-
  read_csv("data_received/StationsData_ARU.27.5a14.csv") 

Far <- 
  read_csv("data_received/BiologicalData_ARU_27.5.b_Faroes.csv", col_types = cols(weight_g = 'd', 
                                                                                  age = 'd',
                                                                                  gender = 'c',
                                                                                  maturity_stage = 'd',
                                                                                  maturity = 'c',
                                                                                  spawning = 'c'
                                                                                  )) %>% 
  mutate(maturity = ifelse(maturity == 'mature', 'Mature', 
                           ifelse(maturity== 'immature', 'Immature', maturity)),
         gender = ifelse(gender == 'm', 'M', 
                           ifelse(maturity== 'f', 'F', gender)))

Far_st <-
  read_csv("data_received/StationsData_ARU_27.5.b_Faroes.csv") %>% 
  rename(person = PERSON, source = SOURCE, country = COUNTRY, division = DIVISION, day = DAY, month = MONTH, year = YEAR, lat = LAT, lon = LON, depth_m = DEPTH_M)


all <-
  Ice %>% 
  bind_rows(Nor) %>% 
  bind_rows(Far)

all_st <-
  Ice_st %>% 
  bind_rows(Nor_st) %>% 
  bind_rows(Far_st)

all %>% 
  filter(!(person=='Elvar Hallfredsson' & age==2 & length_cm>25), !(person=='Elvar Hallfredsson' & age==1 & length_cm>20))

pr <- seq(0, max(all$age, na.rm =T)+1)

vb_pars <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  mutate(age = age + (month-1)/12) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
  split(., .$div_gen) %>% #.[[1]]->x
  purrr::map(function(x){
    print(paste0(unique(x$division), '_', unique(x$gender)))
    nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5)) %>% 
      broom::tidy() %>% 
      mutate(division = unique(x$division),
             gender = unique(x$gender))
  }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/vbpars_bydivision.csv')
  

  lw_pars <-
    all %>% 
    left_join(all_st) %>% 
    mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
    filter(!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
    split(., .$div_gen) %>% #.[[1]]->x
    purrr::map(function(x){
      print(paste0(unique(x$division), '_', unique(x$gender)))
      lm(log(weight_g/1e3)~log(length_cm),x)  %>% 
        broom::tidy() %>% 
        mutate(division = unique(x$division),
               gender = unique(x$gender))
    }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/lwpars_bydivision.csv')
  
  mat_pars <-
    all %>% 
    left_join(all_st) %>% 
    mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
    filter(!is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
    split(., .$div_gen) %>% #.[[1]]->x
    purrr::map(function(x){
      print(paste0(unique(x$division), '_', unique(x$gender)))
      glm(mat~le, data=x %>% mutate(mat = ifelse(maturity=='mature', 1, 0)), family=binomial(link=logit))  %>% 
        broom::tidy() %>% 
        mutate(division = unique(x$division),
               gender = unique(x$gender),
               L50 = -a/b1)
    }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/matpars_bydivision.csv')
  
ml_age <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  mutate(age = ifelse(age < 6, age + (month-1)/12, age)) %>% 
  group_by(division, gender, age) %>% 
  filter(!is.na(age), !is.na(length_cm), length_cm > 0) %>% 
  summarise(ml = mean(length_cm), sdl = sd(length_cm)) %>% 
  write_csv('R/biol_figs_output/meanlength_at_age_bydivision.csv')

mat_pars <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  filter(!is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
  split(., .$div_gen) %>% #.[[1]]->x
  purrr::map(function(x){
    print(paste0(unique(x$division), '_', unique(x$gender)))
    glm(mat~le, data=x %>% mutate(mat = ifelse(maturity=='mature', 1, 0)), family=binomial(link=logit))  %>% 
      broom::tidy() %>% 
      mutate(division = unique(x$division),
             gender = unique(x$gender),
             L50 = -a/b1)
  }) %>% 
  bind_rows() %>% 
  write_csv('R/biol_figs_output/matpars_bydivision.csv')


growth_plot <- 
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  group_by(division, gender) %>% 
  