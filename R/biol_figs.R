#install.packages(c('tidyverse','purrr','broom', 'sf', 'readxl','devtools', 'mapplots'))
#devtools::install_github('hafro/geo')
library(tidyverse)
#library(fishmethods)#needed for "alternative growth moedels" and "age-length key" part of the code
library(sf)

ia <- read_sf("R/gisdata/ICES_Areas_20160601_cut_dense_3857.gpkg") %>%
  st_simplify(dTolerance = 10000) %>% 
  st_transform(4326) %>% 
  #st_geometry() #gets rid of all columns except geometry
  select(Area_Full)

### Get data ###

Nor <- 
  readxl::read_excel("data_received/Template_data_sharing_NOR.xls", sheet = "biological data") %>% 
  rename(gender = gender...10, species = specie) %>% 
  dplyr::select(-c(gender...7, number)) %>% 
  mutate(source = ifelse(source=='Reffleet_ocea', 'Reffleet_ocean', source))

Nor_st <- 
  readxl::read_excel("data_received/Template_data_sharing_NOR.xls", sheet = "station") %>%
  dplyr::select(-c(specie)) %>% 
  rename(lon = long, depth_m = depth) %>%
  mutate(division = as.character(division))

Ice <- 
  read_csv("data_received/BiolData_ARU.27.5a14.csv", col_types = cols(weight_g = 'd')) %>% 
  mutate(maturity = ifelse(maturity == 'mature', 'Mature', 
                ifelse(maturity== 'immature', 'Immature', maturity)))

Ice_st <-
  read_csv("data_received/StationsData_ARU.27.5a14.csv") %>% 
  rename(depth_m = depth)

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
                           ifelse(gender== 'f', 'F', gender)),
         person = 'Lise H. Ofstad') 

Far_st <-
  read_csv("data_received/StationsData_ARU_27.5.b_Faroes.csv") %>% 
  rename(person = PERSON, source = SOURCE, country = COUNTRY, division = DIVISION, day = DAY, month = MONTH, year = YEAR, lat = LAT, lon = LON, depth_m = DEPTH_M, haul_id = HAUL_ID)


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


# Begin analyses to get parameters summarised by division
pr <- seq(0, max(all$age, na.rm =T)+1)

vb_pars <-
  all %>% 
  left_join(all_st) %>% 
  filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  mutate(age = age + (month-1)/12) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
  split(., .$div_gen) %>% #.[[1]]->x
  purrr::map(function(x){
    #print(paste0(unique(x$division), '_', unique(x$gender)))
    nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5)) %>% 
      broom::tidy() %>% 
      mutate(division = unique(x$division),
             gender = unique(x$gender))
  }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/vbpars_bydivision.csv')
  
vb_pars_2018 <-
  all %>% 
  left_join(all_st) %>% 
  filter(year==2018,!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  mutate(age = age + (month-1)/12) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
  split(., .$div_gen) %>% #.[[1]]->x
  purrr::map(function(x){
    #print(paste0(unique(x$division), '_', unique(x$gender)))
    nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5)) %>% 
      broom::tidy() %>% 
      mutate(division = unique(x$division),
             gender = unique(x$gender))
  }) %>% 
  bind_rows() %>% 
  write_csv('R/biol_figs_output/vbpars_bydivision_2018.csv')


  lw_pars <-
    all %>% 
    left_join(all_st) %>% 
    filter(!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
    split(., .$div_gen) %>% #.[[1]]->x
    purrr::map(function(x){
      #print(paste0(unique(x$division), '_', unique(x$gender)))
      lm(log(weight_g/1e3)~log(length_cm),x)  %>% 
        broom::tidy() %>% 
        mutate(term= ifelse(term=='(Intercept)', 'Intercept', term),
               term= ifelse(term=='log(length_cm)', 'Log Length (cm)', term)) %>% 
        mutate(division = unique(x$division),
               gender = unique(x$gender), 
               estimate = ifelse(term=='Intercept', exp(estimate), estimate))
    }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/lwpars_bydivision.csv')

  
  lw_pars_2018 <-
    all %>% 
    left_join(all_st) %>% 
    filter(year==2018,!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
    unite(div_gen, division, gender, remove = F) %>% 
    split(., .$div_gen) %>% #.[[1]]->x
    purrr::map(function(x){
      #print(paste0(unique(x$division), '_', unique(x$gender)))
      lm(log(weight_g/1e3)~log(length_cm),x)  %>% 
        broom::tidy() %>% 
        mutate(term= ifelse(term=='(Intercept)', 'Intercept', term),
               term= ifelse(term=='log(length_cm)', 'Log Length (cm)', term)) %>% 
        mutate(division = unique(x$division),
               gender = unique(x$gender), 
               estimate = ifelse(term=='Intercept', exp(estimate), estimate))
    }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/lwpars_bydivision_2018.csv')
  
  mat_pars <-
    all %>% 
    left_join(all_st) %>% 
    filter(!is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
  unite(div_gen, division, gender, remove = F) %>% 
    split(., .$div_gen) %>% #.[[1]]->x
    purrr::map(function(x){
      #print(paste0(unique(x$division), '_', unique(x$gender)))
      tmp <-
        glm(mat~length_cm, data=x %>% mutate(mat = ifelse(maturity=='Mature', 1, 0)), family=binomial(link=logit))  %>% 
        broom::tidy() %>% 
        mutate(division = unique(x$division),
               gender = unique(x$gender),
               term = ifelse(term=='(Intercept)', 'Intercept',
                             ifelse(term=='length_cm', 'Length (cm)', term))
               ) 
      
        bind_rows(tmp,tibble(term = 'L50', estimate = - tmp$estimate[tmp$term=='Intercept']/tmp$estimate[tmp$term=='Length (cm)']))
    }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/matpars_bydivision.csv')
  
  mat_pars_2018 <-
    all %>% 
    left_join(all_st) %>% 
    filter(year==2018, !is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
    unite(div_gen, division, gender, remove = F) %>% 
    split(., .$div_gen) %>% #.[[1]]->x
    purrr::map(function(x){
      #print(paste0(unique(x$division), '_', unique(x$gender)))
      tmp <-
        glm(mat~length_cm, data=x %>% mutate(mat = ifelse(maturity=='Mature', 1, 0)), family=binomial(link=logit))  %>% 
        broom::tidy() %>% 
        mutate(division = unique(x$division),
               gender = unique(x$gender),
               term = ifelse(term=='(Intercept)', 'Intercept',
                             ifelse(term=='length_cm', 'Length (cm)', term))
        ) 
      
      bind_rows(tmp,tibble(term = 'L50', estimate = - tmp$estimate[tmp$term=='Intercept']/tmp$estimate[tmp$term=='Length (cm)']))
    }) %>% 
    bind_rows() %>% 
    write_csv('R/biol_figs_output/matpars_bydivision_2018.csv')
  
  
ml_age <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  mutate(age = ifelse(age < 6, age + (month-1)/12, age)) %>% 
  group_by(division, gender, age, year) %>% 
  filter(!is.na(age), !is.na(length_cm), length_cm > 0) %>% 
  summarise(ml = mean(length_cm), sdl = sd(length_cm)) %>%
  arrange(year, division, gender, age) %>% 
  write_csv('R/biol_figs_output/meanlength_at_age_bydivision.csv')


size_depth <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  filter(!is.na(length_cm), length_cm > 0) %>% 
  lm(length_cm ~ depth_m + division + depth_m*division, data=.)  %>% 
      broom::tidy() %>% 
  write_csv('R/biol_figs_output/size_depth_lm.csv')

size_depth_latlon <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  filter(!is.na(length_cm), length_cm > 0) %>% 
  lm(length_cm ~ depth_m + lat + lon + depth_m*lat + depth_m*lon + lat*lon, data=.)  %>% 
  broom::tidy() %>% 
  write_csv('R/biol_figs_output/size_depth_latlon_lm.csv')

size_depth_2018 <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  filter(year==2018,!is.na(length_cm), length_cm > 0) %>% 
  lm(length_cm ~ depth_m + division + depth_m*division, data=.)  %>% 
  broom::tidy() %>% 
  write_csv('R/biol_figs_output/size_depth_lm_2018.csv')

size_depth_latlon_2018 <-
  all %>% 
  left_join(all_st) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
  filter(year==2018, !is.na(length_cm), length_cm > 0) %>% 
  lm(length_cm ~ depth_m + lat + lon + depth_m*lat + depth_m*lon + lat*lon, data=.)  %>% 
  broom::tidy() %>% 
  write_csv('R/biol_figs_output/size_depth_latlon_lm_2018.csv')

tmp_vb <-
  all %>% 
  left_join(all_st) %>%
  filter(!is.na(age), !is.na(length_cm), length_cm > 0) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat),
         age = age + (month-1)/12)

overall_vb <- 
  nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=tmp_vb, start=list(Linf=50, K=0.2, t0=-0.5)) 

overall_vb %>% 
  broom::tidy()%>% 
  write_csv('R/biol_figs_output/overall_vb.csv')

yr_min <- 2005; yr_max <- 2018

#needs to be replaced by length distribution plots
growth_expected_plot <- 
  tmp_vb %>% 
  mutate(expected_length = exp(fitted(overall_vb)), residuals = length_cm - expected_length) %>% 
  group_by(rect, year) %>% 
  summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
            `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Expected Length (cm)`),interpolate = FALSE) + 
  #geom_tile(aes(fill=`Expected Length (cm)`)) + 
  # geom_polygon(data=gisland::iceland,aes(long,lat,group=group),
  #              fill='white',col='black') + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)') + 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1))+
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

growth_residuals_plot <- 
  tmp_vb %>% 
  mutate(expected_length = exp(fitted(overall_vb)), residuals = length_cm - expected_length) %>% 
  group_by(rect, year) %>% 
  summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
            `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Residual Length (cm)`),interpolate = FALSE) + 
  #geom_tile(aes(fill=`Expected Length (cm)`)) + 
  # geom_polygon(data=gisland::iceland,aes(long,lat,group=group),
  #              fill='white',col='black') + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))


tmp_lw <-
  all %>% 
  left_join(all_st) %>%
  filter(!is.na(weight_g), !is.na(length_cm), length_cm > 0) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat),
         age = age + (month-1)/12)


# weight at length variation
overall_lw <- 
  lm(log(weight_g/1e3)~log(length_cm),tmp_lw) 

overall_lw %>% 
  broom::tidy()%>% 
  write_csv('R/biol_figs_output/overall_lw.csv')


#needs to be replaced by length distribution plots
weight_expected_plot <- 
  tmp_lw %>% 
  mutate(expected_weight = exp(fitted(overall_lw)), residuals = weight_g - expected_weight) %>% 
  group_by(rect, year) %>% 
  summarise(`Expected Weight (kg)` = mean(expected_weight, na.rm = T),
            `Residual Weight (kg)` = mean(residuals, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Expected Weight (kg)`),interpolate = FALSE) + 
  #geom_tile(aes(fill=`Expected Length (cm)`)) + 
  # geom_polygon(data=gisland::iceland,aes(long,lat,group=group),
  #              fill='white',col='black') + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)') + 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1))+
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))


weight_residuals_plot <- 
  tmp_lw %>% 
  mutate(expected_weight = exp(fitted(overall_lw)), residuals = weight_g/1e3 - expected_weight) %>% 
  filter(residuals < 1) %>% 
  group_by(rect, year) %>% 
  summarise(`Expected Weight (kg)` = mean(expected_weight, na.rm = T),
            `Residual Weight (kg)` = mean(residuals, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Residual Weight (kg)`),interpolate = FALSE) + 
  #geom_tile(aes(fill=`Expected Length (cm)`)) + 
  # geom_polygon(data=gisland::iceland,aes(long,lat,group=group),
  #              fill='white',col='black') + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))


#max age
tmp_maxage <-
  all %>% 
  left_join(all_st) %>%
  filter(!is.na(age)) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat),
         age = age + (month-1)/12)


maxage_plot <- 
  tmp_maxage %>% 
  group_by(rect, year) %>% 
  summarise(`Max. age` = max(age, na.rm = T),
            `95% age` = quantile(age, 0.95, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Max. age`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

age95_plot <- 
  tmp_maxage %>% 
  group_by(rect, year) %>% 
  summarise(`Max. age` = max(age, na.rm = T),
            `95% age` = quantile(age, 0.95, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`95% age`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

#95% length - need to get from length distributions
#max age
tmp_maxl <-
  all %>% 
  left_join(all_st) %>%
  filter(!is.na(length_cm), length_cm > 0) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat),
         age = age + (month-1)/12)


maxl_plot <- 
  tmp_maxl %>% 
  group_by(rect, year) %>% 
  summarise(`Max. length (cm)` = max(length_cm, na.rm = T),
            `95% length (cm)` = quantile(length_cm, 0.95, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Max. length (cm)`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

l95_plot <- 
  tmp_maxl %>% 
  group_by(rect, year) %>% 
  summarise(`Max. length (cm)` = max(length_cm, na.rm = T),
            `95% length (cm)` = quantile(length_cm, 0.95, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`95% length (cm)`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))


l50_plot_0 <- 
  tmp_maxl %>% 
  filter(depth_m <= 300) %>% 
  group_by(rect, year) %>% 
  summarise(`Mean length (cm)` = mean(length_cm, na.rm = T),
            `50% length (cm)` = quantile(length_cm, 0.50, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`50% length (cm)`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

l50_plot_300 <- 
  tmp_maxl %>% 
  filter(depth_m > 300, depth_m <= 500) %>% 
  group_by(rect, year) %>% 
  summarise(`Mean length (cm)` = mean(length_cm, na.rm = T),
            `50% length (cm)` = quantile(length_cm, 0.50, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`50% length (cm)`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

l50_plot_500 <- 
  tmp_maxl %>% 
  filter(depth_m > 500) %>% 
  group_by(rect, year) %>% 
  summarise(`Mean length (cm)` = mean(length_cm, na.rm = T),
            `50% length (cm)` = quantile(length_cm, 0.50, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`50% length (cm)`),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))


#length at ages 8 & 9 (most frequent age)
all %>% 
  left_join(all_st) %>%
  group_by(age) %>% 
  count()

tmp_l8 <-
  all %>% 
  left_join(all_st) %>%
  filter(!is.na(length_cm), length_cm > 0, age %in% c(8,9)) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat),
         age = age + (month-1)/12)

# weight at length variation
overall_l8 <- 
  lm(length_cm~age,tmp_l8) 

plot(length_cm~age,tmp_l8)  + abline(coef(overall_l8))

overall_l8 %>% 
  broom::tidy()%>% 
  write_csv('R/biol_figs_output/overall_l8.csv')


#needs to be replaced by length distribution plots
l8_expected_plot <- 
  tmp_l8 %>% 
  mutate(expected_length = fitted(overall_l8), residuals = length_cm - expected_length) %>% 
  group_by(rect, year) %>% 
  summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
            `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Expected Length (cm)`),interpolate = FALSE) + 
  #geom_tile(aes(fill=`Expected Length (cm)`)) + 
  # geom_polygon(data=gisland::iceland,aes(long,lat,group=group),
  #              fill='white',col='black') + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)') + 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1))+
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

l8_residual_plot <- 
  tmp_l8 %>% 
  mutate(expected_length = fitted(overall_l8), residuals = length_cm - expected_length) %>% 
  group_by(rect, year) %>% 
  summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
            `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=`Residual Length (cm)`),interpolate = FALSE) + 
  #geom_tile(aes(fill=`Expected Length (cm)`)) + 
  # geom_polygon(data=gisland::iceland,aes(long,lat,group=group),
  #              fill='white',col='black') + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)') + 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1))+
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))


#L50 maturity
tmp_l50 <-
  all %>% 
  left_join(all_st) %>%
  filter(!is.na(length_cm), length_cm > 0, !is.na(maturity)) %>% 
  mutate(rect =  mapplots::ices.rect2(lon, lat),
         age = age + (month-1)/12)


l50_plot <- 
  tmp_l50 %>% 
  group_by(rect, year, length_cm, maturity) %>% 
  count() %>% 
  ungroup %>% 
  filter(maturity=='Mature') %>% 
  rename(n_mat = n) %>% 
  left_join(tmp_l50 %>% 
              group_by(rect, year, length_cm) %>% 
              count()) %>% 
  mutate(p = n_mat/n) %>% 
  filter(p <= 0.5, length_cm > 10) %>% 
  group_by(rect, year) %>% 
  summarise(L50 = max(length_cm, na.rm = T)) %>% 
  filter(year > yr_min-1, year < yr_max+1) %>% 
  bind_cols(mapplots::ices.rect(.$rect)) %>% 
  #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
  ggplot() + 
  #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
  geom_tile(aes(lon, lat, fill=L50),interpolate = FALSE) + 
  geom_polygon(data = map_data('world','Greenland'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = map_data('world','Norway'), aes(long, lat, group=group),
               fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
  #mapplots::draw.rect() %>% 
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_viridis_c(direction = -1)+
  xlab('Longitude (W)') +
  ylab('Latitude (N)')+ 
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_text(aes(x = x, y = y, label = label), data = tibble(year = yr_min:yr_max, x= 0, y = 77, label = year)) +
  geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
  coord_sf(xlim = c(-34, 18),ylim = c(57, 80))

png_dims <- c(1000, 675)

 png(paste0('R/biol_figs_output/growth_expected_plot.png'), height = png_dims[1], width = png_dims[2])
 print(growth_expected_plot)
 dev.off()

 png(paste0('R/biol_figs_output/growth_residuals_plot.png'), height = png_dims[1], width = png_dims[2])
 print(growth_residuals_plot)
 dev.off()

  png(paste0('R/biol_figs_output/weight_expected_plot.png'), height = png_dims[1], width = png_dims[2])
 print(weight_expected_plot)
 dev.off()

  png(paste0('R/biol_figs_output/weight_residuals_plot.png'), height = png_dims[1], width = png_dims[2])
 print(weight_residuals_plot)
 dev.off()
 
 png(paste0('R/biol_figs_output/l8_expected_plot.png'), height = png_dims[1], width = png_dims[2])
 print(l8_expected_plot)
 dev.off()
 
 png(paste0('R/biol_figs_output/l8_residual_plot.png'), height = png_dims[1], width = png_dims[2])
 print(l8_residual_plot)
 dev.off()
 
 png(paste0('R/biol_figs_output/age95_plot.png'), height = png_dims[1], width = png_dims[2])
 print(age95_plot)
 dev.off()
 
 png(paste0('R/biol_figs_output/l95_plot.png'), height = png_dims[1], width = png_dims[2])
 print(l95_plot)
 dev.off()
 
 png(paste0('R/biol_figs_output/l50_plot.png'), height = png_dims[1], width = png_dims[2])
 print(l50_plot)
 dev.off()
 
 png(paste0('R/biol_figs_output/l50_plot_0.png'), height = png_dims[1], width = png_dims[2])
 print(l50_plot_0)
 dev.off()
 
 png(paste0('R/biol_figs_output/l50_plot_300.png'), height = png_dims[1], width = png_dims[2])
 print(l50_plot_300)
 dev.off()
 
 png(paste0('R/biol_figs_output/l50_plot_500.png'), height = png_dims[1], width = png_dims[2])
 print(l50_plot_500)
 dev.off()
 
 
