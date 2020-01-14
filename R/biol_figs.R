        #install.packages(c('tidyverse','purrr','broom', 'sf', 'readxl','devtools', 'mapplots'))
        #devtools::install_github('hafro/geo')
        library(tidyverse)
        #library(fishmethods)#needed for "alternative growth moedels" and "age-length key" part of the code
        library(sf)
        
        #TODO
        #Difficult to see the results in maps (fig 5.1.1-5.1.3) because the maps are so small. Could those figures be in appendix and maybe a graph per area (27.2, 27.5.a, 27.5.b, 27.6.a...) present the differences.
        #5.2 Ageing Make a Figure for comparison 
          # Figure 4.4.2.2.1 VBF
          # 
          # o I can see that there are not enough data to calculate VBF by year by gender for all years.
          # 
          # o In the exploratory assessment there is no separation between genders, so the sample amounts are larger (at least from commercial fish data). Use all data per year (females+males+samples without gender) in the results and divide in periods (1994-2000, 2001-2005, 2006-2010, 2011-2015, 2016-2019). And the same for females and males separate. I think it will be easier to compare the results then.
          # 
          # - Figure 4.4.2.2.2 Length-weight
          # 
          # o It would be good to have a figure with all the data (females+males+samples without gender) per year.
          # 
          # o We are working on the catch at age matrix in the input values for the assessment. For the time being we use the length-weight relationship per year for all data together (females+males+samples without gender) in the calculation.
          # 
          # - Figure 4.4.2.2.3 Maturity ogive
          # 
          # o First of all- I can see that there are not enough data to have it year by year.
          # 
          # o I have tried it by periods (1994-2000 only 79 fish, 2001-2005 no data, 2006-2010 around 1000 fish and the same for 2011-2015 and 2016-2019). The three last periods gave almost the same results with L50 around 36-37 cm- and the difference between gender are not large. 
        #My conclusion around this is to use all data together (do not separate by year or period) and make a graph of females, males and females+males in the same figure.
        
        ia <- read_sf("R/gisdata/ICES_Areas_20160601_cut_dense_3857.gpkg") %>%
          st_simplify(dTolerance = 10000) %>% 
          st_transform(4326) %>% 
          #st_geometry() #gets rid of all columns except geometry
          select(Area_Full)
        
        ### Get data ###
        
        Nor <- 
          readxl::read_excel("data_received/Template_data_sharing_NOR.xls", sheet = "biological data") %>% 
          rename(gender = gender...10, species = specie) %>%
          mutate(maturity_stage = as.numeric(ifelse(maturity_stage=='NA', NA, maturity_stage))) %>%  
          dplyr::select(-c(gender...7, number, samplenumber)) %>% 
          mutate(source = ifelse(source=='Reffleet_ocea', 'Reffleet_ocean', 
                                 ifelse(source=='fishery', 'Fishery', source)), 
                 spawning = ifelse(maturity_stage==6, 'yes', 'no')) %>% 
          filter((weight_g < 2000 | is.na(weight_g))) 
        
        Nor_st <- 
          readxl::read_excel("data_received/Template_data_sharing_NOR.xls", sheet = "station") %>%
          dplyr::select(-c(specie)) %>% 
          mutate(depth = as.numeric(ifelse(depth=='NA', NA, depth)),
                 lat = as.numeric(ifelse(lat=='NA', NA, lat)),
                 long = as.numeric(ifelse(long=='NA', NA, long))) %>% 
          rename(lon = long, depth_m = depth) %>%
          mutate(division = as.character(division)) %>% 
          mutate(division = ifelse(division == '27.2a', '27.2.a',
                                   ifelse(division == '27.2b', '27.2.b',
                                          ifelse(division=='27.4a', '27.4.a',
                                                 ifelse(division == '27.3a', '27.3.a',
                                                        ifelse(division == '27.4b', '27.4.b', division)))))) %>% 
        filter(!is.na(division), division != 'NA')
        
        Ice <- 
          read_csv("data_received/BiolData_ARU.27.5a14.csv", col_types = cols(weight_g = 'd')) %>% 
          mutate(maturity = ifelse(maturity == 'mature', 'Mature', 
                        ifelse(maturity== 'immature', 'Immature', maturity))) %>% 
                filter((weight_g < 2000 | is.na(weight_g)))
          
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
          rename(person = PERSON, source = SOURCE, country = COUNTRY, division = DIVISION, day = DAY, month = MONTH, year = YEAR, lat = LAT, lon = LON, depth_m = DEPTH_M, haul_id = HAUL_ID) %>% 
          mutate(division = ifelse(division %in% c('27.5.b.1', '27.5.b.2', '27.6.a'), '27.5.b', division))
        
        
        all <-
          Ice %>% 
          bind_rows(Nor) %>% 
          bind_rows(Far) %>% 
          filter(!(person=='Elvar Hallfredsson' & age==2 & length_cm>25), !(person=='Elvar Hallfredsson' & age==1 & length_cm>20)) %>% 
          filter(!(person=='Pamela J. Woods' & length_cm < 20 & weight_g > 500)) %>% 
          mutate(gender = ifelse(is.na(gender) & person=='Lise H. Ofstad', 'U', gender))
        
        all_st <-
          Ice_st %>% 
          bind_rows(Nor_st) %>% 
          bind_rows(Far_st) %>% 
          mutate(yr = year, 
                 year = ifelse(yr %in% c(1994:2000), 19942000,
                               ifelse(yr %in% c(2001:2005), 20012005,
                                      ifelse(yr %in% c(2006:2010), 20062010, 
                                             ifelse(yr %in% c(2011:2015), 20112015,
                                                    ifelse(yr %in% c(2016:2019), 20162019, 
                                                           ifelse(yr < 1994, 19940000, yr)))))))
        
        
        
        ####--------------Von Bertalanffy growth curves -------------####
        
        ####--------------By division and overall -------------####
        
        vb_pars <-
          all %>% 
          left_join(all_st) %>% 
          filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
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
          filter(year==20162019,!is.na(age), !is.na(length_cm), !is.na(gender), division!='NA', length_cm > 0) %>% 
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
          write_csv('R/biol_figs_output/vbpars_bydivision_2018.csv')
        
        vb_pars %>% 
          bind_cols(vb_pars_2018 %>% right_join(vb_pars %>% select(term, division, gender))) %>% 
          write_csv('R/biol_figs_output/vbpars_bydivision_both.csv')
        
        
        ####--------------VB by stock-------------####
        
        vb_pars_bystock <-
          all %>% 
          left_join(all_st) %>% 
          filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
          mutate(age = age + (month-1)/12,
                 person_sh = person %>% substr(., 1, 3)) %>% 
          unite(st_gen, person_sh, gender, remove = F) %>% 
          split(., .$st_gen) %>% #.[[1]]->x
          purrr::map(function(x){
            #print(paste0(unique(x$division), '_', unique(x$gender)))
            prL<-seq(0,120,1)
            prA<-seq(0,60,1)
            mod <- nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5))
            fit <-exp(predict(mod, data.frame(age=prA),type="response"))
            y <- 
              list(
                mod =  mod %>% 
                  broom::tidy() %>% 
                  mutate(person = unique(x$person),
                         gender = unique(x$gender)),
                x = full_join(x, data.frame(age = prA, fit)) %>% select(-c(st_gen, person_sh))
              )
            return(y)
          }) 
        
        vb_pars_bystock %>% 
          flatten() %>% 
          keep(., names(.)=="mod") %>% 
          bind_rows() %>% 
          write_csv('R/biol_figs_output/vbpars_bystock.csv')
        
        vb_plot_bystock <-
          vb_pars_bystock %>% 
          flatten() %>% 
          keep(., names(.)=="x") %>% 
          bind_rows() %>% 
          mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                       ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                 gender = ifelse(gender=='F', 'Female',
                                 ifelse(gender=='M', 'Male', 
                                        ifelse(gender=='U', 'Unidentified', gender)))) %>%
          filter(!is.na(stock)) %>% 
          unite(age_stock, age, stock, remove = FALSE) %>% 
          rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
          filter(Age < 30) %>% 
          ggplot() +
          geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_stock, color = Stock, fill = Stock), alpha = 0.1) +
          geom_line(aes(x = Age, y = fit, color = Stock),
                    data = vb_pars_bystock %>% 
                      flatten() %>% 
                      keep(., names(.)=="x") %>% 
                      bind_rows() %>% 
                      mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                            ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                   ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                             gender = ifelse(gender=='F', 'Female',
                                             ifelse(gender=='M', 'Male', 
                                                    ifelse(gender=='U', 'Unidentified', gender)))) %>%
                      filter(!is.na(stock), !is.na(fit)) %>% 
                      unite(age_stock, age, stock, remove = FALSE) %>% 
                      rename(Age = age, Stock = stock, Gender = gender) %>% 
                      filter(Age < 30) )+
          theme_bw() + 
          facet_wrap(~Gender, ncol = 1)
          
          vb_pars_bystock_2018 <-
            all %>% 
            left_join(all_st) %>% 
            filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0,
                   year==20162019, !is.na(division)) %>% 
            mutate(age = age + (month-1)/12,
                   person_sh = person %>% substr(., 1, 3)) %>% 
            unite(st_gen, person_sh, gender, remove = F) %>% 
            split(., .$st_gen) %>% #.[[1]]->x
            purrr::map(function(x){
              #print(paste0(unique(x$division), '_', unique(x$gender)))
              prL<-seq(0,120,1)
              prA<-seq(0,60,1)
              mod <- nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5))
              fit <-exp(predict(mod, data.frame(age=prA),type="response"))
              y <- 
                list(
                  mod =  mod %>% 
                    broom::tidy() %>% 
                    mutate(person = unique(x$person),
                           gender = unique(x$gender)),
                  x = full_join(x, data.frame(age = prA, fit)) %>% select(-c(st_gen, person_sh))
                )
              return(y)
            }) 
          
          vb_pars_bystock_2018 %>% 
            flatten() %>% 
            keep(., names(.)=="mod") %>% 
            bind_rows() %>% 
            write_csv('R/biol_figs_output/vbpars_bystock_2018.csv')
          
          vb_plot_bystock_2018 <-
            vb_pars_bystock_2018 %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender)))) %>%
            filter(!is.na(stock)) %>% 
            unite(age_stock, age, stock, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
            filter(Age < 30) %>% 
            ggplot() +
            geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_stock, color = Stock, fill = Stock)) +
            geom_line(aes(x = Age, y = fit, color = Stock),
                      data = vb_pars_bystock %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender)))) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(age_stock, age, stock, remove = FALSE) %>% 
                        rename(Age = age, Stock = stock, Gender = gender) %>% 
                        filter(Age < 30, year==20162019) )+
            theme_bw() + 
            facet_wrap(~Gender, ncol = 1)
          
          vb_pars_bystock_overtime <-
            all %>% 
            left_join(all_st) %>% 
            filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
            mutate(age = age + (month-1)/12,
                   person_sh = person %>% substr(., 1, 3)) %>% 
            unite(st_gen, person_sh, gender, year, remove = F) %>% 
            split(., .$st_gen) %>% #.[[19]]->x
            purrr::map(function(x){
              print(paste0(unique(x$division), '_', unique(x$gender)))
              prL<-seq(0,120,1)
              prA<-seq(0,60,1)
              mod <- NULL; fit <- NULL
              
              try( 
                {mod <- nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5))
                fit <- exp(predict(mod, data.frame(age=prA),type="response"))}, 
                silent = TRUE)
              
              if(!is.null(mod)){
              y <- 
                list(
                  mod = mod %>% 
                    broom::tidy() %>% 
                    mutate(person = unique(x$person),
                           gender = unique(x$gender),
                           year = unique(x$year)),
                  x = full_join(x, data.frame(age = prA, 
                                              fit,
                                              person = unique(x$person),
                                              gender = unique(x$gender),
                                              year = unique(x$year))) %>% select(-c(st_gen, person_sh))
                )
              } else { y <- list (mod = NULL, x = x %>% select(-c(st_gen, person_sh)))}
              return(y)
            }) 
          
          vb_plot_overtime_aru.27.123a4 <-
            vb_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                        ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year)) %>%
            filter(!is.na(stock)) %>% 
            unite(age_year, age, year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
            filter(Age < 30, Stock== 'aru.27.123a4') %>% 
            ggplot() +
            geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_year, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = Age, y = fit, color = Year),
                      data = vb_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(age_stock, age, stock, remove = FALSE) %>% 
                        rename(Age = age, Stock = stock, Gender = gender) %>% 
                        filter(Age < 30, Stock=='aru.27.123a4') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
        
          vb_plot_overtime_aru.27.5b6a <-
            vb_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year)) %>%
            filter(!is.na(stock)) %>% 
            unite(age_year, age, year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
            filter(Age < 30, Stock== 'aru.27.5b6a') %>% 
            ggplot() +
            geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_year, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = Age, y = fit, color = Year),
                      data = vb_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(age_stock, age, stock, remove = FALSE) %>% 
                        rename(Age = age, Stock = stock, Gender = gender) %>% 
                        filter(Age < 30, Stock=='aru.27.5b6a') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
          vb_plot_overtime_aru.27.5a14 <-
            vb_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male',  
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year)) %>%
            filter(!is.na(stock)) %>% 
            unite(age_year, age, year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
            filter(Age < 30, Stock== 'aru.27.5a14') %>% 
            ggplot() +
            geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_year, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = Age, y = fit, color = Year),
                      data = vb_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', gender)),
                               Year = as.factor(year)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(age_stock, age, stock, remove = FALSE) %>% 
                        rename(Age = age, Stock = stock, Gender = gender) %>% 
                        filter(Age < 30, Stock=='aru.27.5a14') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
          ####--------------Exponential length-weigt curves -------------####
          
          ####--------------By division and overall -------------####
          
            lw_pars <-
            all %>% 
            left_join(all_st) %>% 
            filter(!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
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
            filter(year==20162019,!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0) %>% 
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
        
          lw_pars %>% 
            bind_cols(lw_pars_2018 %>% right_join(lw_pars %>% select(term, division, gender))) %>% 
            write_csv('R/biol_figs_output/lwpars_bydivision_both.csv')
        
          ####--------------By stock -------------####
          
          lw_pars_bystock <-
            all %>% 
            left_join(all_st) %>% 
            filter(!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
            mutate(age = age + (month-1)/12,
                   person_sh = person %>% substr(., 1, 3)) %>% 
            unite(st_gen, person_sh, gender, remove = F) %>% 
            split(., .$st_gen) %>% #.[[1]]->x
            purrr::map(function(x){
              #print(paste0(unique(x$division), '_', unique(x$gender)))
              prL<-seq(0,120,1)
              prA<-seq(0,60,1)
              mod <-  lm(log(weight_g/1e3)~log(length_cm),x)
              fit <-exp(predict(mod, data.frame(length_cm=prL),type="response"))
                
              y <- 
                list(
                  mod =  mod %>% 
                    broom::tidy() %>% 
                    mutate(term= ifelse(term=='(Intercept)', 'Intercept', term),
                           term= ifelse(term=='log(length_cm)', 'Log Length (cm)', term)) %>% 
                    mutate(person = unique(x$person),
                           gender = unique(x$gender), 
                           estimate = ifelse(term=='Intercept', exp(estimate), estimate)),
                  x = full_join(x, data.frame(length_cm = prL, 
                                              fit, 
                                              person = unique(x$person),
                                              gender = unique(x$gender))) %>% 
                                  select(-c(st_gen, person_sh))
                )
              return(y)
            }) 
          
          lw_pars_bystock %>% 
            flatten() %>% 
            keep(., names(.)=="mod") %>% 
            bind_rows() %>% 
            write_csv('R/biol_figs_output/lwpars_bystock.csv')
          
          lw_plot_bystock <-
            lw_pars_bystock %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   `Weight (kg)` = weight_g/1000,
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock)) %>% 
            unite(len_stock, length_cm, stock, remove = FALSE) %>% 
            rename(`Length (cm)`  = length_cm, Stock = stock, Gender = gender) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10) %>% 
            ggplot() +
            geom_boxplot(aes(x = `Length (cm)`, y = `Weight (kg)`, group = len_stock, color = Stock, fill = Stock), alpha = 0.1) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Stock),
                      data = lw_pars_bystock %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10) )+
            theme_bw() + 
            facet_wrap(~Gender, ncol = 1)
        
          lw_pars_bystock_overtime <-
            all %>% 
            left_join(all_st) %>% 
            filter(!is.na(weight_g), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
            mutate(age = age + (month-1)/12,
                   person_sh = person %>% substr(., 1, 3)) %>% 
            unite(st_gen, person_sh, gender, year, remove = F) %>% 
            split(., .$st_gen) %>% #.[[19]]->x
            purrr::map(function(x){
              #print(paste0(unique(x$division), '_', unique(x$gender)))
              prL<-seq(0,120,1)
              prA<-seq(0,60,1)
              mod <- NULL; fit <- NULL
              
              try( 
                {mod <-  lm(log(weight_g/1e3)~log(length_cm),x)
                fit <-exp(predict(mod, data.frame(length_cm=prL),type="response"))
                }, 
                silent = TRUE)
              
              if(!is.null(mod)){
                y <- 
                  list(
                    mod =  mod %>% 
                      broom::tidy() %>% 
                      mutate(term= ifelse(term=='(Intercept)', 'Intercept', term),
                             term= ifelse(term=='log(length_cm)', 'Log Length (cm)', term)) %>% 
                      mutate(person = unique(x$person),
                             gender = unique(x$gender), 
                             year = unique(x$year),
                             estimate = ifelse(term=='Intercept', exp(estimate), estimate)),
                    x = full_join(x, data.frame(length_cm = prL, 
                                                fit,
                                                person = unique(x$person),
                                                gender = unique(x$gender),
                                                year = unique(x$year))) %>% 
                                    select(-c(st_gen, person_sh))
                  )
              } else { y <- list (mod = NULL, x = x %>% select(-c(st_gen, person_sh)))}
              return(y)
            }) 
          
          lw_plot_overtime_aru.27.123a4 <-
            lw_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year),
                   `Weight (kg)` = weight_g/1000,
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock), !is.na(fit)) %>% 
            unite(len_year, length_cm, year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock== 'aru.27.123a4') %>% 
            ggplot() +
            geom_boxplot(aes(x = `Length (cm)`, y = `Weight (kg)`, group = len_year, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Year),
                      data = lw_pars_bystock_overtime%>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock=='aru.27.123a4') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
          lw_plot_overtime_aru.27.5a14 <-
            lw_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year),
                   `Weight (kg)` = weight_g/1000,
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock), !is.na(fit)) %>% 
            unite(len_year, length_cm, year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock== 'aru.27.5a14') %>% 
            ggplot() +
            geom_boxplot(aes(x = `Length (cm)`, y = `Weight (kg)`, group = len_year, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Year),
                      data = lw_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock=='aru.27.5a14') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
          
          lw_plot_overtime_aru.27.5b6a <-
            lw_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year),
                   `Weight (kg)` = weight_g/1000,
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock), !is.na(fit)) %>% 
            unite(len_year, length_cm, year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock== 'aru.27.5b6a') %>% 
            ggplot() +
            geom_boxplot(aes(x = `Length (cm)`, y = `Weight (kg)`, group = len_year, color = Year, fill = Year), alpha = 1) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Year),
                      data = lw_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock=='aru.27.5b6a') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
        
          ####--------------Maturity ogives -------------####
          
          
          ####--------------By division and overall -------------####
          
          
          mat_pars <-
            all %>% 
            left_join(all_st) %>% 
            mutate(maturity_stage = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity_stage),
                   maturity = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity),
                   spawning = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, spawning)) %>% 
            filter(!is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0, gender != 'U', !is.na(division)) %>% 
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
              
              bind_rows(tmp,tibble(term = 'L50', 
                                   estimate = - tmp$estimate[tmp$term=='Intercept']/tmp$estimate[tmp$term=='Length (cm)'], 
                                   division = unique(tmp$division),
                                   gender = unique(tmp$gender)))
            }) %>% 
            bind_rows() %>% 
            write_csv('R/biol_figs_output/matpars_bydivision.csv')
          
          mat_pars_2018 <-
            all %>% 
            left_join(all_st) %>% 
            mutate(maturity_stage = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity_stage),
                   maturity = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity),
                   spawning = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, spawning)) %>% 
            filter(year==20162019, !is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0, gender != 'U') %>% 
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
              
              bind_rows(tmp,tibble(term = 'L50', 
                                   estimate = - tmp$estimate[tmp$term=='Intercept']/tmp$estimate[tmp$term=='Length (cm)'], 
                                   division = unique(tmp$division),
                                   gender = unique(tmp$gender)))
            }) %>% 
            bind_rows() %>% 
            write_csv('R/biol_figs_output/matpars_bydivision_2018.csv')
          
          mat_pars %>% 
            bind_cols(mat_pars_2018 %>% right_join(mat_pars %>% select(term, division, gender))) %>% 
            write_csv('R/biol_figs_output/matpars_bydivision_both.csv')
          
          
          ####--------------By stock -------------####
          
          mat_pars_bystock <-
            all %>% 
            left_join(all_st) %>% 
            mutate(maturity_stage = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity_stage),
                   maturity = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity),
                   spawning = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, spawning)) %>% 
            filter(!is.na(maturity), !is.na(length_cm), !is.na(gender), length_cm > 0, gender != 'U', !is.na(division)) %>% 
            mutate(age = age + (month-1)/12,
                   person_sh = person %>% substr(., 1, 3)) %>% 
            unite(st_gen, person_sh, gender, remove = F) %>% 
            split(., .$st_gen) %>% #.[[1]]->x
            purrr::map(function(x){
              #print(paste0(unique(x$division), '_', unique(x$gender)))
              prL<-seq(0,120,1)
              prA<-seq(0,60,1)
              mod <-  glm(mat~length_cm, data=x %>% mutate(mat = ifelse(maturity=='Mature', 1, 0)), family=binomial(link=logit))  
              fit <-  predict(mod, data.frame(length_cm=prL),type="response")
              
              y <- 
                list(
                  mod =  mod %>% 
                    broom::tidy() %>% 
                    mutate(term= ifelse(term=='(Intercept)', 'Intercept', term),
                           term= ifelse(term=='log(length_cm)', 'Log Length (cm)', term)) %>% 
                    mutate(person = unique(x$person),
                           gender = unique(x$gender), 
                           estimate = ifelse(term=='Intercept', exp(estimate), estimate)) %>% 
                    bind_rows(tibble(term = 'L50', 
                                     estimate = - coef(mod)[1]/coef(mod)[2], 
                                     person = unique(x$person),
                                     gender = unique(x$gender))),
                  x = full_join(x, data.frame(length_cm = prL, 
                                              fit,
                                              person = unique(x$person),
                                              gender = unique(x$gender))) %>% 
                    select(-c(st_gen, person_sh))
                )
              return(y)
            }) 
          
          mat_pars_bystock %>% 
            flatten() %>% 
            keep(., names(.)=="mod") %>% 
            bind_rows() %>% 
            write_csv('R/biol_figs_output/matpars_bystock.csv')
          
          mat_plot_bystock <-
            mat_pars_bystock %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock)) %>% 
            group_by(stock, gender, length_cm, maturity) %>% 
            count() %>% 
            filter(maturity  == 'Mature') %>% 
            left_join(mat_pars_bystock %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               length_cm = round(length_cm)) %>%
                        group_by(stock, gender, length_cm) %>% 
                        count(name = 'n_tot')) %>% 
            mutate(p = n/n_tot) %>% 
            unite(len_stock, length_cm, stock, remove = FALSE) %>% 
            rename(`Length (cm)`  = length_cm, Stock = stock, Gender = gender, `Proportion mature` = p) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10) %>% 
            ggplot() +
            geom_point(aes(x = `Length (cm)`, y = `Proportion mature`, color = Stock, fill = Stock)) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Stock),
                      data = mat_pars_bystock %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10) )+
            theme_bw() + 
            facet_wrap(~Gender, ncol = 1)
          
          
          mat_pars_bystock_overtime <-
            all %>% 
            left_join(all_st) %>% 
            mutate(maturity_stage = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity_stage),
                   maturity = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity),
                   spawning = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, spawning)) %>% 
            filter( !is.na(length_cm), !is.na(gender), length_cm > 0, gender != 'U') %>% 
            mutate(age = age + (month-1)/12,
                   person_sh = person %>% substr(., 1, 3)) %>% 
            unite(st_gen, person_sh, gender, year, remove = F) %>% 
            split(., .$st_gen) %>% #.[[19]]->x
            purrr::map(function(x){
              #print(paste0(unique(x$division), '_', unique(x$gender)))
              prL<-seq(0,120,1)
              prA<-seq(0,60,1)
              mod <- NULL; fit <- NULL
              
              try( 
                {mod <-  glm(mat~length_cm, data=x %>% mutate(mat = ifelse(maturity=='Mature', 1, 0)), family=binomial(link=logit))  
                fit <-  predict(mod, data.frame(length_cm=prL),type="response")
                }, 
                silent = TRUE)
              
              if(!is.null(mod)){
                y <- 
                  list(
                    mod =  mod %>% 
                      broom::tidy() %>% 
                      mutate(term= ifelse(term=='(Intercept)', 'Intercept', term),
                             term= ifelse(term=='log(length_cm)', 'Log Length (cm)', term)) %>% 
                      mutate(person = unique(x$person),
                             gender = unique(x$gender), 
                             estimate = ifelse(term=='Intercept', exp(estimate), estimate)) %>% 
                      bind_rows(tibble(term = 'L50', 
                                       estimate = - coef(mod)[1]/coef(mod)[2], 
                                       person = unique(x$person),
                                       gender = unique(x$gender),
                                       year = unique(x$year))),
                    x = full_join(x, data.frame(length_cm = prL, 
                                                fit,
                                                person = unique(x$person),
                                                gender = unique(x$gender),
                                                year = unique(x$year))) %>% 
                      select(-c(st_gen, person_sh))
                  )
              } else { y <- list (mod = NULL, x = x %>% select(-c(st_gen, person_sh)))}
              return(y)
            }) 
          
          mat_plot_overtime_aru.27.123a4 <-
            mat_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year),
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock)) %>% 
            group_by(stock, gender, length_cm, maturity, Year) %>% 
            count() %>% 
            filter(maturity  == 'Mature') %>% 
            left_join(mat_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        group_by(stock, gender, length_cm, Year) %>% 
                        count(name = 'n_tot')) %>% 
            mutate(p = n/n_tot) %>% 
            unite(len_year, length_cm, Year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender, `Proportion mature` = p) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock== 'aru.27.123a4') %>% 
            ggplot() +
            geom_point(aes(x = `Length (cm)`, y = `Proportion mature`, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Year),
                      data = mat_pars_bystock_overtime%>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock=='aru.27.123a4') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
          mat_plot_overtime_aru.27.5b6a <-
            mat_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year),
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock)) %>% 
            group_by(stock, gender, length_cm, maturity, Year) %>% 
            count() %>% 
            filter(maturity  == 'Mature') %>% 
            left_join(mat_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        group_by(stock, gender, length_cm, Year) %>% 
                        count(name = 'n_tot')) %>% 
            mutate(p = n/n_tot) %>% 
            unite(len_year, length_cm, Year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender, `Proportion mature` = p) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock== 'aru.27.5b6a') %>% 
            ggplot() +
            geom_point(aes(x = `Length (cm)`, y = `Proportion mature`, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Year),
                      data = mat_pars_bystock_overtime%>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock=='aru.27.5b6a') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
        
          mat_plot_overtime_aru.27.5a14 <-
            mat_pars_bystock_overtime %>% 
            flatten() %>% 
            keep(., names(.)=="x") %>% 
            bind_rows() %>% 
            mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                  ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                         ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                   gender = ifelse(gender=='F', 'Female',
                                   ifelse(gender=='M', 'Male', 
                                          ifelse(gender=='U', 'Unidentified', gender))),
                   Year = as.factor(year),
                   length_cm = round(length_cm)) %>%
            filter(!is.na(stock)) %>% 
            group_by(stock, gender, length_cm, maturity, Year) %>% 
            count() %>% 
            filter(maturity  == 'Mature') %>% 
            left_join(mat_pars_bystock_overtime %>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        group_by(stock, gender, length_cm, Year) %>% 
                        count(name = 'n_tot')) %>% 
            mutate(p = n/n_tot) %>% 
            unite(len_year, length_cm, Year, remove = FALSE) %>% 
            rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender, `Proportion mature` = p) %>% 
            filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock== 'aru.27.5a14') %>% 
            ggplot() +
            geom_point(aes(x = `Length (cm)`, y = `Proportion mature`, color = Year, fill = Year), alpha = 0.2) +
            geom_line(aes(x = `Length (cm)`, y = fit, color = Year),
                      data = mat_pars_bystock_overtime%>% 
                        flatten() %>% 
                        keep(., names(.)=="x") %>% 
                        bind_rows() %>% 
                        mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                              ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                     ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                               gender = ifelse(gender=='F', 'Female',
                                               ifelse(gender=='M', 'Male', 
                                                      ifelse(gender=='U', 'Unidentified', gender))),
                               Year = as.factor(year),
                               length_cm = round(length_cm)) %>%
                        filter(!is.na(stock), !is.na(fit)) %>% 
                        unite(len_stock, length_cm, stock, remove = FALSE) %>% 
                        rename(`Length (cm)` = length_cm, Stock = stock, Gender = gender) %>% 
                        filter(`Length (cm)` < 54, `Length (cm)` > 10, Stock=='aru.27.5a14') )+
            theme_bw() + 
            scale_fill_viridis_d() + 
            scale_color_viridis_d() +
            facet_wrap(~Gender, ncol = 1)
          
          
          
          
          ####--------------Mean length at age tables -------------####
          
        ml_age <-
          all %>% 
          left_join(all_st) %>% 
          mutate(quarter = ifelse(month %in% c(1,2,3), 1,
                                  ifelse(month %in% c(4,5,6), 2, 
                                         ifelse(month %in% c(7,8,9), 3,
                                                ifelse(month %in% c(10,11,12), 4, month)))),
            age = ifelse(age < 11, age + (quarter-1)/4, age)) %>% 
          filter(age < 21, year==20162019) %>% 
          group_by(country, division, source, age, year) %>% 
          filter(!is.na(age), !is.na(length_cm), length_cm > 0, !is.na(division)) %>% 
          summarise(ml = round(mean(length_cm, na.rm = T), 1), sdl = round(sd(length_cm, na.rm = T), 1)) %>%
          arrange(country, division, source, year, age) 
        
         ml_age %>%
           select(country, division, source, age, ml) %>% 
           unite('CDS', country, division, source) %>% 
           spread(value = ml, key = CDS) %>% 
          write_csv('R/biol_figs_output/meanlength_at_age_bydivision.csv')
         
         ml_age_plot <-
                ml_age %>%
                 ungroup() %>% 
                 mutate(source = ifelse(source=='Fishery', 'commercial', source)) %>% 
                 unite(`Country/Division/Source`, country, division, source, sep = '/') %>% 
                 rename(Age = age, `Mean Length (cm)` = ml, Year = year) %>% 
                 group_by(`Country/Division/Source`) %>% 
                 ggplot() + 
                 geom_line(aes(x = Age, y = `Mean Length (cm)`, color = `Country/Division/Source`)) + 
                 facet_wrap(~Year)
        
         
         ####--------------Size and depth relationships -------------####
         
         #size x depth
         size_depth_plot <-
                 all %>% 
                 left_join(all_st) %>% 
                 mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                       ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                              ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                        Depth = -depth_m,
                        Year = as.factor(yr)) %>%
                 rename(Length = length_cm, Stock = stock) %>%
                 filter(yr > 1999) %>% 
                 ggplot() + 
                 geom_point(aes(y = Depth, x = Length, color = Year))+
                 facet_wrap(~Stock) + 
                 theme_bw()
         
         size_depth <-
          all %>% 
          left_join(all_st) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
          filter(!is.na(length_cm), length_cm > 0, !is.na(division)) %>% 
          lm(length_cm ~ depth_m + division + depth_m*division, data=.)  %>% 
              broom::tidy() %>% 
          write_csv('R/biol_figs_output/size_depth_lm.csv')
        
        size_depth_latlon <-
          all %>% 
          left_join(all_st) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
          filter(!is.na(length_cm), length_cm > 0, !is.na(division)) %>% 
          lm(length_cm ~ depth_m + lat + lon + depth_m*lat + depth_m*lon + lat*lon, data=.)  %>% 
          broom::tidy() %>% 
          write_csv('R/biol_figs_output/size_depth_latlon_lm.csv')
        
        size_depth_2018 <-
          all %>% 
          left_join(all_st) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
          filter(year==20162019,!is.na(length_cm), length_cm > 0) %>% 
          lm(length_cm ~ depth_m + division + depth_m*division, data=.)  %>% 
          broom::tidy() %>% 
          write_csv('R/biol_figs_output/size_depth_lm_2018.csv')
        
        size_depth_latlon_2018 <-
          all %>% 
          left_join(all_st) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
          filter(year==20162019, !is.na(length_cm), length_cm > 0) %>% 
          lm(length_cm ~ depth_m + lat + lon + depth_m*lat + depth_m*lon + lat*lon, data=.)  %>% 
          broom::tidy() %>% 
          write_csv('R/biol_figs_output/size_depth_latlon_lm_2018.csv')
        
        
        
        ####--------------Maps showing expected and residuals from overall relationships -------------####
        
        tmp_vb <-
          all %>% 
          left_join(all_st) %>%
          filter(!is.na(age), !is.na(length_cm), length_cm > 0, !is.na(division)) %>% 
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
          group_by(rect, yr) %>% 
          summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
                    `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1))+
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        growth_residuals_plot <- 
          tmp_vb %>% 
          mutate(expected_length = exp(fitted(overall_vb)), residuals = length_cm - expected_length) %>% 
          group_by(rect, yr) %>% 
          summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
                    `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        
        tmp_lw <-
          all %>% 
          left_join(all_st) %>%
          filter(!is.na(weight_g), !is.na(length_cm), length_cm > 0, !is.na(division)) %>% 
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
          group_by(rect, yr) %>% 
          summarise(`Expected Weight (kg)` = mean(expected_weight, na.rm = T),
                    `Residual Weight (kg)` = mean(residuals, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1))+
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        
        weight_residuals_plot <- 
          tmp_lw %>% 
          mutate(expected_weight = exp(fitted(overall_lw)), residuals = weight_g/1e3 - expected_weight) %>% 
          filter(residuals < 1) %>% 
          group_by(rect, yr) %>% 
          summarise(`Expected Weight (kg)` = mean(expected_weight, na.rm = T),
                    `Residual Weight (kg)` = mean(residuals, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        
        
        ####--------------Expected and residual for ages 8 and 9 only -------------####
        
        #length at ages 8 & 9 (most frequent age)
        all %>% 
          left_join(all_st) %>%
          group_by(age) %>% 
          count()
        
        tmp_l8 <-
          all %>% 
          left_join(all_st) %>%
          filter(!is.na(length_cm), length_cm > 0, age %in% c(8,9, !is.na(division))) %>% 
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
          group_by(rect, yr) %>% 
          summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
                    `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1))+
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        l8_residual_plot <- 
          tmp_l8 %>% 
          mutate(expected_length = fitted(overall_l8), residuals = length_cm - expected_length) %>% 
          group_by(rect, yr) %>% 
          summarise(`Expected Length (cm)` = mean(expected_length, na.rm = T),
                    `Residual Length (cm)` = mean(residuals, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1))+
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        
        
        ####-------------- Max, 95%, median ages and lengths, also by depth -------------####
        
        #max age
        tmp_maxage <-
          all %>% 
          left_join(all_st) %>%
          filter(!is.na(age), !is.na(division)) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat),
                 age = age + (month-1)/12)
        
        
        maxage_plot <- 
          tmp_maxage %>% 
          group_by(rect, yr) %>% 
          summarise(`Max. age` = max(age, na.rm = T),
                    `95% age` = quantile(age, 0.95, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        age95_plot <- 
          tmp_maxage %>% 
          group_by(rect, yr) %>% 
          summarise(`Max. age` = max(age, na.rm = T),
                    `95% age` = quantile(age, 0.95, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        #95% length - need to get from length distributions
        #max age
        tmp_maxl <-
          all %>% 
          left_join(all_st) %>%
          filter(!is.na(length_cm), length_cm > 0, !is.na(division)) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat),
                 age = age + (month-1)/12)
        
        
        maxl_plot <- 
          tmp_maxl %>% 
          group_by(rect, yr) %>% 
          summarise(`Max. length (cm)` = max(length_cm, na.rm = T),
                    `95% length (cm)` = quantile(length_cm, 0.95, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        l95_plot <- 
          tmp_maxl %>% 
          group_by(rect, yr) %>% 
          summarise(`Max. length (cm)` = max(length_cm, na.rm = T),
                    `95% length (cm)` = quantile(length_cm, 0.95, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        # Median lengths
        l50_plot_0 <- 
          tmp_maxl %>% 
          filter(depth_m <= 300) %>% 
          group_by(rect, yr) %>% 
          summarise(`Mean length (cm)` = mean(length_cm, na.rm = T),
                    `50% length (cm)` = quantile(length_cm, 0.50, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        l50_plot_300 <- 
          tmp_maxl %>% 
          filter(depth_m > 300, depth_m <= 500) %>% 
          group_by(rect, yr) %>% 
          summarise(`Mean length (cm)` = mean(length_cm, na.rm = T),
                    `50% length (cm)` = quantile(length_cm, 0.50, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        l50_plot_500 <- 
          tmp_maxl %>% 
          filter(depth_m > 500) %>% 
          group_by(rect, yr) %>% 
          summarise(`Mean length (cm)` = mean(length_cm, na.rm = T),
                    `50% length (cm)` = quantile(length_cm, 0.50, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        ####-------------- Maturity and spawning maps -------------####
        
        
        #L50 maturity
        tmp_l50 <-
          all %>% 
          left_join(all_st) %>%
          mutate(maturity_stage = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity_stage),
                 maturity = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity),
                 spawning = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, spawning)) %>% 
          filter(!is.na(length_cm), length_cm > 0, !is.na(maturity), !is.na(division)) %>% 
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
                      group_by(rect, yr, length_cm) %>% 
                      count()) %>% 
          mutate(p = n_mat/n) %>% 
          filter(p <= 0.5, length_cm > 10) %>% 
          group_by(rect, yr) %>% 
          summarise(L50 = max(length_cm, na.rm = T)) %>% 
          filter(yr > yr_min-1, yr < yr_max+1) %>% 
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
          facet_wrap(~yr, ncol = 3) +
          theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 0, y = 77, label = yr)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        
        #spawning
        tmp_sp <-
          all %>% 
          left_join(all_st) %>%
          mutate(maturity_stage = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity_stage),
                 maturity = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, maturity),
                 spawning = ifelse(person=='Lise H. Ofstad' & (haul_id %in% c(18080027, 18080028) | year %in% c(1994:1998)), NA, spawning)) %>% 
          filter(!is.na(length_cm), length_cm > 0, !is.na(spawning), !is.na(division)) %>% 
          mutate(rect =  mapplots::ices.rect2(lon, lat),
                 age = age + (month-1)/12,
                 quarter = ifelse(month %in% c(1,2,3), 1,
                                  ifelse(month %in% c(4,5,6), 2, 
                                         ifelse(month %in% c(7,8,9), 3,
                                                ifelse(month %in% c(10,11,12), 4, month)))))
        
        spawning_plot <- 
          tmp_sp %>% 
          group_by(rect, quarter, spawning) %>% 
          count() %>% 
          ungroup %>% 
          filter(spawning=='yes') %>% 
          rename(n_sp = n) %>% 
          left_join(tmp_sp %>% 
                      group_by(rect, quarter) %>% 
                      count()) %>% 
          mutate(`Proportion spawners` = n_sp/n) %>% 
          #filter(year > yr_min-1, year < yr_max+1) %>% 
          bind_cols(mapplots::ices.rect(.$rect)) %>% 
          #separate(sq,c("lon","lat"), sep=':',convert = TRUE) %>%
          ggplot() + 
          #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
          geom_tile(aes(lon, lat, fill=`Proportion spawners`),interpolate = FALSE) + 
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
          facet_wrap(~quarter) +
          #theme(legend.position = c(0.9, 0.1)) +
          geom_text(aes(x = x, y = y, label = label), data = tibble(quarter = 1:4, x= 0, y = 77, label = quarter)) +
          geom_sf(data = ia, colour = 'black', fill = NA, lwd = 0.05) +
          coord_sf(xlim = c(-34, 18),ylim = c(57, 80))
        
        tmp_sp %>% 
          group_by(country, division, source, month, spawning) %>% 
          count() %>% 
          ungroup %>% 
          filter(spawning=='yes') %>% 
          rename(n_sp = n) %>% 
          left_join(tmp_sp %>% 
                      group_by(country, division, source, month) %>% 
                      count()) %>% 
          mutate(p = n_sp/n) %>% 
          filter(p > 0.2) %>% 
          select(country, division, source, month, p) %>% 
          arrange(country, desc(p)) %>% 
          write_csv('R/biol_figs_output/sp_bydivision.csv')
        
        #samples
        tmp_samples <-
                all %>% 
                left_join(all_st) %>%
                filter(!is.na(length_cm), length_cm > 0,!is.na(division)) %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat),
                       quarter = ifelse(month %in% c(1,2,3), 1,
                                        ifelse(month %in% c(4,5,6), 2, 
                                               ifelse(month %in% c(7,8,9), 3,
                                                      ifelse(month %in% c(10,11,12), 4, month))))) %>% 
                group_by(division, person, source) %>% 
                count() %>% 
                mutate(ifelse(person=='Elvar Hallfredsson', 'Norway',
                              ifelse(person == 'Pamela J. Woods', 'Iceland',
                                     ifelse(person=='Lise H. Ofstad', 'Faroe Islands', person)))) %>% 
                ungroup %>% 
                select(-c(person)) %>% 
                write_csv('R/biol_figs_output/sample_origin.csv')
        
        
        ####-------------- Figure output -------------####
        
        
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
         
         png(paste0('R/biol_figs_output/spawning_plot.png'), height = 500, width = 500)
         print(spawning_plot)
         dev.off()
         
         png(paste0('R/biol_figs_output/vb_plot_bystock.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(vb_plot_bystock)
         dev.off()
         
         png(paste0('R/biol_figs_output/vb_plot_bystock_2018.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(vb_plot_bystock_2018)
         dev.off()
         
         png(paste0('R/biol_figs_output/vb_plot_overtime_aru.27.123a4.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(vb_plot_overtime_aru.27.123a4)
         dev.off()
         
         png(paste0('R/biol_figs_output/vb_plot_overtime_aru.27.5a14.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(vb_plot_overtime_aru.27.5a14)
         dev.off()
        
         png(paste0('R/biol_figs_output/vb_plot_overtime_aru.27.5b6a.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(vb_plot_overtime_aru.27.5b6a)
         dev.off()
         
         png(paste0('R/biol_figs_output/lw_plot_bystock.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(lw_plot_bystock)
         dev.off()
         
         png(paste0('R/biol_figs_output/lw_plot_overtime_aru.27.123a4.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(lw_plot_overtime_aru.27.123a4)
         dev.off()
         
         png(paste0('R/biol_figs_output/lw_plot_overtime_aru.27.5a14.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(lw_plot_overtime_aru.27.5a14)
         dev.off()
         
         png(paste0('R/biol_figs_output/lw_plot_overtime_aru.27.5b6a.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(lw_plot_overtime_aru.27.5b6a)
         dev.off()
         
         png(paste0('R/biol_figs_output/mat_plot_bystock.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(mat_plot_bystock)
         dev.off()
         
         png(paste0('R/biol_figs_output/mat_plot_overtime_aru.27.123a4.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(mat_plot_overtime_aru.27.123a4)
         dev.off()
         
         png(paste0('R/biol_figs_output/mat_plot_overtime_aru.27.5a14.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(mat_plot_overtime_aru.27.5a14)
         dev.off()
         
         png(paste0('R/biol_figs_output/mat_plot_overtime_aru.27.5b6a.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(mat_plot_overtime_aru.27.5b6a)
         dev.off()

         png(paste0('R/biol_figs_output/size_depth_plot.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(size_depth_plot)
         dev.off()
         
         png(paste0('R/biol_figs_output/ml_age_plot.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
         print(ml_age_plot)
         dev.off()
         
         #ICELAND ONLY
        library(mar)
        mar <- connect_mar()
         
        spat_ind <- 
                 tbl(mar,paste0("raw_index_calc_",19)) %>%
                 rename(year=ar) %>%
                 #filter((synaflokkur == 30 & tognumer %in% c(1:39, NA))|(synaflokkur == 35 & )) %>% 
                 filter(!(year==2011&synaflokkur==35)) %>% 
                 mutate(synaflokkur = ifelse(synaflokkur == 30, 'Spring survey','Autumn survey'),
                        GRIDCELL = as.character(GRIDCELL)) %>% 
                 #filter(year>1992,year<tyr) %>% 
                 collect(n=Inf) 
                         
        all_30_map <- 
                spat_ind %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
                #filter(lengd < 25) %>%
                filter(N > 0) %>%  
                group_by(rect, year, synaflokkur) %>% 
                summarise(logN = log(mean(N, na.rm = T))) %>% 
                rename(Survey = synaflokkur) %>% 
                filter(Survey=='Spring survey') %>% 
                bind_cols(mapplots::ices.rect(.$rect)) %>% 
                 ggplot() + 
                 #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
                 geom_tile(aes(lon, lat, fill=logN)) + 
                 geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
                 theme_bw()+
                 scale_fill_viridis_c(direction = -1)+
                 xlab('Longitude (W)') +
                 ylab('Latitude (N)')+ 
                 facet_wrap(~year, ncol = 5)

        l25_30_map <- 
                spat_ind %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
                filter(lengd < 25) %>%
                filter(N > 0) %>%  
                group_by(rect, year, synaflokkur) %>% 
                summarise(logN = log(mean(N, na.rm = T))) %>% 
                rename(Survey = synaflokkur) %>% 
                filter(Survey=='Spring survey') %>% 
                bind_cols(mapplots::ices.rect(.$rect)) %>% 
                ggplot() + 
                #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
                geom_tile(aes(lon, lat, fill=logN)) + 
                geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
                theme_bw()+
                scale_fill_viridis_c(direction = -1)+
                xlab('Longitude (W)') +
                ylab('Latitude (N)')+ 
                facet_wrap(~year, ncol = 5)
        
        l40_30_map <- 
                spat_ind %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
                filter(lengd > 40) %>%
                filter(N > 0) %>%  
                group_by(rect, year, synaflokkur) %>% 
                summarise(logN = log(mean(N, na.rm = T))) %>% 
                rename(Survey = synaflokkur) %>% 
                filter(Survey=='Spring survey') %>% 
                bind_cols(mapplots::ices.rect(.$rect)) %>% 
                ggplot() + 
                #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
                geom_tile(aes(lon, lat, fill=logN)) + 
                geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
                theme_bw()+
                scale_fill_viridis_c(direction = -1)+
                xlab('Longitude (W)') +
                ylab('Latitude (N)')+ 
                facet_wrap(~year, ncol = 5)
        
        png(paste0('R/biol_figs_output/all_30_map.png'), height = png_dims[2], width = png_dims[1])
        print(all_30_map)
        dev.off()
        
        png(paste0('R/biol_figs_output/l25_30_map.png'), height = png_dims[2], width = png_dims[1])
        print(l25_30_map)
        dev.off()
        
        png(paste0('R/biol_figs_output/l40_30_map.png'), height = png_dims[2], width = png_dims[1])
        print(l40_30_map)
        dev.off()
        
        all_35_map <- 
                spat_ind %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
                #filter(lengd < 25) %>%
                filter(N > 0) %>%  
                group_by(rect, year, synaflokkur) %>% 
                summarise(logN = log(mean(N, na.rm = T))) %>% 
                rename(Survey = synaflokkur) %>% 
                filter(Survey=='Autumn survey') %>% 
                bind_cols(mapplots::ices.rect(.$rect)) %>% 
                ggplot() + 
                #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
                geom_tile(aes(lon, lat, fill=logN)) + 
                geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
                theme_bw()+
                scale_fill_viridis_c(direction = -1)+
                xlab('Longitude (W)') +
                ylab('Latitude (N)')+ 
                facet_wrap(~year, ncol = 5)
        
        l25_35_map <- 
                spat_ind %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
                filter(lengd < 25) %>%
                filter(N > 0, year > 1999, year!=2011) %>%  
                group_by(rect, year, synaflokkur) %>% 
                summarise(logN = log(mean(N, na.rm = T))) %>% 
                rename(Survey = synaflokkur) %>% 
                filter(Survey=='Autumn survey') %>% 
                bind_cols(mapplots::ices.rect(.$rect)) %>% 
                ggplot() + 
                #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
                geom_tile(aes(lon, lat, fill=logN)) + 
                geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
                theme_bw()+
                scale_fill_viridis_c(direction = -1)+
                xlab('Longitude (W)') +
                ylab('Latitude (N)')+ 
                facet_wrap(~year, ncol = 4)
        
        l40_35_map <- 
                spat_ind %>% 
                mutate(rect =  mapplots::ices.rect2(lon, lat)) %>% 
                filter(lengd > 40) %>%
                filter(N > 0, year > 1999, year!=2011) %>%  
                group_by(rect, year, synaflokkur) %>% 
                summarise(logN = log(mean(N, na.rm = T))) %>% 
                rename(Survey = synaflokkur) %>% 
                filter(Survey=='Autumn survey') %>% 
                bind_cols(mapplots::ices.rect(.$rect)) %>% 
                ggplot() + 
                #coord_quickmap(xlim = c(-38, 18),ylim = c(55, 74))+
                geom_tile(aes(lon, lat, fill=logN)) + 
                geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray',col='black',lwd=0.1) +
                theme_bw()+
                scale_fill_viridis_c(direction = -1)+
                xlab('Longitude (W)') +
                ylab('Latitude (N)')+ 
                facet_wrap(~year, ncol = 4)
        
        png(paste0('R/biol_figs_output/all_35_map.png'), height = png_dims[2], width = png_dims[1])
        print(all_35_map)
        dev.off()
        
        png(paste0('R/biol_figs_output/l25_35_map.png'), height = png_dims[2], width = png_dims[1])
        print(l25_35_map)
        dev.off()
        
        png(paste0('R/biol_figs_output/l40_35_map.png'), height = png_dims[2], width = png_dims[1])
        print(l40_35_map)
        dev.off()
        
        
        vb_pars_bystock_byyear_overtime <-
                all %>% 
                left_join(all_st) %>% 
                filter(!is.na(age), !is.na(length_cm), !is.na(gender), length_cm > 0, !is.na(division)) %>% 
                mutate(age = age + (month-1)/12,
                       person_sh = person %>% substr(., 1, 3)) %>% 
                unite(st_gen, person_sh, gender, yr, remove = F) %>% 
                split(., .$st_gen) %>% #.[[19]]->x
                purrr::map(function(x){
                        print(paste0(unique(x$division), '_', unique(x$gender)))
                        prL<-seq(0,120,1)
                        prA<-seq(0,60,1)
                        mod <- NULL; fit <- NULL
                        
                        try( 
                                {mod <- nls(log(length_cm)~log(Linf*(1-exp(-K*(age-t0)))), data=x, start=list(Linf=50, K=0.2, t0=-0.5))
                                fit <- exp(predict(mod, data.frame(age=prA),type="response"))}, 
                                silent = TRUE)
                        
                        if(!is.null(mod)){
                                y <- 
                                        list(
                                                mod = mod %>% 
                                                        broom::tidy() %>% 
                                                        mutate(person = unique(x$person),
                                                               gender = unique(x$gender),
                                                               yr = unique(x$yr)),
                                                x = full_join(x, data.frame(age = prA, 
                                                                            fit,
                                                                            person = unique(x$person),
                                                                            gender = unique(x$gender),
                                                                            yr = unique(x$yr))) %>% select(-c(st_gen, person_sh))
                                        )
                        } else { y <- list (mod = NULL, x = x %>% select(-c(st_gen, person_sh)))}
                        return(y)
                }) 
        

        
        vb_plot_overtime_byyear_aru.27.5a14 <-
                vb_pars_bystock_byyear_overtime %>% 
                flatten() %>% 
                keep(., names(.)=="x") %>% 
                bind_rows() %>% 
                mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                      ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                             ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                       gender = ifelse(gender=='F', 'Female',
                                       ifelse(gender=='M', 'Male',  
                                              ifelse(gender=='U', 'Unidentified', gender))),
                       Year = as.factor(yr)) %>%
                filter(!is.na(stock), yr > 2004) %>% 
                unite(age_year, age, yr, remove = FALSE) %>% 
                rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
                filter(Age < 30, Stock== 'aru.27.5a14') %>% 
                ggplot() +
                geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_year, color = Year, fill = Year), alpha = 0.2) +
                geom_line(aes(x = Age, y = fit, color = Year),
                          data = vb_pars_bystock_byyear_overtime %>% 
                                  flatten() %>% 
                                  keep(., names(.)=="x") %>% 
                                  bind_rows() %>% 
                                  mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                                        ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                               ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                                         gender = ifelse(gender=='F', 'Female',
                                                         ifelse(gender=='M', 'Male', gender)),
                                         Year = as.factor(yr)) %>%
                                  filter(!is.na(stock), !is.na(fit), yr > 2004) %>% 
                                  unite(age_stock, age, stock, remove = FALSE) %>% 
                                  rename(Age = age, Stock = stock, Gender = gender) %>% 
                                  filter(Age < 30, Stock=='aru.27.5a14') )+
                theme_bw() + 
                scale_fill_viridis_d() + 
                scale_color_viridis_d() +
                facet_wrap(~Gender, ncol = 1)

        vb_plot_overtime_byyear_aru.27.123a4 <-
                vb_pars_bystock_byyear_overtime %>% 
                flatten() %>% 
                keep(., names(.)=="x") %>% 
                bind_rows() %>% 
                mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                      ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                             ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                       gender = ifelse(gender=='F', 'Female',
                                       ifelse(gender=='M', 'Male',  
                                              ifelse(gender=='U', 'Unidentified', gender))),
                       Year = as.factor(yr)) %>%
                filter(!is.na(stock), yr > 2004) %>% 
                unite(age_year, age, yr, remove = FALSE) %>% 
                rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
                filter(Age < 30, Stock== 'aru.27.123a4') %>% 
                ggplot() +
                geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_year, color = Year, fill = Year), alpha = 0.2) +
                geom_line(aes(x = Age, y = fit, color = Year),
                          data = vb_pars_bystock_byyear_overtime %>% 
                                  flatten() %>% 
                                  keep(., names(.)=="x") %>% 
                                  bind_rows() %>% 
                                  mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                                        ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                               ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                                         gender = ifelse(gender=='F', 'Female',
                                                         ifelse(gender=='M', 'Male', gender)),
                                         Year = as.factor(yr)) %>%
                                  filter(!is.na(stock), !is.na(fit), yr > 2004) %>% 
                                  unite(age_stock, age, stock, remove = FALSE) %>% 
                                  rename(Age = age, Stock = stock, Gender = gender) %>% 
                                  filter(Age < 30, Stock=='aru.27.123a4') )+
                theme_bw() + 
                scale_fill_viridis_d() + 
                scale_color_viridis_d() +
                facet_wrap(~Gender, ncol = 1)

        vb_plot_overtime_byyear_aru.27.5b6a <-
                vb_pars_bystock_byyear_overtime %>% 
                flatten() %>% 
                keep(., names(.)=="x") %>% 
                bind_rows() %>% 
                mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                      ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                             ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                       gender = ifelse(gender=='F', 'Female',
                                       ifelse(gender=='M', 'Male',  
                                              ifelse(gender=='U', 'Unidentified', gender))),
                       Year = as.factor(yr)) %>%
                filter(!is.na(stock), yr > 2004) %>% 
                unite(age_year, age, yr, remove = FALSE) %>% 
                rename(`Length (cm)` = length_cm, Age = age, Stock = stock, Gender = gender) %>% 
                filter(Age < 30, Stock== 'aru.27.5b6a') %>% 
                ggplot() +
                geom_boxplot(aes(x = Age, y = `Length (cm)`, group = age_year, color = Year, fill = Year), alpha = 0.2) +
                geom_line(aes(x = Age, y = fit, color = Year),
                          data = vb_pars_bystock_byyear_overtime %>% 
                                  flatten() %>% 
                                  keep(., names(.)=="x") %>% 
                                  bind_rows() %>% 
                                  mutate(stock = ifelse(person=='Elvar Hallfredsson', 'aru.27.123a4',
                                                        ifelse(person=='Pamela J. Woods', 'aru.27.5a14', 
                                                               ifelse(person=='Lise H. Ofstad', 'aru.27.5b6a', person))),
                                         gender = ifelse(gender=='F', 'Female',
                                                         ifelse(gender=='M', 'Male',  
                                                                ifelse(gender=='U', 'Unidentified', gender))),
                                         Year = as.factor(yr)) %>%
                                  filter(!is.na(stock), !is.na(fit), yr > 2004) %>% 
                                  unite(age_stock, age, stock, remove = FALSE) %>% 
                                  rename(Age = age, Stock = stock, Gender = gender) %>% 
                                  filter(Age < 30, Stock=='aru.27.5b6a') )+
                theme_bw() + 
                scale_fill_viridis_d() + 
                scale_color_viridis_d() +
                facet_wrap(~Gender, ncol = 1)
        
        png(paste0('R/biol_figs_output/vb_plot_overtime_byyear_aru.27.123a4.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
        print(vb_plot_overtime_byyear_aru.27.123a4)
        dev.off()
        
        png(paste0('R/biol_figs_output/vb_plot_overtime_byyear_aru.27.5a14.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
        print(vb_plot_overtime_byyear_aru.27.5a14)
        dev.off()
        
        png(paste0('R/biol_figs_output/vb_plot_overtime_byyear_aru.27.5b6a.png'), height = png_dims[2]*0.75, width = png_dims[1]*0.75)
        print(vb_plot_overtime_byyear_aru.27.5b6a)
        dev.off()
        
        #OTHER ZOOMED MAPS
        
        
        png(paste0('R/biol_figs_output/l50_plot_0_faroes.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_0 +
                      coord_sf(xlim = c(-12, -2),ylim = c(60, 63.5))+
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= -8, y = 63.25, label = yr))
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_300_faroes.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_300 +
                      coord_sf(xlim = c(-12, -2),ylim = c(60, 63.5))+
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= -8, y = 63.25, label = yr))
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_500_faroes.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_500+
                      coord_sf(xlim = c(-12, -2),ylim = c(60, 63.5))+
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= -8, y = 63.25, label = yr))
        )
        dev.off()
        
        
        png(paste0('R/biol_figs_output/l50_plot_0_iceland.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_0 +
                      coord_sf(xlim = c(-31, -12),ylim = c(62, 68))+
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= -28, y = 67, label = yr))
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_300_iceland.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_300 +
                      coord_sf(xlim = c(-31, -12),ylim = c(62, 68)) +
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= -28, y = 67, label = yr))
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_500_iceland.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_500+
                      coord_sf(xlim = c(-31, -12),ylim = c(62, 68)) +
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= -28, y = 67, label = yr)) 
        )
        dev.off()
        
        
        png(paste0('R/biol_figs_output/l50_plot_0_norway_s.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_0 +
                      coord_sf(xlim = c(-2, 18),ylim = c(57, 66)) +
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x=2, y = 65, label = yr)) 
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_300_norway_s.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_300 +
                      coord_sf(xlim = c(-2, 18),ylim = c(57, 66)) +
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max,  x= 2, y = 65, label = yr)) 
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_500_norway_s.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_500+
                      coord_sf(xlim = c(-2, 18),ylim = c(57, 66)) +
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 8, y = 65, label = yr)) 
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_0_norway_n.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_0 +
                      coord_sf(xlim = c(5, 27),ylim = c(66, 80)) +
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 8, y = 78, label = yr)) 
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_300_norway_n.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_300 +
                      coord_sf(xlim = c(5, 27),ylim = c(66, 80)) + 
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 8, y = 78, label = yr)) 
        )
        dev.off()
        
        png(paste0('R/biol_figs_output/l50_plot_500_norway_n.png'), height = png_dims[1], width = png_dims[2])
        print(l50_plot_500+
                      coord_sf(xlim = c(5, 27),ylim = c(66, 80)) + 
                      geom_text(aes(x = x, y = y, label = label), data = tibble(yr = yr_min:yr_max, x= 6, y = 78, label = yr)) 
        )
        dev.off()
        
        
        
        