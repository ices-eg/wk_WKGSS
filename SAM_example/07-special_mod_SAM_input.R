#This script is for including species-specific modifications to SAM input that was created
#with 06-create_SAM_input

cn     <- read_csv(paste0(yr_dir,'/','catage.csv'))
cw     <- read_csv(paste0(yr_dir,'/','catch_weights.csv'))
  smh_n  <- read_csv(paste0(yr_dir,'/','smh_n.csv'))
  smh_b  <- read_csv(paste0(yr_dir,'/','smh_b.csv'))
  smh_sw <- read_csv(paste0(yr_dir,'/','smh_stock_weights.csv'))
  smh_len    <- read_csv(paste0(yr_dir,'/','smh_len.csv'))
mat    <- read_csv(paste0(yr_dir,'/','maturity.csv'))

#cn, cw, mat, sw need to start from the beginning of the model (I think - based on haddock ex.)
#smb and smh can have missing age columns or -1 for missing year rows.

clean_weights <- function(x){
  #In words: if the 'weight at age' x is greater than 1.96*sd + mean of x (over years) after the minimum and maximum values have been removed, 
  #(OR conversely less than -1.96*sd + mean of x after min and max have been removed), 
  #OR it's a missing value (== -1),
  #then replace with the mean, after min and max have been removed
  #NOTE that when there are multiple min/max matches, only the first is removed
  neg1s <- x!= -1
    
  min_ind <- which.min(x[neg1s])
  max_ind <- which.max(x[neg1s])
  
  if(!(max(x) %in% c(tyr-1, tyr))){
    
  x[neg1s][c(min_ind,max_ind)] <- -1
    
  x <- ifelse(!all(x == -1) & ((x > mean(x[neg1s][-c(min_ind, max_ind)]) + 1.96*sd(x[neg1s][-c(min_ind, max_ind)])) |
              (x < mean(x[neg1s][-c(min_ind, max_ind)]) - 1.96*sd(x[neg1s][-c(min_ind, max_ind)])) |
              (x == -1)),
         mean(x[neg1s][-c(min_ind, max_ind)]),
         x)
  } else {x}
  
}

#Convert all to SAM format

format_SAM <- function(x, 
                       fin_year = tyr + 1, 
                       first_year = 1900,
                       smb = FALSE, 
                       smh = FALSE,
                       cn = FALSE, 
                       add_ages = NULL){
  x <-
    x %>% 
    #mutate_all(~ifelse(is.na(.),-1,.)) %>%  
    filter(Year < fin_year & Year >= first_year)

  if(smb | smh | cn){
    x <-
      x %>% 
      mutate_all(~ifelse(.==0, . + 0.001, .))
  }
  
  dat <-
    x %>% 
    select(-Year) %>% 
    as.matrix()
  
  rownames(dat) <- x$Year
  
  if(smb | smh){
  if(smb){attributes(dat)$time <- c(0.15,0.2)} else {if(smh){attributes(dat)$time <- c(0.7,0.8)} else {attributes(dat)$time <- c(0.45,0.55)}}
    }
  
  if(!is.null(add_ages)){
    dat <- cbind(as_data_frame(add_ages, dat)) # for adding missing columns of age data
  }
  
  dat
}


  cn <- 
    cn %>% 
    purrr::map(function(x) ifelse(x == -1, x, x*1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year/1000))
  
  smh_n <-
    smh_n %>% 
    purrr::map(function(x) ifelse(x == -1, x, x*1000)) %>% 
    bind_cols(.) %>%  
    mutate(Year = round(Year/1000)) %>% 
    filter(Year > 1995) #%>% #not sure why earlier data not working right now...
  #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  
  smh_b <-
    smh_b %>% 
    filter(Year > 1995) #%>% 
  #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  nages <- tbl(mar,'age_minlength') %>% filter(species==19) %>% collect(n=Inf) %>% dim(.) %>% unlist %>% .[1]
  smh_n[smh_n$Year==2011,2:(nages+1)] <- smh_b[smh_b$Year==2011,2:(nages+1)] <- rep(-1,nages) #only partial data from cancelled survey
  

  
  cw <-
    cw %>% 
    mutate(`1` = ifelse(`1` >= `2`, -1, `1`),
           `2` = ifelse(`2` >= `3`, -1, `2`)) %>% #converting obviously wrong weights to NAs. But makes judgment that smaller is the wrong one.
    purrr::map(function(x) clean_weights(x)) %>% 
    purrr::map(function(x) ifelse(is.na(x), mean(x, na.rm = T), x)) %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year*1000),
           `25` = `24`) #careful that years are not changed at this step
  

  smh_sw <-
    smh_sw %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year*1000)) %>% 
    (function(x){bind_rows(cw[cw$Year %in% c(1982:1999,2011),],
                           x[!(x$Year %in% c(1982:1999,2011)),])}) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) %>% 
    arrange(Year)
  
  mat <- 
    mat %>% 
    mutate(`17` = ifelse(`17` == -1, `16`, `17`),
           `18` = ifelse(`18` == -1, `17`, `18`),
           `19` = ifelse(`19` == -1, `18`, `19`),
           `20` = ifelse(`20` == -1, `19`, `20`),
           `21` = ifelse(`21` == -1, `20`, `21`),
           `22` = ifelse(`22` == -1, `21`, `22`),
           `23` = ifelse(`23` == -1, `22`, `23`),
           `24` = ifelse(`24` == -1, `23`, `24`),
           `25` = ifelse(`25` == -1, `24`, `25`)) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) 
  
y1 <- 2000

  cn_s <- format_SAM(cn, first_year = y1) #format_SAM(., add_ages = list(`1`=0.001,`2`=0.001)) # could do this here or above
  cw_s <- format_SAM(cw, first_year = y1) #format_SAM(., add_ages = list(`1`=0,`2`=0)) #could do this here or above
#  smb_n_s <- format_SAM(smb_n, first_year = 1985, smb = TRUE)
#  smb_b_s <- format_SAM(smb_b, first_year = 1985, smb  = TRUE)
  smh_n_s <- format_SAM(smh_n, first_year = y1, smh = TRUE)
  smh_b_s <- format_SAM(smh_b, first_year = y1, smh = TRUE)
#  smb_sw_s <- format_SAM(smb_sw, first_year = 1985) #these do not need timing changes right?
  smh_sw_s <- format_SAM(smh_sw, first_year = y1,smh = TRUE)
  mat_s <- format_SAM(mat, first_year = y1)
  



