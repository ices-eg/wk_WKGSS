library(mar)
mar <- connect_mar()


lesa_kvarnir(mar) %>% 
  filter(tegund==19, !is.na(aldur)) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(ar < 1990) %>% 
  group_by(ar, synaflokkur, aldur) %>% 
  count() %>% 
  collect(n=Inf) %>% 
  arrange(ar) %>% 
  left_join(lesa_kvarnir(mar) %>% 
              filter(tegund==19, !is.na(aldur)) %>% 
              left_join(lesa_stodvar(mar)) %>% 
              filter(ar < 1990) %>% 
              select(ar, synaflokkur, leidangur,skip,veidarfaeri, stod) %>%
              distinct %>% 
              group_by(ar, synaflokkur) %>% 
              count %>% 
              rename(Nstod = n) %>% 
              collect(n=Inf)) %>% 
  arrange(ar) %>% 
  filter(Nstod > 2) %>% 
#  filter(Nstod > 2, dypi_kastad > 400 | dypi_hift > 400) %>% 
  rename(Year = ar, Age = aldur, N = n) %>% 
  ggplot()+
  geom_col(aes(x = Age, y = N)) + 
  facet_wrap(~Year + synaflokkur, scales = 'free_y')

CCs <-
lesa_kvarnir(mar) %>% 
  filter(tegund==19, !is.na(aldur)) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(ar < 1996 | (ar %in% c(1996:2000) & synaflokkur==35)) %>% 
  group_by(ar, synaflokkur, aldur) %>% 
  count() %>% 
  collect(n=Inf) %>% 
  arrange(ar) %>% 
  left_join(lesa_kvarnir(mar) %>% 
              filter(tegund==19, !is.na(aldur)) %>% 
              left_join(lesa_stodvar(mar)) %>% 
              filter(ar < 1996| (ar %in% c(1996:2000) & synaflokkur==35)) %>% 
              select(ar, synaflokkur, leidangur,skip,veidarfaeri, stod) %>%
              distinct %>% 
              group_by(ar, synaflokkur) %>% 
              count %>% 
              rename(Nstod = n) %>% 
              collect(n=Inf)) %>% 
  arrange(ar) %>% 
  filter(Nstod > 2, !(ar %in% c(1961, 1963,1975,1976, 1978, 1979, 1980, 1985, 1989, 1990:1995) & synaflokkur!=30)) %>% 
  #  filter(Nstod > 2, dypi_kastad > 400 | dypi_hift > 400) %>% 
  rename(Year = ar, Age = aldur, N = n) %>% 
  #unite(YS, Year, synaflokkur) %>% 
  mutate(logN = log(N)) %>% 
  ggplot()+
  geom_point(aes(x = Age, y = logN)) + 
  geom_vline(xintercept = 24) +
  geom_vline(xintercept = 15) +
  facet_wrap(~Year + synaflokkur, scales = 'free_y')

lesa_kvarnir(mar) %>% 
  filter(tegund==19, !is.na(aldur)) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(ar < 1996 | (ar %in% c(1996:2000) & synaflokkur==35)) %>% 
  group_by(ar, synaflokkur,aldur) %>% 
  count() %>% 
  collect(n=Inf) %>% 
  arrange(ar) %>% 
  left_join(lesa_kvarnir(mar) %>% 
              filter(tegund==19, !is.na(aldur)) %>% 
              left_join(lesa_stodvar(mar)) %>% 
              filter(ar < 1996| (ar %in% c(1996:2000) & synaflokkur==35)) %>% 
              select(ar, synaflokkur, leidangur,skip,veidarfaeri, stod) %>%
              distinct %>% 
              group_by(ar, synaflokkur) %>% 
              count %>% 
              rename(Nstod = n) %>% 
              collect(n=Inf)) %>% 
  arrange(ar) %>% 
  filter(Nstod > 2, !(ar %in% c(1961, 1963,1975,1978, 1979, 1980, 1985, 1989, 1990:1995) & synaflokkur!=30)) %>% 
  #  filter(Nstod > 2, dypi_kastad > 400 | dypi_hift > 400) %>% 
  rename(Year = ar, Age = aldur, N = n) %>% 
  #unite(YS, Year, synaflokkur) %>% 
  mutate(logN = log(N)) %>% 
  ggplot()+
  geom_col(aes(x = Age, y = N)) + 
  geom_vline(xintercept = 24) +
  geom_vline(xintercept = 15) +
  facet_wrap(~Year + synaflokkur, scales = 'free_y')

Zs <-
  lesa_kvarnir(mar) %>% 
  filter(tegund==19, !is.na(aldur)) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(ar < 1990) %>% 
  group_by(ar, synaflokkur, aldur) %>% 
  count() %>% 
  collect(n=Inf) %>% 
  arrange(ar) %>% 
  left_join(lesa_kvarnir(mar) %>% 
              filter(tegund==19, !is.na(aldur)) %>% 
              left_join(lesa_stodvar(mar)) %>% 
              filter(ar < 1990) %>% 
              select(ar, synaflokkur, leidangur,skip,veidarfaeri, stod) %>%
              distinct %>% 
              group_by(ar, synaflokkur) %>% 
              count %>% 
              rename(Nstod = n) %>% 
              collect(n=Inf)) %>% 
  arrange(ar) %>% 
  filter(Nstod > 2, !(ar %in% c(1961, 1963, 1975,1976, 1978, 1979, 1980, 1985, 1989))) %>% 
  #  filter(Nstod > 2, dypi_kastad > 400 | dypi_hift > 400) %>% 
  rename(Year = ar, Age = aldur, N = n) %>% 
  unite(YS, Year, synaflokkur, remove = FALSE) %>% 
  mutate(logN = log(N)) %>% 
  filter((Age > 14 & Age < 25 ) ) %>% 
  split(., .$YS) %>% #.[[1]]->x
  purrr::map(function(x){
    mod <- glm(logN~Age, data = x)
    y <-
      coef(mod) %>% 
      t() %>% 
      as.data.frame() 
  }) %>% 
  bind_rows(.,.id='Year')%>% 
  filter(!grepl('_30', Year))

png(paste0('R/biol_figs_output/Zs.png'), height = 300, width = 300)
  hist(Zs$Age , main = '', xlab = 'Z')
dev.off()

png(paste0('R/biol_figs_output/CCs.png'), height = 650, width = 650)
print(CCs)
dev.off()

  
write.csv(Zs, 'R/biol_figs_output/Zs.csv')


lesa_kvarnir(mar) %>% 
  filter(tegund==19, !is.na(aldur)) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(ar < 1990) %>% 
  group_by(ar, synaflokkur, leidangur,skip,veidarfaeri,stod) %>% 
  count() %>% 
  collect(n=Inf) %>% 
  arrange(ar) %>% 
  left_join(lesa_kvarnir(mar) %>% 
               filter(tegund==19, !is.na(aldur)) %>% 
               left_join(lesa_stodvar(mar)) %>% 
               filter(ar < 1990) %>% 
               select(ar, synaflokkur, leidangur,skip,veidarfaeri, stod) %>%
               distinct %>% 
               group_by(ar, synaflokkur, leidangur,skip,veidarfaeri) %>% 
               count %>% 
               rename(Nstod = n) %>% 
               collect(n=Inf)) %>% 
  arrange(ar) %>% 
  filter(Nstod > 2) %>% 
  View

#A50 maturity
dat <- lesa_kvarnir(mar) %>% 
  filter(tegund==19, !is.na(aldur), !is.na(kynthroski)) %>% 
  left_join(lesa_stodvar(mar)) %>% 
  filter(synaflokkur==35) %>% 
  mutate(maturity = ifelse(kynthroski==1, 0,
                           ifelse(kynthroski %in% c(2:4), 1,NA))) %>% 
  filter(!is.na(maturity)) %>% 
  select(maturity, age = aldur, year = ar) %>%
  collect(n=Inf)
  
  Amat_coef <- 
    dat %>% 
    split(., .$year) %>% #.[[1]]->x
    purrr::map(function(x){
  mod <-  glm(maturity~age, data=x , family=binomial(link=logit))
  
  y <-
    coef(mod) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(L50 =  - coef(mod)[1]/coef(mod)[2])
  
  return(y)
    
    }) %>% 
    bind_rows(., .id = 'year')
  
  
  lesa_kvarnir(mar) %>% 
    filter(tegund==19, !is.na(aldur)) %>% 
    filter(aldur > 30) %>% 
    group_by(aldur) %>% 
    count
  lesa_kvarnir(mar) %>% 
    filter(tegund==19, !is.na(aldur)) %>% 
    filter(aldur > 32) %>%
    left_join(lesa_stodvar(mar)) %>% 
    select(ar, synaflokkur)
      