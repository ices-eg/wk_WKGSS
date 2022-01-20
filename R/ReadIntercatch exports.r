
# ==================================================================================================
# ReadIntercatch exports function
#
# 08/01/2016 Original coding by Hans Gerritsen (Ireland)
# 15/11/2019 slightly adapted by Martin Pastoors (comments etc; DPLYR)
# ==================================================================================================

datapath <- "//community.ices.dk/ExpertGroups/benchmarks/2020/wkdeep/2014 Meeting docs/06. Data/aru.27.5b6a/InterCatch/"

# get the name of the zipped file
zipfiles = list.files(path=datapath, pattern = ".zip", recursive=FALSE)

# zf <- zipfiles[1]
T1 <- data.frame(NULL, stringsAsFactors = FALSE)
T2 <- data.frame(NULL, stringsAsFactors = FALSE)

zf <- zipfiles[1]
fn <- unzip(file.path(datapath, zf), list = TRUE)$Name[4]

# unzip(file.path(datapath, files=zf), files="canum.txt" )
res <- read.table(unz(file.path(datapath, zf), "canum.txt"), header=FALSE, fill=TRUE)
res <- res[-7]
res2 <- stringr::str_replace_all(res, ",", "")
writeLines(res2, "canum.txt")
t <- FLCore::readVPAFile("canum.txt")

t <- readLines(file.path(getwd(), "canum.txt"))
t <- FLCore::readVPAFile(file.path(getwd(), "cn intercatch.dat"))




for (zf in zipfiles) {
  
  print(zf)
  
  # list all files names inside of a .zip file
  fnames = as.character(unzip(file.path(datapath, zf), list = TRUE)$Name)
  
  
  # f <- fnames[5]
  
  for (f in fnames) {
    
    if(grepl("CatchAndSampleDataTables.txt", f)) {
      
      print("CatchAndSampleDataTables.txt")
      # read every file into R, assuming they are .csv files
      res <- readLines(unz(file.path(datapath, zf), f))
      
      for (i in 1:length(res)) {
        
        if(grepl("TABLE 1", res[i])) {
          print(paste("TABLE 1 at line", i))
          t1 <- data.frame(NULL, stringsAsFactors = FALSE)
          j  <- 6
          repeat{
            j = j + 1
            # print(nchar(res[i+j]))
            if(nchar(res[i+j]) == 0) {
              break
            } else {
              t1 <- bind_rows(t1,data.frame(res[i+j], stringsAsFactors = FALSE))
            }
          }
          t1 <-
            t1 %>% 
            setNames("test") %>% 
            separate(test, into=c("stock","country","year","catchcategory","reportingcategory", "catonraisedorimported",
                                  "misreportedarea","area","season","seasontype","fleet","caton","effort","uniteffort",
                                  "officiallandings","sampledorestimated","sampledcatch","nlengthsamples",
                                  "nlengthmeasured", "nagesamples",	"nagereadings"), sep="\t") %>% 
            mutate_at(c("year","nlengthsamples", "nlengthmeasured", "nagesamples",	"nagereadings"), 
                      list(as.integer)) %>% 
            mutate_at(c("caton", "effort", "officiallandings","sampledcatch"), 
                      list(as.numeric))
        } # End of TABLE 1
        
        if(grepl("TABLE 2", res[i])) {
          print(paste("TABLE2 at line", i))
          t2 <- data.frame(NULL, stringsAsFactors = FALSE)
          j  <- 6
          repeat{
            j = j + 1
            # print(nchar(res[i+j]))
            if(nchar(res[i+j]) == 0 | is.na(nchar(res[i+j]))) {
              break
            } else {
              t2 <- bind_rows(t2,data.frame(res[i+j], stringsAsFactors = FALSE))
            }
          }
          t2 <-
            t2 %>% 
            setNames("test") %>% 
            separate(test, into=c("stock","country","year","catchcategory","reportingcategory", "catonraisedorimported",
                                  "misreportedarea","area","season","seasontype","fleet","caton","effort","uniteffort",
                                  "officiallandings","sampledorestimated","sex","ageorlength","ageorlengthtype",
                                  "canum", "weca",	"leca", "sampledcatch",  
                                  "nlengthsamples", "nlengthmeasured", "nagesamples",	"nagereadings"), sep="\t") %>% 
            mutate_at(c("year","ageorlength","nlengthsamples", "nlengthmeasured", "nagesamples",	"nagereadings"), 
                      list(as.integer)) %>% 
            mutate_at(c("caton", "effort", "canum", "weca", "leca", "sampledcatch"), 
                      list(as.numeric))
          
          
        } # End of TABLE 2
      } # End of i-loop
    } # End of CatchAndSampleDataTables.txt
    
    
    
  } # end of f-loop
  
  print(head(t1, 1))
  
  T1 <- bind_rows(T1, t1)
  T2 <- bind_rows(T2, t2)
}

T1 %>% 
  mutate(area = substr(T1$area,1,6)) %>% 
  group_by(year, country, catchcategory, area) %>% 
  summarize(caton = as.integer(sum(caton/1000, na.rm=TRUE))) %>% 
  reshape2::dcast(year+area+country ~ catchcategory, value.var="caton", sum, margins=c("area","country",  "catchcategory")) %>% 
  group_by(year) %>% 
  do(add_row(., .after=0)) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "right",
               missing=".",
               round=c(0))

T1 %>% 
  mutate(area = substr(T1$area,1,6)) %>% 
  group_by(year, country, catchcategory, area) %>% 
  summarize(caton = as.integer(sum(caton/1000, na.rm=TRUE))) %>% 
  reshape2::dcast(area+year+country ~ catchcategory, value.var="caton", sum, margins=c("year", "country", "catchcategory")) %>% 
  group_by(area) %>% 
  do(add_row(., .after=0)) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "right",
               missing=".",
               round=c(0))

T1 %>% 
  mutate(area = substr(T1$area,1,6)) %>% 
  mutate(country = case_when(
    country == "Denmark"       ~ "DK",
    country == "Faroe Islands" ~ "FO",
    country == "France"        ~ "FR",
    country == "Germany"       ~ "GER",
    country == "Ireland"       ~ "IRE",
    country == "Netherlands"   ~ "NLD",
    country == "Norway"        ~ "NOR",
    country == "Poland"        ~ "POL",
    country == "Russia"        ~ "RUS",
    country == "Spain"         ~ "ESP",
    country == "UK(Scotland)"  ~ "UKS",
    TRUE                       ~ country
  )) %>% 

  group_by(year, country, area) %>% 
  summarize(caton = as.integer(sum(caton/1000, na.rm=TRUE))) %>% 
  reshape2::dcast(area+year ~ country, value.var="caton", sum, margins=c("area", "year", "country")) %>% 
  group_by(area) %>% 
  do(add_row(., .after=0)) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "right",
               missing=".",
               round=c(0))

T1 %>% 
  mutate(area = substr(T1$area,1,6)) %>% 
  mutate(country = case_when(
    country == "Denmark"       ~ "DK",
    country == "Faroe Islands" ~ "FO",
    country == "France"        ~ "FR",
    country == "Germany"       ~ "GER",
    country == "Ireland"       ~ "IRE",
    country == "Netherlands"   ~ "NLD",
    country == "Norway"        ~ "NOR",
    country == "Poland"        ~ "POL",
    country == "Russia"        ~ "RUS",
    country == "Spain"         ~ "ESP",
    country == "UK(Scotland)"  ~ "UKS",
    TRUE                       ~ country
  )) %>% 
  filter(catchcategory == "Landings") %>%
  
  group_by(year, country, area) %>% 
  summarize(caton = as.integer(sum(caton/1000, na.rm=TRUE))) %>% 
  reshape2::dcast(area+year ~ country, value.var="caton", sum, margins=c("area", "year", "country")) %>% 
  group_by(area) %>% 
  do(add_row(., .after=0)) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "right",
               missing=".",
               round=c(0))

