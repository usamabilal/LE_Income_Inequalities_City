rm(list=ls())
library(data.table)
library(tidyverse)
library(readxl)
library(tidycensus)
library(lubridate)
library(bit64)
# life expectancy

le<-fread("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/US_A.CSV")
le<-le %>% 
  mutate(GEOID=as.numeric(`Tract ID`),
         le=as.numeric(`e(0)`)) %>% 
  select(GEOID, le)
states<-c(state.abb, "DC")

# MHI, population, and # households by census tract, from ACS using tidycensus

# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)
ct_data<-map_dfr(states, function(state){
  temp<-get_acs(geography = "tract",
                state=state,
                variables=c(
                  #pop
                  "B01001_001", 
                  # hh
                  "B08201_001",
                  # mhi
                  "B19013_001"),
                survey="acs5",year=2015) %>% 
    select(GEOID, variable, estimate) %>% 
    spread(variable, estimate) %>% 
    mutate(GEOID=as.numeric(GEOID)) %>% 
    rename(pop=B01001_001,
           hh=B08201_001,
           mhi=B19013_001) %>% 
    select(GEOID, pop, hh, mhi)
  temp
})


#obtain CBSA level census data
# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)


  cbsa<-get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                variables=c(
                  #pop
                  "B01001_001", 
                  # hh
                  "B08201_001",
                  # mhi
                  "B19013_001"),
                survey="acs5",year=2015) %>% 
    select(GEOID, variable, estimate) %>% 
    spread(variable, estimate) %>% 
    mutate(GEOID=as.numeric(GEOID)) %>% 
    rename(pop=B01001_001,
           hh=B08201_001,
           mhi=B19013_001) %>% 
    select(GEOID, pop, hh, mhi)
  cbsa

# 2013 delineations from https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cw<-read_excel("Data/list1.xls", skip=2) %>% 
  filter(grepl("Metropolitan", `Metropolitan/Micropolitan Statistical Area`)) %>% 
  rename(cbsa=`CBSA Code`,
         cbsa_name=`CBSA Title`) %>% 
  mutate(county=paste0(`FIPS State Code`,`FIPS County Code`),
         county=as.numeric(county),
         state=floor(county/1000),
         cbsa=as.numeric(cbsa)) %>% 
  select(state, county, cbsa, cbsa_name)

# remove CBSAs with any county in ME=23 WI=55 HI=15 AK = 2; + PR (72)
exclude<-cw %>% filter(state%in%c(23, 55)) %>% pull(cbsa)
exclude_contig<-cw%>%filter(state%in%c(2,15,72))%>%pull(cbsa)
cw<-cw %>% filter(!cbsa%in%exclude_contig)
cw<-cw%>%filter(!cbsa%in%exclude)
# plus remove  Bedford city (all go to bedford county)
cw<-cw %>% filter(county!=51515) %>% select(-state)

#number of CBSA (for counts of # removed in WI & ME)
df_uniq <- unique(cw$cbsa)
length(df_uniq)

ct_data<-ct_data %>% 
  mutate(county=floor(GEOID/1000000),
         state=floor(county/1000))

# get census regions
region<-read_excel("data/state-geocodes-v2014-1.xls", skip=5)
names<-region %>% filter(Division=="0") %>% select(Region, Name) %>% rename(Region_Name=Name)
region<-region %>% filter(`State\n(FIPS)`!="00") %>% 
  mutate(state=as.numeric(`State\n(FIPS)`)) %>% 
  select(state, Region)
region<-full_join(ct_data, region) %>% 
  left_join(cw) %>% 
  filter(!is.na(cbsa)) %>% 
  group_by(cbsa, Region) %>% 
  summarise(pop=sum(pop)) %>% 
  arrange(cbsa, desc(pop)) %>%  
  filter(!duplicated(cbsa)) %>% 
  select(cbsa, Region) %>% left_join(names)
  

dta<-left_join(le, ct_data) %>% full_join(cw) %>% left_join(region)
# remove all CTs not belonging to a cbsa
dta<-dta %>% filter(!is.na(cbsa))
# 1 county in ND with is part of the CBSA for Billings, MT. County is tiny so its only census tract is not part USALEEP
dta %>% filter(is.na(le))
dta<-dta %>% filter(!is.na(le))
# a total of 6 census tracts with missing income. 
dta %>% filter(is.na(mhi))
dta<-dta %>% filter(!is.na(mhi))
summary(dta)


#number of CT 
ct_uniq<-unique(dta$GEOID)
length(ct_uniq)
#number of MSA
msa_uniq <- unique(dta$cbsa)
 length(msa_uniq)
 
save(dta, ct_data, le, cw, region, cbsa, file="data/clean_data.rdata")

