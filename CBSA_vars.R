#Life Expectancy by CD Univariate analysis additions


#load crosswalks 
#cbsa to county (only those in the LEEP dataset)
load('data/cbsa_county.Rdata')


#load the ct to county
load('data/ct_county.Rdata')

#join the ct to county and cbsa to county + removing ct's w/o cbsa/ct in our dataset
cbsa_ct<-ct_county%>%full_join(cbsa_county)%>% 
  filter(!is.na(cbsa))%>%
  filter(GEOID%in%dta$GEOID)


#######################################################################
#######################################################################
###
### CONTEXTUAL VARIABLES: ACS DATA 2012-2016, FOR MODELS
###
#######################################################################

# VARIABLES WE WANT:
# - % NH BLACK 
# - % HISPANIC
# - % FOREIGN BORN
# - % AGED 25+ WITH COLLEGE EDUCATION
# - % AGED <65 WITHOUT INSURANCE
# - % POVERTY
# -% Unemployed
# - % Severe housing burdened
#-index of dissimilarity

v17 <- load_variables(2015, "acs5", cache = TRUE)

# ACS VARIABLES TO DOWNLOAD
#   - B01001_001: TOTAL
#   - B01001_003-B01001_006: 
#       MALE: UNDER 5 YEARS, 5 TO 9 YEARS, 10 TO 14 YEARS, 15 TO 17 YEARS
#   - B01001_020-B01001_025: 
#       MALE: 65 AND 66 YEARS, 67 TO 69 YEARS, 70 TO 74 YEARS, 75 TO 79 YEARS,
#       80 TO 84 YEARS, 85 YEARS AND OVER
#   - B01001_027-B01001_030:
#       FEMALE: UNDER 5 YEARS, 5 TO 9 YEARS, 10 TO 14 YEARS, 15 TO 17 YEARS
#   - B01001_044-B01001_049:
#       FEMALE: 65 AND 66 YEARS, 67 TO 69 YEARS, 70 TO 74 YEARS, 75 TO 79 YEARS,
#       80 TO 84 YEARS, 85 YEARS AND OVER
vars_b01001 <- paste0("B01001_0", sprintf("%02d", c(1, 3:6, 20:25, 27:30, 44:49)))
# - TABLE B03002: HISPANIC OR LATINO ORIGIN BY RACE
#   - B03002_001 : TOTAL
#   - B03002_004 : NOT HISPANIC OR LATINO: BLACK OR AFRICAN AMERICAN ALONE
#   - B03002_012 : HISPANIC OR LATINO
vars_b03002 <- paste0("B03002_0", sprintf("%02d", c(1, 4, 12)))
# - TABLE B05012: NATIVITY IN THE UNITED STATES
#   - B05012_001: TOTAL
#   - B05012_003: FOREIGN-BORN
vars_b05012 <- paste0("B05012_0", sprintf("%02d", c(1, 3)))
# - TABLE B15002: SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 
#   25 YEARS AND OVER
#   - B15002_001: TOTAL
#   - B15002_015: MALE: BACHELOR'S DEGREE
#   - B15002_016: MALE: MASTER'S DEGREE
#   - B15002_017: MALE: PROFESSIONAL SCHOOL DEGREE
#   - B15002_018: MALE: DOCTORATE DEGREE
#   - B15002_032: FEMALE: BACHELOR'S DEGREE
#   - B15002_033: FEMALE: MASTER'S DEGREE
#   - B15002_034: FEMALE: PROFESSIONAL SCHOOL DEGREE
#   - B15002_035: FEMALE: DOCTORATE DEGREE
vars_b15002 <- paste0("B15002_0", sprintf("%02d", c(1, 15:18, 32:35)))
# - TABLE B17001: POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
#   - B17001_001: TOTAL
#   - B17001_002: INCOME IN THE PAST 12 MONTHS BELOW POVERTY LEVEL
vars_b17001 <- paste0("B17001_00", c(1:2))
# - TABLE B27001: HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE
#   - B27001_003 : MALE: UNDER 6 YEARS
#   - B27001_006 : MALE: 6 TO 18 YEARS
#   - B27001_009 : MALE: 19 TO 25 YEARS
#   - B27001_012 : MALE: 26 TO 34 YEARS
#   - B27001_015 : MALE: 35 TO 44 YEARS
#   - B27001_018 : MALE: 45 TO 54 YEARS
#   - B27001_021 : MALE: 55 TO 64 YEARS
#   - B27001_031 : FEMALE: UNDER 6 YEARS
#   - B27001_034 : FEMALE: 6 TO 18 YEARS
#   - B27001_037 : FEMALE: 19 TO 25 YEARS
#   - B27001_040 : FEMALE: 26 TO 34 YEARS
#   - B27001_043 : FEMALE: 35 TO 44 YEARS
#   - B27001_046 : FEMALE: 45 TO 54 YEARS
#   - B27001_049 : FEMALE: 55 TO 64 YEARS
#   - B27001_005 : MALE: UNDER 6 YEARS: NO HEALTH INSURANCE
#   - B27001_008 : MALE: 6 TO 18 YEARS: NO HEALTH INSURANCE
#   - B27001_011 : MALE: 19 TO 25 YEARS: NO HEALTH INSURANCE
#   - B27001_014 : MALE: 26 TO 34 6 YEARS: NO HEALTH INSURANCE
#   - B27001_017 : MALE: 35 TO 44 YEARS: NO HEALTH INSURANCE
#   - B27001_020 : MALE: 45 TO 54 YEARS: NO HEALTH INSURANCE
#   - B27001_023 : MALE: 55 TO 64 YEARS: NO HEALTH INSURANCE
#   - B27001_033 : FEMALE: UNDER 6 YEARS: NO HEALTH INSURANCE
#   - B27001_036 : FEMALE: 6 TO 18 YEARS: NO HEALTH INSURANCE
#   - B27001_039 : FEMALE: 19 TO 25 YEARS: NO HEALTH INSURANCE
#   - B27001_042 : FEMALE: 26 TO 34 YEARS: NO HEALTH INSURANCE
#   - B27001_045 : FEMALE: 35 TO 44 YEARS: NO HEALTH INSURANCE
#   - B27001_048 : FEMALE: 45 TO 54 YEARS: NO HEALTH INSURANCE
#   - B27001_051 : FEMALE: 55 TO 64 YEARS: NO HEALTH INSURANCE
vars_b27001 <- paste0("B27001_0", 
                      sprintf("%02d", c(seq(3, 21, by = 3),
                                        seq(31, 49, by = 3),
                                        seq(5, 23, by = 3),
                                        seq(33, 51, by = 3))))
# TABLE B21506: TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
#     -B25106_001 :Total houses
#     -B25106_006 :Estimate!!Total!!Owner-occupied housing units!!Less than $20,000!!30 percent or more
#     -B25106_010 :Estimate!!Total!!Owner-occupied housing units!!$20,000 to $34,999!!30 percent or more
#     -B25106_014 :Estimate!!Total!!Owner-occupied housing units!!$35,000 to $49,999!!30 percent or more
#     -B25106_018 :Estimate!!Total!!Owner-occupied housing units!!$50,000 to $74,999!!30 percent or more
#     -B25106_022 :Estimate!!Total!!Owner-occupied housing units!!$75,000 or more!!30 percent or more
#     -B25106_028 :Estimate!!Total!!Renter-occupied housing units!!Less than $20,000!!30 percent or more
#     -B25106_032 :Estimate!!Total!!Renter-occupied housing units!!$20,000 to $34,999!!30 percent or more
#     -B25106_036 :Estimate!!Total!!Renter-occupied housing units!!$35,000 to $49,999!!30 percent or more
#     -B25106_040 :Estimate!!Total!!Renter-occupied housing units!!$50,000 to $74,999!!30 percent or more
#     -B25106_044 :Estimate!!Total!!Renter-occupied housing units!!$75,000 or more!!30 percent or more
vars_b25106 <- paste0("B25106_0", 
                      sprintf("%02d", c(1, 6, 10, 14, 18, 22, 28, 32, 36, 40, 44)))

## DOWNLOAD RAW CBSA LEVEL ACS DATA
acs_context_raw <- 
  get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
          survey = "acs5",
          year = 2015,
          variables = c(vars_b01001,
                        vars_b03002,
                        vars_b05012,
                        vars_b15002,
                        vars_b17001,
                        vars_b27001, 
                        vars_b25106))
acs_context <- as.data.frame(acs_context_raw)
acs_context <- reshape2::dcast(acs_context[,c("GEOID","variable","estimate")],
                                    formula = GEOID ~ variable,
                                    value.var = "estimate")
length(unique(cbsa_county$cbsa)) # [1] 377

acs_context<-acs_context%>%
  filter(GEOID%in%cbsa_county$cbsa)

length(unique(acs_context$cbsa)) # [1] 377

nrow(acs_context) # [1] 377

# CALCULATIONS
acs_context1 <- acs_context %>%
  dplyr::mutate(p_age_lt18 = 
                  ((B01001_003 + B01001_004 + B01001_005 + B01001_006 +
                      B01001_027 + B01001_028 + B01001_029 + B01001_030)/
                     B01001_001),
                p_age_ge65 = 
                  ((B01001_020 + B01001_021 + B01001_022 + B01001_023 + 
                      B01001_024 + B01001_025 + B01001_044 + B01001_045 + 
                      B01001_046 +  B01001_048 + B01001_049)/ 
                     B01001_001),
                p_nhblack = B03002_004/B03002_001,
                p_hispanic = B03002_012/B03002_001,
                p_foreignb = B05012_003/B05012_001,
                p_college = 
                  ((B15002_015 + B15002_016 + B15002_017 +  B15002_018 + 
                      B15002_032 + B15002_033 + B15002_034 + B15002_035 )/
                     B15002_001),
                p_poverty = B17001_002/B17001_001,
                p_noinsur = 
                  ((B27001_005 + B27001_008 + B27001_011 + B27001_014 + 
                      B27001_017 + B27001_020 + B27001_023 + B27001_033 + 
                      B27001_036 + B27001_039 + B27001_042 + B27001_045 + 
                      B27001_048 + B27001_051)/
                     (B27001_003 + B27001_006 + B27001_009 + B27001_012 + 
                        B27001_015 + B27001_018 + B27001_021 + B27001_031 + 
                        B27001_034 + B27001_037 + B27001_040 + B27001_043 + 
                        B27001_046 + B27001_049)), 
              #including both renters and homeowners paying more than 30% =housing burdened
                p_house_burd=
                 ((B25106_006 + B25106_010 + B25106_014 + B25106_018 +
                      B25106_022 + B25106_028 + B25106_032 + B25106_036 +
                      B25106_040 + B25106_044 )/B25106_001)
  )%>%
  select(GEOID,p_age_lt18, p_age_ge65, 
         p_nhblack, p_hispanic, p_foreignb, p_college, 
         p_poverty, p_noinsur, p_house_burd) %>%
  as.data.frame()%>%
  rename(cbsa='GEOID')
 


# SAVE
save(acs_context,
     file ='data/census_context.Rdata')



#######################################################################
#######################################################################
###
### CONTEXTUAL VARIABLES: UNEMPLOYMENT, BUREAU OF LABOR STAT. (AGG. 2012-2016)
###
#######################################################################
library(readxl)
bls_raw <- lapply(c(10:15), function(this_year_char){
  this_url <- paste0("https://www.bls.gov/lau/laucnty",this_year_char,".xlsx")
  bls_raw_colnames <- 
    rio::import(file = this_url,
                format = "xlsx",
                skip = 2, col_names = F, n_max = 3)
  bls_raw_colnames <- t(bls_raw_colnames)
  bls_raw_colnames[is.na(bls_raw_colnames)] <- ""
  bls_raw_colnames <- as.data.frame(bls_raw_colnames)
  colnames(bls_raw_colnames) <- paste0("col_", 1:ncol(bls_raw_colnames))
  bls_raw_colnames$col_4 <- paste0(bls_raw_colnames$col_1,
                                   bls_raw_colnames$col_2,
                                   bls_raw_colnames$col_3)
  bls_raw_colnames$col_4 <- gsub(bls_raw_colnames$col_4,
                                 pattern = "[[:space:]]|[[:punct:]]",
                                 replacement = "")
  bls_raw_colnames <- bls_raw_colnames$col_4
  
  bls_raw_data <- 
    rio::import(file = this_url,
                format = "xlsx",
                skip = 6, col_names = F,
                col_types = c(rep("text", times = 6),
                              rep("numeric", times = 4)))
  colnames(bls_raw_data) <- bls_raw_colnames
  bls_raw_data$fips <- paste0(bls_raw_data$StateFIPSCode,
                              bls_raw_data$CountyFIPSCode)
  bls_raw_data <- bls_raw_data[,c("fips", "Year", "LaborForce", "Unemployed")]
  return(bls_raw_data)
})
bls_raw1 <- do.call(rbind, bls_raw)%>%
  filter(Year%in%(201:2015))%>%
  rename(county='fips')%>%
  mutate(county=as.numeric(county))


#previous checks on how this data aligns with cbsa crosswalk- looked good 

# MERGE unemployment DATA to cbsa by COUNTY FIPS
bls_summed <- merge(bls_raw1,
                    cbsa_county, 
                    by='county')%>%
  filter(cbsa%in%cbsa_county$cbsa)

# CALCULATE CBSA AVERAGES
unemployed_cbsa <- bls_summed %>%
  dplyr::group_by(cbsa) %>%
  dplyr::summarize(labor_force = sum(LaborForce, na.rm = T),
                   unemployed = sum(Unemployed, na.rm = T)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(pct_unemployed = (unemployed /labor_force)) %>%
  as.data.frame()%>%
  select(cbsa, pct_unemployed)


## SAVE
save(unemployed_cbsa,
     file = 'data/unemployed_cbsa')


################################################################################
###DISSIMILARITY INDEX
###
###############################################################################

#####  Pull tract data  #####
## We need tract level 

us <- unique(fips_codes$state)[1:51]

tract_acs_data_raw = get_acs(geography = "tract",
                      variables=c(
                        ## Poplation Denom
                        pop="B03002_001",
                        ## Population NH-Black
                        nhb="B03002_004",
                        ## Population Hispanic
                        hisp="B03002_012",
                        ## Population NH-White
                        nhw="B03002_003"), survey="acs5", year=2015, state=us)%>%
   select(GEOID, variable, estimate)  %>%
   distinct() %>%
   pivot_wider(names_from = variable, values_from = estimate)


tract_acs<-tract_acs_data_raw%>%
  mutate(tract=as.numeric(GEOID),
        county=floor(tract/1000000),
         state=floor(county/1000))

#combine the tract w/ cbsa data to allow aggreagation by CBSA 
#filter out any tracts not in cbsa's list (also drops out cities not included in LEEP)
tract_cbsa<-tract_acs%>%full_join(cw)%>%
  filter(!is.na(cbsa))

dissimilarity_index <-tract_cbsa%>% 
  arrange(cbsa, tract) %>% 
  select(cbsa, tract,
         pop, nhb, nhw, hisp) %>% 
  group_by(cbsa) %>% 
  mutate(NHW = sum(nhw),
         NHB = sum(nhb),
         HISP = sum(hisp)) %>% 
  ungroup() %>% 
  mutate(nhw_NHW = nhw/NHW,
         nhb_NHB = nhb/NHB,
         hisp_HISP = hisp/HISP) %>% 
  rowwise() %>% 
  mutate(abs_diff_nhw_nhb = abs(nhw_NHW-nhb_NHB),
         abs_diff_nhw_hisp = abs(nhw_NHW-hisp_HISP)) %>% 
  ungroup() %>% 
  group_by(cbsa) %>% 
  summarize(DI_nhb_nhw = 0.5*sum(abs_diff_nhw_nhb),
            DI_hisp_nhw = 0.5*sum(abs_diff_nhw_hisp)) %>% 
  ungroup()

###############################################################################
###
### MERGE ALL CONTEXTUAL DATA TOGETHER
###
##############################################################################

univariate <- merge(acs_context1,
                    unemployed_cbsa, 
                    by = c("cbsa"),
                    all = T)
univariate <- merge(univariate,
                    dissimilarity_index,
                    by = c("cbsa"),
                    all = T)


# SAVE THE FULL UNIVARIATE DATASET
save(univariate,
    file='data/univariate.Rdata')












## NON-RESCALED DATA
contextual_1216_orig <- merge(acs_1216_context,
                              ahrf_pcp,
                              by = c("cbsa", "cbsa_name"),
                              all = T)
contextual_1216_orig <- merge(contextual_1216_orig,
                              bls_unemployed,
                              by = c("cbsa", "cbsa_name"),
                              all = T)
# SAVE THE FULL ORIGINAL CONTEXTUAL DATA SET
save(contextual_1216_orig,
     file = paste0(path_wd_savedRdata, 
                   "contextual_data/contextual_1216_orig_full_2022_01_21.Rdata"))

####################
# CENTER AND RESCALE BY 1SD
contextual_1216_cent1sd <- 
  reshape2::melt(contextual_1216_orig, 
                 id.vars = c("cbsa","cbsa_name","mmsa"),
                 measure.vars = c("p_age_lt18", "p_age_ge65","p_nhblack",
                                  "p_hispanic","p_foreignb","p_college",
                                  "p_poverty","gini","p_noinsur",
                                  "pcp_rate_100k","p_unemployed"),
                 variable_name = "variable",
                 value.name = "orig_value")
contextual_1216_cent1sd <- contextual_1216_cent1sd %>%
  group_by(variable) %>%
  dplyr::mutate(value_centered = orig_value - mean(orig_value),
                value_cent1sd = value_centered/sd(value_centered)) %>%
  ungroup() %>% as.data.frame()
contextual_1216_cent1sd <- 
  reshape2::dcast(data = contextual_1216_cent1sd,
                  formula = cbsa + cbsa_name + mmsa ~ variable,
                  value.var = "value_cent1sd")
summary(contextual_1216_cent1sd)
save(contextual_1216_cent1sd,
     file = paste0(path_wd_savedRdata, 
                   "contextual_data/contextual_1216_cent1sd_2022_01_24.Rdata"))


############################################

# LOG SCALE DATA - ALL VARIABLES
contextual_1216_log <- 
  reshape2::melt(contextual_1216_orig, 
                 id.vars = c("cbsa","cbsa_name","mmsa"),
                 measure.vars = c("p_age_lt18", "p_age_ge65","p_nhblack",
                                  "p_hispanic","p_foreignb","p_college",
                                  "p_poverty","gini","p_noinsur",
                                  "pcp_rate_100k","p_unemployed"),
                 variable_name = "variable",
                 value.name = "orig_value")
contextual_1216_log <- contextual_1216_log %>%
  dplyr::mutate(value_log = log(orig_value)) %>%
  group_by(variable) %>%
  dplyr::mutate(value_log_rescale = ((value_log - min(value_log))/
                                       (max(value_log)- min(value_log)))*2 - 1) %>%
  ungroup() %>% as.data.frame()
contextual_1216_log_rescale <- 
  reshape2::dcast(data = contextual_1216_log,
                  formula = cbsa + cbsa_name + mmsa ~ variable,
                  value.var = "value_log_rescale")
contextual_1216_log <- 
  reshape2::dcast(data = contextual_1216_log,
                  formula = cbsa + cbsa_name + mmsa ~ variable,
                  value.var = "value_log")
save(contextual_1216_log,
     file = paste0(path_wd_savedRdata, 
                   "contextual_data/contextual_1216_log_2022_01_24.Rdata"))
save(contextual_1216_rescale,
     file = paste0(path_wd_savedRdata, 
                   "contextual_data/contextual_1216_log_rescale_2022_01_24.Rdata"))


# CENTER AND RESCALE LOG TRANSFORMED DATA BY 1SD
#contextual_1216_log,
load(file = paste0(path_wd_savedRdata, 
                   "contextual_data/contextual_1216_log_2022_01_24.Rdata"))
contextual_1216_log_cent1sd <- 
  reshape2::melt(contextual_1216_log, 
                 id.vars = c("cbsa","cbsa_name","mmsa"),
                 measure.vars = c("p_age_lt18", "p_age_ge65","p_nhblack",
                                  "p_hispanic","p_foreignb","p_college",
                                  "p_poverty","gini","p_noinsur",
                                  "pcp_rate_100k","p_unemployed"),
                 variable_name = "variable",
                 value.name = "orig_value")
contextual_1216_log_cent1sd <- contextual_1216_log_cent1sd %>%
  group_by(variable) %>%
  dplyr::mutate(value_centered = orig_value - mean(orig_value),
                value_cent1sd = value_centered/sd(value_centered)) %>%
  ungroup() %>% as.data.frame()
contextual_1216_log_cent1sd <- 
  reshape2::dcast(data = contextual_1216_log_cent1sd,
                  formula = cbsa + cbsa_name + mmsa ~ variable,
                  value.var = "value_cent1sd")
summary(contextual_1216_log_cent1sd)
save(contextual_1216_log_cent1sd,
     file = paste0(path_wd_savedRdata, 
                   "contextual_data/contextual_1216_log_cent1sd_2022_01_27.Rdata"))

