#### For environmental variables #######
library(tidycensus)
library(tidyverse)
library(housingData)

setwd("~/Projects/ImportationRisk/mobilityRelatedImpRisk")

pop <- get_acs(
  geography = "county", 
  variables = c("B01001_001","B01001_002","B01001_026"), 
  year = 2022,
  output = 'wide'
)

pop2022<-pop %>% dplyr::select(GEOID,COUNTY=NAME,"B01001_001E","B01001_002E","B01001_026E") %>%
  rename_at(c("B01001_001E","B01001_002E","B01001_026E"),~c("POP_TOT","MALE_POP","FEMALE_POP"))

GDP <- get_acs(
  geography = "county", 
  variables = "B19301_001", 
  year = 2022,
  output = 'wide'
)

gdp2022<-GDP %>% rename_at(c("NAME","B19301_001E"),~c("COUNTY","GDP")) %>%
  dplyr::select(-B19301_001M)

degrees <- get_acs(
  geography = "county", 
  variables = c("B15003_001","B15003_022","B15003_023","B15003_024","B15003_025"), 
  year = 2022,
  output = 'wide'
)

degrees2022<-degrees %>% dplyr::select(contains("E")) %>%
  rename_at(c("NAME","B15003_001E","B15003_022E","B15003_023E","B15003_024E","B15003_025E"),
            ~c("COUNTY","DEGREE_TOT","BACHELOR","MASTER","PROFFE","DOCTOR"))

#Variables for population without insurance
varsNoInsurance<-c("B27001_001","B27001_005","B27001_008","B27001_011","B27001_014","B27001_017",
                   "B27001_020","B27001_023","B27001_026","B27001_029","B27001_033",
                   "B27001_036","B27001_039","B27001_042","B27001_045","B27001_048",
                   "B27001_051","B27001_054","B27001_057")

noInsurance <- get_acs(
  geography = "county", 
  variables = varsNoInsurance, 
  year = 2022,
  output = 'wide'
)

noInsurance2022<-noInsurance %>% dplyr::select(contains("E")) %>%
  mutate(MALE_INS=B27001_005E+B27001_008E+B27001_011E+B27001_014E+B27001_017E+B27001_020E+B27001_023E+B27001_026E+B27001_029E,
         FEMALE_INS=B27001_033E+B27001_036E+B27001_039E+B27001_042E+B27001_045E+B27001_048E+B27001_051E+B27001_054E+B27001_057E,
         BOTH_INS=MALE_INS+FEMALE_INS)%>%
  dplyr::select(GEOID,COUNTY=NAME,TOT_NO_INS=B27001_001E,contains("INS"))

poverty <- get_acs(
  geography = "county", 
  variables = c("B17001_002","B17001_003","B17001_017"), 
  year = 2022,
  output = 'wide'
)

poverty2022<-poverty %>% dplyr::select(GEOID,COUNTY=NAME,TOT_POV=B17001_002E,MALE_POV=B17001_003E,FEMALE_POV=B17001_017E)

commOtherMeans <- get_acs(
  geography = "county", 
  variables = c("B08301_001","B08301_020"), 
  year = 2022,
  output = 'wide',
  geometry=TRUE
)

commOtherMeans2022<-commOtherMeans %>% dplyr::select(GEOID,COUNTY=NAME,TOT_COMM=B08301_001E,OTHER_COMM=B08301_020E,geometry)

variables2022<-pop2022 %>% left_join(gdp2022) %>% left_join(degrees2022) %>% left_join(noInsurance2022) %>%
  left_join(poverty2022) %>% left_join(commOtherMeans2022) %>%
  dplyr::select(-geometry)

alaska<-variables2022 %>% as_tibble() %>% 
  filter(stringr::str_detect(COUNTY, 'Alaska') ) %>% pull(GEOID)
hawaii<-variables2022 %>% as_tibble() %>% 
  filter(stringr::str_detect(COUNTY, 'Hawaii') ) %>% pull(GEOID)
puertoR<-variables2022 %>% as_tibble() %>% 
  filter(stringr::str_detect(COUNTY, 'Puerto Rico') ) %>% pull(GEOID)

variables2022 <- variables2022 %>% filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  arrange(GEOID,COUNTY)

write_csv(variables2022,file="variables2022.csv")
