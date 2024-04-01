library(tidyverse)
library(tidycensus)
library(housingData)
library(cowplot)

remove(list = ls())
getwd()
#Reading files with Malaria cases by county in the USA
malaria2016<-read_csv("Number_of_Reported_Malaria_Cases_by_County__United_States__2016.csv")
malaria2017<-read_csv("Number_of_Reported_Malaria_Cases_by_County__United_States__2017.csv")
load("~/Projects/Malaria/casesBoth16y17.RData")
casesBoth16y17
#County level map - USA
countiesUSA <- get_acs(
  geography = "county",
  variables = "B01003_001E",
  year = 2022,
  output = 'wide',
  geometry=TRUE
)

#State level map - USA
statesUSA <- get_acs(
  geography = "state",
  variables = "B01003_001E",
  year = 2019,
  output = 'wide',
  geometry=TRUE
)

#I am removing these territories, due to lack of time to organize this maps in a better way
#I only show the continental USA
alaska<-countiesUSA %>% as_tibble() %>% 
  filter(stringr::str_detect(NAME, 'Alaska') ) %>% pull(GEOID)
hawaii<-countiesUSA %>% as_tibble() %>% 
  filter(stringr::str_detect(NAME, 'Hawaii') ) %>% pull(GEOID)
puertoR<-countiesUSA %>% as_tibble() %>% 
  filter(stringr::str_detect(NAME, 'Puerto Rico') ) %>% pull(GEOID)

rbind(malaria2016 %>% filter(STATE %in% c("TEXAS","FLORIDA")) %>%
        select(STATE,COUNTY,FIPS,CASES=MAL_FREQ_2016) %>% mutate(YEAR="2016"),
      malaria2017 %>% filter(STATE %in% c("TEXAS","FLORIDA")) %>%
        select(STATE,COUNTY,FIPS,CASES=MAL_FREQ_2017) %>% mutate(YEAR="2017")) %>%
  ggplot(aes(x=COUNTY,y=CASES,fill=YEAR))+ xlab("") +
  geom_col(position="dodge") + facet_wrap(~STATE,scales = "free_x")+
  theme(axis.text.x = element_text(angle=45,hjust=1),legend.position = c(0.1,0.8),
        text=element_text(size=22))

ggsave(last_plot(),file="NumberOfCases.png",width = 15,height = 8)

# Data for risk estimation ------

#Yearly malaria cases for all countries from 2010 - 2020
#(https://www.who.int/data/gho/data/indicators/indicator-details/GHO/total-number-of-malaria-cases-presumed-confirmed-cases)
#malariaTotalCountries<-read_csv("totalNumberOfMalaria.csv")
malariaTotalCountries<-read_csv("MalariaCasesUntil2021.csv")
malaria2022 <- read_csv("MalariaData2022.csv")

malariaTotalCountries<-malariaTotalCountries %>% 
  select(Country=Location,Year=Period,Cases=FactValueNumeric) %>%
  mutate(Country=Country %>% 
           str_replace_all(c("Democratic Republic of the Congo"="DR Congo","Viet Nam"="Vietnam",
                             "Côte d’Ivoire"="Ivory Coast","United Republic of Tanzania"="Tanzania",
                             "Democratic People's Republic of Korea"="South Korea"))) %>%
  filter(Country != "Congo") %>% mutate(Year=as.factor(Year))

malariaTotalCountries$Country<-str_replace(malariaTotalCountries$Country, " \\s*\\([^\\)]+\\)", "")
malariaTotalCountries %>% filter(Country=="Brazil") %>% select(Year,Cases)

malariaTotalCountries %>% filter(str_detect(Country,"Viet")) %>% print(n=22)
#Read file with arrivals to the USA in from 2000 to present
#From here: https://www.trade.gov/i-94-arrivals-program 
#totalArrivalsByFlight2000ToPresent<-read_csv("MonthlyArrivalsFlight2000ToPresent.csv")
totalArrivalsByFlight2000To2022<-read_excel("yearlyInternationalVisitsToUSA.xlsx")
#For now, I will not use arrivals on 2023, they seem to be incomplete
#totalArrivals2023 <- read_excel("totalArrivalsToUSA2023.xlsx")

arrivalsSelectedYears<-totalArrivalsByFlight2000To2022 %>% 
  select(Country=`International Visitors`,`2019`,`2020`,`2021`,`2022`) %>%
#  left_join(totalArrivals2023 %>% rowwise() %>%
#              mutate_if(is.character, funs(str_squish(.))) %>%
#              ungroup() %>% rename_at(c("CountryOfResidence","TotalArrivals"),~c("Country","2023"))) %>%
  drop_na() %>% pivot_longer(cols = contains("20"),values_to = "NumberOfVisits",names_to = "Year") %>%
  mutate(Country=ifelse(Country=="Zaire","DR Congo",Country)) %>% mutate(Year=as.factor(Year))
#  select(-Country) %>% group_by(Year) %>% summarise_each(sum) %>%
#  ggplot(aes(x=Year,y=NumberOfVisits)) +
#  geom_col()


#population in countries in 2016-2020: https://www.populationpyramid.net/population-size-per-country/2020/
#Better data can be found here: https://www.census.gov/data-tools/demo/idb/#/table?COUNTRY_YEAR=2023&COUNTRY_YR_ANIM=2023&menu=tableViz&TABLE_YEARS=2020&TABLE_USE_RANGE=N&TABLE_USE_YEARS=Y&TABLE_STEP=1&TABLE_ADD_YEARS=2020&TABLE_RANGE=1950,2100

population <- read_excel("population2019To2023.xlsx") %>%
  select(Country,Year,Population=`Total Population`) %>% 
  mutate(Year=as.factor(Year),Population=as.numeric(Population)) %>% drop_na()

#Importation risk for 2016, 2017 and 2020
estTravsByFlightToUSA<-malariaTotalCountries %>% select(Country,Year,Cases) %>% filter(Year %in% c("2019","2020","2021")) %>%
  left_join(population) %>% left_join(arrivalsSelectedYears)  %>%
  mutate(EstTravsWithMalaria=(Cases/Population)*NumberOfVisits) %>%
  drop_na() %>% filter(Cases>0)

##### USA - Mexico border ######

#Cities and counties in the USA - Mexico border
#this has to be verified depending on the year.
#This list may change
#From: https://www.bts.gov/browse-statistical-products-and-data/border-crossing-data/border-crossingentry-data
countiesCrossings<-data.frame(Place=c("Boquillas","Del Rio","Eagle Pass","El Paso","Hidalgo","Laredo","Presidio","Rio Grande City",
                                      "Roma","Tornillo","Ysleta","Brownsville","Progreso","Nogales","San Luis","Sasabe","Lukeville",
                                      "Naco","Douglas","Otay Mesa","Tecate","Calexico East","San Ysidro","Calexico","Andrade",
                                      "Cross Border Xpress","Columbus","Santa Teresa"),
                              County=c("Brewster","Val Verde","Maverick","El Paso","Hidalgo","Webb","Presidio","Starr","Starr",
                                       "El Paso","El Paso","Cameron","Hidalgo","Santa Cruz","Yuma","Pima","Pima","Cochise","Cochise",
                                       "San Diego","San Diego","Imperial","San Diego","Imperial","Imperial","San Diego","Luna",
                                       "Dona Ana"),
                              FIPS=c("48043","48465","48323","48141","48215","48479","48377","48427","48427",
                                     "48141","48141","48061","48215","04023","04027","04019","04019","04003","04003",
                                     "06073","06073","06025","06073","06025","06025","06073","35029","35013"))

populationMexico<- population %>% filter(Country=="Mexico")

# countiesCrossings %>% arrange(Place)
# #Reading file about crossings Mexico - USA
# usMexico<-read_csv("USMexicoCrossing2023-1.csv") %>% rename_at("Port Name",~"Place")
# 
# #We need to verify what Value means in the file. Is is number of people, or of what?
# usMexico %>% left_join(countiesCrossings)
# 
#borderTotal<-read_csv("Border_Crossing_Entry_Data.csv")
#This file, which is an update from the one above, was downloaded from
#https://data.bts.gov/stories/s/Border-Crossing-Entry-Data/jswi-2e7b/ 
#These data only contains crossings from Mexico. I excluded Canada crossings
#in the search in the web page
borderTotal<-read_excel("CrossingBorder2019To2023.xlsx")

malariaMexico<-malariaTotalCountries %>% filter(Country=="Mexico") %>% select(-Country)

totalCrossingsUS_MXSelectedYears<-borderTotal %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  select(Place=`Port Name`,`2019`,`2020`,`2021`,`2022`,`2023`) %>%
  pivot_longer(cols=contains("20"),values_to = "peopleCrossing",names_to = "Year") %>%
  group_by(Place,Year) %>% summarise_each(sum) %>% left_join(countiesCrossings) %>% 
  left_join(populationMexico) %>% left_join(malariaMexico) %>% drop_na() %>% ungroup() %>%
  select(Year,peopleCrossing,FIPS,Population,Cases) %>% group_by(Year,FIPS,Population,Cases) %>%
  summarise_each(sum) %>% mutate(EstTravsWithMalariaBorder=(Cases/Population)*peopleCrossing)

#Airports ranking (https://www.bts.gov/topics/annual-airport-rankings)
#Numbers in "enplaned" columns should be multiplied by 1000
ranking19<-read_csv("airportRank2019.csv")
ranking20<-read_csv("airportRank2020.csv")
ranking21<-read_csv("airportRank2021.csv")

ranking19 %>% select(Airport=Airport1,Passengers2019=`2019 Enplaned Passengers`) %>%
  mutate(Passengers2019=Passengers2019*1000)
ranking20 %>% select(Airport,Passengers2020=`2020 Enplaned Passengers`) %>%
  mutate(Passengers2020=Passengers2020*1000)
ranking21 %>% select(Airport,Passengers2021=`2021 Enplaned Passengers`) %>%
  mutate(Passengers2021=Passengers2021*1000)
# # Ver #This needs more work, I have a list of all airports in the USA
# ver1<-read_csv("Aviation_Facilities.csv")
# ver1 %>% select(STATE_CODE,ARPT_ID,REGION_CODE,STATE_NAME,COUNTY_NAME,ARPT_NAME) %>%
#   filter(STATE_NAME=="TEXAS",COUNTY_NAME=="HARRIS") %>%
#   filter(str_detect(ARPT_NAME,"BUSH"))
# 
# ver1 %>% #filter(str_detect(ARPT_ID,"LAX")) %>%
#   filter(STATE_NAME,"TEXAS")
# 
# ##

#Homogenization of airport names to merge with different data bases about ranking

arrivalFlights<-read_csv("ArrivalAndProbArrival.csv")
arrivalFlights %>% print(n=50)
volumeFlights2022<-read_csv("AirportRankings2022.csv")
zip2fips<-read_csv("zip2fips.csv")

#This works for 2016 and 2017. For 2020 data bases are more similar
AirportsAndZip<-arrivalFlights %>% separate(Airport,c("State","Name"),sep="[(]") %>%
  mutate(Name=Name %>% str_remove_all("[)]")) %>%
  mutate(State=str_sub(State,end=-2)) %>% arrange(State) %>% 
  separate(State,c("Code","StateCode"),", ") %>%
  mutate(Code=ifelse(Code=="Chicago" & Name=="Chicago O'Hare International","Chicago O'Hare",Code)) %>%
  mutate(Code=ifelse(Code=="Chicago" & Name=="Chicago Midway International","Chicago Midway",Code)) %>%
  mutate(Code=ifelse(Code=="Houston" & Name!="William P Hobby","Houston Bush",Code)) %>%
  mutate(Code=ifelse(Code=="New York" & Name=="LaGuardia","New York LaGuardia",Code)) %>%
  mutate(Code=ifelse(Code=="New York" & Name=="John F. Kennedy International","New York JFK",Code)) %>%
  mutate(Code=ifelse(Code=="Washington" & Name=="Ronald Reagan Washington National","Washington National",Code)) %>%
  mutate(Code=ifelse(Code=="Washington" & Name=="Washington Dulles International","Washington Dulles",Code)) %>%
  select(Airport=Code,StateCode,zipcode)

# losQueFaltan2016<-data.frame(Airport=c("Detroit Metro","Guam","Saipan","Palm Springs","Kahului","Cincinnati","Fresno"),
#                     Zipcode=c("48174","96913","96950","92262","96732","41018","93727")) %>% arrange(Airport)
# 
# probArrival2016<-ranking16 %>% mutate(Volume=Enplaned2016*1000) %>%
#   left_join(AirportsAndZip) %>% arrange(desc(Volume)) %>%
#   drop_na() %>% filter(Enplaned2016>0) %>% #print(n=50) %>% 
#   left_join(zip2fips) %>%select(FIPS,Volume) %>% group_by(FIPS) %>%
#   summarise_each(sum) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% mutate(Year="2016")
# 
# probArrival2017<-ranking17 %>% mutate(Volume=Enplaned2017*1000) %>% left_join(AirportsAndZip) %>% 
#   mutate(zipcode=ifelse(Airport=="Detroit Metro","48174",
#                         ifelse(Airport=="Guam","96913",
#                                ifelse(Airport=="Saipan","96950",
#                                       ifelse(Airport=="Palm Springs","92262",
#                                              ifelse(Airport=="Kahului","96732",
#                                                     ifelse(Airport=="Cincinnati","41018",
#                                                            ifelse(Airport=="Fresno","93727",zipcode)))))))) %>%
#   left_join(zip2fips) %>% drop_na() %>% select(FIPS,Volume) %>% group_by(FIPS) %>%
#   summarise_each(sum) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% mutate(Year="2017")

probArrival2019<-ranking19 %>% mutate(Volume=`2019 Enplaned Passengers`*1000) %>% separate(Airport1,c("State","Name"),": ") %>%
  merge(arrivalFlights %>% mutate(Airport= str_sub(Airport, end = -2)) %>%
          separate(Airport,c("State","Name"),sep="[(]") %>% select(Name,zipcode),
        by="Name",all=T) %>% 
  filter(!is.na(Volume)) %>% replace(is.na(.),0) %>%
  mutate(zipcode=ifelse(Name=="Cincinnati/Northern Kentucky International","41018",
                        ifelse(Name=="Daniel K Inouye International","96819",
                               ifelse(Name=="Ellison Onizuka Kona International at Keahole","96740",
                                      ifelse(Name=="Fresno Yosemite International","93727",
                                             ifelse(Name=="Harry Reid International","89119",
                                                    ifelse(Name=="Metro Oakland International","94621",
                                                           ifelse(Name=="Kahului Airport","96732",
                                                                  ifelse(Name=="Ontario International","91761",
                                                                         ifelse(Name=="Palm Beach International","33415",
                                                                                ifelse(Name=="Palm Springs International","92262",
                                                                                       ifelse(Name=="St Louis Lambert International","63145",zipcode)))))))))))) %>%
  select(zipcode,Volume) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% left_join(zip2fips) %>% 
  select(FIPS,Volume,ProbOfArrival) %>% group_by(FIPS) %>% summarise_each(sum) %>% mutate(Year="2019")

losQueFaltan2020<-data.frame(Aeropuerto=c("Cincinnati/Northern Kentucky International",
                                      "Daniel K Inouye International","Ellison Onizuka Kona International at Keahole",
                                      "Fresno Yosemite International","Harry Reid International",
                                      "Metro Oakland International","Kahului Airport",
                                      "Ontario International","Palm Beach International","Palm Springs International",
                                      "St Louis Lambert International"),
                         Zipcode=c("41018","96819",96740,"93727","89119","94621","96732","91761","33415","92262","63145"))

probArrival2020<-ranking20 %>% mutate(Volume=`2020 Enplaned Passengers`*1000) %>% separate(Airport,c("State","Name"),": ") %>%
  merge(arrivalFlights %>% mutate(Airport= str_sub(Airport, end = -2)) %>%
          separate(Airport,c("State","Name"),sep="[(]") %>% select(Name,zipcode),
        by="Name",all=T) %>% 
  filter(!is.na(Volume)) %>% replace(is.na(.),0) %>%
  mutate(zipcode=ifelse(Name=="Cincinnati/Northern Kentucky International","41018",
                        ifelse(Name=="Daniel K Inouye International","96819",
                               ifelse(Name=="Ellison Onizuka Kona International at Keahole","96740",
                                      ifelse(Name=="Fresno Yosemite International","93727",
                                             ifelse(Name=="Harry Reid International","89119",
                                                    ifelse(Name=="Metro Oakland International","94621",
                                                           ifelse(Name=="Kahului Airport","96732",
                                                                  ifelse(Name=="Ontario International","91761",
                                                                         ifelse(Name=="Palm Beach International","33415",
                                                                                ifelse(Name=="Palm Springs International","92262",
                                                                                       ifelse(Name=="St Louis Lambert International","63145",zipcode)))))))))))) %>%
  select(zipcode,Volume) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% left_join(zip2fips) %>% 
  select(FIPS,Volume,ProbOfArrival) %>% group_by(FIPS) %>% summarise_each(sum) %>% mutate(Year="2020")

probArrival2021<-ranking21 %>% mutate(Volume=`2021 Enplaned Passengers`*1000) %>% separate(Airport,c("State","Name"),": ") %>%
  merge(arrivalFlights %>% mutate(Airport= str_sub(Airport, end = -2)) %>%
          separate(Airport,c("State","Name"),sep="[(]") %>% select(Name,zipcode),
        by="Name",all=T) %>% 
  filter(!is.na(Volume)) %>% replace(is.na(.),0) %>%
  mutate(zipcode=ifelse(Name=="Bradley International","06096",
                        ifelse(Name=="Daniel K Inouye International","96819",
                               ifelse(Name=="Ellison Onizuka Kona International at Keahole","96740",
                                      ifelse(Name=="Fresno Yosemite International","93727",
                                             ifelse(Name=="Harry Reid International","89119",
                                                    ifelse(Name=="Metro Oakland International","94621",
                                                           ifelse(Name=="Kahului Airport","96732",
                                                                  ifelse(Name=="Ontario International","91761",
                                                                         ifelse(Name=="Palm Beach International","33415",
                                                                                ifelse(Name=="Palm Springs International","92262",
                                                                                       ifelse(Name=="St Louis Lambert International","63145",
                                                                                              ifelse(Name=="Reno/Tahoe International","89502",zipcode))))))))))))) %>%
  select(zipcode,Volume) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% left_join(zip2fips) %>% 
  select(FIPS,Volume,ProbOfArrival) %>% group_by(FIPS) %>% summarise_each(sum) %>% mutate(Year="2021")

EstCasesArrivingByCountyFlight<-
  rbind(estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>%
          filter(Year=="2019") %>%
          merge(probArrival2019 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithMalaria*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty),
        estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>% filter(Year=="2020") %>%
          merge(probArrival2020 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithMalaria*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty),
        estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>% filter(Year=="2021") %>%
          merge(probArrival2021 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithMalaria*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty))%>%
  group_by(Year,FIPS) %>% summarise_each(sum)

estTravsByFlightToUSA %>% #filter(Year!=2020) %>%
  ggplot(aes(x=log(Cases),y=log(EstTravsWithMalaria)))+
  geom_point(size=4) + geom_smooth(method = "lm",se=F)+ theme_bw() +
  facet_wrap(~Year) + 
  theme(text=element_text(size=20),legend.position = c(0.1,0.9))

ggsave(last_plot(),file="relas1.png",width = 18,height = 7)

EstCasesArrivingByCountyAll<-
  rbind(EstCasesArrivingByCountyFlight,
        totalCrossingsUS_MXSelectedYears %>% ungroup() %>%
          select(Year,FIPS,Volume=peopleCrossing,CasesByCounty=EstTravsWithMalariaBorder)) %>%
  group_by(Year,FIPS) %>% summarise_each(sum)

EstCasesArrivingByCountyAll

max(arrivalsByCounty2016$Volume)
arrivalsByCounty2016<-
  rbind(estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>% filter(Year=="2016") %>%
        merge(probArrival2016 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
        select(FIPS,Volume),
      totalCrossingsUS_MXSelectedYears %>% filter(Year=="2016") %>%
        ungroup() %>% select(FIPS,Volume=peopleCrossing)) %>%
  group_by(FIPS) %>% summarise_each(sum)

my_breaks1 = c(10000,1000000,1000000,100000000,10000000000)
mapBase<-countiesUSA %>% filter(!GEOID %in% c(alaska,hawaii,puertoR))

countiesUSA %>% merge(EstCasesArrivingByCountyAll,by.x="GEOID",by.y="FIPS")%>%
  filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  mutate(Volume=ifelse(is.na(Volume),0,Volume)) %>%
  ggplot()+ theme_void() +
  geom_sf(data=mapBase,fill="white",color="gray") +
  geom_sf(data=statesUSA %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico")),
          fill=NA,color="red",linewidth=0.5)+
  geom_sf(aes(fill=(Volume)),color="gray") +
  theme(text=element_text(size=20))+
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks1, labels = my_breaks1,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="Arrivals") +
  theme(legend.position = c(0.85,0.2)) + facet_wrap(~Year)

#write_csv(arrivalsByCounty2016,file="arrivalsByCounty2016.csv")

my_breaks = c(0.01,0.1,1,10,100,1000,10000)

mapBase<-countiesUSA %>% filter(!GEOID %in% c(alaska,hawaii,puertoR))

malariaMapa2019<-countiesUSA %>% merge(EstCasesArrivingByCountyAll %>% filter(Year=="2019"),by.x="GEOID",by.y="FIPS")%>%
  filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  mutate(CasesByCounty=ifelse(is.na(CasesByCounty),0,CasesByCounty)) %>%
  ggplot()+ theme_void() +
  geom_sf(data=mapBase,fill="white",color="gray") +
  geom_sf(data=statesUSA %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico")),
          fill=NA,color="red",linewidth=0.5)+
  geom_sf(aes(fill=(CasesByCounty)),color="gray") +
  theme(legend.title = element_blank(),text=element_text(size=20))+
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks, labels = my_breaks,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="prob") +
  theme(legend.position = c(0.85,0.2)) + facet_wrap(~Year)


##### Dengue data

library(readxl)
#From https://ourworldindata.org/grapher/dengue-incidence?tab=table&time=2018..latest
#Dengue cases in countries from 1990 to 2019
getwd()

#For Dengue in 2019 - This is important since there was a reduction
#of Dengue in 2020 and 2021 due to the pandemic
dengueUntil2019<-read_excel("dengue-incidenceUntil2019.xlsx")
dengueTotal2019<-dengueUntil2019 %>% filter(Year==2019) %>% drop_na() %>%
  filter(Cases>0) %>% rename_at("Entity",~"Country")

arrivalsIn2019<-totalArrivalsByFlight2000To2022 %>% 
  select(Country=`International Visitors`,`2019`) %>%
  drop_na() %>% pivot_longer(cols = contains("20"),values_to = "NumberOfVisits",names_to = "Year") %>%
  mutate(Country=ifelse(Country=="Zaire","DR Congo",Country)) %>% mutate(Year=as.factor(Year))

population2019 <- read_excel("population2019To2023.xlsx") %>%
  select(Country,Year,Population=`Total Population`) %>% filter(Year=="2019") %>%
  mutate(Year=as.factor(Year),Population=as.numeric(Population)) %>% drop_na()

#Importation risk for 2016, 2017 and 2020
estTravsByFlightToUSADengue<-dengueTotal2019 %>% select(Country,Cases) %>%
  left_join(population2019) %>% left_join(arrivalsIn2019 %>% select(-Year)) %>%
  mutate(EstTravsWithDengue=(Cases/Population)*NumberOfVisits) %>%
  drop_na() %>% filter(Cases>0)

##### USA - Mexico border ######

#Cities and counties in the USA - Mexico border
#this has to be verified depending on the year.
#This list may change
#From: https://www.bts.gov/browse-statistical-products-and-data/border-crossing-data/border-crossingentry-data
countiesCrossings<-data.frame(Place=c("Boquillas","Del Rio","Eagle Pass","El Paso","Hidalgo","Laredo","Presidio","Rio Grande City",
                                      "Roma","Tornillo","Ysleta","Brownsville","Progreso","Nogales","San Luis","Sasabe","Lukeville",
                                      "Naco","Douglas","Otay Mesa","Tecate","Calexico East","San Ysidro","Calexico","Andrade",
                                      "Cross Border Xpress","Columbus","Santa Teresa"),
                              County=c("Brewster","Val Verde","Maverick","El Paso","Hidalgo","Webb","Presidio","Starr","Starr",
                                       "El Paso","El Paso","Cameron","Hidalgo","Santa Cruz","Yuma","Pima","Pima","Cochise","Cochise",
                                       "San Diego","San Diego","Imperial","San Diego","Imperial","Imperial","San Diego","Luna",
                                       "Dona Ana"),
                              FIPS=c("48043","48465","48323","48141","48215","48479","48377","48427","48427",
                                     "48141","48141","48061","48215","04023","04027","04019","04019","04003","04003",
                                     "06073","06073","06025","06073","06025","06025","06073","35029","35013"))

populationMexico2019<- population2019 %>% filter(Country=="Mexico")

borderTotal<-read_excel("CrossingBorder2019To2023.xlsx")

dengueMexico2019<-dengueTotal2019 %>% filter(Country=="Mexico") %>% 
  select(-Country,-Code) %>% mutate(Year=as.factor(Year))

totalCrossingsUS_MX2019<-borderTotal %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  select(Place=`Port Name`,`2019`) %>%
  pivot_longer(cols=contains("20"),values_to = "peopleCrossing",names_to = "Year") %>%
  group_by(Place,Year) %>% summarise_each(sum) %>% left_join(countiesCrossings) %>% 
  left_join(populationMexico2019 %>% select(-Country)) %>% left_join(dengueMexico2019) %>% drop_na() %>% ungroup() %>%
  select(Year,peopleCrossing,FIPS,Population,Cases) %>% group_by(Year,FIPS,Population,Cases) %>%
  summarise_each(sum) %>% mutate(EstTravsWithDengueBorder=(Cases/Population)*peopleCrossing)

#Airports ranking (https://www.bts.gov/topics/annual-airport-rankings)
#Numbers in "enplaned" columns should be multiplied by 1000
ranking19<-read_csv("airportRank2019.csv")
ranking20<-read_csv("airportRank2020.csv")
ranking21<-read_csv("airportRank2021.csv")

ranking19 %>% select(Airport=Airport1,Passengers2019=`2019 Enplaned Passengers`) %>%
  mutate(Passengers2019=Passengers2019*1000)
ranking20 %>% select(Airport,Passengers2020=`2020 Enplaned Passengers`) %>%
  mutate(Passengers2020=Passengers2020*1000)
ranking21 %>% select(Airport,Passengers2021=`2021 Enplaned Passengers`) %>%
  mutate(Passengers2021=Passengers2021*1000)
# # Ver #This needs more work, I have a list of all airports in the USA
# ver1<-read_csv("Aviation_Facilities.csv")
# ver1 %>% select(STATE_CODE,ARPT_ID,REGION_CODE,STATE_NAME,COUNTY_NAME,ARPT_NAME) %>%
#   filter(STATE_NAME=="TEXAS",COUNTY_NAME=="HARRIS") %>%
#   filter(str_detect(ARPT_NAME,"BUSH"))
# 
# ver1 %>% #filter(str_detect(ARPT_ID,"LAX")) %>%
#   filter(STATE_NAME,"TEXAS")
# 
# ##

#Homogenization of airport names to merge with different data bases about ranking

# arrivalFlights<-read_csv("ArrivalAndProbArrival.csv")
# 
# test1<-arrivalFlights %>% select(Airport,zipcode) %>%
#   mutate(Airport= str_sub(Airport, end = -2)) %>%
#   separate(Airport,c("State","Name"),sep="[(]") %>% select(Name,zipcode)
# 
# test2<-data.frame(Name=c("Cincinnati/Northern Kentucky International","Daniel K Inouye International",
#             "Ellison Onizuka Kona International at Keahole","Fresno Yosemite International",
#             "Harry Reid International","Metro Oakland International","Kahului Airport",
#             "Ontario International","Palm Beach International","Palm Springs International",
#             "St Louis Lambert International","Bradley International","Reno/Tahoe International"),
#   zipcode=c("41018","96819","96740","93727","89119","94621","96732","91761","33415","92262",
#             "63145","06096","89502"))
# 
# airportsWithZipcode <- rbind(test1,test2) %>% arrange(Name)

#I created this file manually of airports and their zip codes.
save(airportsWithZipcode,file="airportsWithZipcode.RData")
load("airportsWithZipcode.RData")
airportsWithZipcode

probArrival2019<-ranking19 %>% mutate(Volume=`2019 Enplaned Passengers`*1000) %>% separate(Airport1,c("State","Name"),": ") %>%
  merge(airportsWithZipcode,all=T) %>% 
  filter(!is.na(Volume)) %>% replace(is.na(.),0) %>%
  select(zipcode,Volume) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% left_join(zip2fips) %>% 
  select(FIPS,Volume,ProbOfArrival) %>% group_by(FIPS) %>% summarise_each(sum) %>% arrange(desc(ProbOfArrival))


EstCasesArrivingByCountyFlightDengue<-
  estTravsByFlightToUSADengue %>% mutate(Year=as.factor(Year)) %>%
          merge(probArrival2019,by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithDengue*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty) %>%
  group_by(Year,FIPS) %>% summarise_each(sum)

estTravsByFlightToUSADengue %>% #filter(Year!=2020) %>%
  ggplot(aes(x=log(Cases),y=log(EstTravsWithDengue)))+
  geom_point(size=4) + geom_smooth(method = "lm",se=F)+ theme_bw() +
  facet_wrap(~Year) + 
  theme(text=element_text(size=20),legend.position = c(0.1,0.9))

EstCasesArrivingByCountyAllDengue2019<-
  rbind(EstCasesArrivingByCountyFlightDengue,
        totalCrossingsUS_MX2019 %>% ungroup() %>%
          select(Year,FIPS,Volume=peopleCrossing,CasesByCounty=EstTravsWithDengueBorder)) %>%
  group_by(Year,FIPS) %>% summarise_each(sum)

EstCasesArrivingByCountyAllDengue2019

my_breaks1 = c(10000,1000000,1000000,100000000,10000000000)
mapBase<-countiesUSA %>% filter(!GEOID %in% c(alaska,hawaii,puertoR))

countiesUSA %>% merge(EstCasesArrivingByCountyAllDengue2019,by.x="GEOID",by.y="FIPS") %>%
  filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  mutate(Volume=ifelse(is.na(Volume),0,Volume)) %>%
  ggplot()+ theme_void() +
  geom_sf(data=mapBase,fill="white",color="gray") +
  geom_sf(data=statesUSA %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico")),
          fill=NA,color="red",linewidth=0.5)+
  geom_sf(aes(fill=(Volume)),color="gray") +
  theme(text=element_text(size=20))+
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks1, labels = my_breaks1,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="Arrivals") +
  theme(legend.position = c(0.85,0.2)) + facet_wrap(~Year)

#write_csv(arrivalsByCounty2016,file="arrivalsByCounty2016.csv")

my_breaks = c(0.01,0.1,1,10,100,1000,10000,100000)

mapBase<-countiesUSA %>% filter(!GEOID %in% c(alaska,hawaii,puertoR))

dengueMapa2019<-countiesUSA %>% merge(EstCasesArrivingByCountyAllDengue2019,by.x="GEOID",by.y="FIPS")%>%
  filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  mutate(CasesByCounty=ifelse(is.na(CasesByCounty),0,CasesByCounty)) %>%
  ggplot()+ theme_void() +
  geom_sf(data=mapBase,fill="white",color="gray") +
  geom_sf(data=statesUSA %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico")),
          fill=NA,color="red",linewidth=0.5)+
  geom_sf(aes(fill=(CasesByCounty)),color="gray") +
  theme(legend.title = element_blank(),text=element_text(size=20))+
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks, labels = my_breaks,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="prob") +
  theme(legend.position = c(0.85,0.2)) + facet_wrap(~Year)

plot_grid(malariaMapa2019,dengueMapa2019)

###### Hasta aqui
denguePaho<-read_excel("DenguePaho2019To2024.xlsx") %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
denguePaho %>% select(Country,"2019","2023") %>% print(n=52)

verEsto<-read_csv("~/Downloads/National_extract_V1_2.csv")
verEsto %>% select(full_name,Year,dengue_total) %>% 
  group_by(full_name,Year) %>% summarise_each(sum) %>%
  filter(Year %in% c(2019,2023)) %>% #print(n=22)
  ungroup() %>% select(full_name) %>% group_by(full_name)%>% count() %>%
  arrange(desc(n)) %>% print(n=96)

dengueCounty2023<-read_csv("DengueCounty2023.csv")
dengueState2023<-read_csv("DengueStates2023.csv")
variables2022<-read_csv("variables2022.csv")
getwd()
varsAndImportCases<-variables2022 %>% rename_at("GEOID",~"FIPS") %>% 
  left_join(dengueCounty2023 %>% select(FIPS=County,REPORTED_IMPORTS=Count) %>%
              mutate(REPORTED_IMPORTS=ifelse(REPORTED_IMPORTS=="1 to 4", "0",REPORTED_IMPORTS)) %>%
              mutate(REPORTED_IMPORTS=as.numeric(REPORTED_IMPORTS)))
write_csv(varsAndImportCases,file="varsAndImportCases2022.csv")

maxTemp2023<-read_csv("maxTempCounty2023.csv")

names(varsAndImportCases)

