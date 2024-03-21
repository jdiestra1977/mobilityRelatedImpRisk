library(tidyverse)
library(tidycensus)
library(housingData)
library(cowplot)

remove(list = ls())

setwd("~/Projects/Malaria/")

#Reading files with Malaria cases by county in the USA
malaria2016<-read_csv("Number_of_Reported_Malaria_Cases_by_County__United_States__2016.csv")
malaria2017<-read_csv("Number_of_Reported_Malaria_Cases_by_County__United_States__2017.csv")

#County level map - USA
countiesUSA <- get_acs(
  geography = "county",
  variables = "B01003_001E",
  year = 2019,
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
malariaTotalCountries<-read_csv("totalNumberOfMalaria.csv")
malariaTotalCountries<-malariaTotalCountries %>% 
  select(Region=ParentLocation,Country=Location,Year=Period,Cases=FactValueNumeric) %>%
  mutate(Country=Country %>% 
           str_replace_all(c("Democratic Republic of the Congo"="DR Congo","Viet Nam"="Vietnam",
                             "Côte d’Ivoire"="Ivory Coast","United Republic of Tanzania"="Tanzania",
                             "Democratic People's Republic of Korea"="South Korea"))) %>%
  filter(Country != "Congo")
malariaTotalCountries$Country<-str_replace(malariaTotalCountries$Country, " \\s*\\([^\\)]+\\)", "")

#Read file with arrivals to the USA in from 2000 to present
#From here: https://www.trade.gov/i-94-arrivals-program 
totalArrivalsByFlight2000ToPresent<-read_csv("MonthlyArrivalsFlight2000ToPresent.csv")

arrivalsSelectedYears<-rbind(totalArrivalsByFlight2000ToPresent %>% 
        mutate(TotalVisits = rowSums(across(contains("2016")), na.rm=TRUE)) %>%
        select(Country,WorldRegion,TotalVisits) %>% mutate(Year=2016),
      totalArrivalsByFlight2000ToPresent %>% 
        mutate(TotalVisits = rowSums(across(contains("2017")), na.rm=TRUE)) %>%
        select(Country,WorldRegion,TotalVisits) %>% mutate(Year=2017),
      totalArrivalsByFlight2000ToPresent %>% 
        mutate(TotalVisits = rowSums(across(contains("2017")), na.rm=TRUE)) %>%
        select(Country,WorldRegion,TotalVisits) %>% mutate(Year=2020))
arrivalsSelectedYears$Country<-str_replace(arrivalsSelectedYears$Country, " \\s*\\([^\\)]+\\)", "")
arrivalsSelectedYears<-arrivalsSelectedYears %>% mutate(Country=ifelse(Country=="Zaire","DR Congo",Country))
arrivalsSelectedYears %>% filter(str_detect(Country,"Congo"))

#population in countries in 2016-2020: https://www.populationpyramid.net/population-size-per-country/2020/
#Better data can be found here: https://www.census.gov/data-tools/demo/idb/#/table?COUNTRY_YEAR=2023&COUNTRY_YR_ANIM=2023&menu=tableViz&TABLE_YEARS=2020&TABLE_USE_RANGE=N&TABLE_USE_YEARS=Y&TABLE_STEP=1&TABLE_ADD_YEARS=2020&TABLE_RANGE=1950,2100
popu16_17_20<-rbind(read_csv("population2017.csv") %>% select(Country=COUNTRY,Population=POPULATION) %>%
                     mutate(Year=2017),
                    read_csv("population2016.csv") %>% select(Country=COUNTRY,Population=POPULATION) %>%
                     mutate(Year=2016),
                    read_csv("population2020.csv") %>% select(Country=COUNTRY,Population=POPULATION) %>%
                     mutate(Year=2020))
popu16_17_20 %>% filter(str_detect(Country,"Congo"))

#Importation risk for 2016, 2017 and 2020
estTravsByFlightToUSA<-malariaTotalCountries %>% filter(Year %in% c(2016,2017,2020)) %>%
  left_join(popu16_17_20) %>% left_join(arrivalsSelectedYears)  %>%
  mutate(EstTravsWithMalaria=(Cases/Population)*TotalVisits) %>%
  drop_na() %>% filter(Cases>0)

estTravsByFlightToUSA %>% filter(Year==2017)

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
countiesUSA %>% filter(str_detect(NAME,"Ana"))

# countiesCrossings %>% arrange(Place)
# #Reading file about crossings Mexico - USA
# usMexico<-read_csv("USMexicoCrossing2023-1.csv") %>% rename_at("Port Name",~"Place")
# 
# #We need to verify what Value means in the file. Is is number of people, or of what?
# usMexico %>% left_join(countiesCrossings)
# 
borderTotal<-read_csv("Border_Crossing_Entry_Data.csv")
malariaTotalCountries %>% filter(Country=="Mexico")

borderTotal$Measure %>% unique()

totalCrossingsUS_MXSelectedYears<-borderTotal %>% separate(Date,c("Month","Year")) %>%
  filter(Year %in% c("2016","2017","2020")) %>% filter(str_detect(Border,"Mexico")) %>% 
#  filter(Measure %in% c("Bus Passengers","Pedestrians","Personal Vehicle Passengers",
#                        "Truck Containers Empty","Truck Containers Loaded")) %>%
  select(Place=`Port Name`,State,Year,peopleCrossing=Value) %>% 
  group_by(Place,State,Year) %>% summarise_each(sum) %>% left_join(countiesCrossings) %>% 
  mutate(Cases=ifelse(Year=="2016",596,ifelse(Year=="2017",765,369))) %>%
  mutate(Population=ifelse(Year=="2016",121519221,ifelse(Year=="2017",122839258,125998302))) %>%
  mutate(EstTravsWithMalariaBorder=(Cases/Population)*peopleCrossing)

totalCrossingsUS_MXSelectedYears %>% filter(Year=="2017") %>%
  select(State,Place) %>% unique() %>% pull(State) %>% unique()

#Airports ranking (https://www.bts.gov/topics/annual-airport-rankings)
#Numbers in "enplaned" columns should be multiplied by 1000
ranking16<-read_csv("airportRank2016.csv")
ranking17<-read_csv("airportRank2017.csv")
ranking20<-read_csv("airportRank2020.csv")

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

losQueFaltan2016<-data.frame(Airport=c("Detroit Metro","Guam","Saipan","Palm Springs","Kahului","Cincinnati","Fresno"),
                    Zipcode=c("48174","96913","96950","92262","96732","41018","93727")) %>% arrange(Airport)

probArrival2016<-ranking16 %>% mutate(Volume=Enplaned2016*1000) %>%
  left_join(AirportsAndZip) %>% arrange(desc(Volume)) %>%
  drop_na() %>% filter(Enplaned2016>0) %>% #print(n=50) %>% 
  left_join(zip2fips) %>%select(FIPS,Volume) %>% group_by(FIPS) %>%
  summarise_each(sum) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% mutate(Year="2016")

probArrival2017<-ranking17 %>% mutate(Volume=Enplaned2017*1000) %>% left_join(AirportsAndZip) %>% 
  mutate(zipcode=ifelse(Airport=="Detroit Metro","48174",
                        ifelse(Airport=="Guam","96913",
                               ifelse(Airport=="Saipan","96950",
                                      ifelse(Airport=="Palm Springs","92262",
                                             ifelse(Airport=="Kahului","96732",
                                                    ifelse(Airport=="Cincinnati","41018",
                                                           ifelse(Airport=="Fresno","93727",zipcode)))))))) %>%
  left_join(zip2fips) %>% drop_na() %>% select(FIPS,Volume) %>% group_by(FIPS) %>%
  summarise_each(sum) %>% mutate(ProbOfArrival=Volume/sum(Volume)) %>% mutate(Year="2017")

losQueFaltan2020<-data.frame(Aeropuerto=c("Cincinnati/Northern Kentucky International",
                                      "Daniel K Inouye International","Ellison Onizuka Kona International at Keahole",
                                      "Fresno Yosemite International","Harry Reid International",
                                      "Metro Oakland International","Kahului Airport",
                                      "Ontario International","Palm Beach International","Palm Springs International",
                                      "St Louis Lambert International"),
                         Zipcode=c("41018","96819",96740,"93727","89119","94621","96732","91761","33415","92262","63145"))

probArrival2020<-ranking20 %>% mutate(Volume=Enplaned2020*1000) %>% separate(Airport,c("State","Name"),": ") %>%
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

EstCasesArrivingByCountyFlight<-
  rbind(estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>%
          filter(Year=="2016") %>%
          merge(probArrival2016 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithMalaria*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty),
        estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>% filter(Year=="2017") %>%
          merge(probArrival2017 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithMalaria*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty),
        estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>% filter(Year=="2020") %>%
          merge(probArrival2020 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
          mutate(CasesByCounty=EstTravsWithMalaria*ProbOfArrival) %>%
          select(Year,FIPS,Volume,CasesByCounty))%>%
  group_by(Year,FIPS) %>% summarise_each(sum)

estTravsByFlightToUSA %>% filter(Year!=2020) %>%
  ggplot(aes(x=log(Cases),y=log(EstTravsWithMalaria),color=Region))+
  geom_point(size=4) + geom_smooth(method = "lm",se=F)+ theme_bw() +
  facet_wrap(~Year) + 
  theme(text=element_text(size=20),legend.position = c(0.1,0.9))

ggsave(last_plot(),file="relas1.png",width = 18,height = 7)

estTravsByFlightToUSA %>% filter(Year!=2020) %>%
  ggplot(aes(x=log(TotalVisits),y=log(EstTravsWithMalaria),color=as.factor(Region)))+
  geom_point(size=4) + geom_smooth(method = "lm",se=F)+
  facet_wrap(~Year)

estTravsByFlightToUSA %>% filter(Year!=2020) %>%
  ggplot(aes(x=log(Population),y=log(EstTravsWithMalaria),color=as.factor(Region)))+
  geom_point(size=4) + geom_smooth(method = "lm",se=F)+
  facet_wrap(~Year)

EstCasesArrivingByCountyAll<-
  rbind(EstCasesArrivingByCountyFlight,
        totalCrossingsUS_MXSelectedYears %>% ungroup() %>%
          select(Year,FIPS,Volume=peopleCrossing,CasesByCounty=EstTravsWithMalariaBorder)) %>%
  group_by(Year,FIPS) %>% summarise_each(sum)

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

uno<-countiesUSA %>% merge(arrivalsByCounty2016,by.x="GEOID",by.y="FIPS")%>%
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
  theme(legend.position = c(0.85,0.2)) + 
  ggtitle("Arrivals to counties in  2016")

arrivalsByCounty2017<-rbind(estTravsByFlightToUSA %>% mutate(Year=as.factor(Year)) %>% filter(Year=="2017") %>%
                              merge(probArrival2017 %>% select(-Year),by=NULL) %>% arrange(Country) %>%
                              select(FIPS,Volume),
                            totalCrossingsUS_MXSelectedYears %>% filter(Year=="2017") %>%
                              ungroup() %>% select(FIPS,Volume=peopleCrossing)) %>%
  group_by(FIPS) %>% summarise_each(sum)

dos<-countiesUSA %>% 
  merge(arrivalsByCounty2017,by.x="GEOID",by.y="FIPS")%>%
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
  theme(legend.position = c(0.85,0.2)) + ggtitle("Arrivals to counties in  2017")

tres<-arrivalsByCounty2016 %>% rename_at("Volume",~"Vol2016") %>%
  left_join(arrivalsByCounty2017 %>% rename_at("Volume",~"Vol2017")) %>%
  replace(is.na(.),0) %>%
  ggplot(aes(x=Vol2016,y=Vol2017))+ theme_bw() +
  geom_point(size=5,shape=1) + geom_abline(slope=1,intercept = 0,color="red",linewidth=1) +
  xlab("Arrivals (2016)") + ylab("Arrivals (2017)") +
  theme(text=element_text(size=24))

ver<-plot_grid(uno,dos)

ggsave(ver,file="verUno.png",width = 21,height = 7)
ggsave(tres,file="verTres.png")

plot_grid(uno,dos,tres)

ggsave(ver,file="verUno.png",width=18,height=7)

write_csv(arrivalsByCounty2016,file="arrivalsByCounty2016.csv")
write_csv(arrivalsByCounty2017,file="arrivalsByCounty2017.csv")

my_breaks = c(0.01,0.1,1,10,100,1000,10000)

mapBase<-countiesUSA %>% filter(!GEOID %in% c(alaska,hawaii,puertoR))

mapa2016<-countiesUSA %>% merge(EstCasesArrivingByCountyAll %>% 
                                  filter(Year=="2016"),by.x="GEOID",by.y="FIPS")%>%
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
  theme(legend.position = c(0.85,0.2)) + ggtitle("Estimated importations - 2016")

mapa2017<-countiesUSA %>% merge(EstCasesArrivingByCountyAll %>% filter(Year=="2017"),by.x="GEOID",by.y="FIPS")%>%
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
  theme(legend.position = c(0.85,0.2)) + ggtitle("Estimated importations - 2017")

cuatro<-EstCasesArrivingByCountyAll %>% filter(Year!="2020") %>% select(-Volume) %>%
  pivot_wider(names_from = "Year",values_from = "CasesByCounty") %>%
  rename_at(c("2016","2017"),~c("EstCases2016","EstCases2017")) %>%
  ggplot(aes(x=EstCases2016,y=EstCases2017))+ theme_bw() +
  geom_point(size=5,shape=1) + geom_abline(slope = 1,intercept = 0,color="red",linewidth=1) +
  xlab("Estimated importations (2016)") + ylab("Estimated importations (2017)") +
  theme(text=element_text(size=24))

plot_grid(mapa2016,mapa2017)

ggsave(last_plot(),file="Juntos.png",width = 21,height = 7)
ggsave(cuatro,file="juntos3.png")


mapa2020<-countiesUSA %>% merge(EstCasesArrivingByCountyAll %>% filter(Year=="2020"),by.x="GEOID",by.y="FIPS")%>%
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
  theme(legend.position = c(0.85,0.1)) + ggtitle("Year=2020")

mapas<-plot_grid(mapa2016,mapa2017,mapa2020,ncol=1)

ggsave(mapas,file="Mapas.png",width = 10,height = 25)

corre1<-EstCasesArrivingByCountyAll %>%
  pivot_wider(names_from = "Year",values_from = "CasesByCounty") %>%
  ggplot()+ theme_bw()+
  geom_point(aes(x=`2016`,y=`2020`),size=3,color="red") + 
  geom_point(aes(x=`2017`,y=`2020`),size=3,color="blue") + 
  xlab("Estimated import (2016 - red, 2017 - blue)") + ylab("Estimated importations - 2020")+
  theme(text=element_text(size=20))

# EstCasesArrivingByCountyAll %>%
#   pivot_wider(names_from = "Year",values_from = "CasesByCounty") %>%
#   ggplot(aes(x=`2017`,y=`2020`))+ geom_point(size=3,color="blue") + theme_bw()+
#   xlab("Estimated importations - 2017") + ylab("Estimated importations - 2020")+
#   theme(text=element_text(size=20))

corre2<-EstCasesArrivingByCountyAll %>%
  pivot_wider(names_from = "Year",values_from = "CasesByCounty") %>%
  ggplot(aes(x=`2017`,y=`2016`))+ geom_point(size=3,color="green4") + theme_bw()+
  xlab("Estimated importations - 2017") + ylab("Estimated importations - 2016")+
  theme(text=element_text(size=20))

plot_grid(corre1,corre2)

ggsave(last_plot(),file="Correlations.png",width = 15,height = 7)
#Arrivals 

# # losQueFaltan<-data.frame(Aeropuerto=c("Cincinnati/Northern Kentucky International",
# #                         "Daniel K Inouye International","Fresno Yosemite International",
# #                         "Harry Reid International","Metro Oakland International",
# #                         "Palm Springs International","St Louis Lambert International"),
# #            Zipcode=c("41018","96819","93727","89119","94502","92264","63145"))
# 
# volumeAndProbByFIPS2022<-volumeFlights2022 %>% separate(Airport,c("State","Name"),":") %>%
#   mutate(Name=str_sub(Name,start=2)) %>% select(Name,ActualEnPlaned2022) %>%
#   merge(arrivalFlights %>% mutate(Airport= str_sub(Airport, end = -2)) %>%
#               separate(Airport,c("State","Name"),sep="[(]") %>% select(Name,zipcode),
#         by="Name",all=T) %>% filter(!is.na(ActualEnPlaned2022)) %>% replace(is.na(.),0) %>%
#   mutate(zipcode=ifelse(Name %in% losQueFaltan$Aeropuerto,losQueFaltan$Zipcode,zipcode)) %>%
#   filter(zipcode!="0") %>% arrange(desc(ActualEnPlaned2022)) %>%
#   mutate(ProbOfArrival=ActualEnPlaned2022/sum(ActualEnPlaned2022)) %>% left_join(zip2fips) %>% 
#   select(FIPS,ActualEnPlaned2022,ProbOfArrival) %>% 
#   group_by(FIPS) %>% summarise_each(sum)

#Data frame for number of passengers arriving to each top 50 largest airports 
#in the USA, by county. Also, probability of arriving to each airport, given its ranking.
#This probability is calculated 
volumeAndProbByFIPS<-arrivalFlights %>% left_join(zip2fips) %>% select(FIPS,Passanger,probs) %>% 
  group_by(FIPS) %>% summarise_each(sum) %>% select(-Passanger)

#To expand data frames and get estimated number of travelers in
#each county (with airport) with Malaria.
malariaArrivalsByCountyFlight<-estTravsByFlightToUSA %>% merge(volumeAndProbByFIPS,by=NULL) %>%
  arrange(Country) %>% mutate(CasesByCounty=EstTravsWithMalaria*probs) %>%
  select(FIPS,CasesByCounty) %>% group_by(FIPS) %>% summarise_each(sum)

rbind(malariaArrivalsByCountyFlight %>% mutate(Source="Flight"),
      crossingIn2017USMX %>% ungroup() %>%
        select(FIPS,CasesByCounty=EstTravsWithMalariaBorder) %>%
        mutate(Source="Border")) %>% print(n=64)

#my_breaks = c(0.00000001,0.0000001,0.000001, 0.00001, 0.0001, 0.001, 0.01,0.1)
my_breaks = c(0.01,0.1,1,10,100,1000,10000)

countiesUSA %>% left_join(rbind(malariaArrivalsByCountyFlight %>% mutate(Source="Flight"),
      crossingIn2017USMX %>% ungroup() %>%
        select(FIPS,CasesByCounty=EstTravsWithMalariaBorder) %>%
        mutate(Source="Border")) %>% rename_at("FIPS",~"GEOID"))%>%
  filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  mutate(CasesByCounty=ifelse(is.na(CasesByCounty),0,CasesByCounty)) %>%
  ggplot()+ theme_void() +
  #  geom_sf(data=forGeometryOfCounties,fill="white",color="black") +
  #  geom_sf(data=statesUSA %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico")),
  #          fill=NA,color="red",linewidth=0.8)+
  geom_sf(aes(fill=(CasesByCounty)),color="gray") +
#  geom_point(data = malaria2016,aes(lon,lat),color="blue")+
  theme(legend.title = element_blank(),text=element_text(size=20))+
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks, labels = my_breaks,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="prob") +
  theme(legend.position = c(0.8,0.1))

ggsave(last_plot(),file="mapaArrivals.png")

malariaArrvials<-countiesUSA %>% 
  left_join(rbind(malariaArrivalsByCountyFlight %>% mutate(Source="Flight"),
                  crossingIn2017USMX %>% ungroup() %>%
                    select(FIPS,CasesByCounty=EstTravsWithMalariaBorder) %>%
                    mutate(Source="Border")) %>% rename_at("FIPS",~"GEOID")) %>%
  filter(!GEOID %in% c(alaska,hawaii,puertoR)) %>%
  mutate(CasesByCounty=ifelse(is.na(CasesByCounty),0,CasesByCounty)) %>%
  as_tibble() %>% select(FIPS=GEOID,malariaArrivals=CasesByCounty)

malariaArrvials %>% arrange(desc(malariaArrivals))

write_csv(malariaArrvials,file="malariaArrvials.csv")

malaria2017<-read_csv("Number_of_Reported_Malaria_Cases_by_County__United_States__2017.csv")
malaria2016<-read_csv("Number_of_Reported_Malaria_Cases_by_County__United_States__2016.csv")

uno<-malariaArrvials %>% 
  left_join(malaria2017 %>% select(FIPS,Importations2017=MAL_FREQ_2017)) %>%
  left_join(malaria2016 %>% select(FIPS,Importations2016=MAL_FREQ_2016)) %>%
  replace(is.na(.),0) %>% #filter(malariaArrivals>0) %>%
  pivot_longer(cols = contains("Import")) %>%
  ggplot(aes(x=malariaArrivals,y=value,color=name))+ theme_bw() +
  geom_point(size=3) + facet_wrap(~name) + ylab("Reported importations") + 
  xlab("Estimated arrivals (infected)")+
  theme(legend.position = "none",text=element_text(size=25)) +
  ggtitle("All counties")

dos<-malariaArrvials %>% 
  left_join(malaria2017 %>% select(FIPS,Importations2017=MAL_FREQ_2017)) %>%
  left_join(malaria2016 %>% select(FIPS,Importations2016=MAL_FREQ_2016)) %>%
  replace(is.na(.),0) %>% filter(malariaArrivals>0) %>%
  pivot_longer(cols = contains("Import")) %>%
  ggplot(aes(x=malariaArrivals,y=value,color=name))+ theme_bw() +
  geom_point(size=3) + facet_wrap(~name) + ylab("Reported importations") + 
  xlab("Estimated arrivals (infected)")+
  theme(legend.position = "none",text=element_text(size=25)) +
  ggtitle("Counties with airport (Top 50)")
  
fips2State<-countiesUSA %>% as_tibble() %>% select(FIPS=GEOID,NAME) %>%
  separate(NAME,c("County","STATE"),sep = ", ") %>%
  arrange(FIPS) %>% select(FIPS,STATE)

fips2State %>% filter(str_detect(STATE,"New")) %>% select(STATE) %>% unique()

library(ggrepel)

tres1<-malariaArrvials %>% 
  left_join(malaria2017 %>% select(FIPS,Importations2017=MAL_FREQ_2017)) %>%
  left_join(malaria2016 %>% select(FIPS,Importations2016=MAL_FREQ_2016)) %>%
  replace(is.na(.),0) %>% left_join(fips2State) %>%
  select(-FIPS) %>% group_by(STATE) %>% summarise_each(sum) %>%
  pivot_longer(cols=contains("Import")) %>% #filter(malariaArrivals>0) %>%
  ggplot(aes(x=malariaArrivals,y=value,color=name)) +
  geom_point(size=3) + facet_wrap(~name) + theme_bw() +
  geom_smooth(method = "lm") + #geom_label_repel(aes(label=STATE),size=5) +
  ylab("Reported importations") + xlab("Estimated arrivals (infected)")+
  theme(legend.position = "none",text=element_text(size=25)) +
  ggtitle("All states")

cuatro1<-malariaArrvials %>% 
  left_join(malaria2017 %>% select(FIPS,Importations2017=MAL_FREQ_2017)) %>%
  left_join(malaria2016 %>% select(FIPS,Importations2016=MAL_FREQ_2016)) %>%
  replace(is.na(.),0) %>% left_join(fips2State) %>%
  select(-FIPS) %>% group_by(STATE) %>% summarise_each(sum) %>%
  pivot_longer(cols=contains("Import")) %>% filter(malariaArrivals>0) %>%
  ggplot(aes(x=malariaArrivals,y=value,color=name)) +
  geom_point(size=3) + facet_wrap(~name) + theme_bw() +
  geom_smooth(method = "lm") + #geom_label_repel(aes(label=STATE),size=5) +
  ylab("Reported importations") + xlab("Estimated arrivals (infected)")+
  theme(legend.position = "none",text=element_text(size=25)) +
  ggtitle("States with airport (Top 50)")

plot_grid(uno,dos,tres1,cuatro1,tres,cuatro,ncol=2,labels = "AUTO",label_size = 33)

ggsave(last_plot(),file="correlationsTravel.png",width = 30,height = 20)

11001