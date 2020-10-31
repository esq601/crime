library(httr)
library(readr)
library(tidyverse)
library(pins)
library(jsonlite)
library(lubridate)
library(ggmap)
library(extrafont)
library(ggtext)

loadfonts(device = "win")

start_mo <- 1
end_mo <- 8


murder_plot <- function(city_name,title_name="Homicide", city_df, date_field, date_format = "%m/%d/%Y",category_field, category_verbiage,start_month = 1, end_month = 6,prepo = F) {
  
  date_field <- rlang::enquo(date_field)
  category_field <- rlang::enquo(category_field)
  
  if(prepo == F) {
    temp_df <- city_df %>%
      mutate(rpt_date = as.Date(!!date_field, format = date_format)) %>%
      mutate(year = as.numeric(year(rpt_date)),month = month(rpt_date),week=week(rpt_date)) %>%
      filter(!!category_field %in% category_verbiage) %>%
      filter(month <= end_month & month >= start_month) %>%
      group_by(year) %>%
      summarise(num = n())
  } else {
    temp_df <- city_df %>%
      filter(!!category_field %in% category_verbiage) %>%
      filter(month <= end_month & month >= start_month) %>%
      group_by(year) %>%
      summarise(num = sum(COUNT))
  }

  
  ggplot(temp_df, aes( x = year, y = num, fill = num)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks(n=5)) +
    scale_fill_gradient(low = "#5700c9", high = "#c90000", limits = c(0,max(temp_df$num))) +
    theme(
      plot.background = element_rect(fill = "#fff7fb"),
      legend.position = "none",
      text  = element_text(family = "Bahnschrift", size = 16),
      plot.title = element_text(size = 26),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey80")
    ) +
    labs(title = city_name, 
         subtitle =paste(title_name,"counts between the months of",month.name[start_month],"-",month.name[end_month],sep = " "),
         y = paste(title_name,"Police Reports",sep=" "), x = "Year")
}

murder_df <- function(city_name,title_name="Homicide", city_df, date_field, date_format = "%m/%d/%Y",category_field, category_verbiage,start_month = 1, end_month = 6,prepo = F) {
  
  date_field <- rlang::enquo(date_field)
  category_field <- rlang::enquo(category_field)
  
  if(prepo == F) {
    temp_df <- city_df %>%
      mutate(rpt_date = as.Date(!!date_field, format = date_format)) %>%
      mutate(year = as.numeric(year(rpt_date)),month = month(rpt_date),week=week(rpt_date)) %>%
      filter(!!category_field %in% category_verbiage) %>%
      filter(month <= end_month & month >= start_month) %>%
      group_by(year) %>%
      summarise(num = n())
  } else {
    temp_df <- city_df %>%
      filter(!!category_field %in% category_verbiage) %>%
      filter(month <= end_month & month >= start_month) %>%
      group_by(year) %>%
      summarise(num = sum(COUNT))
  }
  
  temp_df %>%
    mutate(city = city_name)
}

#set_config(config(ssl_verifypeer = 0L))

  #### Los Angeles ####

la_crime <- bind_rows( 
  content(GET("https://data.lacity.org/resource/63jg-8b9z.csv?%24limit=2200000",verbose())),
  content(GET("https://data.lacity.org/resource/2nrs-mtv8.csv?%24limit=1000000"))
)



murder_plot("Los Angeles","Murder",la_crime,date_rptd,"%Y-%m-%d",crm_cd_desc,"CRIMINAL HOMICIDE",start_mo,end_mo)




  #### Seattle ####
sea_crimes_act <- content(GET("https://data.seattle.gov/resource/tazs-3rd5.csv?$limit=1000000",verbose()))

#unique(sea_crimes_act$offense)

murder_plot("Seattle","Murder",sea_crimes_act,report_datetime, "",offense,"Murder & Nonnegligent Manslaughter",start_mo,end_mo)

  #### Phoenix ####

phoenix_crime <- content(GET("https://www.phoenixopendata.com/dataset/cc08aace-9ca9-467f-b6c1-f0879ab1a358/resource/0ce3411a-2fc6-4302-a33f-167f68608a20/download/crimestat.csv",verbose()))



murder_plot(city_name = "Phoenix",city_df = phoenix_crime, date_field = `OCCURRED ON`, date_format = "%m/%d/%Y",
            category_field = `UCR CRIME CATEGORY`,category_verbiage = "MURDER AND NON-NEGLIGENT MANSLAUGHTER",start_month = start_mo,end_month = end_mo)


  #### Atlanta ####

#You need to go here https://www.atlantapd.org/i-want-to/crime-data-downloads to download the Raw files in zip format and upload.  Sucks, i know.

atl_crimes_hist <- read_csv("~/COBRA-2009-2019.csv", 
                            col_types = cols(`Report Date` = col_date(format = "%Y-%m-%d")))

atl_crimes_hist <- atl_crimes_hist %>%
  select(rpt_date = `Report Date`, description = `UCR Literal`)


atl_crimes_curr <- read_csv("~/COBRA-2020.csv", 
                            col_types = cols(rpt_date = col_date(format = "%m/%d/%Y")))

atl_crimes_curr <- atl_crimes_curr %>%
  select(rpt_date, description = UC2_Literal)

atl_crimes <- bind_rows(atl_crimes_hist,atl_crimes_curr) %>%
  mutate(year = as.numeric(year(rpt_date)),month = month(rpt_date),week=week(rpt_date))
#colnames(atl_crimes)
#head(atl_crimes$rpt_date)
unique(atl_crimes$description)

murder_plot("Atlanta","Murder",atl_crimes,rpt_date, "",description,"HOMICIDE",start_mo,end_mo)


  #### Austin ####

aus_crimes <- content(GET("https://data.austintexas.gov/resource/fdj4-gpfu.csv?$limit=2500000", verbose()))
aus_budget <- content(GET("https://data.austintexas.gov/resource/yeeq-kk6v.csv?$limit=1000000"))

aus_budget1 <- aus_budget %>%
  filter(str_detect(dept_nm,"Police") == T) %>%
  filter(str_detect(unit_nm, "Region")==T) %>%
  filter(str_detect(unit_nm, "Patrol")==T) %>%
  group_by(fy,unit_nm) %>%
  summarise(dollars = sum(bud))

ggplot(aus_budget1) +
  geom_path(aes(x = fy, y = dollars, group = unit_nm, color = unit_nm))

top_crimes <- aus_crimes %>%
  group_by(crime_type) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

test <- aus_crimes %>%
  filter(category_description %in% "Murder") %>%
  mutate(month = month(rep_date), year = year(rep_date)) %>%
  filter(month <=7 & month >=6) %>%
  group_by(year) %>%
  summarise(num = n())
unique(aus_crimes$district)

aus_crimes1 <- aus_crimes %>%
  #filter(category_description %in% "Murder") %>%
  #filter(str_detect(crime_type,"DWI") == T) %>%
  filter(family_violence == "Y") %>%
  filter(week(rep_date_time) <= week(Sys.Date())) %>%
  mutate(sector = str_sub(sector,1,2)) %>%
  group_by(year = year(rep_date_time),sector) %>%
  summarise(num = n()) %>%
  group_by(c_dist = as.character(sector)) %>%
  mutate(pct_chng = num / lag(num)) %>%
  filter(is.na(c_dist) == F) %>%
  group_by(sector) %>%
  filter(num > 100)

#ggplot(subset(aus_crimes1,zip_code == "78749")) +
ggplot(aus_crimes1) +
  geom_path(aes(x = year, y = num, group = c_dist, color = c_dist),size=1.25) +
  scale_color_brewer(type = "qual", palette = 8)
unique(aus_crimes$occ_date)


murder_plot("Austin","Murder",aus_crimes,rep_date_time, "%Y-%m-%d",category_description,"Murder",start_mo,end_mo)


  #### Philadelphia ####
#philly_crime <- content(GET("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2",verbose()))
#download not working, had to get manually 3aug20
philly_crime <- read_csv("~/incidents_part1_part2.csv")

unique(philly_crime$text_general_code)


murder_plot("Philadelphia","Murder",philly_crime,dispatch_date,"",text_general_code,"Homicide - Criminal",start_mo,end_mo)


  #### Chicago ####

chi_crimes <- content(GET("https://data.cityofchicago.org/resource/crimes.csv?$limit=100000&$select=year,date_extract_m(date)%20as%20month,date_extract_woy(date)%20as%20week_of_year,primary_type,arrest,COUNT%28*%29&$group=year,month,week_of_year,primary_type,arrest", verbose()))


chi_crimes
unique(chi_crimes$primary_type)
filter(chi_crimes, primary_type == "CRIMINAL SEXUAL ASSAULT")


p1 <-  murder_plot("Chicago","Gun Crime",chi_crimes,category_field = primary_type,category_verbiage =c("HOMICIDE"),start_month =  start_mo,end_month = end_mo,prepo=T)

p1
#c("CRIMINAL SEXUAL ASSAULT","CRIM SEXUAL ASSAULT")

  #### DC ####


dc09<-  read_csv("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.csv") #2009
dc10<-  read_csv("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.csv") #2010
dc11<-  read_csv("https://opendata.arcgis.com/datasets/9d5485ffae914c5f97047a7dd86e115b_35.csv") #2011
dc12<-  read_csv("https://opendata.arcgis.com/datasets/010ac88c55b1409bb67c9270c8fc18b5_11.csv") #2012
dc13<-  read_csv("https://opendata.arcgis.com/datasets/5fa2e43557f7484d89aac9e1e76158c9_10.csv") #2013
dc14<-  read_csv("https://opendata.arcgis.com/datasets/6eaf3e9713de44d3aa103622d51053b5_9.csv")#2014
dc15<-  read_csv("https://opendata.arcgis.com/datasets/35034fcb3b36499c84c94c069ab1a966_27.csv") #2015
dc16<-  read_csv("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.csv") #2016
dc17<-  read_csv("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.csv") #2017
dc18<-  read_csv("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.csv") #2018
  #read_csv("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.csv"), #2019
dc20<-  read_csv("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.csv") #2020

# dc_crime19 <- jsonlite::fromJSON("https://maps2.dcgis.dc.gov/dcgis/rest/services/FEEDS/MPD/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json") 
# 
# dc_crime19$features$attributes
# result_offset <- 100

dc_crime19  <- jsonlite::fromJSON(paste0("https://maps2.dcgis.dc.gov/dcgis/rest/services/FEEDS/MPD/MapServer/1/query?where=OFFENSE%20%3D%20%27HOMICIDE%27&outFields=*&outSR=4326&f=json"))

dc19 <- dc_crime19$features$attributes
dc_crime <- rbind(dc09,dc10,dc11,dc12,dc13,dc14,dc15,dc16,dc17,dc18,dc20)


dc_crime1 <- dc_crime %>%
  select(REPORT_DAT,OFFENSE) %>%
  mutate(REPORT_DAT = as.Date(REPORT_DAT))

dc_crime2 <- dc19 %>%
  select(REPORT_DAT,OFFENSE) %>%
  mutate(REPORT_DAT = as.Date(as.POSIXct(REPORT_DAT/1000, origin = "1970-01-01")))
str(dc_crime2)

dc_crime3 <- bind_rows(dc_crime1,dc_crime2)
#rgdal::readOGR("~/Crime_Incidents_in_2019.kml")

# dc_crime19prop <- dc_crime19 %>%
#   mutate(X = LONGITUDE, Y = LATITUDE) %>%
#   mutate_at(c("WARD","DISTRICT","PSA"), as.numeric)
# 
# dc_crime1 <- bind_rows(dc_crime,dc_crime19prop)

unique(dc_crime1$OFFENSE)


murder_plot("Washington DC","Homicide",dc_crime3,REPORT_DAT,"%Y/%m/%d",OFFENSE,"HOMICIDE",start_mo,end_mo)


  #### San Fransisco ####

sf_crime_hist <- content(GET("https://data.sfgov.org/resource/tmnf-yvry.csv?$limit=3000000",verbose()))

sf_crime_curr <- content(GET("https://data.sfgov.org/resource/wg3w-h783.csv?$limit=1000000",verbose()))

unique(sf_crime_hist$category)
#colnames(sf_crime_curr)
sf_crime <- bind_rows(
  select(sf_crime_hist, date = date, category = category),
  select(sf_crime_curr, date = report_datetime, category = incident_category)
)


murder_plot("San Fransisco","Murder",sf_crime,date,"",category,c("HOMICIDE","Homicide"),4,6)

  #### Minneapolis ####

mn_crime_old <- bind_rows(
  read_csv("https://opendata.arcgis.com/datasets/6d08a19ce96b42ab844bcf08c70e5480_0.csv"), #2010
  read_csv("https://opendata.arcgis.com/datasets/657ebd6b1af14af296e1004ed02080d8_0.csv"), #2011
  read_csv("https://opendata.arcgis.com/datasets/83508d169ebb48199273d21fef90fb30_0.csv"), #2012
  read_csv("https://opendata.arcgis.com/datasets/944cbb45a5fd4f6dbee2f7136f166184_0.csv"), #2013
  read_csv("https://opendata.arcgis.com/datasets/f0279f3673394c66a96c03e6e42287f4_0.csv"), #2014
  read_csv("https://opendata.arcgis.com/datasets/08ff2c3bec594dd2a7a8566b2a81d452_0.csv"), #2015
  read_csv("https://opendata.arcgis.com/datasets/0b12e290edb64816a7cd5270fdd6bacb_0.csv"), #2016
  read_csv("https://opendata.arcgis.com/datasets/3d33a4f94a004fb5816936708642e045_0.csv"), #2017
  read_csv("https://opendata.arcgis.com/datasets/58e6f399e0f04c568b3ba45086d15818_0.csv") #2018
  )

mn_crime_new <- bind_rows(
  read_csv("https://opendata.arcgis.com/datasets/8cd15449ac344aa5a55be7840d67c52d_0.csv"), #2019
  read_csv("https://opendata.arcgis.com/datasets/35c7de976a60450bb894fc7aeb68aef6_0.csv") #2020
)

mn_crime <- bind_rows(
  select(mn_crime_old, date = ReportedDate, offense = Offense),
  select(mn_crime_new, date = reportedDateTime, offense)
)

unique(mn_crime$offense)

murder_plot("Minneapolis","Murder",mn_crime,date,"%Y/%m/%d",offense,"MURDR",start_mo,end_mo)

  #### Kansas City ####

kc_crime_old <- bind_rows(
  read_csv("https://data.kcmo.org/resource/yu5f-iqbp.csv?$limit=1000000"), #2014
  read_csv("https://data.kcmo.org/resource/kbzx-7ehe.csv?$limit=1000000"), #2015
  read_csv("https://data.kcmo.org/resource/wbz8-pdv7.csv?$limit=1000000"), #2016
  read_csv("https://data.kcmo.org/resource/98is-shjt.csv?$limit=1000000"), #2017
  read_csv("https://data.kcmo.org/resource/dmjw-d28i.csv?$limit=1000000")#2018
  )
kc_crime_19 <-  read_csv("https://data.kcmo.org/resource/pxaa-ahcm.csv?$limit=1000000") #2019
kc_crime_20 <-  read_csv("https://data.kcmo.org/resource/vsgj-uufz.csv?$limit=1000000") #2020

test <- kc_crime_19 %>%
  filter(description == "Murder")

#unique(kc_crime_old$involvement_1)

kc_crime <- bind_rows(
  select(kc_crime_old, date = reported_date, offense = description, type = involvement_1, type1 = involvement),
  select(kc_crime_19, date = reported_date, offense = description, type = involvement, type1 = involvement),
  select(kc_crime_20, date = reported_date, offense = description, type = involvement, type1 = involvement)
) %>%
  filter(type == "VIC" | type1 == "VIC")
unique(kc_crime_old$description)

kc_dv <- bind_rows(
  select(kc_crime_19, date = reported_date, offense = description, type = involvement, type1 = involvement,dvflag),
  select(kc_crime_20, date = reported_date, offense = description, type = involvement, type1 = involvement,dvflag)
  
) %>%
  filter(type == "VIC" | type1 == "VIC") %>%
  group_by(year(date),dvflag) %>%
  filter(month(date) >=4 & month(date) <=6) %>%
  summarise(num = n()) %>%
  group_by(dvflag) %>%
  mutate(pct = num/lag(num))

unique(kc_crime$offense)
#c("HOMICIDE/Non Neglige","Murder")
unique(str_subset(kc_crime$offense,"(?i)homicide|murder"))


murder_plot("Kansas City","Murder",kc_crime,date,"%Y/%m/%d",offense,c("HOMICIDE/Non Neglige","Murder"),start_mo,end_mo)

#### STL ####
path <- "~/stlcrime/"
stl_crimeu <-  c(paste0(path,list.files(pattern = "*.csv",path =path,all.files = T)),
                 paste0(path,list.files(pattern = "*.CSV",path =path,all.files = T))
)


stl_crime <-  map_df(stl_crimeu,read_csv,col_types = cols(.default = "c"))

as.Date(max(stl_crime$DateOccur,na.rm=T), format = "%m/%d/%Y")

stl_crime1 <- bind_rows(
  select(mutate(filter(stl_crime,is.na(DateOccured) == F), date = as.Date(DateOccured, format = "%m/%d/%Y")),date, description = Description),
  select(mutate(filter(stl_crime,is.na(DateOccur) == F), date = as.Date(DateOccur, format = "%m/%d/%Y")),date, description = Description),
  select(mutate(filter(stl_crime,is.na(`Date Occur`) == F), date = as.Date(`Date Occur`, format = "%m/%d/%Y")),date, description = Description)
)

stl_crime2 <- stl_crime1 %>%
  filter(date >= as.Date("2008-01-01"))



murder_plot("Saint Louis","Murder",stl_crime2,date,"",description,"HOMICIDE",start_mo,end_mo)

#### Nashville ####

n13<-  read_csv("https://data.nashville.gov/resource/hsz5-g34u.csv?$limit=1000000") #2013
n14<-  read_csv("https://data.nashville.gov/resource/sguy-xf8k.csv?$limit=1000000") #2014
n15<-  read_csv("https://data.nashville.gov/resource/ce74-dvvv.csv?$limit=1000000") #2015
n16<-  read_csv("https://data.nashville.gov/resource/tpvn-3k6v.csv?$limit=1000000") #2016
n17<-  read_csv("https://data.nashville.gov/resource/ei8z-vngg.csv?$limit=1000000") #2017
n18<-  read_csv("https://data.nashville.gov/resource/we5n-wkcf.csv?$limit=1000000")#2018
n19<-  read_csv("https://data.nashville.gov/resource/a88c-cc2y.csv?$limit=1000000") #2019
n20<-  read_csv("https://data.nashville.gov/resource/sie3-y9k4.csv?$limit=1000000") #2020


n14 <- n14 %>%
  mutate(weapon_primary = as.numeric(weapon_primary))


n16 <- n16 %>%
  mutate(weapon_primary = as.numeric(weapon_primary))
n17 <- n17 %>%
  mutate(weapon_primary = as.numeric(weapon_primary))
n18 <- n18 %>%
  mutate(weapon_primary = as.numeric(weapon_primary))
n19 <- n19 %>%
  mutate(weapon_primary = as.numeric(weapon_primary))
n20 <- n20 %>%
  mutate(weapon_primary = as.numeric(weapon_primary))
str(n17$weapon_primary)
nash_crime <- bind_rows(n13,n14,n15,n16,n17,n18,n19,n20) %>%
  select(incident_number,incident_reported,offense_description) %>%
  distinct() %>%
  filter(year(incident_reported) >= 2013)
unique(str_subset(n20$offense_description,"(?i)homicide|murder"))

murder_plot("Nashville","Homicide",nash_crime,incident_reported,"",
            offense_description,c("HOMICIDE- CRIMINALLY NEGLIGENT","HOMICIDE- CRIMINAL","HOMICIDE"),start_mo,end_mo)


#### Jacksonville ####

#https://transparency.jaxsheriff.org/HOTS/Murder

jacks <- read_csv("~/jacksonsville_homicide.csv") %>%
  mutate(crime = "murder")




murder_plot("Jacksonville","Homicide",jacks,IncidentDate,"%m/%d/%Y",crime,"murder",start_mo,end_mo)

#####  Denver ####

denv <- read_csv("https://www.denvergov.org/media/gis/DataCatalog/crime/csv/crime.csv")
denv_off <- read_csv("https://www.denvergov.org/media/gis/DataCatalog/crime/csv/offense_codes.csv")

str_subset(unique(denv$OFFENSE_CATEGORY_ID),"hom")



#### Portland ####
port15 <- read_csv("https://public.tableau.com/views/PPBOpenDataDownloads/CrimeData-2015.csv?:showVizHome=no")
port16 <- read_csv("https://public.tableau.com/views/PPBOpenDataDownloads/CrimeData-2016.csv?:showVizHome=no")
port17 <- read_csv("https://public.tableau.com/views/PPBOpenDataDownloads/CrimeData-2017.csv?:showVizHome=no")
port18 <- read_csv("https://public.tableau.com/views/PPBOpenDataDownloads/CrimeData-2018.csv?:showVizHome=no")
port19 <- read_csv("https://public.tableau.com/views/PPBOpenDataDownloads/CrimeData-2019.csv?:showVizHome=no")

port20 <- read_csv("https://public.tableau.com/views/PPBOpenDataDownloads/CrimeData-2020.csv?:showVizHome=no")

port <- bind_rows(
  port15,port16,port17,port18,port19,port20
) %>%
  mutate(ReportDate = as.Date(ReportDate, "%m/%d/%Y"))

unique(port$OffenseType)

port_mrd <- port %>%
  filter(OffenseType == "Murder and Non-negligent Manslaughter")


#### Saves ####




