start_mo <- 1Q
end_mo <- 8

#df_ptl <- murder_df("Portland*","Murder",port,ReportDate,"%m/%d/%Y",OffenseType,"Murder and Non-negligent Manslaughter",start_mo,end_mo)
df_la <- murder_df("Los Angeles","Murder",la_crime,date_rptd,"%Y-%m-%d",crm_cd_desc,"CRIMINAL HOMICIDE",start_mo,end_mo)
df_sea <- murder_df("Seattle","Murder",sea_crimes_act,report_datetime, "",offense,"Murder & Nonnegligent Manslaughter",start_mo,end_mo)
df_phx <- murder_df(city_name = "Phoenix",city_df = phoenix_crime, date_field = `OCCURRED ON`, date_format = "%m/%d/%Y",
                    category_field = `UCR CRIME CATEGORY`,category_verbiage = "MURDER AND NON-NEGLIGENT MANSLAUGHTER",start_month = start_mo,end_month = end_mo)
df_atl <- murder_df("Atlanta","Murder",atl_crimes,rpt_date, "",description,"HOMICIDE",start_mo,end_mo)
df_aus <- murder_df("Austin","Murder",aus_crimes,rep_date_time, "%Y-%m-%d",category_description,"Murder",start_mo,end_mo)
df_phl <- murder_df("Philadelphia","Murder",philly_crime,dispatch_date,"",text_general_code,"Homicide - Criminal",start_mo,end_mo)
df_chi <- murder_df("Chicago","Gun Crime",chi_crimes,category_field = primary_type,category_verbiage ="HOMICIDE",start_month =  start_mo,end_month = end_mo,prepo=T)
df_dc <- murder_df("Washington DC","Homicide",dc_crime3,REPORT_DAT,"%Y/%m/%d",OFFENSE,"HOMICIDE",start_mo,end_mo)
df_sf <- murder_df("San Fransisco","Murder",sf_crime,date,"",category,c("HOMICIDE","Homicide"),start_mo,end_mo)
df_min<-murder_df("Minneapolis","Murder",mn_crime,date,"%Y/%m/%d",offense,"MURDR",start_mo,end_mo)
df_kc <- murder_df("Kansas City","Murder",kc_crime,date,"%Y/%m/%d",offense,c("HOMICIDE/Non Neglige","Murder"),start_mo,end_mo)
df_stl <- murder_df("Saint Louis*","Murder",stl_crime2,date,"",description,"HOMICIDE",start_mo,end_mo)
df_nsh <- murder_df("Nashville","Homicide",nash_crime,incident_reported,"",
                    offense_description,c("HOMICIDE- CRIMINALLY NEGLIGENT","HOMICIDE- CRIMINAL","HOMICIDE"),start_mo,end_mo)
df_jax <- murder_df("Jacksonville","Homicide",jacks,IncidentDate,"%m/%d/%Y",crime,"murder",start_mo,end_mo)
df_den <- murder_df("Denver","Murder",denv,REPORTED_DATE,"%m/%d/%Y",OFFENSE_CATEGORY_ID,"murder",start_mo,end_mo)









save(file = paste0("crimesummary",Sys.Date(),".Rdata"),df_atl,df_aus,df_chi,df_den,df_stl,df_jax,df_dc,df_kc,df_min,df_nsh,df_phx,df_sea,df_phl,df_la)


df_combine <- bind_rows(df_atl,df_aus,df_stl,df_chi,df_dc,df_den,df_jax,df_kc,df_min,df_nsh,df_phx,df_sea,df_phl,df_la) #df_ptl,

df_combine1 <- df_combine %>%
  filter(year >= 2011) %>%
  mutate(city = as.factor(city))

df_combine1$city <- fct_reorder(df_combine1$city,df_combine1$num)

ggplot(df_combine1, aes( x = year, y = num, fill = forcats::fct_reorder(city,num))) +
  geom_bar(stat = "identity") +
  #geom_path(size = 1.5) +
  theme_minimal() +
  geom_blank(aes(y = 0)) +
  ggsci::scale_fill_simpsons() +
  facet_wrap(~forcats::fct_reorder(city,-num), scales = "free_y",ncol = 3)+
  scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  theme(
    plot.background = element_rect(fill = "#fff7fb"),
    legend.position = "none",
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 26),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.title.x = element_blank(),
    plot.caption = element_text(color = "grey40")
  ) +
  labs(title = "Homicides in Major US Cities", 
       subtitle = "Total number through July",
       y = "Number of Homicides",
       caption= "* data available through June")
