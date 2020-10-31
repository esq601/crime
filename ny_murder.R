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

nypd_hist <- content(GET("https://data.cityofnewyork.us/resource/qgea-i56i.csv?LAW_CAT_CD=FELONY&%24limit=4000000",verbose()))

nypd_curr <- content(GET("https://data.cityofnewyork.us/resource/5uac-w243.csv?LAW_CAT_CD=FELONY&%24limit=4000000",verbose()))


unique(nypd_curr$ofns_desc)

nypd_total <- bind_rows(
  nypd_hist,
  select(nypd_curr,-geocoded_column)
) %>%
  filter(ofns_desc == "MURDER & NON-NEGL. MANSLAUGHTER") %>%
  mutate(month = floor_date(cmplnt_fr_dt,"month")) %>%
  group_by(month, vic_sex) %>%
  summarise(num = n()) %>%
  filter(month >= as.Date("2006-01-01"))


ggplot(nypd_total, aes(x = month, y = num, color = vic_sex)) +
  geom_path()



nypd_total <- bind_rows(
  nypd_hist,
  select(nypd_curr,-geocoded_column)
) %>%
  filter(ofns_desc == "MURDER & NON-NEGL. MANSLAUGHTER") %>%
  mutate(year = year(cmplnt_fr_dt)) %>%
  select(year, susp_race, vic_race) %>%
  mutate(susp_race = case_when(
    is.na(susp_race) == T ~ "Unknown",
    T ~ susp_race
  )) %>%
  group_by(susp_race,vic_race) %>%
  summarise(num = n()) %>%
  #filter(year >= 2006) %>%
  filter(susp_race %in% c("WHITE","BLACK"), vic_race %in% c("WHITE","BLACK"))



ggplot(nypd_total, aes(x = susp_race, y = vic_race)) + 
  #geom_point(aes(color = num,size = num)) +
  geom_text(aes(label = scales::comma(num), size = num, color = num),family = "Bahnschrift") +
  scale_size_continuous(range = c(5,30)) +
  scale_color_distiller(type = "div", palette = 2) +
  theme_minimal() +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 20),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold")
    ) +
  labs(
    title = "NYC Murder Demographic Aggregate",
    subtitle = "2006 through 2020",
    y =  "Victim",
    x = "Suspect"
  ) +
  coord_fixed()
