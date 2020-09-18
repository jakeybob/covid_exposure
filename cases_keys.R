library(tidyverse)
library(jsonlite)
library(lubridate)
library(ckanr)


#### DAILY CASES (SCOTLAND LEVEL OPEN DATA) ####
ckanr_setup(url = "https://www.opendata.nhs.scot/")
covid_cases_resource <- resource_show(id = "287fc645-4352-4477-9c8c-55bc054b7e76") # daily/cumulative cases
covid_cases <- ckan_fetch(x=covid_cases_resource$url) %>%
  rename(new_cases = DailyCases,
         date = Date) %>%
  select(date, new_cases) %>%
  mutate(date = ymd(date))


#### DAILY DOWNLOADED POSITIVE KEYS FROM PROTECT.SCOT APP ####
# json files exported from iOS "Exposure Notifications" Settings pane

data_dir <- "json_exposure_files" # folder containing .json exported files

# read in files
files <- file.path(data_dir, dir(data_dir)) 
for(file in files){
  if(file == files[1]){df <- tibble()}
  file_data <- fromJSON(file)
  for(i in 1:length(file_data$ExposureChecks$Timestamp)){
    df <- df %>% 
      bind_rows(tibble(file_data$ExposureChecks$Files[[i]]["Hash"] ,
                       file_data$ExposureChecks$Files[[i]]["Timestamp"],
                       file_data$ExposureChecks$Files[[i]]["MatchCount"],
                       file_data$ExposureChecks$Files[[i]]["KeyCount"]))
  }
  rm(file_data)
}

# combine json data, format and remove duplicate entries -- likely many as files from subsequent
# days will overlap 13 days of data.
# (provided key count = positive cases entered into app (and their key subsequently downloaded))
df <- df %>% 
  rename(hash = Hash, timestamp = Timestamp, match_count = MatchCount, key_count = KeyCount) %>%
  mutate(timestamp = as_datetime(timestamp)) %>%
  arrange(desc(timestamp)) %>%
  distinct()

# summarise to day level (as more than one key download batch per day) and join to cases data
combined <- df %>% 
  mutate(date = date(timestamp)) %>%
  group_by(date) %>%
  summarise(key_count = sum(key_count)) %>%
  left_join(covid_cases)


#### PLOTS ####
# below chunk optional for different fonts etc
# library(showtext)
# font_add_google(name = "Lato") # https://fonts.google.com
# showtext_auto()

combined %>%
  pivot_longer(c(new_cases, key_count), names_to = "case_type", values_to = "count") %>%
  mutate(case_type = if_else(case_type == "key_count", "new keys", "new cases")) %>%
  ggplot(aes(x = date, y = count, fill = case_type)) +
  geom_col(width = 1, position = "identity") +
  scale_fill_manual(values = c("#77bedb",
                               rgb(107, 17, 165, maxColorValue = 255, alpha = 220))) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     text = element_text(family = "Lato"),
                     legend.position = "bottom",
                     legend.box.margin = margin(-10, 0, 0, 0),
                     axis.text = element_text(face = "bold", size = 10),
                     plot.title = element_text(face = "bold", size = 16),
                     plot.subtitle = element_text(size = 11, margin=margin(0, 0, 10, 0)),
                     legend.text = element_text(face = "bold", size = 9)) +
  labs(x = "", y = "", fill = "", 
       title = "Scotland COVID-19 Cases", 
       subtitle = "new cases, and new keys downloaded by the protect.scot app")

ggsave("pics/plot_cases_keys.png", dpi = 300, width = 200, height = 133, units = "mm")
