library(tidyverse)
library(jsonlite)
library(lubridate)
library(ckanr)
library(scales)

# exported file formats are slightly different, so set to "iPhone" or "Android" here
file_exported_from <- "iphone"


#### DAILY CASES (SCOTLAND LEVEL OPEN DATA) ####
ckanr_setup(url = "https://www.opendata.nhs.scot/")
covid_cases_resource <- resource_show(id = "287fc645-4352-4477-9c8c-55bc054b7e76") # daily/cumulative cases
covid_cases <- ckan_fetch(x=covid_cases_resource$url) %>%
  rename(new_cases = DailyCases,
         date = Date) %>%
  select(date, new_cases) %>%
  mutate(date = ymd(date))


#### DAILY DOWNLOADED POSITIVE KEYS FROM PROTECT.SCOT APP ####
# json files exported from iOS "Exposure Notifications" Settings pane or Android Google settings

data_dir <- "json_exposure_files" # folder containing .json exported files

# read in files
files <- file.path(data_dir, dir(data_dir)) 
for(file in files){
  if(file == files[1]){df <- tibble()}
  file_data <- fromJSON(file)

  if(toupper(file_exported_from) == "IPHONE"){
    for(i in 1:length(file_data$ExposureChecks$Timestamp)){
      df <- df %>% 
        bind_rows(tibble(file_data$ExposureChecks$Files[[i]]["Hash"] ,
                         file_data$ExposureChecks$Files[[i]]["Timestamp"],
                         file_data$ExposureChecks$Files[[i]]["MatchCount"],
                         file_data$ExposureChecks$Files[[i]]["KeyCount"]) %>%
                    rename(hash = Hash, timestamp = Timestamp, match_count = MatchCount, key_count = KeyCount) %>% 
                    select(hash, timestamp, match_count, key_count) %>% 
                    mutate(timestamp = as_datetime(timestamp)) )
    }
  }

  if(toupper(file_exported_from) == "ANDROID"){
    df <- df %>%
      bind_rows(as_tibble(file_data) %>%
                  rename(key_count = keyCount, match_count = matchesCount) %>%
                  select(hash, timestamp, match_count, key_count) %>%
                  mutate(timestamp = dmy_hms(timestamp)) )
  }
  rm(file_data)
}

# combine json data, format and remove duplicate entries -- likely many as files from subsequent
# days will overlap 13 days of data.
# (provided key count = positive cases entered into app (and their key subsequently downloaded))
df <- df %>% 
  arrange(desc(timestamp)) %>%
  mutate(date = date(timestamp)) %>%
  distinct()

# summarise to day level (as more than one key download batch per day) and join to cases data
combined_daily <- df %>% 
  group_by(date) %>%
  summarise(key_count = sum(key_count)) %>%
  full_join(covid_cases) %>%
  arrange(date) %>%
  filter(date >= min(df$date, na.rm = TRUE))


#### PLOTS ####
# below chunk optional for different fonts etc
library(showtext)
font_add_google(name = "Lato") # https://fonts.google.com
showtext_auto()

# new daily cases / keys
combined_daily %>%
  pivot_longer(c(new_cases, key_count), names_to = "case_type", values_to = "count") %>%
  mutate(case_type = if_else(case_type == "key_count", "new keys", "new cases")) %>%
  ggplot(aes(x = date, y = count, fill = case_type)) +
  geom_col(width = 1, position = "identity") +
  scale_fill_manual(values = c("#77bedb",
                               rgb(107, 17, 165, maxColorValue = 255, alpha = 220))) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     text = element_text(family = "Lato"),
                     legend.position = "bottom",
                     legend.box.margin = margin(-10, 0, 0, 0),
                     axis.text = element_text(face = "bold", size = 10),
                     plot.title = element_text(face = "bold", size = 16),
                     plot.subtitle = element_text(size = 11, margin=margin(0, 0, 10, 0)),
                     legend.text = element_text(face = "bold", size = 9)) +
  labs(x = "", y = "", fill = "", 
       title = "Scotland COVID-19 Cases & Keys", 
       subtitle = expression(paste(bold("new cases "), "and", bold(" new keys "), "(downloaded by the ", italic("protect.scot")," app)")))

ggsave("pics/plot_cases_keys.png", dpi = 300, width = 200, height = 133, units = "mm")


# cumulative cases / keys
min_date <- filter(df, key_count > 0)$date %>% min(na.rm = TRUE) # date of first non-zero key count
min_date <- dmy("17/09/2020") # date the app reached 1M downloads

combined_daily %>%
  filter(date >= min_date) %>%
  replace_na(list(key_count = 0, new_cases = 0)) %>%
  mutate(cum_key_count = cumsum(key_count),
         cum_new_cases = cumsum(new_cases)) %>%
  pivot_longer(c(cum_new_cases, cum_key_count), names_to = "case_type", values_to = "count") %>%
  mutate(case_type = if_else(case_type == "cum_key_count", "cumulative new keys", "cumulative new cases")) %>%
  ggplot(aes(x = date, y = count, fill = case_type)) +
  geom_col(width = 1, position = "identity") +
  scale_fill_manual(values = c("#77bedb",
                               rgb(107, 17, 165, maxColorValue = 255, alpha = 220))) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     text = element_text(family = "Lato"),
                     legend.position = "bottom",
                     legend.box.margin = margin(-10, 0, 0, 0),
                     axis.text = element_text(face = "bold", size = 10),
                     plot.title = element_text(face = "bold", size = 16),
                     plot.subtitle = element_text(size = 11, margin=margin(0, 0, 10, 0)),
                     legend.text = element_text(face = "bold", size = 9)) +
  labs(x = "", y = "", fill = "", 
       title = "Scotland COVID-19 Cases & Keys",
       subtitle = expression(paste(bold("cumulative new cases "), "and", bold(" cumulative new keys "), "(downloaded by the ", italic("protect.scot")," app)")))

ggsave("pics/plot_cum_cases_keys.png", dpi = 300, width = 200, height = 133, units = "mm")
