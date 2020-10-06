library(tidyverse)
library(jsonlite)
library(lubridate)
library(ckanr)
library(scales)
source("UTILS.R")

#### DAILY CASES (SCOTLAND LEVEL OPEN DATA) ####
ckanr_setup(url = "https://www.opendata.nhs.scot/")
covid_cases_resource <- resource_show(id = "287fc645-4352-4477-9c8c-55bc054b7e76") # daily/cumulative cases
covid_cases <- ckan_fetch(x=covid_cases_resource$url) %>%
  rename(new_cases = DailyCases,
         date = Date) %>%
  select(date, new_cases) %>%
  mutate(date = ymd(date))


#### DAILY DOWNLOADED POSITIVE KEYS FROM PROTECT.SCOT APP ####
df <- import_exposure_keys()

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
theme_set(theme_bw() + theme(panel.grid.minor = element_blank(),
                               text = element_text(family = "Lato"),
                               legend.position = "bottom",
                               legend.box.margin = margin(-10, 0, 0, 0),
                               axis.text = element_text(face = "bold", size = 10),
                               plot.title = element_text(face = "bold", size = 16),
                               plot.subtitle = element_text(size = 11, margin=margin(0, 0, 10, 0)),
                               legend.text = element_text(face = "bold", size = 9)))

# new daily cases / keys
combined_daily %>%
  pivot_longer(c(new_cases, key_count), names_to = "case_type", values_to = "count") %>%
  mutate(case_type = if_else(case_type == "key_count", "new keys", "new cases")) %>%
  ggplot(aes(x = date, y = count, fill = case_type)) +
  geom_col(width = 1, position = "identity") +
  scale_fill_manual(values = c("#77bedb",
                               rgb(107, 17, 165, maxColorValue = 255, alpha = 220))) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "", fill = "", 
       title = "Scotland COVID-19 Cases & Keys", 
       subtitle = expression(paste(bold("new cases "), "and", bold(" new keys "), "(downloaded by the ", italic("protect.scot")," app)")))

ggsave("pics/plot_cases_keys.png", dpi = 300, width = 200, height = 133, units = "mm")


# cumulative cases / keys
# min_date <- filter(df, key_count > 0)$date %>% min(na.rm = TRUE) # date of first non-zero key count
min_date <- dmy("17/09/2020") # date the app reached 1M downloads

# ratio of cumulative cases to keys
combined_daily %>%
  filter(date >= min_date) %>%
  mutate(cum_key_count = cumsum(key_count),
         cum_new_cases = cumsum(new_cases),
         ratio = cum_key_count/cum_new_cases) %>% 
  filter(date != max(date)) %>% # most decent entry may have quite incomplete case data
  ggplot(aes(x = date, y = ratio)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# trend of cumluative cases and keys
combined_daily %>%
  filter(date >= min_date) %>%
  # replace_na(list(key_count = 0, new_cases = 0)) %>%
  mutate(cum_key_count = cumsum(key_count),
         cum_new_cases = cumsum(new_cases)) %>%
  pivot_longer(c(cum_new_cases, cum_key_count), names_to = "case_type", values_to = "count") %>%
  mutate(case_type = if_else(case_type == "cum_key_count", "cumulative new keys", "cumulative new cases")) %>%
  ggplot(aes(x = date, y = count, fill = case_type)) +
  geom_col(width = 1, position = "identity") +
  scale_fill_manual(values = c("#77bedb",
                               rgb(107, 17, 165, maxColorValue = 255, alpha = 220))) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "", fill = "", 
       # title = "Scotland COVID-19 Cases & Keys",
       title = "Scotland COVID-19 Cases & Keys Since 1M Downloads",
       subtitle = expression(paste(bold("cumulative new cases "), "and", bold(" cumulative new keys "), "(downloaded by the ", italic("protect.scot")," app)")))

ggsave("pics/plot_cum_cases_keys_sep17.png", dpi = 300, width = 200, height = 133, units = "mm")

# #### CORRELATION ####
# # strip out NA and dates with incomplete data
# df_stripped <- combined_daily %>%
#   filter(is.na(new_cases)==F) %>%
#   filter(date >= min_date) %>%
#   filter(date != max(date))
# 
# # construct correlation + conf intervals over sliding daily window (need at least 4 days worth for conf.ints)
# for(day_slide in 1:(dim(df_stripped)[1]-4)){
#   if(day_slide == 1){
#     cor_vec <- numeric(length = (dim(df_stripped)[1]-4))
#     upper_95 <- numeric(length = (dim(df_stripped)[1]-4))
#     lower_95 <- numeric(length = (dim(df_stripped)[1]-4))
#     }
# 
#   vals <- cor.test(lead(df_stripped$new_cases, n = day_slide-1)[1:(length(df_stripped$new_cases) - day_slide)], df_stripped$key_count[1:(length(df_stripped$new_cases) - day_slide)],
#            alternative = "two.sided")
#   cor_vec[day_slide] <- vals$estimate
#   upper_95[day_slide] <- vals$conf.int[1]
#   lower_95[day_slide] <- vals$conf.int[2]
# 
#   if(day_slide == (dim(df_stripped)[1]-4)){cor_df <- tibble(day_slide = 0:(dim(df_stripped)[1]-5),
#                                               cor = cor_vec,
#                                               upper_95 = upper_95,
#                                               lower_95 = lower_95,
#                                               snr = abs(cor/(upper_95 - lower_95)))}
#   }
# 
# cor_df %>%
#   ggplot(aes(x = day_slide, y = snr)) +
#   geom_point() +
#   geom_line()
# 
# cor_df %>%
#   ggplot(aes(x = day_slide, y = cor)) +
#   geom_ribbon(aes(ymin=lower_95, ymax=upper_95), alpha=.4) +
#   geom_point() +
#   geom_line()
# 
# n <- 1 # n = day shift with best correlation
# df_stripped %>%
#   mutate(new_cases = lead(new_cases, n)) %>%
#   filter(is.na(new_cases) == FALSE) %>%
#   pivot_longer(c(new_cases, key_count), names_to = "case_type", values_to = "count") %>%
#   mutate(case_type = if_else(case_type == "key_count", "new keys", "new cases")) %>%
#   ggplot(aes(x = date, y = count, fill = case_type)) +
#   geom_col(width = 1, position = "identity")
