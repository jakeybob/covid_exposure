library(tidyverse)
library(jsonlite)
library(lubridate)
library(scales)
library(showtext)
font_add_google(name = "Lato") # https://fonts.google.com
showtext_auto()
source("UTILS.R")

theme_set(theme_bw() + theme(
  panel.grid.minor = element_line(colour = rgb(.95,.95,.95)),
  text = element_text(family = "Lato"),
  legend.position = "bottom",
  legend.box.margin = margin(-10, 0, 0, 0),
  axis.text = element_text(face = "bold", size = 10),
  plot.title = element_text(face = "bold", size = 16),
  plot.subtitle = element_text(size = 11, margin=margin(0, 0, 10, 0)),
  legend.text = element_text(face = "bold", size = 9)) )


#### IMPORT DATA ####
df1 <- import_exposure_keys("iphone", "phone_1_json_exposure_files") %>% mutate(source = "phone 1")
df2 <- import_exposure_keys("android", "phone_2_json_exposure_files") %>% mutate(source = "phone 2")

df <- df1 %>%
  bind_rows(df2) %>% 
  distinct() %>%
  group_by(source, timestamp) %>% 
  summarise(key_count = sum(key_count)) %>% 
  arrange(source, timestamp) %>% 
  group_by(source) %>% 
  mutate(cum_key_count = cumsum(key_count))
  
#### PLOT 1 ####
df %>% 
  ggplot(aes(x = timestamp, y = key_count, fill = source)) +
  geom_col(width = 3*3600, position = "identity") +
  scale_fill_manual(values = c(rgb(1,0,0, alpha = .8),
                               rgb(0,0,1, alpha = .8))) +
  scale_x_datetime(date_minor_breaks = "days") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "", fill = "", 
       title = "Scotland COVID-19 Key Downloaded Frequency",
       subtitle = expression(paste(bold("new diagnosis keys "), "downloaded by two different phones, via the ", italic("protect.scot")," app over 14 days")))
ggsave("pics/plot_new_two_phones.png", dpi = 300, width = 200, height = 133, units = "mm")


#### PLOT 2 ####
df %>% 
  ggplot(aes(x = timestamp, y = cum_key_count, fill = source, colour = source)) +
  geom_step(size = 1.2) +
  scale_colour_manual(values = c(rgb(1,0,0, alpha = .8),
                               rgb(0,0,1, alpha = .8))) +
  scale_x_datetime(date_minor_breaks = "days") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Cumulative Key Download Comparison",
       subtitle = expression(paste(bold("cumulative diagnosis keys "), "downloaded by two different phones, via the ", italic("protect.scot")," app over 14 days")))
ggsave("pics/plot_cum_two_phones.png", dpi = 300, width = 200, height = 133, units = "mm")

# df %>%
#   group_by(source) %>%
#   mutate(time_diff = timestamp %--% lead(timestamp)/days(1)) %>%
#   ggplot(aes(x = time_diff, fill = source, colour = source)) +
#   geom_density() +
#   facet_wrap(~source, ncol = 1, scales = "free_y")
