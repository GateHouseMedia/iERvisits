library(here)
library(tidyverse)
library(janitor) 
library(lubridate)

## IMPORT DATA ##

neiss_all_years_combined <- read_csv("neiss_all_years_combined.csv", 
                                     col_types = cols(Other_Diagnosis = col_character())) %>% 
  clean_names()

neiss_codes <- read_delim(here::here("neiss_codes.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names()

## CLEAN DATA ##

products <- neiss_codes %>% 
  filter(format_name == "PROD") %>% 
  rename(code = starting_value_for_format,
         product = format_value_label) %>% 
  select(-ending_value_for_format, -format_name)

diagnosis <- neiss_codes %>% 
  filter(format_name == "DIAG") %>% 
  rename(code = starting_value_for_format,
         diagnosis = format_value_label) %>% 
  select(-ending_value_for_format, -format_name)

body_parts <- neiss_codes %>% 
  filter(format_name == "BDYPT") %>% 
  rename(code = starting_value_for_format,
         body_part = format_value_label) %>% 
  select(-ending_value_for_format, -format_name)

neiss_clean <- neiss_all_years_combined %>% 
  left_join(products, by = c("product_1" = "code")) %>% 
  left_join(products, by = c("product_2" = "code")) %>% 
  left_join(diagnosis, by = c("diagnosis" = "code")) %>% 
  left_join(body_parts, by = c("body_part" = "code")) %>% 
  unite("narrative", c("narrative_1", "narrative_2"), sep = " ") %>% 
  mutate(age = case_when(age > 200 ~ (age - 200)/12,
                         TRUE ~ age),
         sex = case_when(sex == 1 ~ "M",
                         sex == 2 ~ "F",
                         TRUE ~ NA_character_))

rm(neiss_all_years_combined)

## KIDS ##

kids <- neiss_clean %>% 
  filter(age < 18) %>% 
  mutate(age_group = case_when(age < 5 ~ "infants_and_toddlers",
                               age >=5 & age < 11 ~ "elementary",
                               age >= 11 & age < 14 ~ "middle",
                               TRUE ~ "high"))

rm(neiss_clean)

monthly_ER_trips_by_age <- kids %>% 
  mutate(age_group = fct_relevel(age_group, "infants_and_toddlers", "elementary", "middle", "high"),
         month = month(treatment_date)) %>%
  group_by(age_group, month) %>% 
  summarize(count = n()) %>% 
  mutate(month = fct_reorder(month.name[month], month))

write.csv(monthly_ER_trips_by_age, here::here("tables", "monthly_ER_trips_by_age.csv"))
  
monthly_ER_trips_by_age %>% 
  ggplot(aes(month, count, group = 1)) +
  geom_line() +
  facet_wrap( ~ age_group, ncol = 1) +
  labs(title = "When do children end up in the ER?",
       x = "",
       y = "ER visits",
       caption = "Source: National Electronic Injury Surveillance System") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Look at May
kids %>% 
  filter(age_group == "middle" | age_group == "high") %>% 
  # filter(data_year > 2010) %>% 
  filter(month(treatment_date) == 5) %>% 
  count(product.x, sort = T) %>% 
  mutate(percent = n/sum(n))

# Look at September
kids %>% 
  filter(age_group == "middle" | age_group == "high") %>% 
  filter(month(treatment_date) == 9) %>% 
  count(product.x, sort = T) %>% 
  mutate(percent = n/sum(n))

## KIDS SPORTS ##

## USE TIDYTEXT PACKAGE TO FIND MOST COMMON SPORTS GETTING CLASSIFIED AS MISCELLANEOUS ##

library(tidytext)

misc_words <- kids %>% 
  filter(product.x == "1200 - SPORTS AND RECREATIONAL ACTIVITY, N.E.C.") %>%
  select(treatment_date, narrative, product.x, product.y) %>% View()
  unnest_tokens(word, narrative, token = "words") %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!word %in% c("dx", "na", "pt")) %>% 
  count(word, sort = T)

# Which sports do we need to add that were not coded as an "activity"? 
# Gymnastics and track & field were added. 
# Rodeo was added

# Swimming injuries were excluded due to difficulty determining between the activity and injuries on slides/diving boards/etc. #### Maybe I want to include swimming
# Bicycling injuries were excluded due to the widespread popularity of biking for leisure rather than as a sport.
# "Exercise" was excluded.
# Skateboards/scooter, "skating not specified", in-line skating, roller skating, and ice-skating were included.

# I attempted to parse out some common sports that were coded as 'miscellaneous.' 
# I combined climbing wall and rock climbing injuries with mountain climbing

other_activities <- c("1272 - GYMNASTICS AND ASSOCIATED EQUIPMENT", "5030 - TRACK & FIELD (ACT., APPAREL, EQP.; EX.  JOG, RUN FIT)")

kids_sports <- kids %>% 
  filter(str_detect(product.x, "ACTIVITY") | product.x %in% other_activities) %>% 
  mutate(product.x = case_when(product.x == "1200 - SPORTS AND RECREATIONAL ACTIVITY, N.E.C." & (str_detect(narrative, "BULL ") | str_detect(narrative, "RODEO")) ~ "rodeo",
                               product.x == "1200 - SPORTS AND RECREATIONAL ACTIVITY, N.E.C." & (str_detect(narrative, "DRILL TEAM") | str_detect(narrative, "DANC")) ~ "3278 - DANCING (ACTIVITY, APPAREL OR EQUIPMENT)",
                               product.x == "1200 - SPORTS AND RECREATIONAL ACTIVITY, N.E.C." & (str_detect(narrative, "CLIMBING WALL") | str_detect(narrative, "ROCK CLIMBING")) ~ "mountain climbing (activity, apparel or equipment)",
                               TRUE ~ product.x)) %>%
  mutate(product_clean = str_remove(str_to_lower(product.x), "\\d+ - ")) %>% 
  mutate(product_clean = str_squish(str_remove(product_clean, "\\([^()]*\\)"))) %>% 
  mutate(product_clean = str_remove(product_clean, ", activity and related equipment")) %>% 
  mutate(product_clean = str_remove(product_clean, " and associated equipment")) %>% 
  mutate(product_clean = str_replace(product_clean, "sports and recreational activity, n.e.c.", "other")) %>% 
  filter(product_clean != "skating not specified",
         product_clean != "exercise",
         product_clean != "roller skating",
         product_clean != "other ball sports",
         product_clean != "ball sports , n.s.",
         product_clean != "other")
  # filter(!str_detect(product_clean, "swimming"))

kids_sports_by_month <- kids_sports %>% 
  count(month = month(treatment_date))
  ggplot(aes(month, n)) +
  geom_line() +
  expand_limits(y = 0)

kids_sports %>% 
  count(product_clean, sort = T) %>% 
  View()

kids_sports %>% 
  count(data_year) %>% 
  ggplot(aes(data_year, n)) +
  geom_line() +
  expand_limits(y = 0)


kids_sports %>% 
  filter(data_year == 2011,
         age > 12) %>%
  group_by(product_clean) %>% 
  summarize(count = n(),
            pct_concussion = sum(str_detect(diagnosis.y, "CONCUSSION"))/count*100) %>% 
  filter(count > 100) %>% 
  arrange(desc(pct_concussion)) %>% 
  View()

kids_sports %>% 
  filter(product_clean == "hockey , not specified") %>% 
  View()

# Top sports over time

top_ten <- c("football", "basketball", "soccer", "baseball", "swimming", "softball", "gymnastics", "volleyball", "wrestling", "cheerleading")

top_ten_sports_over_time <- kids_sports %>% 
  filter(product_clean %in% top_ten) %>% 
  group_by(data_year, product_clean) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(product_clean = fct_reorder(product_clean, -count))

write.csv(top_ten_sports_over_time, here::here("tables", "top_ten_sports_over_time.csv"))

top_ten_sports_over_time %>% 
  ggplot(aes(data_year, count, color = product_clean)) +
  geom_line() +
  labs(title = "How have kids sports-related ER visits changed over time?",
       subtitle = "For the 10 sports that appeared in the data most frequently",
       x = "",
       y = "ER visits",
       caption = "Source: National Electronic Injury Surveillance System") +
  theme_minimal() +
  theme(legend.title = element_blank())
  
kids_sports_by_month <- kids_sports %>% 
  filter(product_clean %in% top_ten) %>% 
  filter(data_year >= 2000) %>%
  group_by(month = month(treatment_date), product_clean) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(month = fct_reorder(month.name[month], month))

write.csv(kids_sports_by_month, here::here("tables", "kids_sports_by_month.csv"))

kids_sports_by_month %>% 
  ggplot(aes(month, count, group = product_clean, color = product_clean)) +
  geom_line() +
  theme_minimal() +
  labs(title = "What month do kids go to the ER, for each sport?",
       subtitle = "Using ER data from 2000 to 2018",
       caption = "Source: National Electronic Injury Surveillance System") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ product_clean, ncol = 2, scales = "free")

# Most common injuries

top_injuries <- kids_sports %>% 
  count(body_part.y, diagnosis.y, sort = T) %>% 
  mutate(percent = n/sum(n))
write.csv(top_injuries, here::here("tables", "top_injuries.csv"))
# considering limiting this to 2010 and after

top_injuries_by_sport <- kids_sports %>% 
  count(product_clean, body_part.y, diagnosis.y, sort = T) %>% 
  group_by(product_clean) %>% 
  top_n(5) %>% 
  mutate(rank = min_rank(-n)) %>% 
  arrange(product_clean, rank) %>% 
  ungroup() 

# Most changed sports

sports_change_over_time <- kids_sports %>% 
  count(data_year, product_clean) %>%
  group_by(product_clean) %>%
  mutate(total_incidents = sum(n)) %>%
  filter(total_incidents > 100) %>% 
  group_by(decade = 5 * (data_year %/% 5), product_clean) %>% 
  summarize(avg_incidents = mean(n)) %>% 
  spread(decade, avg_incidents) %>% 
  filter(!is.na(`1995`) & !is.na(`2015`)) %>% 
  filter(product_clean != "other") %>% 
  mutate(overall_pct_change = (`2015` - `1995`)/`1995`) %>% 
  arrange(overall_pct_change)

top_ten_changed_sports <- sports_change_over_time %>% 
  slice(1:10, (n()-9):n()) %>% 
  gather("decade", "avg_incidents", -overall_pct_change, -product_clean) %>% 
  group_by(product_clean) %>% 
  mutate(pct_change_since_1995 = (avg_incidents - avg_incidents[decade == "1995"])/avg_incidents[decade == "1995"]) %>% 
  ungroup()

top_ten_changed_sports_for_jenn <- top_ten_changed_sports %>% 
  select(-overall_pct_change, -avg_incidents) %>% 
  spread(decade, pct_change_since_1995)

write.csv(top_ten_changed_sports_for_jenn, here::here("tables", "top_ten_changed_sports.csv"))

## PLOTLY ##

library(plotly)

d <- highlight_key(top_ten_changed_sports, ~product_clean)

p <- ggplot(d, aes(decade, pct_change_since_1995, group = product_clean)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(x = "",
       y = "Percent change",
       title = "Children's sports with the most dramatic increases or decreases of ER visits",
       subtitle = "Calculated average number of incidents per sport per five-year period") +
  theme_minimal()

gg <- ggplotly(p, tooltip = c("product_clean", "decade", "avg_incidents"))

highlight(gg, "plotly_hover", color = "#2b8cbe")

