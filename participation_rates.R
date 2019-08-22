library(tidyverse)
library(janitor)
library(readxl)

participation <- read_excel("partcipation_statistics_08_20_2019 10_18.xlsx") %>% 
  clean_names() %>% 
  mutate(girls_participation = case_when(is.na(girls_participation) ~ 0,
                                         TRUE ~ girls_participation)) %>% 
  mutate(total_participation = boys_participation + girls_participation)

participation %>% 
  group_by(sport) %>% 
  summarize(us_participation = sum(total_participation)) %>% 
  arrange(desc(us_participation)) %>% 
  View()

top_high_school_sports <- participation %>% 
  group_by(state) %>% 
  mutate(boys_rank = min_rank(-boys_participation),
         girls_rank = min_rank(-girls_participation),
         total_rank = min_rank(-total_participation)) %>% 
  arrange(state, total_rank) %>% 
  select(-year, -boys_school, -girls_school) %>% 
  filter(total_rank == 1)

write.csv(top_high_school_sports, "top_high_school_sports.csv")

##

library(tigris)

options(tigris_class = "sf")

us <- tigris::states()

ri <- counties("RI")
ri20 <- counties("RI", cb = TRUE, resolution = "20m")
plot(ri)
plot(ri20, border = "red", add = TRUE)

map <- left_join(states, top_high_school_sports, by = c("STUSPS" = "state"))

ggplot(map) + 
  geom_sf(aes(fill = sport), color = "white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  # scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title = "2016 Median income in Texas counties", caption = "Source: US Census/ACS5 2016")


ggplot(tx) + 
  geom_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Texas counties")
