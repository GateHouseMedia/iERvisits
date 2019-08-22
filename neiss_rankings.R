## FIND RANKINGS ##

ranked <- kids_sports %>% 
  # filter(product_clean != "gym or p.e.") %>% 
  count(data_year, product_clean) %>%
  group_by(decade = 5 * (data_year %/% 5), product_clean) %>% 
  summarize(total_incidents = sum(n)) %>% 
  group_by(product_clean) %>%
  filter(any(total_incidents > 1000)) %>% 
  mutate(count = n()) %>% 
  filter(count == 5) %>% 
  group_by(decade) %>% 
  mutate(rank = min_rank(-total_incidents)) %>% 
  ungroup()

## PLOTLY ##

library(plotly)

d <- highlight_key(ranked, ~product_clean)

p <- ggplot(d, aes(decade, rank, group = product_clean)) +
  geom_line() +
  scale_y_reverse() +
  theme(legend.position = "none") +
  labs(x = "",
       y = "Rank",
       title = "Sports-related children's ER visits, ranked by frequency") +
  theme_minimal()

gg <- ggplotly(p, tooltip = c("product_clean", "total_incidents"))

highlight(gg, "plotly_hover", color = "#2b8cbe")

