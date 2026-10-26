library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(ggplot2)
# 2.2 Collect data and wrangle it into a suitable form for analysis
death_rate <- read.csv('death-rate-from-suicides-ghe.csv')
happiness_score <- read.csv('happiness-cantril-ladder.csv')

# rename column
happiness_score <- happiness_score %>%
  rename(
    country = Entity,
    code = Code,
    year = Year,
    satisfaction_score = Self.reported.life.satisfaction
  )

death_rate <- death_rate %>%
  rename(
    country = Entity,
    code = Code,
    year = Year,
    suicide_rate = Death.rate.from.self.harm.among.both.sexes
  )

# 2.3 data checking
dim(death_rate)
dim(happiness_score)

names(death_rate)
names(happiness_score)

str(death_rate)
str(happiness_score)

head(death_rate, 10)
head(happiness_score, 10)

tail(death_rate, 10)
tail(happiness_score, 10)

summary(death_rate)
summary(happiness_score)

colSums(is.na(death_rate))
colSums(is.na(happiness_score))

death_rate %>%
  filter(code == "" | is.na(code)) %>%
  distinct(country)

duplicated(death_rate[, c("Entity", "Year")]) |> sum()
duplicated(happiness[, c("Entity", "Year")]) |> sum()

range(death_rate$Year, na.rm = TRUE)
range(happiness$Year, na.rm = TRUE)

length(unique(death_rate$Entity))
length(unique(happiness$Entity))

sort(unique(death_rate$Entity))[1:20]
sort(unique(happiness$Entity))[1:20]

death_rate %>%
  filter(code == "" | is.na(code)) %>%
  distinct(country)

# delete rows without code
happiness_score <- happiness_score %>%
  filter(!is.na(code), code != "")
death_rate <- death_rate %>%
  filter(!is.na(code), code != "")

# check all rows with code
death_rate %>%
  filter(code == "" | is.na(code)) %>%
  distinct(country)

# question1 data
# find common countries and years in two sheet
common_countries <- intersect(death_rate$country, happiness_score$country)
common_years <- intersect(death_rate$year, happiness_score$year)
length(common_countries)
common_years

# filter same country and year
death_q1 <- death_rate %>%
  filter(country %in% common_countries,
         year %in% common_years)

happiness_q1 <- happiness_score %>%
  filter(country %in% common_countries,
         year %in% common_years)

# merge two sheet and prepare for question1
q1_data <- death_q1 %>%
  inner_join(happiness_q1, by = c("country", "code", "year"))

# check structure after merge
dim(q1_data)
head(q1_data)
summary(q1_data)

# use country code match continent
q1_data <- q1_data %>%
  mutate(
    continent = countrycode(country,
                            origin = "country.name",
                            destination = "continent")
  )

# check if any country missing
q1_data %>%
  filter(is.na(continent)) %>%
  distinct(country)

# check structure after add continent
range(q1_data$year)
length(unique(q1_data$country))
q1_data %>%
  distinct(country, continent) %>%
  count(continent)
q1_data %>%
  count(year)

# question2 data
unique(q1_data$country)

countries_2021 <- q1_data %>%
  filter(year == 2021) %>%
  distinct(country) %>%
  arrange(country)

nrow(countries_2021)

# use country code to match central point latitude and longitude
country_codes_2021 <- q1_data %>%
  filter(year == 2021) %>%
  distinct(country, code) %>%
  arrange(country)

world <- ne_countries(scale = "medium", returnclass = "sf")
world_centroids <- world %>%
  st_make_valid() %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(iso_a3, name, lat, lon)

country_coords <- country_codes_2021 %>%
  left_join(world_centroids, by = c("code" = "iso_a3")) %>%
  select(country, code, name, lat, lon)
country_coords %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(country, code)

# manually add missing centraid coordinates for France and Norway
country_coords <- country_coords %>%
  mutate(
    lat = case_when(
      country == "France" ~ 42.4602,
      country == "Norway" ~ 69.1569,
      TRUE ~ lat
    ),
    lon = case_when(
      country == "France" ~ -2.2137,
      country == "Norway" ~ 15.4681,
      TRUE ~ lon
    )
  ) %>%
  select(country, code, lat, lon)

country_coords %>%
  filter(country %in% c("France", "Norway"))

# check if anything missing currently
country_coords %>%
  filter(is.na(lat) | is.na(lon))

# use api and connect to nasapower
library(nasapower)
test_row <- country_coords %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  slice(1)

test_power <- get_power(
  community = "RE",
  pars = "ALLSKY_SFC_SW_DWN",
  temporal_api = "monthly",
  lonlat = c(test_row$lon, test_row$lat),
  dates = c("2020", "2021"),
  time_standard = "LST"
)

names(test_power)
test_power

# function to catch daylight in nasa power with coordination
get_country_solar_2021 <- function(country_name, lat, lon) {
  message("Running: ", country_name)
  
  tryCatch({
    df <- get_power(
      community = "RE",
      pars = "ALLSKY_SFC_SW_DWN",
      temporal_api = "monthly",
      lonlat = c(lon, lat),
      dates = c("2020", "2021"),
      time_standard = "LST"
    )
    
    df %>%
      filter(YEAR == 2021) %>%
      transmute(
        country = country_name,
        year = YEAR,
        annual_solar_radiation = ANN
      )
    
  }, error = function(e) {
    message("Failed: ", country_name, " | ", conditionMessage(e))
    tibble(
      country = country_name,
      year = 2021,
      annual_solar_radiation = NA_real_
    )
  })
}

# use function get solar data
solar_data_all <- purrr::pmap_dfr(
  country_coords %>%
    select(country, lat, lon),
  function(country, lat, lon) {
    get_country_solar_2021(country, lat, lon)
  }
)
solar_data_2021 <- solar_data_all
write.csv(solar_data_all,'solar_data_all.csv')

q1_latest <- q1_data %>%
  filter(year == 2021)
q2_data <- q1_latest %>%
  left_join(solar_data_2021, by = c("country", "year"))

# Explore the data visually to answer your questions:
# Q1. How do mental health outcomes vary across different regions globally?
# two mental-health-related indicators/ proxy indicators of mental wellbeing and adverse outcomes
q1_data <- q1_data %>%
  mutate(
    happiness_score = as.numeric(happiness_score),
    suicide_rate = as.numeric(suicide_rate)
  )
class(q1_data$suicide_rate)
class(q1_data$satisfaction_score)
q1_trend <- q1_data %>%
  group_by(continent, year) %>%
  summarise(
    mean_happiness = mean(satisfaction_score, na.rm = TRUE),
    mean_suicide = mean(suicide_rate, na.rm = TRUE),
    median_happiness = median(satisfaction_score, na.rm = TRUE),
    median_suicide = median(suicide_rate, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  )

# 图1：各大洲 happiness 趋势图/各大洲 suicide rate 趋势图
fig_q1_continent_happiness_trend <- ggplot(
  q1_trend,
  aes(x = year, y = mean_happiness, color = continent)
  fig_q1_continent_happiness_trend <- ggplot(
    q1_trend,
    aes(x = year, y = mean_happiness, color = continent)
  ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.3) +
    labs(
      title = "Life Satisfaction Trends by Continent, 2011-2021",
      x = "Year",
      y = "Mean life satisfaction score",
      color = "Continent"
    ) +
    scale_x_continuous(breaks = seq(2011, 2021, by = 2)) +
    scale_color_manual(values = c(
      "Africa" = "#C97C5D",        # muted coral
      "Asia" = "#C9A227",          # muted gold
      "Europe" = "#7A9A5B",        # olive green
      "Americas" = "#6E8FA8", # dusty blue
      "Oceania" = "#B48EAD"       # soft mauve
    )) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey90"),
      
      axis.line = element_line(colour = "grey45", linewidth = 0.4),
      axis.ticks = element_line(colour = "grey45", linewidth = 0.4),
      
      panel.background = element_blank(),
      plot.background = element_blank()
    )
fig_q1_continent_happiness_trend

fig_q1_continent_suicide_trend <- ggplot(
  q1_trend,
  aes(x = year, y = mean_suicide, color = continent)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  labs(
    title = "Suicide Rate Trends by Continent, 2011-2021",
    x = "Year",
    y = "Mean suicide rate (deaths per 100,000 population)",
    color = "Continent"
  ) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 2)) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Asia" = "#C9A227",
    "Europe" = "#7A9A5B",
    "Americas" = "#6E8FA8",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey90"),
    axis.line = element_line(colour = "grey45", linewidth = 0.4),
    axis.ticks = element_line(colour = "grey45", linewidth = 0.4),
    panel.background = element_blank(),
    plot.background = element_blank()
  )
fig_q1_continent_suicide_trend
# merge two plot
library(patchwork)

fig_q1_trends_combined <- fig_q1_continent_happiness_trend +
  fig_q1_continent_suicide_trend +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_q1_trends_combined
ggsave(
  filename = "fig_q1_trend_combined.png",
  plot = fig_q1_trends_combined,
  width = 11.5,
  height = 5.5,
  dpi = 300
)

# 图2:每年样本国家数量趋势
p3 <- ggplot(q1_trend, aes(x = year, y = n_countries, color = continent)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Number of Countries Included by Continent Over Time",
    x = "Year",
    y = "Number of countries",
    color = "Continent"
  ) +
  theme_minimal(base_size = 13)

p3

# define latest year
latest_year <- max(q1_data$year, na.rm = TRUE)
latest_year
q1_latest <- q1_data %>%
  filter(year == latest_year)

# 图3: latest year 各大洲 happiness boxplot
p4 <- ggplot(q1_latest, aes(x = continent, y = happiness_score)) +
  geom_boxplot() +
  labs(
    title = paste("Life Satisfaction by Continent in", latest_year),
    x = "Continent",
    y = "Happiness score"
  ) +
  theme_minimal(base_size = 13)

p4

# 图4:latest year 各大洲 suicide rate boxplot
fig_q1_suicide_distribution <- ggplot(
  q1_latest,
  aes(x = continent, y = suicide_rate, fill = continent)
) +
  geom_boxplot(width = 0.38, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(
    aes(color = continent),
    width = 0.12,
    size = 1.2,
    alpha = 0.65,
    show.legend = FALSE
  ) +
  labs(
    title = "Suicide Rate by Continent in 2021",
    x = "Continent",
    y = "Suicide rate (deaths per 100,000 population)"
  ) +
  scale_fill_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#C9A227",
    "Europe" = "#7A9A5B",
    "Oceania" = "#B48EAD"
  )) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#C9A227",
    "Europe" = "#7A9A5B",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey90"),
    axis.line.x = element_line(colour = "grey45", linewidth = 0.4),
    axis.line.y = element_line(colour = "grey45", linewidth = 0.4),
    axis.ticks = element_line(colour = "grey45", linewidth = 0.4)
  )

fig_q1_suicide_distribution

# 图5:latest year 各大洲 happiness violin + boxplot
fig_q1_life_distribution <- ggplot(
  q1_latest,
  aes(x = continent, y = satisfaction_score, fill = continent)
) +
  geom_boxplot(width = 0.38, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(
    aes(color = continent),
    width = 0.12,
    size = 1.2,
    alpha = 0.65,
    show.legend = FALSE
  ) +
  labs(
    title = "Life Satisfaction by Continent in 2021",
    x = "Continent",
    y = "Life satisfaction score"
  ) +
  scale_fill_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#C9A227",
    "Europe" = "#7A9A5B",
    "Oceania" = "#B48EAD"
  )) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#C9A227",
    "Europe" = "#7A9A5B",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey90"),
    axis.line.x = element_line(colour = "grey45", linewidth = 0.4),
    axis.line.y = element_line(colour = "grey45", linewidth = 0.4),
    axis.ticks = element_line(colour = "grey45", linewidth = 0.4)
  )

fig_q1_life_distribution

# merge two pic
library(patchwork)

fig_q1_distribution_combined <- fig_q1_life_distribution + fig_q1_suicide_distribution

fig_q1_distribution_combined
ggsave(
  filename = "fig_q1_distribution_combined.png",
  plot = fig_q1_distribution_combined,
  width = 12,
  height = 5.8,
  dpi = 300
)
# 图7：latest year 各大洲 suicide violin + boxplot
p7 <- ggplot(q1_latest, aes(x = continent, y = suicide_rate)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.size = 1) +
  labs(
    title = paste("Distribution of Suicide Rates by Continent in", latest_year),
    x = "Continent",
    y = "Suicide rate"
  ) +
  theme_minimal(base_size = 13)

p7

library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

map_happiness <- world %>%
  left_join(q1_latest, by = c("iso_a3" = "code"))

# 图8：latest year happiness 世界地图
p8 <- ggplot(map_happiness) +
  geom_sf(aes(fill = satisfaction_score), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  labs(
    title = paste("Global Distribution of Life Satisfaction in", 2021),
    subtitle = "Country-level happiness scores",
    fill = "Happiness"
  ) +
  theme_minimal(base_size = 13)

p8

# 图9：latest year suicide rate 世界地图
map_suicide <- world %>%
  left_join(q1_latest, by = c("iso_a3" = "code"))

p9 <- ggplot(map_suicide) +
  geom_sf(aes(fill = suicide_rate), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "D", na.value = "grey90") +
  labs(
    title = paste("Global Distribution of Suicide Rate in", 2021),
    subtitle = "Country-level suicide rates",
    fill = "Suicide rate"
  ) +
  theme_minimal(base_size = 13)

p9

fig_q1_life_map <- ggplot(map_happiness) +
  geom_sf(aes(fill = satisfaction_score), color = "white", linewidth = 0.15) +
  scale_fill_gradientn(
      colours = c("#FFF7ED", "#FDBA74", "#FB923C", "#F97316", "#C2410C"),
      na.value = "grey90",
      name = "Life satisfaction score"
    )+
  labs(
    title = "Global Life Satisfaction in 2021"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
fig_q1_life_map

fig_q1_suicide_map <- ggplot(map_suicide) +
  geom_sf(aes(fill = suicide_rate), color = "white", linewidth = 0.15) +
  scale_fill_gradientn(
    colours = c("#E6F7F2", "#A8E6CF", "#56C596", "#2E8B57", "#1B5E20"),
    na.value = "grey90",
    name = "Suicide rate"
  ) +
  labs(
    title = "Global Suicide Rate in 2021"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
fig_q1_suicide_map

library(patchwork)

fig_q1_maps_combined <- fig_q1_life_map + fig_q1_suicide_map

fig_q1_maps_combined
ggsave(
  filename = "fig_q1_maps_combined.png",
  plot = fig_q1_maps_combined,
  width = 12,
  height = 5.8,
  dpi = 300
)
# 图10：latest year happiness vs suicide scatter
p10 <- ggplot(q1_latest, aes(x = happiness_score, y = suicide_rate, color = continent)) +
  geom_point(alpha = 0.8, size = 2.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  labs(
    title = paste("Life Satisfaction and Suicide Rate in", latest_year),
    subtitle = "Country-level comparison",
    x = "Happiness score",
    y = "Suicide rate",
    color = "Continent"
  ) +
  theme_minimal(base_size = 13)

p10

# 图11：latest year happiness vs suicide scatter + country labels
library(ggrepel)

p11 <- ggplot(q1_latest, aes(x = happiness_score, y = suicide_rate, color = continent)) +
  geom_point(alpha = 0.8, size = 2.3) +
  geom_text_repel(aes(label = country), size = 3, max.overlaps = 20) +
  labs(
    title = paste("Country Outliers: Happiness vs Suicide Rate in", latest_year),
    x = "Happiness score",
    y = "Suicide rate",
    color = "Continent"
  ) +
  theme_minimal(base_size = 13)

p11

# 图12：各大洲平均 happiness 和 suicide 排名
# calculate average
continent_summary_latest <- q1_latest %>%
  group_by(continent) %>%
  summarise(
    mean_happiness = mean(happiness_score, na.rm = TRUE),
    mean_suicide = mean(suicide_rate, na.rm = TRUE),
    .groups = "drop"
  )

# happiness ranking
p12 <- ggplot(continent_summary_latest,
              aes(x = reorder(continent, mean_happiness), y = mean_happiness)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Average Happiness Score by Continent in", latest_year),
    x = "Continent",
    y = "Mean happiness score"
  ) +
  theme_minimal(base_size = 13)

p12

# suicide rate ranking
p13 <- ggplot(continent_summary_latest,
              aes(x = reorder(continent, mean_suicide), y = mean_suicide)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Average Suicide Rate by Continent in", latest_year),
    x = "Continent",
    y = "Mean suicide rate"
  ) +
  theme_minimal(base_size = 13)

p13

# 图13：国家层面 top / bottom 10 happiness
top_happiness <- q1_latest %>%
  arrange(desc(happiness_score)) %>%
  slice_head(n = 10)

bottom_happiness <- q1_latest %>%
  arrange(happiness_score) %>%
  slice_head(n = 10)

p14 <- ggplot(top_happiness,
              aes(x = reorder(country, happiness_score), y = happiness_score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top 10 Countries by Happiness Score in", latest_year),
    x = "Country",
    y = "Happiness score"
  ) +
  theme_minimal(base_size = 13)

p14

p15 <- ggplot(bottom_happiness,
              aes(x = reorder(country, happiness_score), y = happiness_score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Bottom 10 Countries by Happiness Score in", latest_year),
    x = "Country",
    y = "Happiness score"
  ) +
  theme_minimal(base_size = 13)

p15

# 图14：国家层面 top / bottom 10 suicide rate
top_suicide <- q1_latest %>%
  arrange(desc(suicide_rate)) %>%
  slice_head(n = 10)

bottom_suicide <- q1_latest %>%
  arrange(suicide_rate) %>%
  slice_head(n = 10)

p16 <- ggplot(top_suicide,
              aes(x = reorder(country, suicide_rate), y = suicide_rate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top 10 Countries by Suicide Rate in", latest_year),
    x = "Country",
    y = "Suicide rate"
  ) +
  theme_minimal(base_size = 13)

p16

p17 <- ggplot(bottom_suicide,
              aes(x = reorder(country, suicide_rate), y = suicide_rate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Bottom 10 Countries by Suicide Rate in", latest_year),
    x = "Country",
    y = "Suicide rate"
  ) +
  theme_minimal(base_size = 13)

p17

# Q2. How do differences in annual sunlight exposure across global regions associate with variations in mental health indicators?
# 图1：solar vs happiness
p_q2_1 <- ggplot(q2_data,
                 aes(x = annual_solar_radiation,
                     y = satisfaction_score,
                     color = continent)) +
  geom_point(alpha = 0.7, size = 2.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Annual Solar Radiation and Life Satisfaction in 2021",
    x = "Annual solar radiation",
    y = "Life satisfaction score",
    color = "Continent"
  ) +
  theme_minimal(base_size = 13)

p_q2_1

fig_q2_solar_life <- ggplot(
  q2_data,
  aes(x = annual_solar_radiation, y = satisfaction_score, color = continent)
) +
  geom_point(alpha = 0.7, size = 2.0) +
  geom_smooth(
    data = q2_data,
    aes(x = annual_solar_radiation, y = satisfaction_score),
    method = "loess",
    se = FALSE,
    inherit.aes = FALSE,
    color = "grey30",
    linewidth = 0.6
  ) +
  labs(
    title = "Solar Radiation and Life Satisfaction in 2021",
    x = "Annual solar radiation",
    y = "Life satisfaction score",
    color = "Continent"
  ) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#56C596",
    "Europe" = "#F59E0B",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90")
  )

fig_q2_solar_life

# pic2: solar & suicide
fig_q2_solar_suicide <- ggplot(
  q2_data,
  aes(x = annual_solar_radiation, y = suicide_rate, color = continent)
) +
  geom_point(alpha = 0.7, size = 2.0) +
  geom_smooth(
    data = q2_data,
    aes(x = annual_solar_radiation, y = suicide_rate),
    method = "loess",
    se = FALSE,
    inherit.aes = FALSE,
    color = "grey30",
    linewidth = 0.6
  ) +
  labs(
    title = "Solar Radiation and Suicide Rate in 2021",
    x = "Annual solar radiation",
    y = "Suicide rate (deaths per 100,000 population)",
    color = "Continent"
  ) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#56C596",
    "Europe" = "#F59E0B",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90")
  )

fig_q2_solar_suicide
# merge two plot
library(patchwork)

fig_q2_overall_combined <- fig_q2_solar_life + fig_q2_solar_suicide +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

fig_q2_overall_combined

# cluster in continent
p_q2_3 <- ggplot(q2_data,
                 aes(x = annual_solar_radiation,
                     y = satisfaction_score)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ continent) +
  labs(
    title = "Annual Solar Radiation and Life Satisfaction by Continent in 2021",
    x = "Annual solar radiation",
    y = "Life satisfaction score"
  ) +
  theme_minimal(base_size = 13)

p_q2_3
fig_q2_by_continent <- ggplot(
  q2_data,
  aes(x = annual_solar_radiation, y = satisfaction_score)
) +
  geom_point(alpha = 0.7, size = 1.2, color = "#D9A441") +
  geom_smooth(method = "lm", se = FALSE, color = "grey20", linewidth = 0.5) +
  facet_wrap(~ continent) +
  labs(
    title = "Solar Radiation and Life Satisfaction by Continent in 2021",
    x = "Annual solar radiation",
    y = "Life satisfaction score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90")
  )

fig_q2_by_continent
ggsave(
  filename = "fig_q2_by_continent.png",
  plot = fig_q2_by_continent,
  width = 10,
  height = 6,
  dpi = 300
)
# bubble plot
p_q2_4 <- ggplot(q2_data,
                 aes(x = annual_solar_radiation,
                     y = satisfaction_score,
                     color = continent,
                     size = suicide_rate)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Annual Solar Radiation, Life Satisfaction and Suicide Rate in 2021",
    x = "Annual solar radiation",
    y = "Life satisfaction score",
    color = "Continent",
    size = "Suicide rate"
  ) +
  theme_minimal(base_size = 13)

p_q2_4

# Q3. To what extent do life satisfaction and suicide rate show similar or contrasting patterns across global regions?
q3_latest$continent <- factor(
  q3_latest$continent,
  levels = c("Africa", "Americas", "Asia", "Europe", "Oceania")
)

fig_q3_scatter_clean <- ggplot(
  q3_latest,
  aes(x = satisfaction_score, y = suicide_rate, color = continent)
) +
  geom_point(alpha = 0.75, size = 2.0) +
  geom_smooth(
    data = q3_latest,
    aes(x = satisfaction_score, y = suicide_rate),
    method = "loess",
    se = FALSE,
    inherit.aes = FALSE,
    color = "grey20",
    linewidth = 0.6
  ) +
  labs(
    title = "Country level Life Satisfaction and Suicide Rate in 2021",
    x = "Life satisfaction score",
    y = "Suicide rate (deaths per 100,000 population)",
    color = "Continent"
  ) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#56C596",
    "Europe" = "#F59E0B",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90")
  )

fig_q3_scatter_clean
ggsave(
  filename = "fig_q3_country_scatter.png",
  plot = fig_q3_scatter_clean,
  width = 10,
  height = 6,
  dpi = 300
)
happiness_cut <- mean(q3_latest$satisfaction_score, na.rm = TRUE)
suicide_cut <- mean(q3_latest$suicide_rate, na.rm = TRUE)

q3_quadrant <- q3_latest %>%
  mutate(
    quadrant = case_when(
      satisfaction_score >= happiness_cut & suicide_rate < suicide_cut ~ "High satisfaction / Low suicide",
      satisfaction_score >= happiness_cut & suicide_rate >= suicide_cut ~ "High satisfaction / High suicide",
      satisfaction_score < happiness_cut & suicide_rate < suicide_cut ~ "Low satisfaction / Low suicide",
      satisfaction_score < happiness_cut & suicide_rate >= suicide_cut ~ "Low satisfaction / High suicide"
    )
  )
library(dplyr)
fig_q3_quadrant <- ggplot(
  q3_quadrant,
  aes(x = satisfaction_score, y = suicide_rate, color = quadrant)
) +
  geom_point(alpha = 0.8, size = 2.2) +
  geom_vline(xintercept = happiness_cut, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  geom_hline(yintercept = suicide_cut, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  labs(
    title = "Country Patterns of Life Satisfaction and Suicide Rate in 2021",
    x = "Life satisfaction score",
    y = "Suicide rate (deaths per 100,000 population)",
    color = "Pattern"
  ) +
  scale_color_manual(values = c(
    "High satisfaction / Low suicide" = "#56C596",
    "High satisfaction / High suicide" = "#F59E0B",
    "Low satisfaction / Low suicide" = "#6E8FA8",
    "Low satisfaction / High suicide" = "#C97C5D"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90")
  )

fig_q3_quadrant
# Facet by continent
p_q3_2 <- ggplot(q3_latest,
                 aes(x = satisfaction_score,
                     y = suicide_rate)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ continent) +
  labs(
    title = paste("Life Satisfaction and Suicide Rate by Continent in", 2021),
    x = "Life satisfaction score",
    y = "Suicide rate"
  ) +
  theme_minimal(base_size = 13)

p_q3_2

# question3 step2
fig_q3_continent_means <- ggplot(
  q3_continent_summary,
  aes(x = mean_happiness, y = mean_suicide, label = continent, color = continent)
) +
  geom_point(size = 4.2) +
  geom_text_repel(size = 3.8, show.legend = FALSE) +
  scale_x_continuous(
    limits = c(4.3, 7.3),
    breaks = c(4.5, 5.0, 5.5, 6.0, 6.5, 7.0)
  ) +
  scale_y_continuous(
    limits = c(6, 13),
    breaks = c(6, 8, 10, 12)
  ) +
  labs(
    title = "Regional Mean Life Satisfaction and Suicide Rate in 2021",
    x = "Regional mean life satisfaction score",
    y = "Regional mean suicide rate (deaths per 100,000 population)"
  ) +
  scale_color_manual(values = c(
    "Africa" = "#C97C5D",
    "Americas" = "#6E8FA8",
    "Asia" = "#56C596",
    "Europe" = "#F59E0B",
    "Oceania" = "#B48EAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90")
  )

fig_q3_continent_means
ggsave(
  filename = "fig_q3_continent_means.png",
  plot = fig_q3_continent_means,
  width = 10,
  height = 6,
  dpi = 300
)
# Quadrant classification
happiness_cut <- mean(q3_latest$happiness_score, na.rm = TRUE)
suicide_cut <- mean(q3_latest$suicide_rate, na.rm = TRUE)

q3_quadrant <- q3_latest %>%
  mutate(
    quadrant = case_when(
      happiness_score >= happiness_cut & suicide_rate < suicide_cut ~ "High happiness / Low suicide",
      happiness_score >= happiness_cut & suicide_rate >= suicide_cut ~ "High happiness / High suicide",
      happiness_score < happiness_cut & suicide_rate < suicide_cut ~ "Low happiness / Low suicide",
      happiness_score < happiness_cut & suicide_rate >= suicide_cut ~ "Low happiness / High suicide"
    )
  )

p_q3_4 <- ggplot(q3_quadrant,
                 aes(x = happiness_score,
                     y = suicide_rate,
                     color = quadrant)) +
  geom_point(alpha = 0.8, size = 2.3) +
  geom_vline(xintercept = happiness_cut, linetype = "dashed") +
  geom_hline(yintercept = suicide_cut, linetype = "dashed") +
  labs(
    title = paste("Country Classification by Life Satisfaction and Suicide Rate in", latest_year),
    x = "Life satisfaction score",
    y = "Suicide rate",
    color = "Quadrant"
  ) +
  theme_minimal(base_size = 13)

p_q3_4

# find other interest insights only using Tableau or R. 

