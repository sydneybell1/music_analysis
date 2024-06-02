library(readr)
library(dplyr)
library(ggplot2)

music_data <- read_csv("data/playlist_2010to2023.csv")

# selecting the artists and counting their frequency
artist_chart <- music_data %>% group_by(artist_name) %>%
  summarise(count = n())
# same thing by year
artist_chart_by_year <- music_data %>% group_by(artist_name, year) %>% 
  summarise(num_songs = n()) %>%
  ungroup() %>% 
  group_by(artist_name) %>%
  mutate(num_years = n()) %>% 
  mutate(flag_seven_years_or_more = case_when(
    num_years >= 7 ~ 1, # finding artists with at least 5 years
    TRUE ~ 0
  ))

artist_chart_by_year %>% group_by(artist_name) %>%
  filter(flag_seven_years_or_more == 1) %>% 
  summarise(tot_years = sum(flag_seven_years_or_more)) %>% 
  View()


# selecting the album and counting their frequency
album_chart <- music_data %>% group_by(album) %>% 
  summarise(count = n())

# track popularity by year
song_popularity <- music_data %>%
  filter(track_popularity >= 90)

artist_popular <- music_data %>% 
  filter(artist_popularity >= 90)

# EDA for danceability
hist(song_popularity$danceability)
hist(music_data$danceability)
hist(artist_popular$danceability)

# EDA for energy
music_data %>% group_by(energy) %>% count() %>% View()
# values 0.05 to 0.999
music_data_cleaned <- music_data %>%
  mutate(energy_grouped = case_when(
    energy > 0 & energy <= 0.1 ~ "(0, 0.1]",
    energy > 0.1 & energy <= 0.2 ~ "(0.1, 0.2]",
    energy > 0.2 & energy <= 0.3 ~ "(0.2, 0.3]",
    energy > 0.3 & energy <= 0.4 ~ "(0.3, 0.4]",
    energy > 0.4 & energy <= 0.5 ~ "(0.4, 0.5]",
    energy > 0.5 & energy <= 0.6 ~ "(0.5, 0.6]",
    energy > 0.6 & energy <= 0.7 ~ "(0.6, 0.7]",
    energy > 0.7 & energy <= 0.8 ~ "(0.7, 0.8]",
    energy > 0.8 & energy <= 0.9 ~ "(0.8, 0.9]",
    TRUE ~ "(0.9-1]"
  )
  #,energy_grouped = case_when(
  # energy > 0 & energy <= 0.1 ~ 0.1,
  # energy > 0.1 & energy <= 0.2 ~ 0.2,
  # energy > 0.2 & energy <= 0.3 ~ 0.3,
  # energy > 0.3 & energy <= 0.4 ~ 0.4,
  # energy > 0.4 & energy <= 0.5 ~ 0.5,
  # energy > 0.5 & energy <= 0.6 ~ 0.6,
  # energy > 0.6 & energy <= 0.7 ~ 0.7,
  # energy > 0.7 & energy <= 0.8 ~ 0.8,
  # energy > 0.8 & energy <= 0.9 ~ 0.9,
  # TRUE ~ 1)
  )
# looking at the frequencies
music_data_cleaned %>%
  group_by(energy_grouped) %>% count() %>%
  View()


# EDA for key
music_data %>% group_by(key) %>% count() %>% View()
# looking at the frequencies
hist(music_data$key)

# EDA for tempo


# EDA for genres


# EDA for loudness


# EDA for liveness


# EDA for duration


# EDA for time signature





uncommon_artists <- artist_chart %>% 
  filter(count == 1)
common_artists <- artist_chart %>%
  filter(count >= 20)



  





