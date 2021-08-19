library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)

# Read data
old <- read_csv("data/raw/old.csv", col_names = c('year', 'song', 'length', 'album', 'artist', 'plays', 'last_played'))
new <- read_csv("data/raw/new.csv", col_names = c('year', 'song', 'length', 'album', 'artist', 'plays', 'last_played'))
artist_info <- read_csv("data/raw/artist_info.csv")

combined <- old %>% bind_rows(new) %>% select(-length)

# Convert to date column
combined$last_played %<>% dmy_hm()

# Convert artist columns to title case to ensure easy matching
combined$artist %<>% str_to_title()
artist_info$artist %<>% str_to_title()

# Work out which songs appear in one csv and not the other
old_not_in_new <- anti_join(old, new, by = c('song', 'album'))
new_not_in_old <- anti_join(new, old, by = c('song', 'album'))

combined %<>% mutate(
  plays = if_else(is.na(plays), 0, plays)
)

# Combine duplicate songs into one row
combined %<>% group_by(song, album, year, artist) %>% summarise(
  plays = sum(plays),
  last_played = date(max(last_played, na.rm = TRUE))
)

# Calculate album statistics
album_listens <- combined %>% group_by(album, artist) %>% summarise(
  year = min(year),
  plays = min(plays),
  last_played = date(max(last_played, na.rm = TRUE))
)

# Connect artist info such as country and first letter
album_listens <- album_listens %>% left_join(artist_info, by = "artist")

# Do all artists have a corresponding record in the artist_info dataframe?
new_artists <- album_listens %>% anti_join(artist_info, by = "artist")
new_artists <- unique(new_artists$artist)

# Write to files
album_listens %>% write_csv("data/calc/album_listens_full.csv", col_names = TRUE)
album_listens %>% filter(plays > 0) %>% write_csv("data/calc/album_listens.csv", col_names = TRUE)

new_not_in_old %>% write_csv("data/checks/new_not_in_old.csv", col_names = TRUE)
old_not_in_new %>% write_csv("data/checks/old_not_in_new.csv", col_names = TRUE) # This should be empty!
new_artists %>% write.csv("data/checks/new_artists.csv") # This should also be empty!