#### pigeon_themes ----
# A set of custom themes for ggplot2
# because, why not?

#### initial setup ----
library(tidyverse)
library(patchwork)

rihanna_data <- readRDS("Rihanna_spotify.RDS")
rihanna <- rihanna_data %>%
  filter(!grepl("\\(",album_name),
         !grepl(":",album_name))
rihanna_rereleases <- rihanna_data %>%
  filter(grepl("[\\(:]",album_name))



plt_scatter <- ggplot(rihanna, aes(x = duration_ms, y = tempo, color = album_name)) +
  geom_jitter()

plt_box <- ggplot(rihanna, aes(x = album_name, y = valence, fill = album_name)) +
  geom_boxplot() +
  geom_jitter()

rihanna_grouped <- rihanna %>%
  group_by(album_name) %>%
  summarise(loud_mean = mean(loudness),
            year = mean(album_release_year))

rihanna_grouped_keyYear <- rihanna_data %>%
  group_by(key_name, album_release_year, mode) %>%
  summarise(avgDance = mean(danceability),
            avgEnergy = mean(energy),
            avgSpeech = mean(speechiness))

facet_jitter <- ggplot(rihanna_grouped_keyYear, aes(x = album_release_year, y = avgSpeech, color = key_name)) +
  geom_line(position = position_jitter(width = .025, height = .025, seed = 112211)) +
  geom_point(position = position_jitter(width = .025, height = .025, seed = 112211)) +
  facet_wrap(vars(mode), nrow = 2) +
  theme_minimal()

nofacet_jitter <- ggplot(rihanna_grouped_keyYear, aes(x = album_release_year, y = avgSpeech, color = key_name)) +
  geom_line(position = position_jitter(width = .025, height = .025, seed = 112211)) +
  geom_point(position = position_jitter(width = .025, height = .025, seed = 112211)) +
  theme_minimal()

facet_jitter / nofacet_jitter

# ggplot(rihanna_data, aes(x = album_release_year, y = speechiness, color = key_name)) +
#   facet_wrap(vars(key_mode)) +
#   geom_line(position = position_jitter(width = .25, height = .25, seed = 112211)) +
#   geom_point(position = position_jitter(width = .25, height = .25, seed = 112211))


plt_line <- ggplot(rihanna_grouped, aes(x = year, y = loud_mean)) +
  geom_line() +
  geom_label(aes(x = year, y = loud_mean, label = album_name))

# ggplot(rihanna, aes(x = mode_name, y = duration_ms, color = album_name)) +
#   geom_jitter() +
#   facet_wrap(vars(album_name), scales = "free_x")  +
#   theme_minimal() +
#   theme(panel.border = element_rect(size = .5, color = "black", fill = NA),
#         panel.spacing = unit(2, "line")) +
#   scale_x_discrete(labels = unique(rihanna$mode_name))

#### Cyberpunk ----


#### KidNeuroLab ----

#### Rihanna Album Themes ----
