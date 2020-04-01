#### pigeon_themes ----
# A set of custom themes for ggplot2
# because, why not?

#### initial setup ----
library(tidyverse)
library(patchwork)

rihanna <- readRDS("Rihanna_spotify.RDS")
rihanna <- rihanna %>%
  filter(!grepl("\\(",album_name),
         !grepl(":",album_name))

plt_scatter <- ggplot(rihanna, aes(x = duration_ms, y = tempo, color = album_name)) +
  geom_jitter()

plt_box <- ggplot(rihanna, aes(x = album_name, y = valence, fill = album_name)) +
  geom_boxplot() +
  geom_jitter()

rihanna_grouped <- rihanna %>%
  group_by(album_name) %>%
  summarise(loud_mean = mean(loudness),
            year = mean(album_release_year))

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
