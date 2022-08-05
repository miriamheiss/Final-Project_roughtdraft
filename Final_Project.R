library(tidyverse)
library(spotifyr)
library(plotly)


# Find TS spotify id
# results <- search_spotify("Taylor Swift", type = "artist")

ts_id <- "06HL4z0CvFAxyc27GXpf02"


# Get data ----------------------------------------------------------------

# taylor_swift_raw <- get_artist_albums(ts_id)
#saveRDS(taylor_swift_raw, "ts_albums.rds")
ts_albums_raw <- readRDS("ts_albums.rds")

# ts_audio_raw <- get_artist_audio_features(ts_id)
# saveRDS(ts_audio_raw, "ts_audio.rds")
ts_audio_raw <- readRDS("ts_audio.rds")


# Clean data --------------------------------------------------------------

taylor_swift_albums <- ts_albums_raw %>% 
  group_by(name) %>%  
  slice(1)

albums <- c("evermore (deluxe version)",
            "folklore (deluxe version)",
            "Lover",
            "reputation",
            "1989",
            "Red (Taylor's Version)",
            "Speak Now",
            "Fearless (Taylor's Version)",
            "Taylor Swift")

ts_audio_clean <- ts_audio_raw %>% 
  filter(album_name %in% albums) %>% 
  select(track_name, energy, key_name, album_name, duration_ms, time_signature)

ts_graph<- ggplot(data = ts_audio_clean,
       mapping = aes(x = key_name,
                     y = energy,
                     color = album_name, 
                     text = track_name))+ 
  geom_point(alpha = 1)+ 
  scale_color_manual(values = c("deepskyblue2",
                                "lightsalmon4",
                                "goldenrod",
                                "seashell4",
                                "palevioletred1",
                                "darkred",
                                "grey20",
                                "orchid4",
                                "palegreen4"))+ 
  labs(title = "Energy by Key (Taylor Swift)",
       y = "Energy",
       x = "Key", 
       color = "Album")+ 
  annotate(geom = "label", x = "A#", y = 0, label = "Low")+ 
  annotate(geom = "label", x = "A#", y = 1, label = "High")+ 
  theme_linedraw()

ggplotly(ts_graph, tooltip = "text")




