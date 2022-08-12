library(tidyverse)
library(spotifyr)
library(patchwork)
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


# Taylor Swift --------------------------------------------------------------

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
  geom_hline(yintercept = 0.5)+
  geom_point(size = 2)+ 
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
  theme_linedraw()+ 
  theme(legend.position = "right")

ggplotly(ts_graph, tooltip = "text")

ts_graph

# Bach --------------------------------------------------------------

#results <- search_spotify("J. S. Bach", type = "artist")
bach_id <- "5aIqB5nVVvmFsvSdExz408"


#bach_raw <- get_artist_albums(bach_id)

#bach_audio_raw <- get_artist_audio_features(bach_id)
#saveRDS(bach_audio_raw, "bach_audio.rds")
bach_audio_raw <- readRDS("bach_audio.rds")

bach_albums <- c("J. S. Bach: Cantatas", "Invention", "Bach on Porthan Organ")
 
bach_cleaned <- bach_audio_raw %>% 
  filter(album_name %in% bach_albums) %>% 
  select(key_mode, track_name, energy, album_name, 
         duration_ms, time_signature)



bach_graph <- bach_cleaned %>%  
  ggplot(mapping = aes(x = key_mode, 
         y = energy, 
         color = album_name, 
         text = track_name))+ 
  geom_hline(yintercept = 0.5)+
  geom_point(size = 2.5, alpha = 0.75) + 
  scale_color_manual(values = c("lightpink3",
                                "paleturquoise4",
                                "thistle4"))+ 
  scale_y_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.00)) + 
  coord_cartesian(ylim = c(0.0, 1.00))+ 
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "left")+ 
  labs(title = "Energy by Key (J. S. Bach)", 
       x = "Key", 
       y = "Energy",
       color = "Album")+ 
  annotate(geom = "label", x = "A# minor", y = 0, label = "Low")+ 
  annotate(geom = "label", x = "A# minor", y = 1, label = "High") 

bach_graph


ggplotly(bach_graph, tooltip = "text")
  

bach_graph + ts_graph


# Bach with tempo/valence -------------------------------------------------------------- 

bach_cleaned_with_tempo <- bach_audio_raw %>% 
  filter(album_name %in% bach_albums) %>% 
  select(key_mode, track_name, energy, album_name, 
         duration_ms, time_signature, tempo, valence)

bach_tempo <- bach_cleaned_with_tempo %>%
  ggplot(mapping = aes(x = tempo,
                       y = valence))+ 
  coord_cartesian(ylim = c(0.0, 1))+
  geom_point(aes(color = album_name), size = 2.5)+ 
  geom_smooth(se = FALSE, method = "loess", formula = y~x, color = "black")+
  theme_linedraw()+
  theme(legend.position = "left")+
  scale_color_manual(values = c("lightpink3",
                                "paleturquoise4",
                                "thistle4"))+ 
  labs(title = "Valence by Tempo (J. S. Bach)",
       x = "Tempo",
       y = "Valence",
       color = "Album")

bach_tempo+ts_tempo

 # Taylor Swift with tempo/valence -------------------------------------------------------------- 

ts_cleaned_with_tempo <- ts_audio_raw %>% 
  filter(album_name %in% albums) %>% 
  select(track_name, energy, key_name, album_name, 
         duration_ms, time_signature, tempo, valence)

ts_tempo <- ts_cleaned_with_tempo %>%   
  ggplot(mapping = aes(x = tempo,
                       y = valence))+ 
  geom_point(aes( color = album_name),size = 2.5)+ 
  geom_smooth(se = FALSE, color = "black", method = "loess", formula = y~x) +
  theme_linedraw()+
  theme(legend.position = "right")+ 
  scale_color_manual(values = c("deepskyblue2",
                                "lightsalmon4",
                                "goldenrod",
                                "seashell4",
                                "palevioletred1",
                                "darkred",
                                "grey32",
                                "orchid4",
                                "palegreen4"))+ 
  labs(title = "Valence by Tempo (Taylor Swift)", 
       x = "Tempo",
       y = "Valence",
       color = "Album")


# New Graph ---------------------------------------------------------------


ts_duration <- ts_audio_raw %>% 
  filter(album_name %in% albums) %>% 
  select(duration_ms, album_name) %>% 
  mutate(minutes = duration_ms / 1000 / 60) %>% 
  ggplot(mapping = aes(y = minutes,
                       x = album_name,
                       color = album_name))+ 
  geom_point()+ 
  scale_color_manual(values = c("deepskyblue2",
                                "lightsalmon4",
                                "goldenrod",
                                "seashell4",
                                "palevioletred1",
                                "darkred",
                                "grey32",
                                "orchid4",
                                "palegreen4"))


ts_duration



bach_duration <- bach_audio_raw %>% 
  filter(album_name %in% bach_albums) %>%
  # select(duration_ms, album_name) %>% 
  mutate(minutes = duration_ms / 1000 / 60) %>% 
  ggplot(mapping = aes(y = minutes,
                       x = album_name,
                       color = album_name))+ 
  geom_point() 

bach_duration




















