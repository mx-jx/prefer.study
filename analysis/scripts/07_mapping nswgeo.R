install.packages("nswgeo")
library(nswgeo)
library(ggplot2)
library(ggautomap)

# Example data
overdose_data <- data.frame(
  Region = c("Sydney CBD", "Parramatta", "Newcastle", "Wollongong", "Central Coast", 
             "Bondi Beach", "Manly", "Darling Harbour", "Surry Hills", "Chatswood",
             "Liverpool", "Penrith", "Blacktown", "Campbelltown", "Hurstville",
             "Randwick", "Strathfield", "Bankstown", "Sutherland", "Mascot"),
  OverdoseCount = c(5, 8, 3, 10, 7, 4, 6, 2, 9, 5, 8, 3, 6, 4, 7, 5, 8, 9, 2, 6)
)



sydney_locations <- data.frame(
  Location = c("Location1", "Location2", "Location3", "Location4", "Location5",
               "Location6", "Location7", "Location8", "Location9", "Location10",
               "Location11", "Location12", "Location13", "Location14", "Location15"),
  Latitude = c(-33.8688, -33.8548, -33.8696, -33.8711, -33.8682,
               -33.8674, -33.8644, -33.8628, -33.8604, -33.8583,
               -33.8563, -33.8542, -33.8523, -33.8502, -33.8484),
  Longitude = c(151.2093, 151.2083, 151.2105, 151.2112, 151.2117,
                151.2123, 151.2141, 151.2159, 151.2181, 151.2207,
                151.2234, 151.2264, 151.2300, 151.2338, 151.2383))

nsw_map <- nswgeo::nsw_lga_sf()

nsw_map <- nswgeo::nsw_lga_sf()


ggplot(nswgeo::suburbs) + geom_sf(aes(fill = suburbname), show.legend = FALSE)


ggplot(lhd) + geom_sf(aes())


# SCATTER MAPS

covid_cases_nsw %>%
  ggplot(aes(location = lga)) +
  geom_boundaries(feature_type = "nswgeo.lga") +
  geom_geoscatter(aes(colour = type), sample_type = "random", size = 0.5) +
  coord_automap(feature_type = "nswgeo.lga", xlim = c(147, 153), ylim = c(-33.7, -29)) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  theme_void()

# INSETS

covid_cases_nsw %>%
  ggplot(aes(location = lga)) +
  geom_boundaries(feature_type = "nswgeo.lga") +
  geom_geoscatter(aes(colour = type), size = 0.5) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lga", inset = configure_inset(
    centre = "Blacktown", radius = 40, units = "km",
    scale = 7, translation = c(400, -100)
  )) +
  theme_void()

# PACKED POINTS 

covid_cases_nsw %>%
  dplyr::filter(year >= 2021) %>%
  ggplot(aes(location = lhd)) +
  geom_boundaries(feature_type = "nswgeo.lhd") +
  geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 35), size = 1) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Sydney", radius = 80, units = "km", feature_type = "nswgeo.lhd",
    scale = 6, translation = c(650, -100)
  )) +
  facet_wrap(vars(year)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(strip.text = element_text(size = 12))

# CHLOROPLETH MAP

covid_cases_nsw %>%
  ggplot(aes(location = lhd)) +
  geom_choropleth() +
  geom_boundaries(
    feature_type = "nswgeo.lhd", colour = "black", linewidth = 0.1,
    outline.aes = list(colour = NA)
  ) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 5, translation = c(400, -100)
  )) +
  scale_fill_steps(low = "#e6f9ff", high = "#00394d", n.breaks = 5, na.value = "white") +
  theme_void()

summarised_data <- data.frame(
  lhd = c("Western Sydney", "Sydney", "Far West", "Mid North Coast", "South Western Sydney"),
  cases = c(250, 80, 20, NA, 100)
)

# CHLOROPLEHT WITH SUMMARISED DATA

summarised_data <- data.frame(
  lhd = c("Western Sydney", "Sydney", "Far West", "Mid North Coast", "South Western Sydney"),
  cases = c(250, 80, 20, NA, 100)
)

summarised_suburbs <- data.frame(
  lhd = c("Northern Beaches", "Blacktown", "Sydney", "Randwick"),
  Overdoses = c(250, 80, 20, 130)
)

summarised_suburbs %>%
  ggplot(aes(location = lhd)) +
  geom_sf_inset(aes(fill = Overdoses), stat = "automap", colour = NA) +
  geom_boundaries(
    feature_type = "nswgeo.lga", colour = "black", linewidth = 0.1,
    outline.aes = list(colour = NA)
  ) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lga", inset = configure_inset(
    centre = "Blacktown", radius = 60, units = "km",
    scale = 3.5, translation = c(350, 0)
  )) +
  scale_fill_gradient(low = "#e6f9ff", high = "#00394d", na.value = "grey90") +
  theme_void() +
  xlim(147, 158) +
  ylim(-38, -30) +
  labs(x="Latitude",
       y="Longitude") +                    # clears other plot elements
  theme_bw() 
theme(panel.background = element_rect(fill = "lightblue"))








library(cartographer)
head(covid_cases_nsw)

covid_cases_nsw |>
  dplyr::count(lga) |>
  add_geometry(lga, feature_type = "nswgeo.lga") |>
  ggplot() +
  geom_sf(aes(fill = n)) +
  geom_sf(fill = NA, data = map_sf("nswgeo.lga")) +
  theme_void()

ggplot(lhd) + geom_sf(aes(fill = lhd_name, =), show.legend = TRUE) + theme_test()+
geom_rect(aes(xmin = 147, xmax = 154, ymin = -36, ymax = -30), color = "red", fill = NA) 



ggplot(lhd)  + lhd_name = ("Hunter New England")

str(lhd)

ggplot(states)+ geom_sf(aes(fill = STE_NAME21), show.legend = TRUE) + theme_bw()

?theme


theme(panel.background = element_rect(fill = "lightblue")) +
  guides(size = "none") +
  ggsn::north(location = "topleft", scale = 0.8, symbol = 12,
              x.min = 151.5, x.max = 152.5, y.min = -36, y.max = -38) +
  ggsn::scalebar(location = "bottomleft", dist = 100,
                 dist_unit = "km", transform = TRUE, 
                 x.min=150.5, x.max=152, y.min=-38, y.max=-30,
                 st.bottom = FALSE, height = 0.025,
                 st.dist = 0.05, st.size = 2.5)

str(suburbs)

ggplot(suburbs) + geom_sf()
