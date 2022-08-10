library(arrow)
library(dplyr)
library(ggplot2)
library(tictoc)
library(here)

tic()
cat("filtering data... ")
nyc_taxi <- open_dataset("~/Datasets/nyc-taxi-tiny/")
nyc_coord <- nyc_taxi |>
  filter(
    !is.na(pickup_longitude),
    !is.na(pickup_latitude),
    !is.na(dropoff_longitude),
    !is.na(dropoff_latitude)
  ) |>
  collect()
toc()


tic()
cat("writing image... ")
pic <- ggplot(nyc_coord) +
  geom_point(aes(dropoff_longitude, dropoff_latitude), size = .2, stroke = 0, colour = "#800020") +
  geom_point(aes(pickup_longitude, pickup_latitude), size = .2, stroke = 0, colour = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  coord_equal(xlim = c(-74.05, -73.75), ylim = c(40.6, 40.9))

ggsave(
  filename = here("output", "tiny-taxi-scatter.png"),
  plot = pic,
  width = 4000,
  height = 4000,
  units = "px",
  bg = "black"
)
toc()
