library(arrow)
library(dplyr)
library(tictoc)
library(tidyr)
library(here)
library(ambient)
library(ragg)


# set up ------------------------------------------------------------------

x0 <- -74.05
y0 <- 40.6
x1 <- x0 + 0.3
y1 <- y0 + 0.3
pixels <- 4000

nyc_taxi <- open_dataset("~/Datasets/nyc-taxi/")

# count pickups and dropoffs at each pixel --------------------------------

tic()
cat("counting pickups at each pixel... ")

pickup <- nyc_taxi |>
  filter(
    !is.na(pickup_longitude),
    !is.na(pickup_latitude),
    pickup_longitude > x0,
    pickup_longitude < x1,
    pickup_latitude > y0,
    pickup_latitude < y1
  ) |>
  mutate(
    x = as.integer(round(pixels * (pickup_longitude - x0) / (x1 - x0))),
    y = as.integer(round(pixels * (pickup_latitude - y0) / (y1 - y0)))
  ) |>
  count(x, y, name = "pickup") |>
  compute()

toc()

tic()
cat("counting dropoffs at each pixel... ")

dropoff <- nyc_taxi |>
  filter(
    !is.na(dropoff_longitude),
    !is.na(dropoff_latitude),
    dropoff_longitude > x0,
    dropoff_longitude < x1,
    dropoff_latitude > y0,
    dropoff_latitude < y1
  ) |>
  mutate(
    x = as.integer(round(pixels * (dropoff_longitude - x0) / (x1 - x0))),
    y = as.integer(round(pixels * (dropoff_latitude - y0) / (y1 - y0)))
  ) |>
  count(x, y, name = "dropoff") |>
  compute()

toc()


# converting to grids -----------------------------------------------------

tic()
cat("placing counts on a grid... ")

x_coord <- 0:pixels
y_coord <- 0:pixels

grid <- expand_grid(
  x = x_coord,
  y = y_coord
) |>
  as_arrow_table() |>
  left_join(pickup, by = c("x", "y")) |>
  left_join(dropoff, by = c("x", "y")) |>
  mutate(
    pickup = case_when(is.na(pickup) ~ 0, TRUE ~ pickup),
    dropoff = case_when(is.na(dropoff) ~ 0, TRUE ~ dropoff)
  ) |>
  arrange(x, y) |>
  collect()

toc()

tic()
cat("constructing image matrices... ")

pickup_grid <- long_grid(
  x = x_coord,
  y = y_coord,
  z = 0
)

dropoff_grid <- long_grid(
  x = x_coord,
  y = y_coord,
  z = 0
)

pickup_grid$z <- grid$pickup
dropoff_grid$z <- grid$dropoff

pickup_grid <- as.matrix(pickup_grid, value = z)
dropoff_grid <- as.matrix(dropoff_grid, value = z)

toc()

# write image files -------------------------------------------------------

tic()
cat("writing pickup image... ")

gradient <- function(...) {
  (colorRampPalette(colors = c(...)))(1024)
}

op <- par(mar = c(0, 0, 0, 0))
agg_png(
  filename = here("output", "full-taxi-scatter-pickup.png"),
  width = pixels,
  height = pixels,
  bg = "black"
)
image(
  z = log10(t(pickup_grid)),
  axes = FALSE,
  asp = 1,
  useRaster = TRUE,
  col = gradient("black", "white")
)
dev.off()
par(op)

toc()

tic()
cat("writing dropoff image... ")

op <- par(mar = c(0, 0, 0, 0))
agg_png(
  filename = here("output", "full-taxi-scatter-dropoff.png"),
  width = pixels,
  height = pixels,
  bg = "black"
)
image(
  z = log10(t(dropoff_grid)),
  axes = FALSE,
  asp = 1,
  useRaster = TRUE,
  col = gradient("black", "white")
)
dev.off()
par(op)

toc()

tic()
cat("writing joint image... ")

op <- par(mar = c(0, 0, 0, 0))
agg_png(
  filename = here("output", "full-taxi-scatter.png"),
  width = pixels,
  height = pixels,
  bg = "#000000"
)
image(
  z = log10(t(dropoff_grid)),
  axes = FALSE,
  asp = 1,
  useRaster = TRUE,
  col = gradient("#40001000", "#800020ff")
)
image(
  z = log10(t(pickup_grid)),
  axes = FALSE,
  add = TRUE,
  asp = 1,
  useRaster = TRUE,
  col = gradient("#22222200", "#ffffffff")
)
dev.off()
par(op)

toc()

