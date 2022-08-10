library(arrow)
library(dplyr)
library(tictoc)
library(tidyr)
library(here)
library(ragg)

# set up ------------------------------------------------------------------

x0 <- -74.05
y0 <- 40.6
x1 <- x0 + 0.3
y1 <- y0 + 0.3
pixels <- 4000

nyc_taxi <- open_dataset("~/Datasets/nyc-taxi/")


# count pickups at each pixel ---------------------------------------------

# First and most computationally expensive step: count the number of
# pickups that fall within the region spanned by each pixel in the image.
# This does almost all of the work for us, because we reduce billions of
# records to a much smaller table of pixels and counts

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
  collect()

toc()

# place counts on a grid --------------------------------------------------

# Second step is to slightly tidy the output. The output of the previous
# step isn't very organised, and it will only list a pixel if it has a
# nonzero number of taxi pickups. This step uses expand_grid() to
# initialise a "grid like" tibble with every combination of x and y values
# and counts the number of pickups

tic()
cat("placing counts on a grid... ")

x_coord <- 0:pixels
y_coord <- 0:pixels

grid <- expand_grid(
  x = x_coord,
  y = y_coord
) |>
  left_join(pickup, by = c("x", "y")) |>
  mutate(pickup = replace_na(pickup,  0))

toc()



# convert to matrix -------------------------------------------------------

# Third step is to explicitly convert this to a matrix. There's several
# ways to do this, but in this case it's super-easy because expand_grid()
# orders the elements of grid$pickup so that they can be passed straight
# to matrix()

tic()
cat("constructing image matrix... ")
pickup_grid <- matrix(
  data = grid$pickup,
  nrow = pixels + 1,
  ncol = pixels + 1
)
toc()

# write image file --------------------------------------------------------

# Now that we have a matrix whose cells correspond to the pixel intensities
# to be plotted, all that we have to do is write it to a file. I'll use
# image() to create the plot and write it to a png file. In this case I'm
# using the agg_png() function from the ragg package, but I could have
# used png() if I'd wanted

tic()
cat("writing image... ")

op <- par(mar = c(0, 0, 0, 0))

agg_png(
  filename = here("output", "taxi-scatter.png"),
  width = pixels,
  height = pixels,
  bg = "black"
)

image(
  z = log10(t(pickup_grid)),
  axes = FALSE,
  asp = 1,
  useRaster = TRUE
)

dev.off()
par(op)
toc()

