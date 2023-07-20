library(tidyverse)
library(ambient)

seed <- 1
version <- "04"
fname <-  paste0("crystalline_", version, "_", seed, ".png")


set.seed(seed)

grain <- 5000

palette <- paletteer::paletteer_c(
  palette = "scico::oslo",
  n = grain
)

grid <- long_grid(
  x = seq(0, 1, length.out = grain),
  y = seq(0, 1, length.out = grain)
)

trans_cells <- function(x, y, frequency = 3, octaves = 4) {
  fracture(
    noise = gen_worley,
    fractal = billow,
    octaves = octaves,
    frequency = frequency,
    value = "cell",
    #seed = 10,
    x = x,
    y = y
  ) %>%
    rank()
}

trans_inner <- function(x, y, frequency = 5) {
  fracture(
    noise = gen_simplex,
    fractal = fbm,
    octaves = 10,
    frequency = frequency,
    #seed = 6,
    x = x,
    y = y
  ) %>%
    normalise()
}

trans_last <- function(x, y, frequency = .2) {
  fracture(
    noise = gen_simplex,
    fractal = fbm,
    octaves = 10,
    frequency = frequency,
    #seed = 4,
    x = x,
    y = y
  )
}

indexify <- function(x, n) {
  round(1 + (n - 1) * normalise(x))
}


grid <- grid %>%
  mutate(
    cells = trans_cells(x, y, frequency = 30, octaves = 2),
    x_ish = trans_inner(cells + x, cells + y/2, frequency = 5),
    y_ish = trans_inner(cells + x/2, cells + y, frequency = 5),
    final = trans_last(x + x_ish, y + y_ish, frequency = 1),
    index = indexify(final, grain),
    color = palette[index]
  )


png(
  filename = here::here("image", fname),
  width = grain,
  height = grain,
)
op <- par(mar = c(0,0,0,0))
plot(as.raster(grid, value = color))
dev.off()
par(op)
