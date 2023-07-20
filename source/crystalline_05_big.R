library(tidyverse)
library(ambient)

seeds <- 108

crystalline <- function(seed) {
  
  version <- "05"
  fname <-  paste0("crystalline_", version, "_", seed, "_big.png")
  cat("\nmaking file", fname, "\n")
  
  set.seed(seed)
  
  grain <- 6500
  
  palette <- paletteer::paletteer_c(
    palette = "scico::oslo",
    n = grain
  )
  
  grid <- long_grid(
    x = seq(0, 1, length.out = grain),
    y = seq(0, 1, length.out = grain)
  )
  
  trans_cells <- function(x, y, frequency = 3, octaves = 4) {
    cat("trans_cells... \n")
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
      rank() %>% 
      normalise()
  }
  
  trans_inner <- function(x, y, frequency = 5) {
    cat("trans_inner... \n")
    fracture(
      noise = gen_simplex,
      fractal = ridged,
      octaves = 10,
      frequency = frequency,
      #seed = 6,
      x = x,
      y = y
    ) %>%
      normalise()
  }
  
  trans_curl <- function(x, y, frequency = .2) {
    cat("trans_curl... \n")
    curl_noise(
      generator = fracture,
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
  
  distort <- trans_curl(grid$x, grid$y, frequency = 1)
  
  grid <- grid %>%
    mutate(
      cells = trans_cells(distort$x, distort$y, frequency = 3, octaves = 6),
      final = trans_inner(x + cells, y + cells, frequency = .1),
      index = indexify(final, grain),
      color = palette[index]
    )
  
  cat("writing image... \n")
  
  png(
    filename = here::here("image", fname),
    width = grain,
    height = grain,
  )
  op <- par(mar = c(0,0,0,0))
  plot(as.raster(grid, value = color))
  dev.off()
  par(op)
  
}

for(s in seeds) crystalline(s)
