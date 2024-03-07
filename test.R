library(tidyverse)
library(here)
source("R/aux_functions.R")

raw <- read.csv("data/DH216.csv", sep = ";") |> rownames_to_column("idcell")

n_cell_rows <- 10  # Numero de filas de traqueidas medidas

# Genera subset by year
d <- raw |> dplyr::filter(Year == 1997) |> mutate(ybottom = BY - Height)

plot_cells(d)

n_cell_rows <- 10  # Numero de filas de traqueidas medidas



# Identificar dentro de ese año la bottom-left cell
# ybottom = BY - Height
bottom_left_cell <- d[which.min(d$ybottom),]


p1 <- plot_cells(d)
p1 + geom_point(data = bottom_left_cell, aes(x=X, y=Y), colour = "red")

