library(tidyverse)
library(here)
source("R/aux_functions.R")

raw <- read.csv("data/DH216.csv", sep = ";") |> rownames_to_column("idcell")

n_cell_rows <- 10  # Numero de filas de traqueidas medidas

# Genera subset by year
d <- raw |> dplyr::filter(Year == 1995) |> mutate(ybottom = BY - Height)

plot_cells(d)
