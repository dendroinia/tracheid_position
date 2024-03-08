library(tidyverse)
library(here)
source("R/aux_functions.R")

raw <- read.csv("data/DH216.csv", sep = ";") |> rownames_to_column("idcell")

# n_cell_rows <- 10  # Numero de filas de traqueidas medidas

# Genera subset by year
d <- raw |> dplyr::filter(Year == 2009) |>
  mutate(ybottom = BY - Height,
         xleft = BX)

dall <- raw |>
  mutate(ybottom = BY - Height,
         xleft = BX)



identify_first <- function(df){
  # identica el grupo potencial de celulas bottom left.
  aux_bottom_cells <- df |>
    filter(ybottom < ceiling(quantile(df$ybottom, 0.1))) |>
    # slice_min(ybottom, n = ceiling(nrow(df)/n_cell_rows)) |>
    mutate(potential_row = cut(ybottom, breaks=2))   # aqui agrupo por valores de Y

  # identifica la celula bottom_left
  if(length(unique(aux_bottom_cells$potential_row)) > 1) {

    bottom_left_cell <- aux_bottom_cells |>
      slice_min(potential_row) |> # me quedo con el grupo mas bajo de Y
      slice_min(xleft) # dentro de esas con la mas baja de X
  } else {
    bottom_left_cell <- aux_bottom_cells |>  # me quedo con el grupo mas bajo de Y
      slice_min(xleft)
    }

  df_out <- df |>
    mutate(traqueid_row = ifelse(idcell %in% bottom_left_cell$idcell, 1, NA)) |>
    mutate(traqueid_pos = ifelse(idcell %in% bottom_left_cell$idcell, 1, NA))

  return(df_out)
  }







result <- dall |> group_split(Year) |>
  map(~ identify_first(.)) |>
  map(~ find_next_cell(.)) |>
  bind_rows()


ggplot(result) +
  geom_rect(aes(xmin=BX, xmax=BX+Width, ymin=BY,  ymax=BY+Height),
            fill="lightblue", colour="lightblue") +
  facet_wrap(~Year, scales = "free") +
  geom_rect(
    data = (result |> filter(traqueid_row == 1 & traqueid_pos == 1)),
    aes(xmin=BX, xmax=BX+Width, ymin=BY,  ymax=BY+Height), fill="red", colour="red") +
  geom_rect(
    data = (result |> filter(traqueid_row == 1 & traqueid_pos == 2)),
    aes(xmin=BX, xmax=BX+Width, ymin=BY,  ymax=BY+Height), fill="blue", colour="blue")



d95 <- dall |> filter(Year == 2008)

p1 <- plot_cells(d95)

r08 <- result |> filter(Year == 2008)



f <- result |> filter(Year == 1995)

df <- f



find_next_cell <- function(df){

  cell_ref <- df |>
      filter(traqueid_row == 1 | traqueid_pos == 1)

  vecinos <- df |>
    mutate(xdif = X - cell_ref$X,
           ydif = Y - cell_ref$Y) |>
    mutate(distancia = sqrt((xdif)^2 + (ydif^2))) |>
    slice_min(distancia, n = 8) |>
    filter(xdif >= 0) |>
    mutate(potential_rows = cut(ybottom, breaks=2)) |>
    filter(xdif != 0)

  if(length(unique(vecinos$potential_rows)) > 1) {
    next_cell <- vecinos |>
      slice_min(potential_rows) |> # me quedo con el grupo mas bajo de Y
      slice_min(xleft) # dentro de esas con la mas baja de X
  } else {
    next_cell <- vecinos |>  # me quedo con el grupo mas bajo de Y
      slice_min(xleft)
  }


  df_out <- df |>
    mutate(traqueid_row =
             ifelse(idcell == next_cell$idcell, cell_ref$traqueid_row, traqueid_row)) |>
    mutate(traqueid_pos =
             ifelse(idcell == next_cell$idcell,(cell_ref$traqueid_pos + 1), traqueid_pos))

  return(df_out)

  }




ff <- find_next_cell(f)





plot_cells(f)


ggplot(f) +
  geom_rect(aes(xmin=BX, xmax=BX+Width, ymin=BY,  ymax=BY+Height),
            fill="lightblue", colour="lightblue") +
  geom_rect(data = v,
            aes(xmin=BX, xmax=BX+Width,
                ymin=BY,  ymax=BY+Height),
            fill="gray", colour="gray") +
  geom_rect(data = vv,
            aes(xmin=BX, xmax=BX+Width,
                ymin=BY,  ymax=BY+Height),
            fill="blue", colour="blue")



  geom_rect(
    data = (f |> filter(traqueid_row == 1 | traqueid_pos == 1)),
    aes(xmin=BX, xmax=BX+Width,
        ymin=BY,  ymax=BY+Height), fill="red", colour="red")






  # 8 vecinos mas s
  # vecinos <- df[order(df$distancia), ][1:n, ]



