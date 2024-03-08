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
      slice_min(xleft) |>
      slice_min(ybottom) # dentro de esas con la mas baja de X
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

