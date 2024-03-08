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

