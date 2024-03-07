# Auxiliary functions

plot_cells <- function(df, centroides = TRUE,
                       bx = "BX", width = "Width", by = "BY", height = "Height",
                       x= "X", y = "Y") {

  p <- ggplot(df) +
    geom_rect(aes(xmin = !!sym(bx), xmax = !!sym(bx) + !!sym(width),
                  ymin = !!sym(by), ymax = !!sym(by) + !!sym(height)))

  if (centroides == TRUE) {
    out <- p + geom_point(aes(x = !!sym(x), y = !!sym(y)))
  } else {
    out <- p
  }

  return(out)
}




#
# ggplot(d) +
#   geom_rect(aes(xmin=BX, xmax=BX+Width, ymin=BY,  ymax=BY+Height))+
#   geom_point(aes(x = X, y = Y)) +
#   geom_point(data = bottom_left_cell,
#              aes(x = X, y = Y), colour = "red")
#
#
# ggplot(d) +
#   geom_rect(aes(xmin=BX, xmax=BX+Width, ymin=BY,  ymax=BY+Height)) +
#   ,
# colour = as.factor(row), fill= as.factor(row_cluster))) +
#   geom_point(aes(x = X, y = Y))
