rename_dimred_xy <- function(df) {
  colnames(df) <- gsub("^comp_1", "x", colnames(df))
  colnames(df) <- gsub("^comp_2", "y", colnames(df))
  df
}
