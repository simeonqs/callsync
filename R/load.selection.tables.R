#' @title load.selection.tables
#'
#' @description Loads multiple Raven selection tables into one dataframe.
#'  Also adds a column with file-selection
#'
#' @param path_selection_tables the path to the folder containing selection
#' tables. Folder should not contain any other files.
#'
#' @return Returns data frame with all selection tables.
#'
#' @export
#' @importFrom stringr "str_remove"
#' @importFrom dplyr "bind_rows"
#' @importFrom utils "read.csv"

load.selection.tables = function(path_selection_tables){

  files = path_selection_tables |>
    list.files('*txt', full.names = TRUE)
  selection_tables = files |> lapply(load.selection.table)
  names(selection_tables) = files |> basename()|>
    stringr::str_remove('.Table.1.selections.txt')
  dat = selection_tables |> dplyr::bind_rows(.id = 'file') |> as.data.frame()
  dat = dat[dat$View == 'Waveform 1',]

  dat$fs = paste(dat$file, dat$Selection, sep = '-')

  return(dat)

}
