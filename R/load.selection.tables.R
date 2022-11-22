#' @title load.selection.tables
#'
#' @description Loads multiple Raven selection tables into one dataframe.
#'  Also adds a column with file-selection
#'
#' @param path_selection_tables the path to the folder containing selection tables. Folder should not contain
#' any other files.
#'
#' @return Returns data frame with all selection tables.
#'
#' @export
#' @importFrom stringr "str_remove"
#' @importFrom dplyr "bind_rows"
#' @importFrom utils "read.csv"

load.selection.tables = function(path_selection_tables){

  selection_tables = path_selection_tables |>
    list.files('*txt', full.names = T) |>
    lapply(function(x){
      temp = read.csv(x, sep = '\t')
      if(ncol(temp) != 8){ # if there are decibel columns, make sure it still works
        temp = read.csv(x, sep = '\t',
                        colClasses = c('numeric', 'character', 'numeric', 'numeric', 'numeric',
                                       'numeric', 'numeric', 'character', 'character', 'character',
                                       'character'))
        temp = temp[,colnames(temp) %in% c('Selection', 'View', 'Channel', 'Begin.Time..s.', 'End.Time..s.',
                                           'Low.Freq..Hz.', 'High.Freq..Hz.', 'Delta.Time..s.', 'Annotation')]
      } else {
        temp = read.csv(x, sep = '\t', colClasses = c('numeric', 'character', 'numeric', 'numeric', 'numeric',
                                                      'numeric', 'numeric', 'character'))
      }
      return(temp)
    })
  names(selection_tables) = path_selection_tables |>
    list.files('*txt') |> stringr::str_remove('.Table.1.selections.txt')
  dat = selection_tables |> dplyr::bind_rows(.id = 'file') |> as.data.frame()
  dat = dat[dat$View == 'Waveform 1',]

  dat$fs = paste(dat$file, dat$Selection, sep = '-')

  return(dat)

}
