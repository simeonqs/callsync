#' @title load.selection.table
#'
#' @description Loads single Raven selection table into a dataframe.
#'
#' @param path_selection_table the path to the file containing the selection
#' table.
#'
#' @return Returns data frame with all selections.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' path_git = 'https://raw.githubusercontent.com'
#' path_repo = '/simeonqs/callsync/master/tests/testthat/files'
#' file_1 = '/2020_10_27_091634.Table.1.selections.txt'
#' file_2 = '/2020_10_27_132148.Table.1.selections.txt'
#' url_1 = paste0(path_git, path_repo, file_1)
#' url_2 = paste0(path_git, path_repo, file_2)
#' local_file_1 = paste(tempdir(), file_1, sep = '/')
#' local_file_2 = paste(tempdir(), file_2, sep = '/')
#' if(!file.exists(local_file_1))
#'   download.file(url_1, destfile = local_file_1, mode = 'wb',)
#' if(!file.exists(local_file_2))
#'   download.file(url_2, destfile = local_file_2, mode = 'wb')
#' st = load.selection.tables(path_selection_tables = tempdir())
#'
#' @export

load.selection.table = function(path_selection_table){

  selection_table = read.csv(path_selection_table, sep = '\t')
  # if there are decibel columns, make sure it still works
  if(ncol(selection_table) == 11){
    temp = read.csv(path_selection_table, sep = '\t',
                    colClasses = c('numeric', 'character', 'numeric',
                                   'numeric', 'numeric', 'numeric', 'numeric',
                                   'character', 'character', 'character',
                                   'character'))
    selection_table =
      selection_table[,colnames(selection_table) %in%
                        c('Selection', 'View', 'Channel', 'Begin.Time..s.',
                          'End.Time..s.', 'Low.Freq..Hz.', 'High.Freq..Hz.',
                          'Delta.Time..s.', 'Annotation')]
  }
  if(ncol(selection_table) == 9){
    selection_table =
      read.csv(path_selection_table, sep = '\t',
               colClasses = c('numeric', 'character', 'numeric', 'numeric',
                              'numeric', 'numeric', 'numeric', 'character',
                              'character'))
  }
  if(ncol(selection_table) == 8){
    selection_table =
      read.csv(path_selection_table, sep = '\t',
               colClasses = c('numeric', 'character', 'numeric', 'numeric',
                              'numeric', 'numeric', 'numeric', 'character'))
  }

  return(selection_table)

}
