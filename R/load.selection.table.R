#' @title load.selection.table
#'
#' @description Loads single Raven selection table into a dataframe.
#'
#' @param path_selection_table the path to the file containing the selection
#' table.
#'
#' @return Returns data frame with all selections.
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
