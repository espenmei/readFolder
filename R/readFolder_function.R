#' Read folder
#' 
#' This function reads all files in a folder.
#' @param dir directory to folder.
#' @param sep separator for columns in data files. Defaults to tabulator.
#' @param format the format to be returned. Either a list or data.frame. Defaults to data.frame.
#' @param header does the files contain a header. Defaults to TRUE.
#' @examples
#' dir = c:/experiments/experiment1/
#' data = readFolder(dir)

readFolder = function(dir, sep ="\t", format = "data.frame", header = T) {
  # reads all files in directory and combines to one dataframe
  # param
  #   dir = string describing directory
  # return
  #   dataframe containing all subjects
  files = paste(dir, list.files(path = dir), sep = "") # all files in dir
  
  dataList = lapply(files, read.table, sep = sep, header = header) # files to list with [[]] for each subjects
  
  
  if(format == "data.frame") {
    return(do.call(rbind, dataList))# rowbind all subjects to single frame
  } else if(format == "list"){
    return(dataList)
  }
  
}

