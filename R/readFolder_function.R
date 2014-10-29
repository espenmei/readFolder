#' Read folder
#' 
#' This function reads all files in a folder.
#' @param dir directory to folder.
#' @param sep separator for columns in data files. Defaults to tabulator.
#' @param format the format to be returned. Either a list or data.frame. Defaults to data.frame.
#' @param header does the files contain a header. Defaults to TRUE.
#' @param fun function to be applied to each file in the folder.
#' @examples
#' dir = "c:/experiments/experiment1/"
#' data = readFolder(dir)

readFolder = function(dir, pattern = "*", sep = "\t", format = "data.frame", header = T, fun) {
  # reads all files in directory and combines to one dataframe
  # param
  #   dir = string describing directory
  # return
  #   dataframe containing all subjects
  
  #Check parameters
  if (!is.character(dir)) stop("dir needs to be of type character")
  if (!file.exists(dir)) stop(paste("dir '",dir,"' does not exist",sep=""))
  if (length(list.files(dir, pattern = glob2rx(pattern))) == 0) stop(paste("no files in dir matching",pattern))
  if (!is.character(sep)) stop("sep needs to be of type character")
  if (!is.character(format)) stop("format needs to be of type character")
  if (!is.logical(header)) stop("header needs to be of type logical")
  if (!is.character(pattern)) stop("pattern needs to be of type character")
  
  #Remove trailing file separation
  if (substring(dir,nchar(dir)) == .Platform$file.sep) {
    dir = substring(dir,1,nchar(dir)-1)
  }
  
  files = paste(dir, list.files(path = dir, pattern = glob2rx(pattern)), sep = .Platform$file.sep) # all files in dir
  
  dataList = lapply(files, read.table, sep = sep, header = header) # files to list with [[]] for each subjects
  
  if(!missing(fun)) {
    dataList = lapply(dataList, fun) 
  }
  
  if(format == "data.frame") {
    return(do.call(rbind, dataList))# rowbind all subjects to single frame
  } else if(format == "list"){
    return(dataList)
  }
  
}
