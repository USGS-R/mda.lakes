#'
#'@title Combine file-based table output
#'
#'@param pattern Pattern to be passed to \code{\link{Sys.glob}}
#'@param ... Additional parameters passed to \code{\link{read.table}}
#'
#'@description
#'Attempts to load all files matching a certain pattern and combine
#'them vertically (rbind). Returns the result. If no files match the 
#'pattern, then an empty data.frame is returned.
#'
#'@author Luke Winslow
#'
#'
#'@export
comb_output_table = function(pattern, ...){
	
	files = Sys.glob(pattern)
	
	out = data.frame()
	
	for(i in 1:length(files)){
	  if(file.info(files[i])$size > 0){
  		tmp = read.table(files[i], ...)
  		out = rbind(out, tmp)
	  }
	}
	
	return(out)
}