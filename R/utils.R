
.check.colnames <- function(col, data){
  if(!col %in% colnames(data)){
    browser
    return(paste0("Column ", col, " not found in dataset."))
  }
}
