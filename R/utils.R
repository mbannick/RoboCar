.return.error <- function(err){
  if(length(err) > 0) stop(paste0(err, sep="\n"))
}