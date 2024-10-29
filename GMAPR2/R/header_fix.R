header_fix <- function(x){
  data <- x
  column_names <- colnames(data)
    # rename_at(vars(ends_with('TimeStamp'), ~"TimeStamp"))
  # colnames(data) <- column_names
  return(data)
}
