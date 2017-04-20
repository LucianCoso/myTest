

# Read docx file
readDoc <- function(file)
  {
  if(!grepl(".docx$", file))
    {
    stop("Uploaded file must be a .docx file!")
    }
  doc <-as.data.frame(getTablesFromDocx(file))
  list(values = doc$VALUE[1])
  }

