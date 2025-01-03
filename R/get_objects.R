# function to remove all the objects created in the current function to clean up the environment:
# from this stackoverflow post: https://stackoverflow.com/questions/72543027/is-there-an-r-function-to-specifically-remove-objects-created-within-one-script
get.objects <- function(path2file = NULL, exception = NULL, source = FALSE, message = FALSE) {
  library("utils")
  library("tools")
  
  # Step 0-1: Possibility to leave path2file = NULL if using RStudio.
  # We are using rstudioapi to get the path to the current file
  if(is.null(path2file)) path2file <- rstudioapi::getSourceEditorContext()$path
  
  # Check that file exists
  if (!file.exists(path2file)) {
    stop("couldn't find file ", path2file)
  }
  
  # Step 0-2: If .Rmd file, need to extract the code in R chunks first
  # Use code in https://felixfan.github.io/extract-r-code/
  if(file_ext(path2file)=="Rmd") {
    require("knitr")
    tmp <- purl(path2file)
    path2file <- paste(getwd(),tmp,sep="/")
    source = TRUE # Must be changed to TRUE here
  }
  
  # Step 0-3: Start by running the script if you are calling an external script.
  if(source) source(path2file)
  
  # Step 1: screen the script
  summ_script <- getParseData(parse(path2file, keep.source = TRUE))
  
  # Step 2: extract the objects
  list_objects <- summ_script$text[which(summ_script$token == "SYMBOL")]
  # List unique
  list_objects <- unique(list_objects)
  
  # Step 3: find where the objects are.
  src <- paste(as.vector(sapply(list_objects, find)))
  src <- tapply(list_objects, factor(src), c)
  
  # List of the objects in the Global Environment
  # They can be in both the Global Environment and some packages.
  src_names <- names(src)
  
  list_objects = NULL
  for (i in grep("GlobalEnv", src_names)) {
    list_objects <- c(list_objects, src[[i]])
  }
  
  # Step 3bis: if any exception, remove from the list
  if(!is.null(exception)) {
    list_objects <- list_objects[!list_objects %in% exception]
  }
  
  # Step 4: done!
  # If message, print message:
  if(message) {
   # cat(paste0("  ",length(list_objects)," objects  were created in the script \n  ", path2file,"\n"))
  }
  
  return(list_objects)
}