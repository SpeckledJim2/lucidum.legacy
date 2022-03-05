.onLoad <- function(libname, pkgname) {
  #SPECIFICATION_PATH <- options()$lucidum$specification_path
  # so lucidum knows where to look for specification files
  path <- options()$lucidum$specification_path
  if(is.null(path)){
    path <- 'specification path has not been set'
  } else if (!dir.exists(path)){
    path <- paste0('specification path "', path,'" does not exist')
  }
  assign("SPECIFICATION_PATH",
         path,
         envir = parent.env(environment())
  )
}
