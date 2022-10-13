# app initialization and load dataset
#' Launch Lucidum
#' @description Launch Lucidum
#' @keywords internal
#' @export
lucidum <- function(dataset=NULL, ...) {
  server_env <- environment(server)
  if(is.null(dataset)){
    .GlobalEnv$lucidum_dataset_name <- NULL
  } else {
    .GlobalEnv$lucidum_dataset_name <- deparse(substitute(dataset))
  }
  on.exit(rm(lucidum_dataset_name, envir=.GlobalEnv))
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, launch.browser = TRUE, ...)
}
init_lucidum <- function(session){
  # populate the table list
  table_lists <- return_global_env_tables()
  if(exists('lucidum_dataset_name')){
    if(!is.null(get('lucidum_dataset_name'))){
      if(lucidum_dataset_name %in% unlist(table_lists)){
        # I am reapting the same thing over and over here, need to simplify
        updateSelectInput(session,
                          inputId = 'dataset',
                          choices = table_lists,
                          selected = lucidum_dataset_name
        )
      } else {
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "dataset_error",
                          title = 'Dataset error',
                          text = 'Not a valid lucidum dataset',
                          btn_labels = c('OK')
        )
        updateSelectInput(session,
                          inputId = 'dataset',
                          choices = c('select dataset', table_lists),
                          selected = 'select dataset'
        )
      }
    } else {
      updateSelectInput(session,
                        inputId = 'dataset',
                        choices = c('select dataset', table_lists),
                        selected = 'select dataset'
      )
    }
  } else {
    updateSelectInput(session,
                      inputId = 'dataset',
                      choices = c('select dataset', table_lists),
                      selected = 'select dataset'
    )
  }
}
load_dataset <- function(dataset){
  if(!is.null(dataset) & dataset!='select dataset'){
    if(exists(dataset)){
      if(valid_lucidum_dt(get(dataset))){
        d <- setDT(get(dataset))
      } else {
        stop()
      }
      # check columns are all of correct type - nothing funny like shapefiles etc
      # col_classes <- unname(sapply(d, function(x){class(x)[1]}))
      # if(!(all(col_classes %in% c('numeric','character','integer','POSIXct','Date','logical')))){
      #   d <- NULL
      # }
    } else {
      d <- NULL
    }
  } else {
    d <- NULL
  }
  return(d)
}
contains_postcode_cols <- function(d){
  # TRUE if d contains one of PostcodeUnit, PostcodeSector or PostcodeArea
  # 'PostcodeUnit' %in% names(d) | 'PostcodeSector' %in% names(d) | 'PostcodeArea' %in% names(d)
  'PostcodeArea' %in% names(d)
}
valid_lucidum_dt <- function(d){
  valid <- TRUE
  if(nrow(d)<=1) {valid<-FALSE}
  if(ncol(d)<=1) {valid<-FALSE}
  if(length(numerical_cols(d))==0) {valid<-FALSE}
  return(valid)
}
return_global_env_tables <- function(){
  # function returns list of tables present in Global Environment
  # split by whether they contain postcode columns or not
  # with special tables used by Toolkit removed from list
  names_not_allowed <- c('d')
  starting_tables <- Filter(function(x) is.data.frame(get(x)), ls(envir=.GlobalEnv))
  starting_tables <- Filter(function(x) valid_lucidum_dt(get(x)), starting_tables)
  starting_tables <- c(setdiff(starting_tables, names_not_allowed))
  postcode_tables <- Filter(function(x) contains_postcode_cols(get(x)), starting_tables)
  postcode_tables <- c(setdiff(postcode_tables, names_not_allowed))
  non_postcode_tables <- setdiff(starting_tables, postcode_tables)
  if(length(postcode_tables)==1) postcode_tables <- list(postcode_tables)
  if(length(non_postcode_tables)==1) non_postcode_tables <- list(non_postcode_tables)
  # include number of rows and columns
  n_rows <- function(x){paste0('(',format(nrow(get(x)), big.mark = ','),
                               ' x ',
                               format(ncol(get(x)), big.mark = ',')
                               ,', ',
                               format(utils::object.size(get(x)), units = 'auto'),
                               ')'
                               )}
  if(length(postcode_tables)>0){
    names(postcode_tables) <- paste(postcode_tables, lapply(postcode_tables, n_rows))
  }
  if(length(non_postcode_tables)>0){
    names(non_postcode_tables) <- paste(non_postcode_tables, lapply(non_postcode_tables, n_rows))
  }
  list('Datasets with PostcodeArea column for MappaR' = postcode_tables,
       'Other datasets' = non_postcode_tables
       )
}
possible_response_columns <- function(d, original_cols, lucidum_cols){
  original_numerical_cols <- intersect(original_cols, numerical_cols(d))
  lucidum_numerical_cols <- intersect(lucidum_cols, numerical_cols(d))
  list('Original dataset columns' = original_numerical_cols,
       'lucidum columns' = lucidum_numerical_cols)
}
possible_weight_columns <- function(){

}
modify_banding_level <- function (current_banding_level, modifier){

  # the banding levels are
  # 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50 etc
  # function below lets us move through these levels

  if (modifier==0){
    1
  } else {

    exponent <- floor(log10(current_banding_level))
    mantissa <- current_banding_level / 10^exponent
    overrule <- NA
    if(modifier==-1){

      # couple of special rules to get 4 and 12 bandings in there (good for months/quarters)
      if(current_banding_level==20){
        overrule <- 12
      } else if (current_banding_level==12){
        overrule <- 10
      } else if (current_banding_level==5){
        overrule <- 4
      } else if (current_banding_level==4){
        overrule <- 2
      }

      if(mantissa==1){
        mantissa <- 5
        exponent <- exponent - 1
      } else if (mantissa==2) {
        mantissa <- 1
      } else if (mantissa==5){
        mantissa <- 2
      }
    } else if (modifier==1){

      # couple of special rules to get 4 and 12 bandings in there (good for months/quarters)
      if(current_banding_level==2){
        overrule <- 4
      } else if (current_banding_level==4){
        overrule <- 5
      } else if (current_banding_level==10){
        overrule <- 12
      } else if (current_banding_level==12){
        overrule <- 20
      }

      if(mantissa==1){
        mantissa <- 2
      } else if (mantissa==2) {
        mantissa <- 5
      } else if (mantissa==5){
        mantissa <- 1
        exponent <- exponent + 1
      }
    }

    if(is.na(overrule)){
      mantissa * 10^exponent
    } else {
      overrule
    }

  }
}
banding_guesser <- function(x){

  # speed up - just use first 10000 rows
  if(length(x)>10000){x <- x[1:10000]}

  s <- stats::sd(x, na.rm = TRUE)/20

  if(is.na(s) | is.nan(s)) {s <- 1}
  if (s==0){s <- 1}
  exponent <- floor(log10(s))
  mantissa <- s / 10^exponent
  if (mantissa<2){
    m <- 1
  } else if (mantissa<5) {
    m <- 2
  } else {
    m <- 5
  }
  banding <- m * 10^(exponent+1)

  # some special cases to modify banding
  if(length(table(x))<=5 & min(x, na.rm = TRUE)==0 & max(x, na.rm = TRUE)==1) banding <- 1
  if(length(table(x))<=100 & min(x, na.rm = TRUE)<=20 & max(x, na.rm = TRUE)<=100 & max(x, na.rm = TRUE)>=1) banding <- 1

  if(class(x)[1] %in% c('IDate','Date', 'POSIXct')){
    banding <- pmax(1,pmin(10,banding))
  }
  # return banding
  banding


}
