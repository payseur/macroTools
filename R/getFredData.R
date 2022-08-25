#' Title
#'
#' @param fetch_symbols
#' @param data_file_name
#' @param data_dir
#' @param force_refresh
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
getFredData <- function(fetch_symbols, data_file_name, data_dir, force_refresh = FALSE, ...) {

  if(nchar(Sys.getenv("FRED_API_KEY")) == 0){
    error("FRED API Key is not set.")
  }

  data_file_path <- file.path(data_dir, paste0(data_file_name,".RDS"))
  if(force_refresh  || !file.exists(data_file_path)){
    dat <- purrr::map_df(fetch_symbols, fredr::fredr, ...) %>% dplyr::select(date, series_id, value)
    readr::write_rds(dat, data_file_path)
  }else{
    dat <- readr::read_rds(data_file_path)
  }
  return(dat)
}
