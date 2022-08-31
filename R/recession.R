


#' Title
#'
#' @param fetch_symbol
#' @param data_file_name
#' @param data_dir
#' @param force_refresh
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getFredRecessionData <- function(fetch_symbol = "USREC", data_file_name, data_dir, force_refresh = FALSE, ...){
  dat <- getFredData(fetch_symbol, data_file_name, data_dir, force_refresh, ...)

  rec <- dat %>%
    filter(series_id == fetch_symbol) %>%
    mutate(start_date = lubridate::as_date(ifelse((lag(value)-value == -1) , date, NA))) %>%
    mutate(end_date = lubridate::as_date(ifelse((lead(value)-value == -1), date, NA))) %>%
    mutate(y0 = lag(value)-value) %>%
    mutate(y1 = lead(value)-value) %>%
    select(start_date,end_date)

  start_rec <- rec[which(!is.na(rec[,1])),1]
  end_rec <- rec[which(!is.na(rec[,2])),2]

  if(nrow(start_rec) > nrow(end_rec)){
    end_rec <- rbind(end_rec,NA)
  }else{
    start_rec <- rbind(NA, start_rec)
  }
  recs <- cbind(start_rec, end_rec)

  return(recs)
}

