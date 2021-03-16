pace <- function(hour, min, sec, dist) {
  secs <- hour * 3600 + min * 60 + sec
  pace <- secs / dist
  paste(
    pace %/% 60, "min", 
    round(pace %% 60,1), "sec"
    )
}
