#' @title Get the count of events based on Quad category
#' 
#' @description \code{get_quad_count} takes a vector of quad categories
#' (including verbal/material cooperation/conflict), determines the corresponding
#' CAMEO codes, then get the counts for the specified source/target countries
#' and date by calling \code{get_CAMEO_count}
#' 
#' @param conn A MySQL connection to the ICEWS database. Supply your credentials
#' to \code{conn <- dbConnect(MySQL(), user="username", password="password", 
#' dbname="event_data", host="our.host.ip")}
#' @param quad A string indicates a quad category, i.e. one of
#' \code{verb.coop, verb.conf, matl.coop, matl.confl)}
#' @param date A string in the YYYYMMDD format. If \code{NULL}, will count events on
#' any date
#' @param source.country A string specifying the ISOA3Code of the source country.
#' If \code{NULL}, will count events from all countries
#' @param target.country A string specifying the ISOA3Code of the target country.
#' If \code{NULL}, will count events targeted at all countries
#' @examples
#' \dontrun{
#' library("plyr")
#' quads <- c("verb.coop", "verb.conf", "matl.coop", "matl.conf")
#' # Get quad counts between all country-dyads in a given date
#' result1 <- ldply(quads, .fun=getIcewsQuad, date="20021025")
#' # Get quad counts between one dyad for all dates
#' result2 <- ldply(quads, .fun=getIcewsQuad, 
#'                 source.country="AFG", target.country="ALB")
#' # Get quad counts for one dyad in one date
#' result3 <- ldply(quads, .fun=getIcewsQuad, date="20021025", source.country="AFG", target.country="ALB")
#' }

get_quad_count <- function(conn, quad, date=NULL, source.country=NULL, target.country=NULL) {
  if (!exists("conn")) stop("No MySQL connection ('conn')")
  
  # Get SQL list of roots codes for quad category
  quad.root.codes <- list(
    verb.coop="('01', '02', '03', '04', '05')",
    matl.coop="('06', '07', '08')",
    verb.conf="('09', '10', '11', '12', '13')",
    matl.conf="('14', '15', '16', '17', '18', '19', '20')"
  )
  cameo.codes <- quad.root.codes[[quad]]
  
  # Get counts from DB
  counts <- quadEvents(conn, cameo.codes, date, source.country, target.country)
  
  if (nrow(counts) > 0) {
    res <- data.frame(date=counts[ , 1], 
                      source_country=counts[ , 2],
                      target_country=counts[ , 3],
                      quad_category=rep(quad, nrow(counts)), 
                      quad_count=counts[ , 4])  
  } else {
    res <- data.frame(date=NULL, 
                      source_country=NULL, target_country=NULL, 
                      quad_category=NULL, quad_count=NULL)
  }
  
  return(res)
}