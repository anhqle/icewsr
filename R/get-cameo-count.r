#' @title Get the count of events based on CAMEO codes.
#' 
#' @description \code{get_CAMEO_count} returns the count of all the events that have
#' a specified CAMEO code
#' 
#' @details This function takes one (or several) CAMEO codes, a date, and two
#' vectors for source and target countries. It will return the count of events
#' that have the specified CAMEO code and that happen between the source and
#' target countries on the given date.
#' 
#' @param conn A MySQL connection to the ICEWS database. Supply your credentials
#' to \code{conn <- dbConnect(MySQL(), user="username", password="password", 
#' dbname="event_data", host="our.host.ip")}
#' @param cameo.codes A string of the CAMEO codes
#' @param date A string in the YYYYMMDD format. If \code{NULL}, will count events on
#' any date
#' @param source.country A string specifying the ISOA3Code of the source country.
#' If \code{NULL}, will count events from all countries
#' @param target.country A string specifying the ISOA3Code of the target country.
#' If \code{NULL}, will count events targeted at all countries
#' @examples
#' \dontrun{
#' get_CAMEO_count <- function(conn, cameo.codes="('01', '02', '03', '04', '05')",
#' date="20021025", source.country="AFG", target.country="ALB")
#' }

get_CAMEO_count <- function(conn, cameo.codes, date=NULL, source.country=NULL, target.country=NULL) {
  sql.select <- ("SELECT e.event_date AS DATE
                 , cSource.ISOA3Code AS source_country
                 , cTarget.ISOA3Code AS target_country
                 , COUNT(*) AS quad_count")
  sql.from <- ("FROM simple_events e
               JOIN eventtypes t ON e.eventtype_id = t.eventtype_ID
               JOIN countries cSource ON e.source_country_id = cSource.id
               JOIN countries cTarget ON e.target_country_id = cTarget.id")
  
  # Write the where clause, which depends on whether country is supplied or not
  # If not, use all the countries, i.e. only impose the cameo.codes where condition
  sql.where <- paste("WHERE", paste("SUBSTRING(t.code, 1, 2) IN", cameo.codes))
  if (!is.null(source.country)) {
    sql.where <- paste(sql.where, "AND", 
                       paste0("cSource.ISOA3Code IN ('", source.country, "')"))
  }
  if (!is.null(target.country)) {
    sql.where <- paste(sql.where, "AND", 
                       paste0("cTarget.ISOA3Code IN ('", target.country, "')"))
  }
  # date is YYYYMMDD
  if (!is.null(date)) {
    sql.where <- paste(sql.where, "AND",
                       paste0("e.event_date IN (STR_TO_DATE('", date, "', '%Y%m%d'))"))
  }
  
  # Write the group by clause, grouping by date, source, and target
  sql.group <- ("GROUP BY e.event_date, e.source_country_id, e.target_country_id
                ORDER BY e.event_date ASC")
  
  # Paste the select, from, where, group clause
  sql <- paste(sql.select, sql.from, sql.where, sql.group, ";")
  
  # Get and return the result
  res <- dbGetQuery(conn, sql)
  return(res)
}