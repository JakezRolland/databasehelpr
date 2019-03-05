#' SelectQuery
#'
#' @param query Requete SQL
#' @param con Connection DBI
#' @export
#' @import DBI
#' @import errorhandlr
#' @return Renvois un data.frame avec les resultats
SelectQuery <- function(query, con) {
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    rs <- dbSendQuery(con, query)
    data <- fetch(rs)
    dbClearResult(rs)
    return(data)
  }, error = function(err) onError(err,functionName,step ))
}

#' DeleteQuery
#'
#' @param query String SQL query
#' @param con DBI Connection
#'
#' @import DBI
#' @return
#' @export
#'
#' @examples
DeleteQuery <- function(query, con) {
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    rs <- dbSendQuery(con, query)
    dbClearResult(rs)

    return(rs)
  }, error = function(err) onError(err,functionName,step ))
}
#' sendQuery
#'
#' @param query String SQL query
#' @param con DBI connection
#'
#' @return
#' @export
#'
#' @examples
sendQuery <- function(query, con) {
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    rs <- dbSendQuery(con, query)
    return(rs)
  }, error = function(err) onError(err,functionName,step ))
}
