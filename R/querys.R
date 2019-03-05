#' SelectQuery
#'
#' @param query Requete SQL
#' @param con Connexion sur laquelle executer la connexion sql
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
#' @param query
#' @param con
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
#' @param query
#' @param con
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
