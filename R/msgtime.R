#' Overwrite [message()], [warning()], and [stop()] to prepend a time
#' stamp to their output.
#'
#' This function overwrites the functions in the `base` package rather
#' than merely masking them, so all packages are affected.
#'
#' @param enable Whether to enable or disable the time
#'   stamping. Setting this to `FALSE` will restore the original
#'   functions.
#' 
#' @note This is potentially dangerous, since it involves messing
#'   around with the base package's namespace. The functionality of
#'   warning() and stop() is also slightly degraded, since their
#'   immediate calling environment is changed to the wrapper function.
#'
#' @examples
#' message("Hello.")
#' warning("Hello?")
#' \dontrun{
#' stop("Hello!")
#' }
#' msgtime(TRUE)
#' message("Hello.")
#' warning("Hello?")
#' \dontrun{
#' stop("Hello!")
#' }
#' msgtime(FALSE)
#' message("Hello.")
#' @export
msgtime <- local({backup <- list();
  function(enable){
    if(enable){
      if(length(backup)){
        warning("Timestamper is already enabled.", call.=FALSE)
      }else{
        backup <<- list()
        backup$message <<- base::message
        unlockBinding("message", baseenv())
        assign("message",
               function(..., domain = NULL, appendLF = TRUE){
                 timestamp <- paste0("[",as.character(Sys.time()),"] ")
                 backup$message(timestamp, appendLF=FALSE)
                 args <- list(...)
                 if(!missing(domain)) args$domain <- domain
                 if(!missing(appendLF)) args$appendLF <- appendLF
                 do.call(backup$message, args)
               }, baseenv())
        lockBinding("message", baseenv())
        
        backup$warning <<- base::warning
        unlockBinding("warning", baseenv())
        assign("warning",
               function(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
                        domain = NULL){
                 timestamp <- paste0("[",as.character(Sys.time()),"] ")
                 args <- list(timestamp, ...)
                 if(!missing(call.)) args$call. <- call.
                 if(!missing(immediate.)) args$immediate. <- immediate.
                 if(!missing(noBreaks.)) args$noBreaks. <- noBreaks.
                 if(!missing(domain)) args$domain <- domain
                 
                 do.call(backup$warning, args)
               }, baseenv())
        lockBinding("warning", baseenv())
        
        backup$stop <<- base::stop
        unlockBinding("stop", baseenv())
        assign("stop",
               function(..., call. = TRUE, domain = NULL){
                 timestamp <- paste0("[",as.character(Sys.time()),"] ")
                 args <- list(timestamp, ...)
                 if(!missing(call.)) args$call. <- call.
                 if(!missing(domain)) args$domain <- domain
                 
                 do.call(backup$stop, args)
               }, baseenv())
        lockBinding("stop", baseenv())
      }
    }else{
      if(length(backup)==0){
        warning("Timestamper is already disabled.", call.=FALSE)
      }else{
        unlockBinding("message", baseenv())
        assign("message", backup$message, baseenv())
        lockBinding("message", baseenv())
        
        unlockBinding("warning", baseenv())
        assign("warning", backup$warning, baseenv())
        lockBinding("warning", baseenv())
        
        unlockBinding("stop", baseenv())
        assign("stop", backup$stop, baseenv())
        lockBinding("stop", baseenv())
        
        backup <<- list()
      }
    }
    invisible()
  }
})
