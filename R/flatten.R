#' Flatten (Nested) Lists.
#'
#' Flatten \code{lists} according to specifications made via
#' \code{start_after} and/or \code{stop_at}. When keeping 
#' the defaults, the function will traverse \code{src} to retrieve the
#' values at the respective bottom layers/bottom elements. These values are
#' arranged in a named \code{list} where the respective names can be
#' interpreted as the the paths to the retrieved values.   
#'
#' @param x An arbitrarily deeply nested \code{list}
#' @param start_after An \code{integer} specifying the layer after which to 
#' start the flattening. \code{NULL} means to start at the very top.
#' @param stop_at An \code{integer} specifying the layer at which to stop
#' the flattening. \code{NULL} means there is not stop criterion.
#' @param delim_path A \code{character} specifying how the names
#' of the resulting flattened list should be pasted.
#' @param ... Further args.
#' @return A named \code{list} that features the desired degree of flattening.
#' @author Janko Thyson \email{janko.thyson.rstuff@@googlemail.com}
#' @export
flatten <- function (x, start_after=NULL, stop_at=NULL, delim_path=".",
                     do_warn=TRUE, ... ) {
  
  # VALIDATE
  if (!is.list(x)) {
    stop("'src' must be a list.")
  }
  if (not.null(start_after) && not.null(stop_at)) {
    if (start_after == 1 && stop_at == 1)
      stop(sprintf("Invalid specification:\nstart_after: %s\nstop_at: %s\n",
                   start_after, stop_at))
  }

  # INNER FUNCTIONS
  .startAfterInner <- function(envir, nms, out.1, ...) {
    idx_diff <- diff(c(envir$start_after, length(envir$counter)))

    # UPDATE IF DEGREE OF NESTEDNESS EXCEEDS START CRITERION
    if (idx_diff > 0) {
      idx_cutoff <- seq(length(envir$counter) - idx_diff + 1, length(envir$counter))
      idx_left <- envir$counter[-idx_cutoff]
      nms.1 <- nms[idx_cutoff]
      names(out.1) <- paste(nms.1, collapse=envir$delim_path)
      # UPDATE SRC
      idx_append <- sapply(envir$history, function (x_hist) {
        all(idx_left == x_hist)        
      })

      if (any(idx_append)) {                                          
        envir$src[[idx_left]] <- append(envir$src[[idx_left]], values=out.1)                    
      } else {
        envir$src[[idx_left]] <- out.1
        # UPDATE HISTORY
        envir$history <- c(envir$history, list(idx_left))
      }
      envir$out <- envir$src          
    } else if (idx_diff < 0) {
      envir$out <- envir$src
    }
    # RESET
    envir$nms <- envir$nms[-length(envir$nms)]
    envir$counter <- envir$counter[-length(envir$counter)]
    return(TRUE)
  }
  
  .updateOutInner <- function (envir, out.1, ...) {
    # UPDATE OUT
    envir$out <- c(get("out", envir = envir), out.1)
    # RESET
    envir$nms <- envir$nms[-length(envir$nms)]
    envir$counter <- envir$counter[-length(envir$counter)]
    return(TRUE)
  }
  
  .flattenInner <- function(x, envir, ...) {
    if ( is(x, "list") && length(x) != 0 ) {
      # UPDATE
      envir$counter_history <- c(envir$counter_history, list(envir$counter))
      
      # EXIT IF DEGREE EXCEEDS CUTOFF
      if (not.null(envir$stop_at)) {
        if (length(envir$counter) > envir$stop_at) { 
          nms <- get("nms", envir=envir)
          out.1 <- list(x)
          names(out.1) <- paste(nms, collapse=envir$delim_path)

          # DECISION ON FLATTENING
          if (not.null(envir$start_after)) {
            .startAfterInner(envir=envir, nms=nms, out.1=out.1)
            return(NULL)
          } else {
            .updateOutInner(envir=envir, out.1=out.1)
            return(NULL)
          }
        }
      }

      # LOOP OVER ELEMENTS
      for (i in seq_along(x)) {
        # UPDATE COUNTER
        envir$counter <- c(envir$counter, i)
        # UPDATE NAMES
        list_names <- if (is.null(names(x[i]))) paste0("X", i) else names(x[i])
        assign("nms", c(get("nms", envir=envir), list_names), envir=envir)
        # RECURSIVE FLATTENING
        .flattenInner(x=x[[i]], envir) # call  recursively
        # RESET COUNTER
        if (i == length(x)) {
          envir$nms <- envir$nms[-length(envir$nms)]
          envir$counter <- envir$counter[-length(envir$counter)]
        }
      }
    } else {
      nms <- get("nms", envir=envir)
      out.1 <- list(x)
      names(out.1) <- paste(nms, collapse=envir$delim_path)

      # DECISION ON FLATTENING
      if (not.null(envir$start_after))
        .startAfterInner(envir=envir, nms=nms, out.1=out.1)
      else
        .updateOutInner(envir=envir, out.1=out.1)
    }
        
    return(TRUE)
  }
  
  out                     <- list()
  # ENVIR
  envir                   <- new.env()
  envir$counter           <- NULL
  envir$counter_history   <- NULL
  envir$delim_path        <- delim_path
  envir$do_warn           <- do_warn
  envir$do_block_warning  <- FALSE
  envir$history           <- NULL
  envir$nms               <- NULL
  envir$out               <- list()
  envir$src               <- x
  envir$start_after       <- start_after
  
  if (not.null(stop_at)) {
    stop_at_0 <- stop_at
    if (stop_at == 1) {
      return(src)
    } else {
      stop_at <- stop_at - 1
    }
  }
  
  envir$stop_at           <- stop_at

  .flattenInner(x, envir)
  
  if (envir$do_warn) {
    max_length <- max(sapply(envir$counter_history, length))

    if (not.null(start_after)) {            
      if (start_after > max_length) {                        
        warning(paste0("Argument 'start_after=", start_after, 
                       "' exceeds maximum degree of sublayer nestedness (=", 
                       max_length, ")."))
      }
    }
    if (not.null(stop_at)) {
      if (stop_at_0 > max_length){
        warning(paste0("Argument 'stop_at=", stop_at_0, 
                       "' exceeds maximum degree of sublayer nestedness (=", 
                       max_length, ")."))    
      }
    }
  }
  
  out <- envir$out
  return(out)    
}

