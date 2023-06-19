#' Check for Signs of Intelligence
#'
#' @description
#' \code{is.smart} checks the argument \code{i} for attributes and objects indicating the "smart.data" class
#'
#' @param ... One or more smart.data objects
#'
#' @return A vector of logical results the length of the input.
#'
#' @export
is.smart <- function(...){
	purrr::map(rlang::list2(...), \(x){
		c(smart.data_class_exists = any(grepl("smart", class(x)))
			, has_orig_data = !purrr::is_empty(x$.__enclos_env__$private$orig.data)
			) |> all(na.rm = TRUE)
		}) |>
		unlist()
}
#
#' Upgrade a Smart Object
#'
#' \code{smart.upgrade} assigns a new \code{smart.data} object to replace the existing object.  The primary use case for this function is when there is a package upgrade, and down-rev objects need to be upgraded.  Objects \code{$smart.rules}, \code{private$orig.data}, \code{$cache}, and \code{$name} are preserved.
#'
#' @param ... Names or symbols of 'smart.data' objects
#' @param env The environment object where the smart object(s) reside
#' @param chatty (logical) When \code{TRUE}, additional messages are sent to the
#'
#' @export
smart.upgrade <- function(..., env = globalenv(), chatty = FALSE){
	if (...length() == 0){ message("Nothing to do!"); return() }

	env <- substitute(env);
	env <- if (class(env) %in% c("call", "name")){ eval(env) } else { eval(str2lang(env)) }

	queue <- as.character(rlang::exprs(...));

	.func <- function(i){
		.temp <- smart.data$new(env[[i]]$data, env[[i]]$name);

		# :: Xfer data
		.temp$.__enclos_env__$private$orig.data <- env[[i]]$.__enclos_env__$private$orig.data;
		.temp$data <- env[[i]]$data;

		# :: Xfer smart.rules
		.temp$smart.rules <- env[[i]]$smart.rules;

		if ("for_transformation" %in% names(.temp$smart.rules)){
			if (purrr::is_empty(attr(.temp$smart.rules$for_transformation, "state"))){
				setattr(.temp$smart.rules$for_transformation, "state", "enforced")
			}

			purrr::walk(.temp$smart.rules$for_transformation, \(x) if (purrr::is_empty(attr(x, "active"))){ setattr(x, "state", FALSE) });
		}

		if (hasName(env[[i]], "cache")){
			.temp$cache_mgr(action = upd);
		}

		# :: Complete the xfer
		if (chatty){ message("Preparing to upgrade " %s+% i, appendLF = FALSE) }
		assign(i, .temp, envir = env);

		if (chatty){ message(": Success!", appendLF = TRUE)}
	}

	purrr::walk(queue, .func);
}
#
#' Get a Smart Object
#'
#' \code{get.smart} retrieves \code{\link{smart.data}} objects from the global smart-cache or throws a message if the global cache does not exist
#'
#' @param ... Names of the smart objects (as found in class member \code{name}) given as symbols or strings
#' @param list.only (logical | FALSE) When \code{TRUE} existing keys of the smart-cache are returned before exiting
#'
#' @return A list of retrieved \code{smart.data} objects, invisibly.
#'
#' @export
get.smart <- function(..., list.only = FALSE){
	if (list.only)( return(.___SMART___$keys()) )

	.these <- if (...length() == 0){
			if (interactive()){
				tcltk::tk_select.list(.___SMART___$keys(), multiple = TRUE, title = "Choose 'smart.data' Objects")
			} else {
				.___SMART___$keys()
			}
	} else { as.character(rlang::exprs(...)) }

	.these <- purrr::map(purrr::set_names(.these), \(x) .___SMART___$get(x));

	if (length(.these) == 1){ .these[[1]] } else { .these }
}
