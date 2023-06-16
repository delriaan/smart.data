# https://roxygen2.r-lib.org/articles/rd-other.html
#' Smart Taxonomy Constructor
#' \code{taxonomy} is an S4 class to create a taxonomy object for use with \code{\link{smart.data}}.
#'
#' @slot term A symbol indicating the term to be accessed with \code{\link{smart.data}} method \code{$use()}: must be of class 'language'
#' @slot desc A text description of the term
#' @slot fields One or more strings indicating the fields in \code{smart.data$data} that map to the taxonomy term
#' @slot law Passively set: used by \code{\link{smart.data}} method \code{$enforce.rules()}
#' @slot state A string indicating whether the taxonomy term has been processed by \code{\link{smart.data}} method \code{$enforce.rules()}
#'
#' @return An object of class "taxonomy"
#'
#' @examples
#' identifier <- new("taxonomy", term = rlang::sym("identifier"), desc = "Identifies unique instances of a type of reference")
#'
#' @export
taxonomy <- {
	setClass(
		Class = "taxonomy"
		, slots = list(term = "character", desc = "character"
									 , fields = "character", law = "language", state = "character")
		)}

setMethod("initialize", "taxonomy"
	, function(.Object, term, state = "pending", desc = "~", fields = character(), ...){
		.Object <- callNextMethod()
		.Object@term <- term
		.Object@desc <- desc
		.Object@fields <- fields
		.Object@law <- rlang::expr(private$update.taxonomy(!!rlang::ensym(term)))
		.Object@state <- state
		.Object
	})

setMethod("as.list", "taxonomy"
	, function(x, ...){
			lapply(getSlots("taxonomy") |> names() |> rlang::set_names()
						 , \(nm) slot(x, nm))
	})

#' Taxonomy Validation
#'
#' @param x An object of class "taxonomy" created with \code{link{taxonomy}}
#' @return A logical scalar
#' @export
is.taxonomy <- function(x) {
	is(x, "taxonomy") & all(c("term", "desc") %in% names(getSlots("taxonomy")))
}

#' Taxonomy Coercion
#'
#' @param x A list, JSON string, data.* object with named elements the same as the slots in class \code{\link{taxonomy}}
#'
#' @return Objects of class "taxonomy"
#' @export
as.taxonomy <- function(x){
	x <- as.list(x)
	stopifnot(c("term", "desc") %in% names(x))
	do.call(taxonomy, args = x)
}

setMethod("as.taxonomy", "taxonomy"
	, definition = function(x){ x })

setMethod("as.taxonomy", "list"
	, definition = function(x){
			do.call(taxonomy, args = x)
	})

setMethod("as.taxonomy", "data.table"
	, function(x){
			apply(x, MARGIN = 1, FUN = as.taxonomy(), simplify = FALSE) |>
			list2env(envir = rlang::caller_env())
	})

setMethod("as.taxonomy", "character"
	, function(x){
			if (is(x, "json")){
				jsonlite::fromJSON(x) |> as.list() |> list2env(envir = rlang::current_env())
				rm(x)
				law <- substitute(str2lang(i), env = list(i = law))
				do.call(taxonomy, args = mget(ls()))
			} else {
				message("Cannot convert to taxonomy (unsupported class)")
				return(invisible())
			}
	})

#' Smart Naming Constructor
#' \code{name_map} is an S4 class to create a naming map for use with \code{\link{smart.data}}
#'
#' @slot name_map A named list with names as the new names of \code{$data} and the values as existing names
#' @slot law A quoted expression that, when executed, will invoke \code{\link[data.table]{setnames}} for compatibility with \code{\link{smart.data}} method \code{$enforce.rules()}
#' @slot state A string indicating whether the name map has been processed by \code{\link{smart.data}} method \code{$enforce.rules()}
#' @return An object of class "name_map"
#' @export
name_map <- {
	setClass(
		Class = "name_map"
		, slots = list(name_map = "vector", law = "call", state = "character")
		)}

setMethod("initialize", "name_map"
	, function(.Object, name_map, state = "pending", law = rlang::expr(ls()), ...){
		.Object <- callNextMethod()
		.Object@name_map <- rlang::dots_list(!!!name_map, .named = TRUE, .ignore_empty = "all", .homonyms = "last")
		.Object@law <- { rlang::expr(
			data.table::setnames(
				x = self$data
				, old = !!unlist(.Object@name_map, use.names = FALSE)
				, new = !!names(.Object@name_map)
				, skip_absent = TRUE
			))
		}
		.Object@state <- state
		.Object
	})

#' Name Map Coercion
#'
#' @param x A list with named elements the same as the slots
#' @return An object of class "name_map"
#' @export
as.name_map <- function(x){
	x <- as.list(x)
	name_map(name_map = x$name_map, law = x$law)
}

setMethod("as.list", "name_map"
	, function(x, ...){
		lapply(getSlots("name_map") |> names() |> rlang::set_names()
					 , \(nm) slot(x, nm))
	})
