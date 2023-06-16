# https://roxygen2.r-lib.org/articles/rd-other.html
#' Smart Taxonomy Constructor
#' \code{taxonomy} is an S4 class to create a taxonomy object for use with \code{\link{smart.data}}.
#'
#' @slot term A symbol indicating the term to be accessed with \code{\link{smart.data}} method \code{$use()}: must be of class 'language'
#' @slot desc A text description of the term
#' @slot fields One or more strings indicating the fields in \code{smart.data$data} that map to the taxonomy term
#' @slot law A logical constant only provided for compatibility with \code{\link{smart.data}} method \code{$enforce.rules()}
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
		, slots = list(
				term = "language"
				, desc = "character"
				, fields = "vector"
				, law = "logical"
				, state = "character"
				)
		, prototype = list(
				term = rlang::expr(NULL)
				, desc = "~"
				, fields = vector(mode = "character", length = 1L)
				, law = TRUE
				, state = "pending"
				)
	)
}

#' Taxonomy Validation
#'
#' @param x An object of class "taxonomy" created with \code{link{taxonomy}}
#' @return A logical scalar
#' @export
is.taxonomy <- function(x) "taxonomy" %in% class(x)

#' Taxonomy Coercion
#'
#' @param x A list with named elements the same as the slots
#' @return An object of class "taxonomy"
#' @export
as.taxonomy <- function(x){
	x <- purrr::modify_at(x, "state", \(i) "pending") |>
		purrr::modify_at("fields", \(i) if (rlang::is_call(i)){ "" } else { as.character(i) })

	do.call(taxonomy, args = x, quote = TRUE)
}

#' Smart Naming Constructor
#' \code{name_map} is an S4 class to create a naming map for use with \code{\link{smart.data}}
#'
#' @slot name_map A named list with names as the new names of \code{$data} and the values as existing names
#' @slot law A quoted expression that, when executed, will invoke \code{\link[data.table]{setnames}} for compatibility with \code{\link{smart.data}} method \code{$enforce.rules()}
#' @slot history A list holding the history of name changes on each invocation of \code{$naming.rule()}
#' @slot state A string indicating whether the name map has been processed by \code{\link{smart.data}} method \code{$enforce.rules()}
#' @return An object of class "name_map"
#' @export
name_map <- {
	setClass(
		Class = "name_map"
		, slots = list(
				name_map = "list"
				, law = "language"
				, history = "list"
				, state = "character"
				)
		, prototype = list(
				name_map = list()
				, law = rlang::expr(NULL)
				, history = list()
				, state = character()
				)
	)
}

#' Name Map Coercion
#'
#' @param x A list with named elements the same as the slots
#' @return An object of class "name_map"
#' @export
as.name_map <- function(x){
	name_map(
		name_map = eval(x$name_map)
		, law = x$law
		, history = eval(x$history)
		, state = "pending"
		)
}
