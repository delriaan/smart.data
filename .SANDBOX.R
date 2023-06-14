library(magrittr)
self <- new.env()
self$data <- data.table::as.data.table(mtcars)
self$smart.rules <- rlang::env(
	for_naming = NULL
	, for_transformation = list()
	, for_usage = new.env()
	)

# :: Naming <IN-PROGRESS> ----
# Class Definition
name_map <- { setClass(
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
)}

# Function re-definitions <IN-PROGRESS>
naming.rule = function(..., show = FALSE){
	.nms <- rlang::dots_list(..., .named = TRUE)

	cur_law <- if (!rlang::is_empty(self$smart.rules$for_naming)){ self$smart.rules$for_naming@law } else { NULL }

	cur_names <- if (rlang::is_empty(cur_law)){
			rlang::set_names(names(self$data)) |> as.list()
		} else { self$smart.rules$for_naming@history }

	new_law <- { rlang::expr(data.table::setnames(
			x = self$data
			, old = !!unlist(.nms, use.names = FALSE)
			, new = !!names(.nms)
			, skip_absent = TRUE
			))
	}

	self$smart.rules$for_naming <- { name_map(
			name_map = .nms
			, law = new_law
			, history = data.table::rbindlist(list(.nms, cur_names), use.names = FALSE) |> unique()
			, state = "pending"
			)}

	if (show){ print(new_law) }

	invisible(self)
}

# Tests
naming.rule(!!!{names(self$data) %>% rlang::set_names(toupper(.))}, show = TRUE)
self$smart.rules$for_naming@history
self$smart.rules$for_naming@law

# The next test verifies that the history of name changes is being propagated
naming.rule(!!!{
	names(self$data) %>%
		rlang::set_names(toupper(.)) %>%
		names() %>%
		rlang::set_names(sample(colors(), length(.)))
}, show = TRUE)
self$smart.rules$for_naming@history
self$smart.rules$for_naming@law
#
# :: Taxonomy <IN-PROGRESS> ----
# Class Definition
taxonomy <- { setClass(
	Class = "taxonomy"
	, slots = list(
			term = "language"
			, desc = "character"
			, fields = "character"
			, law = "logical"
			, state = "character"
			)
	, prototype = list(
			term = rlang::expr(NULL)
			, desc = character()
			, fields = character()
			, law = TRUE
			, state = "enforced"
			)
)}

is.taxonomy <- \(x) "taxonomy" %in% class(x)

# Class Instantiate
self$smart.rules$for_usage %$% {
	# Test :: Class Instantiations
	identifier <- new("taxonomy", term = rlang::sym("identifier"), desc = "Identifies unique instances of a type of reference")

	flag <- new("taxonomy", term = rlang::sym("flag"), desc = "Logical indicator")

	demographic <- new("taxonomy", term = rlang::sym("demographic"), desc = "Demographic details such as name, date of birth, race, gender")

	category <- new("taxonomy", term = rlang::sym("category"), desc = "Indicates a categorical variable")

	event.date <- new("taxonomy", term = rlang::sym("event.date"), desc = "The event dates or duration boundary dates")

	join.key <- new("taxonomy", term = rlang::sym("join.key"), desc = "Indicates the field(s) to use for 'data.table' joins")
}

# Test: Manual slot assignment
self$smart.rules$for_usage$flag@fields <- c("mpg", "qsec")

# Test: Finding old names in updated names using the naming history
(\(.needle){
	purrr::map_chr(.needle, \(i){
		purrr::keep(self$smart.rules$for_naming@history, \(x) i %in% x) |> names()
	})
})(c("mpg", "qsec"))

#' @section Function re-definitions <IN-PROGRESS>
#' @note `term.map` should be a string vector or named list of strings
#' @re
taxonomy.rule = function(term.map = NULL, update = FALSE, show = FALSE, chatty = FALSE){
	update_taxonomy <- \(.needle){
		purrr::map_chr(.needle, \(i){
			purrr::keep(self$smart.rules$for_naming@history, \(x) i %in% x) |> names()
		})
	}

	term.map <- purrr::keep(term.map, is.taxonomy)

	if (rlang::is_empty(term.map)){
		stop("At least one element of `term.map` must be of class 'taxonomy'")
	}

	# Check for pre-existing term map ====
	.map_exists <- !rlang::is_empty(self$smart.rules$for_usage %$% ls());
	if (.map_exists){ .dflt_map <- self$smart.rules$for_usage %$% mget(ls()) }

	# Check to see if a naming rule exists: if so, the taxonomy fields need to follow the law of naming ====
	if (self$smart.rules$for_naming@state == "pending"){
		self$enforce.rules(for_naming, chatty = chatty)
	}

	# Update the term map?
	if (update & !rlang::is_empty(self$smart.rules$for_naming)){
		if (!rlang::is_empty(self$smart.rules$for_naming@history)){
			term.map <- purrr::map(\(i){ i@fields <- update_taxonomy(i@fields) })
		}
	}

	# Define/Update the taxonomy ====

	if (show){ print(self$smart.rules$for_usage) }


	invisible(self);
}

#
# :: Use (related to taxonomy.rule) <IN-PROGRESS> ----
use = function(..., subset = TRUE, retain = NULL, omit = NULL, show = FALSE, chatty = FALSE){
	# .checkout is a helper function to handle expressions containing calls such as 'c' or 'list'
	.checkout <- (\(x){
		as.call(
			rlang::list2(
				c
				, !!!sapply(x, \(i){ if ("call" %in% class(i)){ as.character(i[-1]) } else { as.character(i) }})
			)) |>
			eval() |>
			unique()
	})

	field_list	<- self$smart.rules$for_usage %$% mget(ls()) |> lapply(\(x) x@fields)
	term_list <- if (...length() > 0){ rlang::enexprs(...) |> .checkout() } else { names(field_list) }

	# Create the list of field names to use ====
	retain	<- rlang::enexprs(retain) |> .checkout() |> paste(collapse = "|") |> grep(names(self$data), value = TRUE)
	omit		<- rlang::enexprs(omit) |> .checkout() |> paste(collapse = "|") |>
		(\(i) if (any(i == "")){ "#"} else { grep(pattern = i, x = names(self$data), value = TRUE) })()

	# Process 'retain', 'omit', and '.field_list', and return the output ====
	all_fields <- field_list[term_list] |>
		unlist() |>
		c(retain) |>
		purrr::compact() |>
		purrr::discard(\(i) i %in% omit)

	# Return 'self$data' selected by taxonomy fields
	self$data[eval(rlang::enexpr(subset)), mget(all_fields)] |>
		data.table::setattr("class", c("data.table", "data.frame"))
}
# debug(use)

# Tests
use() %>% print()
use(flag, retain=c(cyl)) %>% print()
use(omit=gear) %>% print()
use(omit=c(gear, vs)) %>% print()
