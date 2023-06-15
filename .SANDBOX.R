library(magrittr)
self <- new.env()
self$data <- data.table::as.data.table(mtcars)
self$smart.rules <- rlang::env(
	for_naming = NULL
	, for_transformation = list()
	, for_usage = new.env()
	)
#
# :: $enforce.rules() <IN-PROGRESS> ----
self$enforce.rules = function(..., chatty = FALSE){
			.rules = intersect(self$smart.rules %$% ls(), as.character(rlang::enexprs(...)))

			purrr::walk(.rules, \(i){
				rule = self$smart.rules[[i]];

				# Only enforce pending rules
				if (purrr::is_empty(self$smart.rules[[i]]@state)){ self$smart.rules[[i]]@state <<- "pending" }

				if (self$smart.rules[[i]]@state == "pending"){
					if (chatty){ message("Enforcing "%s+% i) }
					self$smart.rules[[i]]@law |> eval()
					self$smart.rules[[i]]@state <<- "enforced"
				}
			});

			attr(self$data, "pkg.ver") <- packageVersion("smart.data");

			invisible(self);
		}
# debug(self$enforce.rules)
#
# :: $naming.rule() <IN-PROGRESS> ----
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

# Function re-definitions <COMPLETE>
naming.rule = function(..., show = FALSE){
	.nms <- rlang::dots_list(..., .named = TRUE)

	cur_law <- if (!rlang::is_empty(self$smart.rules$for_naming)){
		if (self$smart.rules$for_naming@state == "pending"){
			self$enforce.rules(for_naming)
		}
		self$smart.rules$for_naming@law
	} else { NULL }

	cur_names <- if (rlang::is_empty(cur_law)){
		rlang::set_names(names(self$data)) |> as.list()
	} else {
		self$smart.rules$for_naming@history |> lapply(\(x) x[1]) |> as.list()
	}

	new_law <- { rlang::expr(data.table::setnames(
			x = self$data
			, old = !!unlist(.nms, use.names = FALSE)
			, new = !!names(.nms)
			, skip_absent = TRUE
			))
	}

	self$smart.rules$for_naming <- {
		name_map(
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
	names(self$data)
	self$smart.rules$for_naming@history
	self$smart.rules$for_naming@law
	self$smart.rules$for_naming@state

# The next test verifies that the history of name changes is being propagated
# and that the existing rule is enforced (also tests $enforce.rules)
naming.rule(!!!{
	names(self$data) %>%
		rlang::set_names(toupper(.)) %>%
		names() %>%
		rlang::set_names(sample(colors(), length(.)))
}, show = TRUE)
	names(self$data)
	self$smart.rules$for_naming@history
	self$smart.rules$for_naming@law
	self$smart.rules$for_naming@state

#
# :: $taxonomy.rule() <COMPLETE> ----
# Class and Function Definitions
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

update.taxonomy <- \(tax){
	tax <- rlang::enexpr(tax) |> as.character()
	new_fields <- self$smart.rules$for_usage[[tax]]@fields |>
			purrr::map_chr(\(i){
				purrr::keep(self$smart.rules$for_naming@history, \(x) i %in% x) |> names()
			})

	if (!rlang::is_empty(new_fields)){
		self$smart.rules$for_usage[[tax]]@fields <- new_fields
	}

	invisible(self)
}

# Class Instantiate [PASS]
self$smart.rules$for_usage %$% {
	# Test :: Class Instantiations
	identifier <- new("taxonomy", term = rlang::sym("identifier"), desc = "Identifies unique instances of a type of reference")

	flag <- new("taxonomy", term = rlang::sym("flag"), desc = "Logical indicator")

	demographic <- new("taxonomy", term = rlang::sym("demographic"), desc = "Demographic details such as name, date of birth, race, gender")

	category <- new("taxonomy", term = rlang::sym("category"), desc = "Indicates a categorical variable")

	event.date <- new("taxonomy", term = rlang::sym("event.date"), desc = "The event dates or duration boundary dates")

	join.key <- new("taxonomy", term = rlang::sym("join.key"), desc = "Indicates the field(s) to use for 'data.table' joins")
}

# Test: Manual slot assignment [PASS]
# self$smart.rules$for_usage$flag@fields <- c("mpg", "qsec")
# self$smart.rules$for_usage$flag

# Test: Finding old names in updated names using the naming history [PASS]
# cat(glue::glue("Current field names: { paste(names(self$data), collapse=', ')}"), sep ="\n");
#
# # Test: Interactive taxonomy updates [PASS]
# self$smart.rules$for_usage %$% mget(ls()) |>
# 	lapply(\(x){
# 			x@fields |> as.character() |> as.list()
# 		}) |>
# 		listviewer::jsonedit_gadget() |>
# 		purrr::compact() |>
# 		purrr::iwalk(\(x, y){
# 			if (!rlang::is_empty(unlist(x))){
# 				# Assign user selections
# 				self$smart.rules$for_usage[[y]]@fields <<- unlist(x)
#
# 				# Update based on naming rules
# 				update.taxonomy(!!y)
# 			}
# 		})
# self$smart.rules$for_usage %$% mget(ls())
# self$smart.rules$for_usage$join.key

#' @section Function re-definitions <COMPLETE>
#' @note `term.map` should be a string vector or named list of strings
#' @updates:
#' 1. Removed parameter `update` and replaced with `...` to catch legacy argument values
taxonomy.rule = function(term.map = NULL, show = FALSE, chatty = FALSE, ...){

	term.map <- purrr::keep(term.map, is.taxonomy)

	# Check for pre-existing term map ====
	default_taxonomy <- self$smart.rules$for_usage %$% mget(ls())
	.map_exists <- !rlang::is_empty(default_taxonomy);

	# Check to see if a naming rule exists: if so, the taxonomy fields need to follow the law of naming ====
	.pending_names <- self$smart.rules$for_naming@state == "pending";
	if (.pending_names){ self$enforce.rules(for_naming, chatty = chatty) }

	# Update the taxonomy, possibly interactively ====
	(if (.map_exists){
		if (!rlang::is_empty(term.map)){ purrr::list_merge(default_taxonomy, !!!term.map) } else { default_taxonomy }
		} else {
			if (!rlang::is_empty(term.map)){ term.map } else { stop("At least one element of `term.map` must be of class 'taxonomy'") }
		}) |>
		lapply(\(x){ x@fields |> as.character() |> unique() |> as.list() }) |>
		(\(x){ if (interactive()){ listviewer::jsonedit_gadget(x) } else { x } })() |>
		purrr::compact() |>
		purrr::iwalk(\(x, y){
			if (!rlang::is_empty(unlist(x))){
				# Assign user selections
				self$smart.rules$for_usage[[y]]@fields <<- unlist(x)

				# Update based on naming rules
				update.taxonomy(!!y)
			}
		})

	if (show){ print(self$smart.rules$for_usage %$% mget(ls())) }

	invisible(self);
}

taxonomy.rule()
self %$% mget(ls())

#
# :: $use() (related to taxonomy.rule) <COMPLETE> ----
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
use(category, retain=c(slate)) %>% print()
use(omit=pink) %>% print()
use(omit=c(pink, orchid)) %>% print()

