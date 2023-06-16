#' @Issue https://github.com/delriaan/smart.data/issues/3

# library(magrittr)
# self <- new.env()
# private <- new.env()
# self$data <- data.table::as.data.table(mtcars)
# private$orig.data <- data.table::as.data.table(mtcars)

# DEF :: Function re-definitions <COMPLETE> ----
# self$naming.rule = function(..., show = FALSE){
# 	.nms <- rlang::dots_list(..., .named = TRUE)
#
# 	cur_law <- if (!rlang::is_empty(self$smart.rules$for_naming)){
# 		if (self$smart.rules$for_naming@state == "pending"){
# 			self$enforce.rules(for_naming)
# 		}
# 		self$smart.rules$for_naming@law
# 	} else { NULL }
#
# 	cur_names <- if (rlang::is_empty(cur_law)){
# 		rlang::set_names(names(self$data)) |> as.list()
# 	} else {
# 		self$smart.rules$for_naming@history |> lapply(\(x) x[1]) |> as.list()
# 	}
#
# 	new_law <- { rlang::expr(data.table::setnames(
# 			x = self$data
# 			, old = !!unlist(.nms, use.names = FALSE)
# 			, new = !!names(.nms)
# 			, skip_absent = TRUE
# 			))
# 	}
#
# 	self$smart.rules$for_naming <- {
# 		name_map(
# 			name_map = .nms
# 			, law = new_law
# 			, history = data.table::rbindlist(list(.nms, cur_names), use.names = FALSE) |> unique()
# 			, state = "pending"
# 			)}
#
# 	if (show){ print(new_law) }
#
# 	invisible(self)
# }
# self$taxonomy.rule = function(term.map = NULL, show = FALSE, chatty = FALSE, ...){
# 	term.map <- purrr::keep(term.map, is.taxonomy)
#
# 	# Check for pre-existing term map ====
# 	default_taxonomy <- self$smart.rules$for_usage %$% mget(ls())
#
# 	.map_exists <- !rlang::is_empty(default_taxonomy);
#
# 	# Check to see if a naming rule exists: if so, the taxonomy fields need to follow the law of naming ====
# 	.pending_names <- self$smart.rules$for_naming@state == "pending";
# 	if (.pending_names){ self$enforce.rules(for_naming, chatty = chatty) }
#
# 	# Update the taxonomy, possibly interactively ====
# 	(if (.map_exists){
# 			if (!rlang::is_empty(term.map)){ purrr::list_merge(default_taxonomy, !!!term.map) } else { default_taxonomy }
# 	} else {
# 		if (!rlang::is_empty(term.map)){
# 			term.map
# 		} else {
# 			stop("At least one element of `term.map` must be of class 'taxonomy'")
# 		}
# 	}) |>
# 	lapply(\(x){ x@fields |> as.character() |> unique() |> as.list() }) |>
# 	(\(x){ if (interactive()){ listviewer::jsonedit_gadget(x) } else { x } })() |>
# 	purrr::compact() |>
# 	purrr::iwalk(\(x, y){
# 		if (!rlang::is_empty(unlist(x))){
# 			# Assign user selections
# 			self$smart.rules$for_usage[[y]]@fields <<- unlist(x)
#
# 			# Update based on naming rules
# 			private$update.taxonomy(!!y)
# 		}
# 	})
#
# 	if (show){ print(self$smart.rules$for_usage %$% mget(ls())) }
#
# 	invisible(self);
# }
# self$use = function(..., subset = TRUE, retain = NULL, omit = NULL, show = FALSE, chatty = FALSE){
# 	# .checkout is a helper function to handle expressions containing calls such as 'c' or 'list'
# 	.checkout <- (\(x){
# 		as.call(
# 			rlang::list2(
# 				c
# 				, !!!sapply(x, \(i){ if ("call" %in% class(i)){ as.character(i[-1]) } else { as.character(i) }})
# 			)) |>
# 			eval() |>
# 			unique()
# 	})
#
# 	field_list	<- self$smart.rules$for_usage %$% mget(ls()) |> lapply(\(x) x@fields)
# 	term_list <- if (...length() > 0){ rlang::enexprs(...) |> .checkout() } else { names(field_list) }
#
# 	# Create the list of field names to use ====
# 	retain	<- rlang::enexprs(retain) |> .checkout() |> paste(collapse = "|") |> grep(names(self$data), value = TRUE)
# 	omit		<- rlang::enexprs(omit) |> .checkout() |> paste(collapse = "|") |>
# 		(\(i) if (any(i == "")){ "#"} else { grep(pattern = i, x = names(self$data), value = TRUE) })()
#
# 	# Process 'retain', 'omit', and '.field_list', and return the output ====
# 	all_fields <- field_list[term_list] |>
# 		unlist() %>% .[. != ""] |>
# 		c(retain) |>
# 		purrr::compact() |>
# 		purrr::discard(\(i) i %in% omit)
#
# 	# Return 'self$data' selected by taxonomy fields
# 	self$data[eval(rlang::enexpr(subset)), mget(all_fields)] |>
# 		data.table::setattr("class", c("data.table", "data.frame"))
# }
# self$reset = function(replay = FALSE, chatty = TRUE){
# 	.reset = if (chatty & interactive()){
# 			tcltk::tk_messageBox(type = "yesno", message = "Reset data to the original values?", "You sure 'bout that?") != "no"
# 		} else { FALSE }
#
# 	if (.reset){
# 		self$capture();
#
# 		# Reset naming rules
# 		assign("for_naming", {
# 			as.name_map(list(
# 				name_map = list()
# 				, law = quote(rlang::expr(NULL))
# 				, state = "pending"
# 				, history = list()
# 				))
# 		}, envir = self$smart.rules)
#
# 		# Reset taxonomy
# 		assign("for_usage", {
# 			self$smart.rules$for_usage %$% mget(ls()) |>
# 				purrr::imap(\(x, y){
# 					as.taxonomy(list(
# 						term = rlang::sym(y)
# 						, desc = character()
# 						, fields = character()
# 						, law = TRUE
# 						, state = "enforced"
# 						))
# 				}) |> list2env(envir = new.env())
# 		} , envir = self$smart.rules)
#
# 		# Reset data
# 		self$data <- private$orig.data;
# 		if (replay){
# 			message("Under development");
#
# 			private$history %$% mget(ls()) |>
# 				purrr::iwalk(\(x, y){
# 					message(sprintf("Replaying `%s`: ", y))
# 					purrr::iwalk(x, \(i, j){
# 						if (grepl("usage", j)){
# 							purrr::imap(i, \(ii, jj) as.taxonomy(ii) ) |> list2env(envir = self$smart.rules$for_usage)
# 							self$taxonomy.rule(chatty = FALSE)
# 							self$enforce.rules(for_usage)
# 						} else{
# 							self$smart.rules$for_naming <- as.name_map(i)
# 							self$enforce.rules(for_naming)
# 						}
# 					})
# 				})
# 		}
# 	}
#
# 	invisible(self);
# }
# self$enforce.rules = function(..., chatty = FALSE){
# 			rules = if (...length() == 0){
# 					self$smart.rules %$% ls(pattern = "name")
# 				} else {
# 					intersect(self$smart.rules %$% ls(), as.character(rlang::enexprs(...)))
# 				}
#
# 			enforce <- \(i){
# 				# Only enforce pending rules
# 				if (sprintf("self$smart.rules$%s@state == \"pending\"", i) |> rlang::parse_expr() |> eval()){
# 					if (chatty){ message("Enforcing "%s+% i) }
#
# 					sprintf("self$smart.rules$%s@law |> eval()", i) |>
# 						rlang::parse_expr() |>
# 						eval()
#
# 					sprintf("self$smart.rules$%s@state <<- \"enforced\"", i) |>
# 						rlang::parse_expr() |>
# 						eval()
# 				}
# 			}
#
# 			purrr::walk(rules, \(x){
# 				if (grepl("usage", x)){
# 					self$smart.rules[[x]] %$% ls() |>
# 						purrr::walk(\(i){
# 							enforce(paste0("for_usage$", i))
# 							private$update.taxonomy(!!i)
# 						})
# 				} else {
# 					enforce(x)
# 				}
# 			})
#
# 			attr(self$data, "pkg.ver") <- packageVersion("smart.data");
#
# 			self$capture();
# 			invisible(self);
# 		}
#
# DEF :: New Class and Function Definitions <COMPLETE> ----
# taxonomy <- { setClass(
# 	Class = "taxonomy"
# 	, slots = list(
# 			term = "language"
# 			, desc = "character"
# 			, fields = "vector"
# 			, law = "logical"
# 			, state = "character"
# 			)
# 	, prototype = list(
# 			term = rlang::expr(NULL)
# 			, desc = "~"
# 			, fields = vector(mode = "character", length = 1L)
# 			, law = TRUE
# 			, state = "pending"
# 			)
# )}
# is.taxonomy <- \(x) "taxonomy" %in% class(x)
# as.taxonomy <- \(x){
# 	x <- purrr::modify_at(x, "state", \(i) "pending") |>
# 		purrr::modify_at("fields", \(i) if (rlang::is_call(i)){ "" } else { as.character(i) })
#
# 	do.call(taxonomy, args = x, quote = TRUE)
# }
# name_map <- { setClass(
# 	Class = "name_map"
# 	, slots = list(
# 			name_map = "list"
# 			, law = "language"
# 			, history = "list"
# 			, state = "character"
# 			)
# 	, prototype = list(
# 			name_map = list()
# 			, law = rlang::expr(NULL)
# 			, history = list()
# 			, state = character()
# 			)
# )}
# as.name_map <- \(x){
# 	name_map(
# 		name_map = eval(x$name_map)
# 		, law = x$law
# 		, history = eval(x$history)
# 		, state = "pending"
# 		)
# }
# self$capture <- \(){
# 	# The function
# 	f <- \(k, iter = 0) purrr::imap(k, \(x, y){
# 			# message(sprintf("Processing `%s`:", y))
# 			if (is.environment(x)){
# 				# message(sprintf("... recursion L%s", iter + 1))
# 				f(x %$% mget(ls()), iter+1)
# 			} else {
# 				methods::slotNames(x) |>
# 					rlang::set_names() |>
# 					purrr::imap(\(i, j){
# 						# message(sprintf("Processing `%s`:", y))
# 						ex <- rlang::expr(`@`(x, !!rlang::sym(i)))
# 						rlang::expr_text(eval(ex)) |> rlang::parse_expr()
# 					})
# 			}
# 		})
#
# 	# Labeling function
# 	hist_name <- \(){ sprintf(
# 		"hist_%s"
# 		, rlang::env_names(private$history) |> length() |>
# 			(`+`)(1) |>
# 			stringi::stri_pad_left(width = 3, pad = "0")
# 		)
# 	}
#
# 	# Execution
# 	assign(hist_name(), self$smart.rules %$% mget(ls()) |> f(), envir = private$history)
# }

# Set during initialization with `$new()` ----
# private$history <- new.env()
# private$update.taxonomy <- \(tax){
# 	tax <- rlang::enexpr(tax) |> as.character()
# 	cur_fields <- self$smart.rules$for_usage[[tax]]@fields
#
# 	new_fields <- if (rlang::is_empty(cur_fields)|| identical(cur_fields, "")){
# 			NULL
# 		} else {
# 			self$smart.rules$for_usage[[tax]]@fields |>
# 			purrr::map(\(i){
# 				purrr::keep(self$smart.rules$for_naming@history, \(x) i %in% x) |> names()
# 			})
# 		}
#
# 	if (!rlang::is_empty(new_fields)){
# 		self$smart.rules$for_usage[[tax]]@fields <- new_fields
# 	}
#
# 	invisible(self)
# } # <-Set during initialization with `$new()`
#
# self$smart.rules <- rlang::env(for_naming = NULL, for_usage = new.env())
#
# TEST :: Manually define taxonomy objects
self$smart.rules$for_usage %$% {
	# Test :: Class Instantiations
	identifier <- new("taxonomy", term = rlang::sym("identifier"), desc = "Identifies unique instances of a type of reference")

	flag <- new("taxonomy", term = rlang::sym("flag"), desc = "Logical indicator")

	demographic <- new("taxonomy", term = rlang::sym("demographic"), desc = "Demographic details such as name, date of birth, race, gender")

	category <- new("taxonomy", term = rlang::sym("category"), desc = "Indicates a categorical variable")

	event.date <- new("taxonomy", term = rlang::sym("event.date"), desc = "The event dates or duration boundary dates")

	join.key <- new("taxonomy", term = rlang::sym("join.key"), desc = "Indicates the field(s) to use for 'data.table' joins")
}
#
# TEST :: $naming.rule() <COMPLETE> ----
self$naming.rule(!!!{names(self$data) %>% rlang::set_names(toupper(.))}, show = TRUE)
	names(self$data)
	self$smart.rules$for_naming@history
	self$smart.rules$for_naming@law
	self$smart.rules$for_naming@state

# The next test verifies that the history of name changes is being propagated
# and that the existing rule is enforced (also tests $enforce.rules)
self$naming.rule(!!!{
	names(self$data) %>%
		rlang::set_names(toupper(.)) %>%
		names() %>%
		rlang::set_names(sample(colors(), length(.)))
}, show = TRUE)
	names(self$data)
	self$smart.rules$for_naming@history
	self$smart.rules$for_naming@law
	self$smart.rules$for_naming@state

self$enforce.rules(for_naming)
	names(self$data)
	self$smart.rules$for_naming@history
	self$smart.rules$for_naming@law
	self$smart.rules$for_naming@state
#
# TEST :: $taxonomy.rule() <COMPLETE> ----
self$taxonomy.rule()
self$enforce.rules(for_usage)
self$smart.rules$for_usage %$% mget(ls())
#
# TEST :: $use() (related to taxonomy.rule) <COMPLETE> ----
self$use() %>% names()
self$use(category, retain=c(green)) %>% names()
self$use(omit=chart) %>% names()
self$use(omit=c(chart, brown)) %>% names()
#
# TEST :: $reset()* <COMPLETE> ----
self %$% ls.str()
self$reset(replay = TRUE, chatty = TRUE)
self %$% ls.str()
#
# TEST :: $capture()* <COMPLETE> ----
private$history %$% mget(ls()) |> View()

