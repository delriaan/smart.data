#' @title Smart Interaction With Data
#' @description
#' \code{smart.data} is an R6 class that facilitates the manipulation of a data set by providing rules-based operations and taxonomical column reference.
#'
#' You will need to install related package \href{https://github.com/delriaan/book.of.utilities}{book.of.utilities}
#'
#' @importFrom magrittr %>% %T>% %<>% or %$%
#' @importFrom data.table like %like% %ilike% %flike%
#' @importFrom book.of.utilities unregex as.regex
#' @importFrom stringi %s+%
#' @importFrom utils hasName
#'
#' @export
smart.data <-	{ R6::R6Class(
	classname = "smart.data"
	, lock_objects = FALSE
	, public	= { list(
		#' @field data A \code{\link[data.table]{data.table}} =object holding the current state of the data
		data = NULL,
		#' @field name The label to use for the data
		name = NULL,
		#' @field id A unique ID for the object
		id = NULL,
		#' @field smart.rules An environment holding user-generated sets of rules
		smart.rules = NULL,
		#' @description Initialize the class object.
		#' @param x The input data
		#' @param name The name for the smart class when used in smart functions
		#' @param ... Arguments used to initialize the smart cache (see \code{\link[cachem]{cache_layered}}).  If none are provided, a composite cache is created of types \code{memory} and \code{disk}, both using defaults (see \code{\link[cachem]{cache_mem}} and \code{\link[cachem]{cache_disk}})
		initialize = function(x, name = "new_data", ...){
			self$smart.rules <- rlang::env(for_naming = NULL, for_usage = new.env())

			if (!"smart_cache" %in% search()){
				attach(new.env(), name = "smart_cache")

				assign(
					x = ".___SMART___"
					, value = cachem::cache_layered(cachem::cache_mem(), ...)
					, envir = as.environment("smart_cache")
					);
			}

			if (is.smart(x)){
				private$orig.data <- data.table::copy(x$data);

				if ("for_usage" %in% names(x$smart.rules)){
					self$smart.rules$for_usage <- x$smart.rules$for_usage;
				}
			} else {
				private$orig.data <- data.table::copy(data.table::as.data.table(x));
				private$history <- new.env();
				private$update.taxonomy <- \(tax){
					tax <- rlang::enexpr(tax) |> as.character();
					cur_fields <- self$smart.rules$for_usage[[tax]]@fields;
					new_fields <- if (rlang::is_empty(cur_fields)|| identical(cur_fields, "")){
						NULL
					} else {
						self$smart.rules$for_usage[[tax]]@fields |>
							purrr::map(\(i){
								purrr::keep(self$smart.rules$for_naming@history, \(x) i %in% x) |> names()
							});
					}
					if (!rlang::is_empty(new_fields)){
						self$smart.rules$for_usage[[tax]]@fields <- new_fields;
					}
					invisible(self);
				}
				private$version <- packageVersion("smart.data");
			}

			self$data <- { data.table::setattr(
				data.table::copy(private$orig.data)
				, "class"
				, c(class(private$orig.data), "smart.data")
				)}

			self$name <- name

			self$id <- paste0(
				self$name, "_"
				, data.table::address(self$data), "_"
				, format(Sys.time(), "%Y%m%d%H%M%S")
				);

			invisible(self);
		},
		#' @description
		#' \code{$naming.rule} provides the prescribed laws for renaming the fields found in \code{self$data}.  The specified fields found in \code{self$data} are renamed when the "law" is enforced.
		#'
		#' @param ... Key-value pairs denoting the naming scheme: new_name = old_name. Items with no key will be used as its own name. \code{\link[rlang]{dots_list}} supported.
		#' @param show (logical) When \code{TRUE} the structure of the rule is printed to console
		#'
		#' @return Before invisibly returning the class object, a list is saved to \code{self$smart.rules$for_naming} containing derived objects used by attribute "law": the expression representing the actions to take when enforced
		naming.rule = function(..., show = FALSE){
			.nms <- rlang::dots_list(..., .named = TRUE)
			if ("rn" %in% names(self$data)){ .nms <- c("rn", .nms) }

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
		},
		#' @description
		#' \code{$taxonomy.rule()} sets class object \code{$smart.rules$for_usage} which is referenced by method \code{$use()}.  \code{$taxonomy.rule} creates a mapping between the usage and data type of a field, labeled by specific terms, and the names of the fields that participate.
		#'
		#' Parameter \code{term.map} is a \code{\link[data.table]{data.table}} object with the following required fields:
		#' \describe{
		#'  \item{term}{The name of the taxonomy term.  Defaults are \code{c("identifier", "event.date", "flag", "demographic", "category", "join.key")}}
		#'  \item{desc}{A description for each term's interpretation or context for usage }
		#'  \item{rule}{A quoted expression containing the rules to be enforced on fields mapped to the current term. The variable for the field must be expressed as 'x' or '.x' and the field name as 'y' or '.y': the law that is enforced exists in the context of \code{\link[purrr]{imap}}}
		#'	\item{fields \code{(optional)}}{The fields in \code{$data} mapped to the term}
		#' }
		#'
		#' @param term.map (NULL) A 2D tabular object that maps terms to rules -> X ~ term<character[]> + desc<character[]> + rule<language[]> (see section 'Term Map')
		#' @param show (logical|FALSE) Should the rule be shown?
		#' @param chatty (logical|FALSE) Should additional information be printed to the console?
		#' @param ... Not used
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
				if (!rlang::is_empty(term.map)){
					purrr::list_merge(default_taxonomy, !!!term.map)
				} else { default_taxonomy }
			} else {
				if (!rlang::is_empty(term.map)){
					term.map
				} else {
					stop("At least one element of `term.map` must be of class 'taxonomy'")
				}
			}) |>
				lapply(\(x){ x@fields |> as.character() |> unique() |> as.list() }) |>
				(\(x){ if (interactive()){ listviewer::jsonedit_gadget(x) } else { x } })() |>
				purrr::compact() |>
				purrr::iwalk(\(x, y){
					if (!rlang::is_empty(unlist(x))){
						# Assign user selections
						self$smart.rules$for_usage[[y]]@fields <<- unlist(x)

						# Update based on naming rules
						private$update.taxonomy(!!y)
					}
				})

			if (show){ print(self$smart.rules$for_usage %$% mget(ls())) }

			invisible(self);
		},
		#' @description
		#' \code{$transformation.rule()} updates class field \code{$data}.  Rules should be provided as quoted expressions. If given as key-value pairs, each key will print to console as a message during enforcement (regardless of the value of argument \code{chatty}). Any loops or other control-of-flow should be defined here, and the object of transformation must use the symbol \code{this.data} and scoped assignment operator \code{<<-} (e.g. \code{this.data <<- 'expression'})
		#'
		#' @param ... Quoted expression containing the code that execute the desired transformations  If names are provided (e.g., does_this = quote(...)), the name will be printed to console if the rule enforcement argument \code{chatty} is |code{TRUE}
		#' @param set.active (integer[]) When provided, the indices of \code{self$smart.rules$for_transformation} that should be set to 'active': other indices are set to 'inactive'
		#' @param show (logical|FALSE) Should the rule be shown?
		#' @param chatty (logical|FALSE) Should additional information be printed to the console?
		#' @param update (logical|FALSE) Should provided rules function as updates?
		transformation.rule = function(..., set.active = 0, show = FALSE, chatty = FALSE, update = FALSE){
			if (!"for_transformation" %in% names(self$smart.rules)){ self$smart.rules$for_transformation <- rlang::exprs(default = { }) }

			.nms = names(self$smart.rules$for_transformation);
			.rules = rlang::enexprs(..., .named = TRUE);

			# Update?
			if (...length() > 0){
				if (update){
					.upd_nms = intersect(names(.rules), .nms);
					.new_nms = setdiff(names(.rules), .upd_nms);

					self$smart.rules$for_transformation[c(.upd_nms, .new_nms)] <- .rules;
				} else {
					self$smart.rules$for_transformation <- .rules;
				}
			}

			purrr::walk(
				1:length(self$smart.rules$for_transformation)
				, \(x) data.table::setattr(self$smart.rules$for_transformation[[x]], "active", (x %in% c(set.active)) | (set.active == 0))
				);

			if (any(purrr::map_lgl(self$smart.rules$for_transformation, \(x) attr(x, "active")))){
				data.table::setattr(self$smart.rules$for_transformation, "state", "pending")
			}

			invisible(self);
		},
		#'	@description
		#'	\code{$enforce.rules} operates on rules saved in \code{$smart.rules} and evaluates the quoted "law" on \code{$data}.  Custom rules can be directly added to class member \code{$smart.rules}: they must have an attribute "law" containing a quoted expression referencing object \code{.data} and \code{rule}.  The rule should contain any objects referenced by the quoted expression.
		#'	@param ... (string[]) The names of the rules to enforce
		#'	@param chatty (logical|FALSE) Should additional execution information be printed to console?
		#'
		#'	@return Invisibly, the class object with member \code{$data} modified according to the rules enforced
		enforce.rules = function(..., chatty = FALSE){
			rules = if (...length() == 0){
					self$smart.rules %$% ls(pattern = "name")
				} else {
					intersect(self$smart.rules %$% ls(), as.character(rlang::enexprs(...)))
				}

			enforce <- \(i){
				# Only enforce pending rules
				if (sprintf("self$smart.rules$%s@state == \"pending\"", i) |> rlang::parse_expr() |> eval()){
					if (chatty){ message("Enforcing "%s+% i) }

					sprintf("self$smart.rules$%s@law |> eval()", i) |>
						rlang::parse_expr() |>
						eval()

					sprintf("self$smart.rules$%s@state <<- \"enforced\"", i) |>
						rlang::parse_expr() |>
						eval()
				}
			}

			purrr::walk(rules, \(x){
				if (grepl("usage", x)){
					self$smart.rules[[x]] %$% ls() |> purrr::walk(\(i){
						enforce(paste0("for_usage$", i))
						private$update.taxonomy(!!i)
					})
				} else {
					enforce(x)
				}
			})

			attr(self$data, "pkg.ver") <- packageVersion("smart.data");

			self$capture();
			invisible(self);
		},
		#' @description
		#' \code{$set.private} operates on the private class list accessed internally as \code{$private} using key-value pairs (i.e. key = value)
		#'
		#' @param ... A named list: existing names in private indicate elements to overwrite
		#'
		#' @return Invisibly, the class object with member \code{$data} modified according to the rules enforced
		set.private = function(...){
			.this <- rlang::list2(...);
			private[names(.this)] <- .this;
			invisible(self);
		},
		#' @description
		#' \code{$reset} restores the object \code{$data} to the original value supplied when the class was initiated.  Rules are set to a state of "pending", and transformation rules are set as inactive (\code{active = FALSE})
		#' @param replay (logical | TRUE) When \code{TRUE}, captured history is executed to reproduce the last saved state.
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#'
		#' @return Invisibly, the class object with member \code{$data} modified to the original state
		reset = function(replay = FALSE, chatty = TRUE){
			.reset = if (chatty & interactive()){
				tcltk::tk_messageBox(
					type = "yesno"
					, message = "Reset data to the original values?", "You sure 'bout that?") != "no"
			} else { FALSE }

			if (.reset){
				self$capture();

				# Reset naming rules
				assign("for_naming", {
					as.name_map(list(
						name_map = list()
						, law = quote(rlang::expr(NULL))
						, state = "pending"
						, history = list()
					))
				}, envir = self$smart.rules)

				# Reset taxonomy
				assign("for_usage", {
					self$smart.rules$for_usage %$% mget(ls()) |>
						purrr::imap(\(x, y){
							as.taxonomy(list(
								term = rlang::sym(y)
								, desc = character()
								, fields = character()
								, law = TRUE
								, state = "enforced"
							))
						}) |> list2env(envir = new.env())
				} , envir = self$smart.rules)

				# Reset data
				self$data <- private$orig.data;
				if (replay){
					message("Under development");

					private$history %$% mget(ls()) |>
						purrr::iwalk(\(x, y){
							message(sprintf("Replaying `%s`: ", y))
							purrr::iwalk(x, \(i, j){
								if (grepl("usage", j)){
									purrr::imap(i, \(ii, jj) as.taxonomy(ii) ) |> list2env(envir = self$smart.rules$for_usage)
									self$taxonomy.rule(chatty = FALSE)
									self$enforce.rules(for_usage)
								} else{
									self$smart.rules$for_naming <- as.name_map(i)
									self$enforce.rules(for_naming)
								}
							})
						})
				}
			}

			invisible(self);
		},
		#' @description
		#' \code{$use} takes as input taxonomy terms and returns the fields in \code{$data} mapped to each term supplied.  The idea here is to provide a semantic method for retrieving data.
		#'
		#' When no arguments are supplied, the default is to return the fields mapped to existing terms.  Using \code{retain = ''} returns all values making it equivalent to \code{smart_obj$data}
		#'
		#' @param ... Taxonomy terms as defined by \code{self$transformation.rule}: can be object names or strings
		#' @param subset A list of quoted expressions that will row-wise filter the data using \code{\link[data.table]{data.table}} syntax
		#' @param retain A vector of strings or symbols denoting the fields to retain in the output.  Pattern-matching is supported
		#' @param omit A vector of strings or symbols denoting the fields to omit from the output
		#' @param show (logical|FALSE) Should the rule be shown?
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#'
		#' @return \code{self$data} with columns selected based on the terms supplied
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
			retain	<- rlang::enexprs(retain) |> .checkout() |>
				paste(collapse = "|") |> grep(names(self$data), value = TRUE)
			omit		<- rlang::enexprs(omit) |> .checkout() |> paste(collapse = "|") |>
				(\(i) if (any(i == "")){ "#"} else { grep(pattern = i, x = names(self$data), value = TRUE) })()

			# Process 'retain', 'omit', and '.field_list', and return the output ====
			all_fields <- field_list[term_list] |>
				unlist() %>% .[. != ""] |>
				c(retain) |>
				purrr::compact() |>
				purrr::discard(\(i) i %in% omit)

			# Return 'self$data' selected by taxonomy fields
			self$data[eval(rlang::enexpr(subset)), mget(all_fields)] |>
				data.table::setattr("class", c("data.table", "data.frame"))
		},
		#' @description
		#' \code{$cache_mgr} adds the current object to the shared "smart" cache space (see \code{\link[cachem]{cache_layered}}).  The shared cache is layered as 'memory' followed by 'disk.'
		#'
		#' @param action One of \code{register} or \code{unregister}
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#' @param ... Additional arguments to use when initializing a cache object
		cache_mgr = function(action, chatty = FALSE, ...){
			if (!"max.sz" %in% ls(private)){
				private$max.sz <- stringi::stri_extract_all_regex(
						str = system("systeminfo", intern = TRUE)
						, pattern = "Avail.+ory.+"
						, omit_no_match = TRUE
						, simplify = TRUE
						) %>%
					.[!. == ""] |>
					stringi::stri_extract_all_regex("[0-9]", simplify = TRUE) |>
						as.vector() |>
						paste(collapse = "") |>
						as.integer() * 0.01 * (1024^2)
			}

			logi_vec <- { c(
					add.cache 	= any(as.character(substitute(action)) %ilike% "^(add|regi)")
					, rem.cache = any(as.character(substitute(action)) %ilike% "^(rem|unreg)")
					, upd.cache = any(as.character(substitute(action)) %ilike% "^(upd|refr)")
					) %>% .[which(.)]
				} |> names();

			add.cache <- function(nm){
				if (.___SMART___$exists(nm)){
					message(sprintf("'%s' already exists in smart-cache", nm))
				} else {
					if (chatty){ message("Adding %s to Smart Cache ..." |> sprintf(nm)) }
					.___SMART___$set(nm, self)
				}
			}

			rem.cache <- function(nm){
				if (.___SMART___$exists(nm)){
					.___SMART___$remove(nm)
				} else {
					message(sprintf("'%s' is not found in the smart-cache", nm))
				}
			}

			upd.cache <- function(nm){
				rlang::caller_env()$rem.cache(nm);
				rlang::caller_env()$add.cache(nm);
			}

			do.call(logi_vec, args = list(nm = self$name));

			if (chatty){ message("Cache object completed!")}
			invisible(self);
		},
		#' @description
		#' \code{$capture} captures the current state of objects in \code{$smart.rules} and saves distinct entries in \code{private$history}
		capture = function(){
			# The function
			f <- \(k, iter = 0){
				purrr::imap(k, \(x, y){
					if (is.environment(x)){
						f(x %$% mget(ls()), iter+1)
					} else {
						slotNames(x) |>
						rlang::set_names() |>
						purrr::imap(\(i, j){
							ex <- rlang::expr(`@`(x, !!rlang::sym(i)))
							rlang::expr_text(eval(ex)) |> rlang::parse_expr()
						})
					}
				})
			}

			# Labeling function
			hist_name <- \(){
				sprintf(
					"hist_%s"
					, rlang::env_names(private$history) |> length() |>
						(`+`)(1) |>
						stringi::stri_pad_left(width = 3, pad = "0")
				)
			}

			# Execution
			assign(hist_name(), self$smart.rules %$% mget(ls()) |> f(), envir = private$history)
		}
	)}
	, private = { list(orig.data = NULL, version = NULL, history = NULL)}
	, active	= { list()}
	)
}
