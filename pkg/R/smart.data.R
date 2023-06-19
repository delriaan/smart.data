#' @title Smart Interaction With Data
#' @description
#' The goal of the \code{smart.data} package is to provide an API that allows for semantic interaction with tabular data as well as governed manipulation of the same.  Each \code{smart.data} object is an R6 reference class instance which can be symbolically retrieved from memory cache (see \code{\link[cachem]{cachem_mem}}) as well as by direct workspace object invocation.
#'
#' @importFrom magrittr %>% %T>% %<>% or %$%
#' @importFrom stringi %s+%
#' @importFrom utils hasName
#'
#' @export
smart.data <-	{ R6::R6Class(
	classname = "smart.data"
	, lock_objects = FALSE
	, public	= { list(
		#' @field data A \code{\link[data.table]{data.table}} object holding the current state of the data
		data = NULL,
		#' @field name The label to use to semantically retrieve the \code{smart.data} object from cache
		name = NULL,
		#' @field id An auto-generated unique ID for the \code{smart.data} object
		id = NULL,
		#' @field smart.rules An environment holding user-generated sets of rules
		smart.rules = NULL,
		#' @description Initialize the class object.
		#' @param x The input data
		#' @param name The name for the smart class when used in smart functions
		#' @param ... Arguments used to initialize the smart cache (see \code{\link[cachem]{cache_layered}} and related).  If none are provided, the default is to create a memory cache via \code{\link[cachem]{cache_mem}}
		initialize = function(x, name = "new_data", ...){
			if (!".___SMART___" %in% ls("package:smart.data")){
				rlang::env_unlock(rlang::pkg_env("smart.data"))

				assign(
					x = ".___SMART___"
					, value = cachem::cache_layered(cachem::cache_mem(), ...)
					, envir = as.environment("package:smart.data")
					);

				rlang::env_lock(rlang::pkg_env("smart.data"))
			}

			if (is.smart(x)){
				private$orig.data <- copy(x$data);

				if ("for_usage" %in% names(x$smart.rules)){
					self$smart.rules$for_usage <- x$smart.rules$for_usage;
				}
			} else {
				private$orig.data <- copy(as.data.table(x, keep.rownames = TRUE));
				private$history <- new.env();
				private$version <- packageVersion("smart.data");
			}

			self$data <- copy(private$orig.data)

			self$name <- name

			self$id <- paste0(self$name, "_", address(self$data), "_", format(Sys.time(), "%Y%m%d%H%M%S"));

			self$smart.rules <- new.env();
			self$smart.rules[["for_naming"]] <- name_map(name_map = rlang::set_names(names(self$data)), state = "pending")
			self$smart.rules[["for_usage"]] <- new.env()

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
			new_map <- rlang::dots_list(..., .named = TRUE, .ignore_empty = "all")

			cur_map <- self$smart.rules$for_naming

			new_map %<>% .[unlist(.) %in% (cur_map@name_map)]

			if (rlang::is_empty(new_map)){
				message("None of the old names provided are found in existing data field names")
				return(invisible(self))
			}

			new_map <- (\(x, y){
				names(x)[unlist(y) %in% unlist(x)] <- names(y);
				x;
			})(cur_map@name_map, new_map)

			self$smart.rules$for_naming <- { name_map(name_map = new_map) |>
					setattr(
						"history"
						, rbindlist(
								list(new_map, cur_map@name_map)
								, use.names = FALSE
								) |>
							unique())
			}

			if (show){ self$smart.rules$for_naming }

			invisible(self)
		},
		#' @description
		#' \code{$taxonomy.rule()} sets class object \code{$smart.rules$for_usage} which is referenced by method \code{$use()}.  \code{$taxonomy.rule} creates a mapping of fields in \code{$data} to labels that can be used to reference them.
		#'
		#' Parameter \code{term.map} is a \code{\link[data.table]{data.table}} object with the following required fields:
		#'
		#' @param ... (\code{\link[rlang]{dots_list}}) A taxonomy objects created with \code{\link{taxonomy}}. Alternatively, \emph{named} lists may be provided which conform to the following:\cr
		#' \describe{
		#'  \item{term}{(character) The name of the taxonomy term}
		#'  \item{desc}{(character) A description for each term's interpretation or context for usage }
		#'	\item{fields}{(string[]) Optionally provided: contains a vector of field names in \code{$data} mapped to the term}
		#' }
		#' @param gui (logical|FALSE) Should an the GUI for interactive rules management be shown?  \code{TRUE} invokes \code{\link[listviewer]{jsonedit_gadget}} which has the benefit of multi-select drag-n-drop arrangement of terms as well as provides the ability to duplicate field entries under multiple terms.  An additional entry in the GUI ("<DATA NAMES>") is provided containing fields names that can be interactively selected.
		#' @param chatty (logical|FALSE) Should additional information be printed to the console?
		taxonomy.rule = function(..., gui = FALSE){
			term.map <- rlang::dots_list(...,.named = TRUE, .ignore_empty = "all") |> lapply(as.taxonomy)

			# Check for pre-existing term map ====
			default_taxonomy <- self$smart.rules$for_usage %$% mget(ls())

			.taxonomy.exists <- !rlang::is_empty(default_taxonomy);

			# If a naming rule exists and is pending, enforce the naming rule first ====
			if (self$smart.rules$for_naming@state == "pending"){
				self$enforce.rules(for_naming, chatty = chatty)
			}

			# Update the taxonomy, possibly interactively ====
			these_taxonomies <- {
				if (.taxonomy.exists){
					if (!rlang::is_empty(term.map)){
						default_taxonomy[names(term.map)] <- term.map
					} else {
						default_taxonomy
					}
				} else if (!rlang::is_empty(term.map)){
					term.map
				} else {
					stop("At least one element of `term.map` must be of class 'taxonomy'")
				}
			}

			# Create a list of entries per taxonomy object slot @fields
			field_list <- purrr::map(these_taxonomies, \(i) i@fields);

			# Conditional interactive editing of the taxonomy
			if (interactive() & gui){
				field_list <- append(field_list, list(`<DATA NAMES>` = setdiff(names(self$data), unique(unlist(field_list, use.names = FALSE))))) |>
					listviewer::jsonedit_gadget() |>
					purrr::discard_at("<DATA NAMES>") |>
					purrr::compact()
			}

			# Only process entries with non-empty entries for @fields
			purrr::iwalk(field_list, \(i, j){
				if (!rlang::is_empty(i)){
					these_taxonomies[[j]]@fields <- unlist(i)
				}
				assign(j, these_taxonomies[[j]], envir = self$smart.rules$for_usage)
			})

			invisible(self);
		},
		#'	@description
		#'	\code{$enforce.rules} operates on rules saved in \code{$smart.rules} and evaluates the quoted "law" on \code{$data}.  Custom rules can be directly added to class member \code{$smart.rules}: they must have an attribute "law" containing a quoted expression referencing object \code{.data} and \code{rule}.  The rule should contain any objects referenced by the quoted expression.
		#' @param ... (string[]) The names of the rules to enforce
		#' @param chatty (logical|FALSE) Should additional execution information be printed to console?
		#'
		#' @return Invisibly, the class object with member \code{$data} modified according to the rules enforced
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

					sprintf("self$smart.rules$%s@law%s", i, ifelse(grepl("naming", i), " |> eval()", "")) |>
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
		#' \code{$reset} restores the object \code{$data} to the original value and re-initializes rules
		#' @param safe (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#'
		#' @return Invisibly, the class object with member \code{$data} modified to the original state
		reset = function(safe = TRUE){
			replay <- FALSE;

			.reset = if (safe & interactive()){
					tcltk::tk_messageBox(
						type = "yesno"
						, message = "Reset data to the original values?", "You sure 'bout that?"
						) != "no"
				} else {
					!interactive() & !safe
				}

			if (.reset){
				self$data <- copy(private$orig.data)
				self$smart.rules[["for_naming"]] <- name_map(name_map = rlang::set_names(names(self$data)), state = "pending")
				self$smart.rules[["for_usage"]] <- new.env()
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
		#' @param retain,omit A vector of strings and symbols denoting the fields to retain in the output: pattern-matching is supported
		#' @param show (logical|FALSE) Should the rule be shown?
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#'
		#' @return \code{self$data} with columns selected based on the terms supplied
		use = function(..., subset = TRUE, retain = NULL, omit = NULL, show = FALSE, chatty = FALSE){
			# .checkout is a helper function to handle expressions containing calls such as 'c' or 'list'
			.checkout <- \(x){
					if (is.list(x)){
						as.character(x)
					} else if (length(as.character(x)) > 1){
							as.character(x)[-1]
						} else {
							as.character(x)
						}
				}
			term_list <- if (...length() > 0){
				rlang::enexprs(...) |> .checkout()
			} else {
				self$smart.rules$for_usage %$% ls()
			}
			retain <- rlang::enexpr(retain) |> .checkout()
			omit <- rlang::enexpr(omit) |> .checkout()

			if (rlang::is_empty(retain)){ retain <- NULL }
			if (rlang::is_empty(omit)){ omit <- NULL }

			# Create the list of field names to reference, retain, or omit ====
			retain <- if (!rlang::is_empty(retain)){
				paste(retain, collapse = "|") |>
				grep(names(self$data), value = TRUE)
			}

			omit <- if(!rlang::is_empty(omit)){
				paste(omit, collapse = "|") |>
				(\(i) if (any(i == "")){ "#" } else { grep(pattern = i, x = names(self$data), value = TRUE) })()
			}

			# Process 'retain', 'omit', and '.field_list', and return the output ====
			all_fields <- { mget(term_list, envir = self$smart.rules$for_usage) |>
				purrr::map(\(i) i@fields |> unlist()) |>
				purrr::compact() |>
				unlist(use.names = FALSE) |>
				c(retain) |>
				purrr::compact() |>
				purrr::discard(\(i) i %in% omit) |>
				unique()
			}

			# Return 'self$data' selected by taxonomy fields
			if (rlang::is_empty(all_fields)){
				stop("No taxonomy found.")
			} else {
				self$data[eval(rlang::enexpr(subset)), mget(all_fields)]
			}
		},
		#' @description
		#' \code{$cache_mgr} adds the current object to the shared "smart" cache space (see \code{\link[cachem]{cache_layered}}).  The shared cache is layered as 'memory' followed by 'disk.'
		#'
		#' @param action One of the following (partial matching supported):\cr
		#' \itemize{
		#' \item{Add to cache: \code{register}, \code{add}}
		#' \item{Remove from cache: \code{unregister}, \code{remove}}
		#' \item{Update in cache: \code{update}, \code{refresh}}
		#' }
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#' @param ... Additional arguments to use when initializing a cache object
		cache_mgr = function(action, chatty = FALSE, ...){
			if (!"max.sz" %in% ls(private)){
				private$max.sz <- stringi::stri_extract_all_regex(
						str = system("systeminfo", intern = TRUE)
						, pattern = "Avail.+ory.+"
						, omit_no_match = TRUE
						, simplify = TRUE
						) |>
					purrr::discard(\(x) x == "") |>
					stringi::stri_extract_all_regex("[0-9]", simplify = TRUE) |>
					as.vector() |>
					paste(collapse = "") |>
					as.integer() * 0.01 * (1024^2)
			}

			logi_vec <- { c(
					add.cache 	= any(grepl("^(add|regi)"	, x = as.character(substitute(action)), ignore.case = TRUE))
					, rem.cache = any(grepl("^(rem|unreg)", x = as.character(substitute(action)), ignore.case = TRUE))
					, upd.cache = any(grepl("^(upd|refr)"	, x = as.character(substitute(action)), ignore.case = TRUE))
					) |>
					purrr::keep(\(x) x) |>
					names();
				}

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
		#' \code{$capture} captures the current state of objects in \code{$smart.rules} and saves distinct entries as JSON objects.
		capture = function(){
			# The function
			f <- \(k, iter = 0){
				purrr::imap(k, \(x, y){
					if (is.environment(x)){
						f(x %$% mget(ls()), iter+1)
					} else {
						as.list(x) |>
								purrr::modify_at("law", deparse) |>
								jsonlite::toJSON()
					}
				})
			}

			# Labeling function
			hist_name <- \(){
				if (rlang::is_empty(self$smart.rules$for_naming) & rlang::is_empty(self$smart.rules$for_usage)){
					"init_hist"
				} else {
					sprintf(
						"hist_%s"
						, rlang::env_names(private$history) |> length() |>
							stringi::stri_pad_left(width = 3, pad = "0")
						)
				}
			}

			# Execution
			assign(hist_name()
			 , self$smart.rules %$% mget(ls()) |> f()
			 , envir = private$history
			 )
		}
	)}
	, private = { list(orig.data = NULL, version = NULL, history = NULL)}
	, active	= { list(
			#' @field get.history Returns the saved history from calls to \code{$capture()}
			get.history = function(){
				smart_hist <- private$history %$% mget(ls(pattern = "^hist"))
				if (!rlang::is_empty(smart_hist)){
						purrr::imap(smart_hist, \(x, rule){
							purrr::compact(x) |>
								purrr::map(\(i){
									purrr::map(i, \(j){
										jsonlite::fromJSON(j) |>
											purrr::modify_at("law", \(k) paste(k, collapse = "\n" ) |> str2lang())
									})
								})
						})
				} else {
					message("No history found to retrieve: exiting ...")
				}
			}
		)}
	)
}
