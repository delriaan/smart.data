#' @title Smart Interaction With Data
#' @description
#' \code{smart.data} is an R6 class that facilitates the manipulation of a data set by providing rules-based operations and taxonomical column reference.
#'
#' You will need to install related package \href{https://github.com/delriaan/book.of.utilities}{book.of.utilities}
#'
#' @importFrom magrittr %>% %T>% %<>% or
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
		#' @field smart.rules A list holding user-generated sets of rules
		smart.rules = list(),
		#' @description
		#' \code{print} Prints the contents of \code{$data} before invisibly returning the class object
		print = function(){
			message(sprintf("Smart Data, version %s", private$version));

			if (!purrr::is_empty(self$smart.rules)){
				message(c(
					"Smart Rules:"
					, purrr::imap_chr(self$smart.rules, \(x,y) sprintf("... %s [%s]", y, attr(x, "state") %||% "unknown"))
					) |> paste(collapse = "\n"));
			}
			print(self$data);
		},
		#' @description Initialize the class object.
		#' @param x The input data
		#' @param name The name for the smart class when used in smart functions
		#' @param ... Arguments used to initialize the smart cache (see \code{\link[cachem]{cache_layered}}).  If none are provided, a composite cache is created of types \code{memory} and \code{disk}, both using defaults (see \code{\link[cachem]{cache_mem}} and \code{\link[cachem]{cache_disk}})
		initialize = function(x, name = "new_data", ...){
			if (!hasName(rlang::pkg_env("smart.data"), ".___SMART___")){
				rlang::env_unlock(rlang::pkg_env("smart.data"));

				assign(
					x = ".___SMART___"
					, value = cachem::cache_layered(cachem::cache_mem(), ...)
					, envir = rlang::pkg_env("smart.data")
					);

				rlang::env_lock(rlang::pkg_env("smart.data"));
			}

			if (is.smart(x)){
				private$orig.data <- data.table::copy(x$data);

				if ("for_usage" %in% names(x$smart.rules)){
					self$smart.rules$for_usage <- x$smart.rules$for_usage;
				}
			} else { private$orig.data <- data.table::copy(data.table::as.data.table(x)) }

			self$data <- data.table::setattr(data.table::copy(private$orig.data), "class", c(class(private$orig.data), "smart.data"));
			private$version <- packageVersion("smart.data");

			self$name <- { ifelse(
				name %in% (.___SMART___$keys())
				, paste0(name, "_", which(grepl(name, .___SMART___$keys())) |> max() |> stringi::stri_pad_left(width = 3, pad = "0"))
				, name
				)
			}

			self$id <- paste0(self$name, "_", data.table::address(self$data), "_", format(Sys.time(), "%Y%m%d%H%M%S"));

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
			.nms <- if (...length() == 0){
						list(new = names(self$data), old = names(self$data))
					} else {
						rlang::exprs(..., .named = TRUE) |> (\(x) list(new = names(x), old = unlist(x)))()
					}

			self$smart.rules$for_naming <- .nms |>
					data.table::setattr(
						"law"
						, rlang::expr(data.table::setnames(
								x = self$data
								, old = !!.nms$old
								, new = !!.nms$new
								, skip_absent = TRUE
								))
						) |>
					data.table::setattr("state", "pending");

			if (show){
				message(paste0(
					"Current naming rules:\n----------\n"
					, paste(
							names(self$smart.rules$for_naming)
							, self$smart.rules$for_naming
							, sep = " <- "
							, collapse = "\n"
							)
					))
			}

			invisible(self);
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
		#' @param update (logical|TRUE) Should the existing map be updated via interactive selection?
		#' @param show (logical|FALSE) Should the rule be shown?
		#' @param chatty (logical|FALSE) Should additional information be printed to the console?
		taxonomy.rule = function(term.map = NULL, update = FALSE, show = FALSE, chatty = FALSE){
			# @def self$smart.rules$for_usage ~ term.map + fields<character[][]>
			# @def .law: An expression representing the actions to take when enforced

			# Check for pre-existing term map ====
			.map_exists <- hasName(self$smart.rules, "for_usage");

			.default_map <- { data.table::data.table(
				term = c("identifier", "demographic", "flag", "event.date", "category", "join.key")
				, desc = { c(
						"Identifies unique instances of a type of reference"
						, "Demographic details such as name, date of birth, race, gender"
						, "Logical indicator"
						, "The event dates or duration boundary dates"
						, "Indicates a categorical variable"
						, "Indicates the field(s) to use for 'data.table' joins"
						)}
				, rule = replicate(6, rlang::expr(x))
				, fields = replicate(6, list(character()))
				, key = "term"
				)
			}

			if (.map_exists) {
				.default_map <- data.table::rbindlist(list(
					.default_map[!(term %in% self$smart.rules$for_usage$term)]
					, self$smart.rules$for_usage
					), use.names = TRUE)
			}

			# Check to see if a naming rule exists: if so, the taxonomy fields need to follow the law of naming ====
			if ("for_naming" %in% names(self$smart.rules)){
				if (attr(self$smart.rules$for_naming, "state") == "pending"){
					self$enforce.rules(for_naming, chatty = chatty)
				}

				if (!purrr::is_empty(term.map)){
					term.map <- term.map[, fields := purrr::map(fields, \(x){
						purrr::modify_if(
							x
							, \(i) !rlang::is_empty(i)
							, \(i){ stringi::stri_replace_all_fixed(
										i
										, pattern = self$smart.rules$for_naming
										, replacement = names(self$smart.rules$for_naming)
										, vectorize_all = FALSE
										)
									}
							)}
						)]
				}
			}

			# Validate and process 'term.map' ====
			if (!purrr::is_empty(term.map)){
				if (!data.table::is.data.table(term.map)){ data.table::setDT(term.map) }

				if (rlang::is_empty(intersect(c("term", "desc"), names(term.map)))){
					stop("Error: each taxonomy must have the following fields provided: term, desc");
				} else {
					term.map[, c("term", "desc") := purrr::map(mget(c("term", "desc")), unlist)]
				}

				# term.map <- term.map[(if (!update){ !term %in% .default_map$term })]; # Add or update terms (anything remaining when not updating is added)
			} else {
				term.map <- .default_map
			}

			term.map[, rule := rlang::expr(x)]

			# Update the term map?
			if (update){
				# Update or retain current/default
				term.map <- data.table::rbindlist(list(.default_map[!(term %in% term.map$term)], term.map), use.names = TRUE, fill = TRUE)
			}

			data.table::setkey(term.map, term);

			# Define/Update the taxonomy ====
			.taxonomy <- {
					# Choose the values of 'term' to define/update
					.term_list = {
						tcltk::tk_select.list(
							choices = term.map$term
							, preselect = if (update){ term.map[purrr::map_lgl(term.map$fields, \(x) any(!purrr::is_empty(unlist(x)))), term] }
							, multiple = TRUE
							, title = paste0("Choose one or more terms to map to fields in ", self$name)
							) |>
							purrr::compact() |> (\(x){
								if (purrr::is_empty(x)){
									message("Nothing selected: existing terms retained.");
									term.map$term
								} else { x }
							})()
					}

					# Populate 'fields' for each term
					term.map[(term %in% .term_list)][
					, fields := purrr::map2(term, fields, \(x, y){
							.out = if (any(purrr::is_empty(unlist(y))) || update){
									tcltk::tk_select.list(
										choices = names(self$data)
										, title = sprintf("Choose term [%s] fields", x)
										, preselect = if (!identical(character(), unlist(y))){ unlist(y) }
										, multiple = TRUE
										)
								} else { unlist(y) }
							if (purrr::is_empty(.out)){ NA } else { list(c(.out)) }
						})
					][!is.na(fields)][, fields := purrr::map(fields, unlist)];
				}

			self$smart.rules$for_usage <- .taxonomy;

			if (show){ print(self$smart.rules$for_usage) }

			data.table::setattr(self$smart.rules$for_usage, "law", rlang::expr());
			data.table::setattr(self$smart.rules$for_usage, "state", "pending");

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
			.rules = as.character(rlang::enexprs(...)) %>% .[!(. == "for_usage")]

			.func <- function(i){
				rule = self$smart.rules[[i]];

				if (purrr::is_empty(rule)){
					if (tcltk::tk_messageBox(type = "yesno", message = sprintf("There is no rule '%s': continue?", i), caption = "Missing Rule") == "no"){
						return(invisible(self))
					}
				}

				# Only enforce pending rules
				if (purrr::is_empty(attr(rule, "state"))){ data.table::setattr(rule, "state", "pending") }

				if (attr(rule, "state") == "pending"){
					# Before transform ...
					this.data <- self$data;

					if (chatty){ message("Enforcing "%s+% i) }

					if (grepl("transform", i)){
						# Multiple transformations can exist in a single transformation rule.  Only active ones are needed.
						.active = purrr::map_lgl(rule, attr, "active");

						if (any(.active)){ purrr::iwalk(rule[.active], \(x, y){ message(paste0("\t ...", y)); eval(x);  }) }

						# Set active sub-rules to FALSE
						purrr::walk(1:length(rule), \(x) data.table::setattr(self$smart.rules[[i]][[x]], "active", FALSE));

					} else { attr(rule, "law") |> eval() }

					# After transform ...
					self$data <- this.data;
					attr(self$smart.rules[[i]], "state") <- "enforced";
				}
			}

			purrr::walk(.rules, .func);

			if (chatty){
				message("Post-transformation structure");
				print(str(self$data));
			}

			attr(self$data, "pkg.ver") <- packageVersion("smart.data");

			invisible(self);
		},
		#' @description
		#' \code{$set.private} operates on the private class list accessed internally as \code{$private} using key-value pairs (i.e. key = value)
		#'
		#' @param ... A named list: existing names in private indicate elements to overwrite
		#'
		#' @return Invisibly, the class object with member \code{$data} modified according to the rules enforced
		set.private = function(...){.this = rlang::list2(...); private[names(.this)] <- .this; invisible(self); },
		#' @description
		#' \code{$reset} restores the object \code{$data} to the original value supplied when the class was initiated.  Rules are set to a state of "pending", and transformation rules are set as inactive (\code{active = FALSE})
		#'
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#'
		#' @return Invisibly, the class object with member \code{$data} modified to the original state
		reset = function(chatty = TRUE){
			.reset = if (chatty){
					tcltk::tk_messageBox(type = "yesno", message = "Reset data to the original values?", "You sure 'bout that?")
				} else { "no"}

			if (.reset == "yes" | !chatty){
				self$smart.rules <- purrr::imap(self$smart.rules, \(x, y){
						.this <- if (grepl("transform", y)){ purrr::map(x, data.table::setattr, name = "active", value = TRUE) } else { x }

						data.table::setattr(.this, name = "state", value = "pending")
					});

				self$data <- private$orig.data;
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
			.taxonomy = self$smart.rules$for_usage |> data.table::setkey(term);
			.this_data = self$data;
			.checkout = function(.out){
				if (!purrr::is_empty(.out)){
						if (length(.out) > 1){
							paste(.out[-1], collapse = "|")
						} else { .out }
					} else { NULL }
				}

			if (!"for_usage" %in% names(self$smart.rules)){ self$taxonomy.rule(chatty = chatty, show = show) }

			# Force arguments into a string-vector
			.term_list = if (...length() > 0){ rlang::enexprs(...) |> as.character() |> unlist() } else { .taxonomy$term }
			if (chatty){ message(paste(.term_list, collapse = ", ")) }

			# Create the list of field names to use ====
			retain	<- substitute(retain) |> as.character() |> .checkout();
			omit		<- substitute(omit) |> as.character() |> .checkout();

			.field_list	<- .taxonomy[list(term = .term_list), on = "term", unlist(fields)];

			# Process 'retain', 'omit', and '.field_list', and return the output ====
			if (!purrr::is_empty(retain)){
				.field_list <- c(.field_list, unregex(as.regex(!!retain), .this_data)) |> unique()
			}

			if (!purrr::is_empty(omit)){
				.field_list <- .field_list[!.field_list %in% unregex(as.regex(!!omit), x = .this_data)]
			}

			.field_list <- unique(.field_list);
			if (chatty){ message(paste(.field_list, collapse = ", ")) }

			# Return 'self$data' selected by taxonomy fields
			self$data[eval(rlang::enexpr(subset)), mget(.field_list)] |> data.table::setattr("class", c("data.table", "data.frame"))
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
						system("systeminfo", intern = TRUE), "Avail.+ory.+", omit_no_match = TRUE, simplify = TRUE) %>%	.[!. == ""] |>
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
		}
		)}
	, private = { list(orig.data = NULL, version = NULL)}
	, active	= { list()}
)}
