#' @title Smart Interaction With Data
#' @description R6 class \code{smart.data} facilitates the manipulation of a data set by providing rules-based operations for specific contexts.
#' @importFrom magrittr %>% %T>% %<>% or
<<<<<<< Updated upstream
=======
#' @importFrom data.table like %like% %ilike% %flike%
#' @importFrom book.of.utilities unregex as.regex
>>>>>>> Stashed changes
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
		#' @field cache A shared access object via \code{\link[cachem]{cache_mem}} or external environment
		cache = NULL,
		#' @field smart.rules A list holding user-generated sets of rules
		smart.rules = list(),
		#' @description
		#' \code{print} Prints the contents of \code{$data} before invisibly returning the class object
		print = function(){
			message(sprintf("Smart Data, version %s", private$version));

			if (!is_empty(self$smart.rules)){ message(c(
				"Smart Rules:", imap_chr(self$smart.rules, ~sprintf("... %s [%s]", .y, attr(.x, "state") %||% "unknown"))
					) %>% paste(collapse = "\n"));
			}
			print(self$data);
		},
		#' @description Initialize the class object.
		#' @param x The input data
		#' @param name The name for the smart class when used in smart functions
		#' @param ... Arguments used to initialize the smart cache (see \code{\link[cachem]{cache_layered}}).  If none are provided, a composite cache is created of types \code{memory} and \code{disk}, both using defaults (see \code{\link[cachem]{cache_mem}} and \code{\link[cachem]{cache_disk}})
		initialize = function(x, name = "new.data", ...){
			if (is.smart(x)){
				private$orig.data <- copy(x$data);

				if ("for_usage" %in% names(x$smart.rules)){
					self$smart.rules$for_usage <- x$smart.rules$for_usage;
				}
			} else { private$orig.data <- copy(as.data.table(x)) }

			self$data <- setattr(copy(private$orig.data), "class", c(class(private$orig.data), "smart.data"));
			self$name <- name;
			self$id <- paste0(name, "_", data.table::address(self$data), "_", format(Sys.time(), "%Y%m%d%H%M%S"));
			private$version <- packageVersion("smart.data");

			if (!".___SMART___" %in% ls(globalenv(), all.names = TRUE)){
				if (...length() == 0){ assign(".___SMART___", cachem::cache_layered(cachem::cache_mem(), cachem::cache_disk()), envir = globalenv())
					} else { assign(".___SMART___", cachem::cache_layered(...), envir = globalenv())}
				}

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
			.nms = if (...length() == 0){ names(self$data) %>% rlang::set_names() } else { rlang::list2(...) }

			self$smart.rules$for_naming <- .nms %>%
					setattr("law", rlang::expr(setnames(self$data, old = !!unlist(.nms, use.names = FALSE), new = !!names(.nms), skip_absent = TRUE))) %>%
					setattr("state", "pending");

			if (show){ message("Current naming rules:\n----------\n" %s+% paste(names(self$smart.rules$for_naming), self$smart.rules$for_naming, sep = " <- ", collapse = "\n"))}

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
		#'	\item{fields}{The fields in \code{$data} mapped to the term}
		#' }
		#' Once \code{term.map} is saved to \code{$smart.rules$for_usage}, it is given an attribute entitled "law" which contains a quoted expression defining how the rules are enforced when \code{$enforce.rules()} is invoked.  \emph{Note:} \code{$smart.rules$for_usage} can be modified directly when adding custom taxonomy terms.  All one has to do is create a similarly-structured object and row-bind it to the existing \code{$smart.rules$for_usage} object (being careful to preserve the existing "law" attribute).
		#'
		#' @param term.map (NULL) A 2D tabular object that maps terms to rules -> X ~ term<character[]> + desc<character[]> + rule<language[]> (see section 'Term Map')
		#' @param update (logical|TRUE) Should the existing map be updated via interactive selection?
		#' @param new.terms (data.table|NULL) A \code{link[data.table]{data.table}} object of additional taxonomy rules to add to existing ones.
		#' @param show (logical|FALSE) Should the rule be shown?
		#' @param chatty (logical|FALSE) Should additional information be printed to the console?
		taxonomy.rule = function(term.map = NULL, update = FALSE, show = FALSE, chatty = FALSE){
			# @def self$smart.rules$for_usage ~ term.map + fields<character[][]>
			# @def .law: An expression representing the actions to take when enforced

			# Check for pre-existing term map ====
			.map_exists = "for_usage" %in% names(self$smart.rules);

			.default_map = { data.table(
				term = c("identifier", "demographic", "flag", "event.date", "category", "join.key")
				, desc = { c(
						"Identifies unique instances of a type of reference"
						, "Demographic details such as name, date of birth, race, gender"
						, "Logical indicator"
						, "The event dates or duration boundary dates"
						, "Indicates a categorical variable"
						, "Indicates the field(s) to use for 'data.table' joins"
						)}
				, rule = { rlang::exprs(
						identifier	= { switch(
							class(.x)
							, "character" = trimws(x)
							, "numeric" 	= ifelse(x > 2^30, x, as.integer(x))
							, "integer64" = bit64::as.integer.integer64(x)
							, { message("No handler for class(es) " %s+% {class(x) %>% unique() %>% paste(collapse = ", ")} %s+% ": returning as-is ..."); x; }
							)
						}
						, event.date	= modify_if(.x = x, .p = ~is_empty(.x)|is.na(.x), .f = ~"1900-01-01", .else = as.character)
						, flag				= if (rlang::has_length(unique(x) %>% na.omit(), 2)){
															if (any(is.logical(x))){
																modify_if(x, is.na, ~FALSE)
															} else if (all(x %in% c(1,0,NA))){
																modify_if(is.na, ~FALSE, .else = as.logical)
															} else { I(x) }
														} else { I(x) }
						, demographic = as.factor(x)
						, category		= as.factor(x)
						, join.key		= x
					)}
				, fields = map(1:6, ~new("character"))
				, key = "term"
				)}

			if (.map_exists) { .default_map <- rbindlist(list(.default_map[!(term %in% self$smart.rules$for_usage$term)], self$smart.rules$for_usage), use.names = TRUE) }

			# Check to see if a naming rule exists: if so, the taxonomy fields need to follow the law of naming ====
			if ("for_naming" %in% names(self$smart.rules)){
				if (attr(self$smart.rules$for_naming, "state") == "pending"){ self$enforce.rules(for_naming, chatty = chatty)}
				if (!is_empty(term.map)){
					term.map %<>% .[, fields := map(fields, ~{
						stri_replace_all_fixed(
							.x
							, pattern = self$smart.rules$for_naming
							, replacement = names(self$smart.rules$for_naming)
							, vectorize_all = FALSE
							)
						})]
					}
			}

			# Validate and process 'term.map' ====
			if (!is_empty(term.map)){
				if (!is.data.table(term.map)){ setDT(term.map) }
				if (!all(names(.default_map)[1:3]) %in% names(term.map)){
					stop("Error: each taxonomy must have the following fields provided: term, desc, rule");
				} else { term.map[, c("term", "desc") := map(mget(c("term", "desc")), unlist)] }

				# term.map <- term.map[(if (!update){ !term %in% .default_map$term })]; # Add or update terms (anything remaining when not updating is added)
			} else { term.map <- .default_map }

			# Update the term map?
			if (update){
				term.map <- { list(.default_map[!(term %in% term.map$term)], term.map) %>% rbindlist(use.names = TRUE, fill = TRUE)} # Update or retain current/default
			}

			setkey(term.map, term);

			# Define/Update the taxonomy ====
			.taxonomy = {
					# Choose the values of 'term' to define/update
					.term_list = { tcltk::tk_select.list(
							choices = term.map$term
							, preselect = if (update){ term.map[map_lgl(term.map$fields, ~!is_empty(unlist(.x)) %>% any()), term] }
							, multiple = TRUE
							, title = "Choose one or more terms to map to fields in " %s+% self$name
							) %>%
							purrr::compact() %>% {
								if (is_empty(.)){ message("Nothing selected: existing terms retained."): term.map$term } else { . }}
						}

					# Populate 'fields' for each term
					term.map[(term %in% .term_list)][, fields := map2(term, fields, ~{
						.out = if (update | any(is_empty(unlist(.y)))){
								tcltk::tk_select.list(
									choices = names(self$data)
									, title = sprintf("Choose term [%s] fields", .x)
									, preselect = if (!identical(character(), unlist(.y))){ unlist(.y) }
									, multiple = TRUE
									)
								} else { unlist(.y) }
						if (is_empty(.out)){ NA } else { list(c(.out)) }
					})][!is.na(fields)][, fields := map(fields, unlist)];
				}

			self$smart.rules$for_usage <- .taxonomy[]

			if (show){ print(self$smart.rules$for_usage)}

			.law = rlang::expr({ pwalk(!!.taxonomy, function(...){
					.rule = list(...);
					.rule$fields %<>% unlist();

					# Enforce the law
					this.data <<- this.data[, c(.rule$fields) := mget(.rule$fields) %>% imodify(~{ x = .x; y = .y; eval(.rule$rule) })];

					# Add meta-data to compliant field in self$data
					walk(.rule$fields, ~{
						setattr(this.data[[.x]], "role", c(attr(this.data[[.x]], "role") %>% unique(), .rule$term));
						setattr(this.data[[.x]], "desc", c(attr(this.data[[.x]], "desc") %>% unique(), .rule$desc));
					});
				});
			});

			setattr(self$smart.rules$for_usage, "law", .law);
			setattr(self$smart.rules$for_usage, "state", "pending");

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
						self$smart.rules$for_transformation <- .rules
					}
			}

			walk(1:length(self$smart.rules$for_transformation), ~setattr(self$smart.rules$for_transformation[[.x]], "active", (.x %in% c(set.active)) | (set.active == 0)));

			if (any(map_lgl(self$smart.rules$for_transformation, ~attr(.x, "active")))){ setattr(self$smart.rules$for_transformation, "state", "pending") }

			invisible(self);
		},
		#'	@description
		#'	\code{$enforce.rules} operates on rules saved in \code{$smart.rules} and evaluates the quoted "law" on \code{$data}.  Custom rules can be directly added to class member \code{$smart.rules}: they must have an attribute "law" containing a quoted expression referencing object \code{.data} and \code{rule}.  The rule should contain any objects referenced by the quoted expression.
		#'	@param ... (string[]) The names of the rules to enforce
		#'	@param chatty (logical|FALSE) Should additional execution information be printed to console?
		#'
		#'	@return Invisibly, the class object with member \code{$data} modified according to the rules enforced
		enforce.rules = function(..., chatty = FALSE){
			.rules = substitute(c(...)) %>% as.character() %>% .[-1L];
			walk(.rules, ~{
				rule = self$smart.rules[[.x]];
				if (purrr::is_empty(rule)){
					if (tcltk::tk_messageBox(type = "yesno", message = sprintf("There is no rule '%s': continue?", .x), caption = "Missing Rule") == "no"){ return(invisible(self)) }
					}

				# Only enforce pending rules
				if (purrr::is_empty(attr(rule, "state"))){ setattr(rule, "state", "pending") }

				if (attr(rule, "state") == "pending"){
					# Before transform ...
					this.data <- self$data;

					if (chatty){ message("Enforcing "%s+% .x) }

					if (.x %ilike% "transform"){
						# Multiple transformations can exist in a single transformation rule.  Only active ones are needed.
						.active = map_lgl(rule, attr, "active");

						if (any(.active)){ iwalk(rule[.active], ~{ message("\t ..." %s+% .y); eval(.x);  }) }

						.this = .x;

						# Set active sub-rules to FALSE
						walk(1:length(rule), ~setattr(self$smart.rules[[.this]][[.x]], "active", FALSE));

					} else { attr(rule, "law") %>% eval() }

					# After transform ...
					self$data <- this.data;
					attr(self$smart.rules[[.x]], "state") <- "enforced";
				}
			});

			if (chatty){ message("Post-transformation structure"); print(str(self$data)) }

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
			.reset = if (chatty){ tcltk::tk_messageBox(type = "yesno", message = "Reset data to the original values?", "You sure 'bout that?") } else { "no"}
			if (.reset == "yes" | !chatty){
				self$smart.rules <- self$smart.rules %>% imap(~{
						.this = if (.y %ilike% "transform"){ map(.x, setattr, name = "active", value = TRUE) } else { .x }
						setattr(.this, name = "state", value = "pending")
					})
				self$data <- private$orig.data;
			}
			invisible(self);
		},
		#' @description
		#' \code{$use} takes as input taxonomy terms and returns the fields in \code{$data} mapped to each term supplied.  The idea here is to provide a semantic method for retrieving data.
		#'
		#' @section Note:
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
			.taxonomy = self$smart.rules$for_usage %>% setkey(term);
			.this_data = self$data;
			.checkout = function(.out){ if (!is_empty(.out)){ if (length(.out) > 1){ paste(.out[-1], collapse = "|") } else { .out } } else { NULL }}

			if (!"for_usage" %in% names(self$smart.rules)){ self$taxonomy.rule(chatty = chatty, show = show) }

			# Force arguments into a string-vector
			.term_list = if (...length() > 0){ substitute(c(...)) %>% as.character() %>% .[-1L] %>% unlist() } else { .taxonomy$term }
			if (chatty){ message(paste(.term_list, collapse = ", ")) }

			# Create the list of field names to use ====
			retain <- substitute(retain) %>% as.character() %>% .checkout()
			omit <- substitute(omit) %>% as.character() %>% .checkout()
			.field_list	= .taxonomy[list(term = .term_list), on = "term", unlist(fields)];

			# Process 'retain', 'omit', and '.field_list', and return the output ====
			if (!is_empty(retain)){
				.field_list <- c(.field_list, unregex(as.regex(!!retain), .this_data)) %>% unique() }

			if (!is_empty(omit)){
				.field_list <- .field_list[!.field_list %in% unregex(as.regex(!!omit), x = .this_data)] }

			if (chatty){ message(paste(.field_list, collapse = ", ")) }

			# Return 'self$data' selected by taxonomy fields
			self$data[eval(rlang::enexpr(subset)), mget(.field_list)];
		},
		#' @description
		#' \code{$cache_mgr} adds the current object to the shared "smart" cache space (see \code{\link[cachem]{cache_layered}}).  The shared cache is layered as 'memory' followed by 'disk.'
		#'
		#' @param action One of \code{register} or \code{unregister}
		#' @param chatty (logical | TRUE) When \code{TRUE}, a confirmation dialog is invoked.
		#' @param gcache A predefined \code{cache} object from package \code{cachem}.  If \code{NULL}, a memory-cache object is initiated using 10\% of available memory (unless overridden by values specified in argument \code{...})
		#' @param ... Additional arguments to use when initializing a cache object
		cache_mgr = function(action, chatty = FALSE, gcache = NULL, ...){
			if (!"max.sz" %in% ls(private)){
				private$max.sz <- stringi::stri_extract_all_regex(
						system("systeminfo", intern = TRUE), "Avail.+ory.+", omit_no_match = TRUE, simplify = TRUE) %>%	.[!. == ""] %>%
					stringi::stri_extract_all_regex("[0-9]", simplify = TRUE) %>%
					as.vector() %>%
					paste(collapse = "") %>%
					as.integer() * 0.01 * (1024^2)
			}

			logi_vec = { c(
					add.cache = any(as.character(substitute(action)) %ilike% "^(add|regi)")
					, rem.cache = any(as.character(substitute(action)) %ilike% "^(rem|unreg)")
					, upd.cache = any(as.character(substitute(action)) %ilike% "^(upd|refr)")
					) %>% .[which(.)]
			} %>% names();

			if (!exists(".___SMART___", envir = globalenv())){
				assign(
					".___SMART___"
					, if (!rlang::is_empty(gcache)){ gcache } else { cachem::cache_mem(max_size = private$max.sz, ...) }
					, envir = globalenv()
					);
			}
			self$cache <- globalenv()[[".___SMART___"]];

			add.cache = function(...){
				if ((self$cache)$exists(self$name)){
					message(sprintf("'%s' already exists in smart-cache", self$name))
					} else {
						if (chatty){ message("Adding %s to Smart Cache ..." %>% sprintf(self$name)) }
						(self$cache)$set(self$name, self)
					}
			}

			rem.cache = function(...){
				if ((self$cache)$exists(self$name)){ (self$cache)$remove(self$name)
					} else { message(sprintf("'%s' is not found in the smart-cache", self$name)) }
				}

			upd.cache = function(...){ rem.cache(); add.cache(); }

			do.call(logi_vec, args = list());

			if (chatty){ message("Cache object completed!")}
			invisible(self);
		}
	)}
	, private = { list(orig.data = NULL, version = NULL)}
	, active	= { list()}
	)}
#
is.smart <- function(...){
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

	imap(rlang::list2(...), ~{
		c(smart.data_class_exists = any(class(.x)	%ilike% "smart")
		, has_orig_data = !is_empty(.x$.__enclos_env__$private$orig.data)
		) %>% all(na.rm = TRUE)
	}) %>% unlist()
}

#
smart.upgrade <- function(..., env = globalenv(), chatty = FALSE){
#' Upgrade a Smart Object
#'
#' \code{smart.upgrade} assigns a new \code{smart.data} object to replace the existing object.  The primary use case for this function is when there is a package upgrade, and down-rev objects need to be upgraded.  Objects \code{$smart.rules}, \code{private$orig.data}, \code{$cache}, and \code{$name} are preserved.
#'
#' @param ... Names or symbols of 'smart.data' objects
#' @param env The environment object where the smart object(s) reside
#' @param chatty (logical) When \code{TRUE}, additional messages are sent to the
#'
#' @export

	if (...length() == 0){ message("Nothing to do!"); return() }

	env = substitute(env);
	env = if (class(env) %in% c("call", "name")){ eval(env) } else { eval(str2lang(env)) }
	queue = as.character(rlang::exprs(...));

	walk(queue, ~{
		.temp <- smart.data$new(env[[.x]]$data, env[[.x]]$name);

		# :: Xfer data
		.temp$.__enclos_env__$private$orig.data <- env[[.x]]$.__enclos_env__$private$orig.data;
		.temp$data <- env[[.x]]$data;

		# :: Xfer smart.rules
		.temp$smart.rules <- env[[.x]]$smart.rules;

		if ("for_transformation" %in% names(.temp$smart.rules)){
			if (is_empty(attr(.temp$smart.rules$for_transformation, "state"))){ setattr(.temp$smart.rules$for_transformation, "state", "enforced") }
			.temp$smart.rules$for_transformation %>% walk(~{ if (is_empty(attr(.x, "active"))){ setattr(.x, "state", FALSE) }});
		}

		if (hasName(env[[.x]], "cache")){
			.temp$cache <- env[[.x]]$cache;
			.temp$cache_mgr(action = upd);
		}

		# :: Complete the xfer
		if (chatty){ message("Preparing to upgrade " %s+% .x, appendLF = FALSE) }
		assign(.x, .temp, envir = env);

		if (chatty){ message(": Success!", appendLF = TRUE)}
	});
}

# STUB: Future functions will use class field 'cache' to invoke other smart objects sharing the same cache object.  The shared cache needs to reside in the 'smart.data' namespace once loaded.
get.smart <- function(..., list.only = FALSE){
#' Get a Smart Object
#'
#' \code{get.smart} retrieves \code{link[smart.data]} objects from the global smart-cache or throws a message if the global cache does not exist
#'
#' @param ... Names of the smart objects (as found in class member \code{name}) given as symbols or strings
#' @param list.only (logical | FALSE) When \code{TRUE} existing keys of the smart-cache are returned before exiting
#'
#' @return A list of retrieved \code{smart.data} objects, invisibly.
#'
#' @export

	if (!exists(".___SMART___", envir = globalenv())){
		message("Global smart cache not detected: initialize via smart.data method '$cache_mgr()' from an existing smart.data object.");
		return(invisible());
	} else {
			if (list.only)( return(.___SMART___$keys()) )
			.these =  if (...length() == 0){
					tcltk::tk_select.list(.___SMART___$keys(), multiple = TRUE, title = "Choose 'smart.data' Objects")
				} else { as.character(rlang::exprs(...)) }
			invisible(imap(purrr::set_names(.these), ~.___SMART___$get(.x)) %>% { if (length(.) == 1){ .[[1]] } else { . }});
		}
}
