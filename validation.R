# library(smart.data)
# library(magrittr); library(stringi)
library(data.table); library(purrr);
library(book.of.utilities);
library(magrittr);
library(stringi, include.only = "%s+%")

orig_data <- mtcars
data_names <- (\(x) rlang::set_names(x, toupper(x)))(names(mtcars))

#
# ~ PART I: Functionality Tests====
smrt <- smart.data$
	new(x = orig_data, name = "smart_cars")$
	# smrt$
	naming.rule(!!!data_names)$
	enforce.rules(for_naming)

names(smrt$data)
smrt$smart.rules$for_naming@history
smrt$smart.rules$for_naming@law
smrt$smart.rules$for_naming@state

smrt$taxonomy.rule(
	identifier = new("taxonomy", term = rlang::sym("identifier"), desc = "Identifies unique instances of a type of reference")
	, flag = new("taxonomy", term = rlang::sym("flag"), desc = "Logical indicator")
	, demographic = new("taxonomy", term = rlang::sym("demographic"), desc = "Demographic details such as name, date of birth, race, gender")
	, category = new("taxonomy", term = rlang::sym("category"), desc = "Indicates a categorical variable")
	, event.date = new("taxonomy", term = rlang::sym("event.date"), desc = "The event dates or duration boundary dates")
	, join.key = new("taxonomy", term = rlang::sym("join.key"), desc = "Indicates the field(s) to use for 'data.table' joins")
	)
smrt$enforce.rules(for_usage)
smrt$smart.rules$for_usage %$% mget(ls())
smrt$use()
#
# ~ PART II: Amending the Transformation Rule =====
smrt$transformation.rule(
	change.case = this.data <<- this.data[, car_model := toupper(car_model)]
	, add_index = this.data <<- this.data[, rec_idx := seq_along(car_model)]
	, update = TRUE
	);
View(smrt$smart.rules$for_transformation) # List should be length five ()

smrt$enforce.rules(for_usage, for_transformation, chatty = TRUE);

smrt$smart.rules$for_transformation |> purrr::map(attr, "active") # All results should be 'FALSE'
smrt$data # Column 'car_model' should only contain numbers and upper-case characters

# ~ PART III: Using the Taxonomy and Resetting ====
smrt$reset(chatty = !TRUE)
View(smrt$smart.rules) # All top-level list elements should have attribute "state" set to "pending"; transformation rules should all have attribute "active" set to 'TRUE'

smrt$enforce.rules(for_transformation)$
	enforce.rules(for_usage)$
	data;


smrt$use(identifier)
smrt$use(identifier, category, retain = disp, chatty = TRUE) # Should contain the previous output

# debug(smrt$use)
smrt$use(identifier, category, retain = c(hp, mpg), chatty = TRUE) # Should contain the previous output + 'mpg'
smrt$use(omit = c(mpg), chatty = TRUE)
smrt$use(identifier, category, retain = c(cyl, am), omit = c(mpg, am), chatty = TRUE) # Should NOT contain 'mpg', or 'am'
smrt$use(retain = `*`, chatty = TRUE)
smrt$use(retain = `*`, subset = cyl == 8 & carb == 4)
smrt$use()

# ~ PART IV: Taxonomy Inheritance ====
smrtr <- smart.data$new(smrt, "cars")
smrtr$taxonomy.rule(update = TRUE) # Taxonomy terms should be identical

# ~ PART V: Smart Cache ----
smrt$name <- "smart_cars"
smrt$cache_mgr(action = add)
get.smart(smart_cars)$use(retain = drat)
is.smart(smrt)
#
# ~ pkgdown ----
# usethis::use_pkgdown()
# usethis::use_proprietary_license("Chionesu George")
# pkgdown::build_site(pkg = "pkg", override = list(destination = "../docs"))
