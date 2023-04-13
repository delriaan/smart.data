# library(smart.data)
# library(magrittr); library(stringi)
library(data.table); library(purrr); library(stringi, include.only = "%s+%")
library(book.of.utilities);

orig.data <- mtcars;
#
# ~ PART I: Functionality Tests====
smrt <- smart.data$
	new(orig.data, "smart_cars")$
	naming.rule(MPG = "mpg", chatty = TRUE)$
	taxonomy.rule()$
	enforce.rules(for_naming)$
	transformation.rule(
		add_col = this.data[, car_model := rownames(orig.data)[1:nrow(this.data)]]
		, subset = this.data <<- this.data[1:10, ]
		, update = TRUE
		)$
	enforce.rules(for_transformation)

smrt$taxonomy.rule(update = TRUE)
smrt$use(identifier, category)
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
