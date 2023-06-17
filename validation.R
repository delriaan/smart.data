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
	naming.rule(!!!data_names)$
	enforce.rules(for_naming)

names(smrt$data)
smrt$smart.rules$for_naming@law

smrt$
	naming.rule(!!!data_names)$
	enforce.rules(for_naming)

names(smrt$data)

smrt$taxonomy.rule(
	identifier = new("taxonomy", term = "identifier", desc = "Identifies unique instances of a type of reference")
	, flag = new("taxonomy", term = "flag", desc = "Logical indicator")
	, demographic = new("taxonomy", term = "demographic", desc = "Demographic details such as name, date of birth, race, gender")
	, category = new("taxonomy", term = "category", desc = "Indicates a categorical variable")
	, event.date = new("taxonomy", term = "event.date", desc = "The event dates or duration boundary dates")
	, join.key = new("taxonomy", term = "join.key", desc = "Indicates the field(s) to use for 'data.table' joins")
	)
smrt$smart.rules$for_usage %$% mget(ls())
smrt$enforce.rules(for_usage)
smrt$use() |> print()
smrt$use(identifier) |> print()
smrt$use(category) |> print()
smrt$use(category, omit="DISP", ) |> print()
smrt$use(category, identifier) |> print()
smrt$use(retain="*", omit="WT") |> print()
#
# ~ PART II: Using the Taxonomy and Resetting ====
smrt$get.history

smrt$reset(safe = TRUE)

names(smrt$data)
smrt$smart.rules$for_naming@law
smrt$smart.rules$for_naming@state

View(smrt$smart.rules) # All top-level list elements should have attribute "state" set to "pending"; transformation rules should all have attribute "active" set to 'TRUE'

# ~ PART III: Smart Cache ----
smrt$name <- "smart_cars"
smrt$cache_mgr(action = upd)
get.smart(smart_cars)$use(omit = "DRAT") |> print()
is.smart(smrt)
#
# ~ pkgdown ----
# usethis::use_pkgdown()
# usethis::use_proprietary_license("Chionesu George")
# pkgdown::build_site(pkg = "pkg", override = list(destination = "../docs"))
