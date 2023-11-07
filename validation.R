# library(smart.data)

# 									mpg 	cyl disp  hp drat wt  	qsec	vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

dir(paste0(getwd(), "/pkg/R"), pattern = "R$", full.names = TRUE) |> purrr::walk(source);
orig_data <- data.table::as.data.table(mtcars, keep.rownames = TRUE) |> data.table::setnames("rn", "make_model")
data_names <- (\(x) rlang::set_names(x, toupper(x)))(names(orig_data))

#
# PART I: Functionality Tests ====
smart.start()
smrt <- smart.data$
	new(x = orig_data, name = "smart_cars")$
	naming.rule(!!!data_names)$
	enforce.rules(for_naming)
smrt
# names(smrt$data)
# smrt$smart.rules$for_naming@law

# :: IDENTIFIER
# make_model

# :: PERFORMANCE
# [, 1]	mpg	Miles/(US) gallon
# [, 7]	qsec	1/4 mile time
# [, 4]	hp	Gross horsepower

# :: METRICS
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors

# :: CHARACTERISTICS
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)

# debug(smrt$taxonomy.rule): Issue #7 ----
smrt$taxonomy.rule(
	identifier = taxonomy(term = "identifier", desc = "Object identifier", fields = "MAKE_MODEL")
	, category = taxonomy(term = "category", desc = "Category", fields = c("CYL", "GEAR", "DISP"))
	, gui = TRUE
	)
smrt$enforce.rules(for_usage)
smrt

# debug(smrt$use) ----
# smrt$smart.rules$for_usage %$% mget(ls())
smrt$use() |> print()
smrt$use(identifier) |> print()
smrt$use(category) |> print()
smrt$use(category, omit="DISP", ) |> print()
smrt$use(category, identifier) |> print()
smrt$use(retain="*", omit="WT") |> print()
smrt$use(category, identifier, retain = c(Q,V)) |> print()
#
# ~ PART II: Using the Taxonomy and Resetting ====
smrt$get.history

smrt$reset(safe = TRUE)

names(smrt$data)
smrt$smart.rules$for_naming@state
smrt$
	naming.rule(!!!data_names)$
	enforce.rules(for_naming)

names(smrt$data)

# ~ PART III: Smart Cache ----
smrt$name <- "smart_cars"
is.smart(smrt)
smrt$cache_mgr(action = upd)
get.smart(smart_cars)$data
#
