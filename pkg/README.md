---
engines:
- path: /usr/share/positron/resources/app/quarto/share/extension-subtrees/julia-engine/\_extensions/julia-engine/julia-engine.js
toc-title: Table of contents
---

`smart.data` aims to provide an API that allows for semantic interaction
with tabular data as well as governed manipulation of the same.

Each `smart.data` object is an R6 reference class instance which can be
symbolically retrieved from memory cache (see [`cachem::cache_mem`]())
as well as by direct workspace object invocation.

::: {}
> **Installation**
>
> ``` r
> remotes::install_github("delriaan/smart.data", subdir = "pkg")
> ```
:::

# Key Features

## Setup

To demonstrate the key features of the `smart.data` package, we'll use
`mtcars`:

::: cell
``` {.r .cell-code}
library(smart.data, quietly = TRUE)
smart.start()
smrt <- smart.data$new(x = mtcars, name = "smart_cars")
```
:::

## Naming API

:::: cell
``` {.r .cell-code}
# `make_model = "rn"` is needed because row names are retained when the 
#   input data is converted to a `data.table` object
naming_spec <- c(
    make_model = "rn"
    , (\(x) rlang::set_names(x, toupper(x)))(names(mtcars))
    )

smrt$
    naming.rule(!!!naming_spec)$
    enforce.rules(for_naming)

print(head(smrt$data))
```

::: {.cell-output .cell-output-stdout}
              make_model   MPG   CYL  DISP    HP  DRAT    WT  QSEC    VS    AM
                  <char> <num> <num> <num> <num> <num> <num> <num> <num> <num>
    1:         Mazda RX4  21.0     6   160   110  3.90 2.620 16.46     0     1
    2:     Mazda RX4 Wag  21.0     6   160   110  3.90 2.875 17.02     0     1
    3:        Datsun 710  22.8     4   108    93  3.85 2.320 18.61     1     1
    4:    Hornet 4 Drive  21.4     6   258   110  3.08 3.215 19.44     1     0
    5: Hornet Sportabout  18.7     8   360   175  3.15 3.440 17.02     0     0
    6:           Valiant  18.1     6   225   105  2.76 3.460 20.22     1     0
        GEAR  CARB
       <num> <num>
    1:     4     4
    2:     4     4
    3:     4     1
    4:     3     1
    5:     3     2
    6:     3     1
:::
::::

## Taxonomy API

### Defining Taxonomy Terms

Next, we'll define a set of taxonomy terms and map them to subsets of
the field names.

> *Note*: existing name maps in a "pending" state will automatically be
> enforced to ensure taxonomy field selection is based on current data
> names:

Taxonomy terms:

- **identifier**: make_model
- **performance**: MPG, QSEC, HP
- **metrics**: CYL, DISP, DRAT, WG, GEAR, CARB
- **style**: VS, AM

:::: cell
``` {.r .cell-code}
identifier <- taxonomy(
    term = "identifier"
    , desc = "Make and Model"
    , fields = c("make_model")
    )

performance <- taxonomy(
    term = "performance"
    , desc = "Performance stats"
    , fields = c("MPG", "QSEC", "HP")
    )

metrics <- taxonomy(
    term = "metrics"
    , desc = "Physical Metrics"
    , fields = c("CYL", "DISP", "DRAT", "WT", "GEAR", "CARB")
)

style <- taxonomy(
    term = "characteristics"
    , desc = "Categorical Descriptors"
    , fields = c("VS", "AM")
    )

smrt$
    taxonomy.rule(identifier, performance, metrics, style)$
    enforce.rules(for_usage)

with(smrt$smart.rules$for_usage, identifier)
```

::: {.cell-output .cell-output-stdout}
    An object of class "taxonomy"
    Slot "term":
    [1] "identifier"

    Slot "desc":
    [1] "Make and Model"

    Slot "fields":
    [1] "make_model"

    Slot "law":
    {
        cur_fields <- self$smart.rules$for_usage[["identifier"]]@fields
        new_fields <- if (rlang::is_empty(cur_fields) || identical(cur_fields, 
            "")) {
            NULL
        }
        else {
            names(purrr::keep(attr(self$smart.rules$for_naming, "history"), 
                function(i) any(i %in% self$smart.rules$for_usage[["identifier"]]@fields)))
        }
        if (!rlang::is_empty(new_fields)) {
            self$smart.rules$for_usage[["identifier"]]@fields <- new_fields
        }
        invisible(self)
    }

    Slot "state":
    [1] "enforced"
:::
::::

### Using Taxonomy Terms

To use the defined terms, class method `$use()` is invoked. The output
can be tailored depending on the arguments supplied:

:::::::::: cell
``` {.r .cell-code}
# Use everything
smrt$use() |> head()
```

::: {.cell-output .cell-output-stdout}
              make_model   CYL  DISP  DRAT    WT  GEAR  CARB   MPG  QSEC    HP
                  <char> <num> <num> <num> <num> <num> <num> <num> <num> <num>
    1:         Mazda RX4     6   160  3.90 2.620     4     4  21.0 16.46   110
    2:     Mazda RX4 Wag     6   160  3.90 2.875     4     4  21.0 17.02   110
    3:        Datsun 710     4   108  3.85 2.320     4     1  22.8 18.61    93
    4:    Hornet 4 Drive     6   258  3.08 3.215     3     1  21.4 19.44   110
    5: Hornet Sportabout     8   360  3.15 3.440     3     2  18.7 17.02   175
    6:           Valiant     6   225  2.76 3.460     3     1  18.1 20.22   105
          VS    AM
       <num> <num>
    1:     0     1
    2:     0     1
    3:     1     1
    4:     1     0
    5:     0     0
    6:     1     0
:::

``` {.r .cell-code}
# Use 'identifier' fields
smrt$use(identifier) |> head()
```

::: {.cell-output .cell-output-stdout}
              make_model
                  <char>
    1:         Mazda RX4
    2:     Mazda RX4 Wag
    3:        Datsun 710
    4:    Hornet 4 Drive
    5: Hornet Sportabout
    6:           Valiant
:::

``` {.r .cell-code}
# Use 'performance' fields
smrt$use(performance) |> head()
```

::: {.cell-output .cell-output-stdout}
         MPG  QSEC    HP
       <num> <num> <num>
    1:  21.0 16.46   110
    2:  21.0 17.02   110
    3:  22.8 18.61    93
    4:  21.4 19.44   110
    5:  18.7 17.02   175
    6:  18.1 20.22   105
:::

``` {.r .cell-code}
# Use 'metrics' fields omitting field "DISP"
smrt$use(metrics, omit=DISP) |> head()
```

::: {.cell-output .cell-output-stdout}
         CYL  DRAT    WT  GEAR  CARB
       <num> <num> <num> <num> <num>
    1:     6  3.90 2.620     4     4
    2:     6  3.90 2.875     4     4
    3:     4  3.85 2.320     4     1
    4:     6  3.08 3.215     3     1
    5:     8  3.15 3.440     3     2
    6:     6  2.76 3.460     3     1
:::

``` {.r .cell-code}
# Use 'style' and 'identifier' fields also retaining field "QSEC
smrt$use(style, identifier, retain = QSEC) |> head()
```

::: {.cell-output .cell-output-stdout}
          VS    AM        make_model  QSEC
       <num> <num>            <char> <num>
    1:     0     1         Mazda RX4 16.46
    2:     0     1     Mazda RX4 Wag 17.02
    3:     1     1        Datsun 710 18.61
    4:     1     0    Hornet 4 Drive 19.44
    5:     0     0 Hornet Sportabout 17.02
    6:     1     0           Valiant 20.22
:::

``` {.r .cell-code}
# Use 'style' and 'identifier' fields also retaining fields "DISP" and "QSEC"
smrt$use(style, identifier, retain = c(DISP, QSEC)) |> head()
```

::: {.cell-output .cell-output-stdout}
          VS    AM        make_model  DISP  QSEC
       <num> <num>            <char> <num> <num>
    1:     0     1         Mazda RX4   160 16.46
    2:     0     1     Mazda RX4 Wag   160 17.02
    3:     1     1        Datsun 710   108 18.61
    4:     1     0    Hornet 4 Drive   258 19.44
    5:     0     0 Hornet Sportabout   360 17.02
    6:     1     0           Valiant   225 20.22
:::

``` {.r .cell-code}
# Use 'style' and 'identifier' fields also retaining fields "DISP" and "QSEC" 
#       filtering for rows where field 'make_model' contains the pattern "Merc"
smrt$use(style, identifier, retain = c(DISP, QSEC)
                 , subset = grepl("Merc", make_model)) |> head()
```

::: {.cell-output .cell-output-stdout}
          VS    AM make_model  DISP  QSEC
       <num> <num>     <char> <num> <num>
    1:     1     0  Merc 240D 146.7  20.0
    2:     1     0   Merc 230 140.8  22.9
    3:     1     0   Merc 280 167.6  18.3
    4:     1     0  Merc 280C 167.6  18.9
    5:     0     0 Merc 450SE 275.8  17.4
    6:     0     0 Merc 450SL 275.8  17.6
:::
::::::::::

## Using the Smart Cache

Each `smart.data` object can be given an name during class invocation or
by assigning directly after the fact. When names are compliant for use
with `cachem`, the class instance can be sent to the cache contained
within the package environment:

:::::: cell
``` {.r .cell-code}
smrt$cache_mgr(action = register)

# View existing cached objects
get.smart(list.only = TRUE)
```

::: {.cell-output .cell-output-stdout}
    [1] "smart_cars"
:::

``` {.r .cell-code}
# Verify equivalent calls
identical(get.smart(smart_cars)$data, smrt$data)
```

::: {.cell-output .cell-output-stdout}
    [1] TRUE
:::

``` {.r .cell-code}
# Use cached objects with taxonomy
get.smart(smart_cars)$
    use(performance, identifier, subset = grepl("Merc", make_model))
```

::: {.cell-output .cell-output-stdout}
         MPG  QSEC    HP  make_model
       <num> <num> <num>      <char>
    1:  24.4  20.0    62   Merc 240D
    2:  22.8  22.9    95    Merc 230
    3:  19.2  18.3   123    Merc 280
    4:  17.8  18.9   123   Merc 280C
    5:  16.4  17.4   180  Merc 450SE
    6:  17.3  17.6   180  Merc 450SL
    7:  15.2  18.0   180 Merc 450SLC
:::
::::::

If the underlying data is changed, the cached instance can be updated by
using `$cache_mgr(action = replace)`.

The benefit of caching is that the cached instance can be invoked from
anywhere in your workflow without worrying about environment scope
(except for parallel computing contexts).

# Future Work

- Adding user-supplied rules will be added to the rules API. Currently,
  rules that govern the internal data field names as well as define
  taxonomy terms are available as `S4` classes stored internally to each
  class instance.

- Adding the ability to `$use()` shared taxonomy terms across
  `smart.data` objects. The idea would be to work on class instances
  having data that can be joined with shared taxonomy terms returning a
  collection of mapped fields across participating objects.

- Extending the taxonomy API to use classification labels in class
  method `$use()`
