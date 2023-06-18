# Smart Data

The goal of the `smart.data` package is to provide an API that allows for semantic interaction with tabular data as well as governed manipulation of the same.  Each `smart.data` object is an R6 reference class instance which can be symbolically retrieved from memory cache (see [`cachem::cache_mem`]()) as well as by direct workspace object invocation.

Use `remotes::install_github("delriaan/smart.data", subdir = "pkg")` to install.

# Future Work
- Adding user-supplied rules will be added to the rules API.  Currently, rules that govern the internal data field names as well as define taxonomy terms are available as `S4` classes stored internally to each class instance.

- Adding the ability to `$use()` shared taxonomy terms across `smart.data` objects.  The idea would be to work on class instances having data that can be joined with shared taxonomy terms returning a collection of mapped fields across participating objects.

- Extending the taxonomy API to use classification labels in class method `$use()`

