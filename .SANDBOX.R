identifier <- smart.data::as.taxonomy(list(term = "identifier", desc = "Identifies unique instances in a set"))
jsonlite::toJSON(as.list(identifier) |> purrr::modify_at("law", rlang::expr_deparse)) |> smart.data::as.taxonomy()
