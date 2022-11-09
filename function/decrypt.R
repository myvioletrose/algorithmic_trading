who <- function(x) {
        str_split(x, "") %>%
                unlist %>%
                grep(pattern = "[a-z]", value = T) %>%
                str_c(collapse = "")
}

pwd <- function(x) {
        str_split(x, "") %>%
                unlist %>%
                grep(pattern = "[a-z0-9]", value = T) %>%
                str_c(collapse = "")
}