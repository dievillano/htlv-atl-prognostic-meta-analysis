clean_effect_table <- function(data) {
  data |> 
    dplyr::mutate(
      across(dplyr::where(is.character), stringr::str_squish),
      definition = stringr::str_replace_all(definition, " ", ""),
      definition = stringr::str_replace_all(
        definition, "(?<=[0-9])\\s(?=[0-9]{3})", ""
      ),
      definition = stringr::str_replace_all(
        definition, "(?<=[0-9]),(?=[0-9]{3})", ""
      ),
      definition = stringr::str_squish(definition)
    )
}