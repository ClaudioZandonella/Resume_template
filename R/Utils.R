#####################
####    Utils    ####
#####################

#----    extract_year    ----

extract_year <- function(dates){
  date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
  date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

  date_year
}

#----    parse_dates    ----

parse_dates <- function(dates){

  date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
  date_month[is.na(date_month)] <- "1"

  paste("1", date_month, extract_year(dates), sep = "-") %>%
    lubridate::dmy()
}

#----    arrange_timeline    ----

arrange_timeline <- function(data){
  data %>%
    filter(in_resume == TRUE) %>%
    dplyr::mutate(
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "N/A",
        no_start  & has_end ~ as.character(end),
        has_start & no_end  ~ paste("Current", "-", start),
        TRUE                ~ paste(end, "-", start))) %>%
    dplyr::arrange(desc(end_year), desc(start), desc(order))
}

#----    timeline_education    ----

timeline_education <- function(data){
  data %>%
    dplyr::mutate(
      continuously = start_year == lead(end_year, default = "1990"),
      timeline = dplyr::case_when(
        continuously  & no_end  ~ paste("Current", "-", "&nbsp; &nbsp; &nbsp; &nbsp;"),
        continuously  & has_end ~ paste(end, "-", "&nbsp; &nbsp; &nbsp; &nbsp;"),
        !continuously  ~ timeline))
}

#----    timeline_events    ----

timeline_events <- function(data){
  data %>%
    dplyr::mutate(
      duplicated = duplicated(end_year),
      timeline = dplyr::case_when(
        duplicated ~ paste("&nbsp; &nbsp; &nbsp; &nbsp;"),
        !duplicated  ~ timeline))
}

#----    timeline_publications    ----

timeline_publications <- function(data){
  data %>%
    mutate(end_year = if_else(end == "Under Review",
                              true = "1900", false = end_year)) %>%
    dplyr::arrange(desc(end_year), title) %>%
    dplyr::mutate(
      duplicated = duplicated(end_year),
      timeline = dplyr::case_when(
        duplicated ~ paste("&nbsp; &nbsp; &nbsp; &nbsp;"),
        !duplicated  ~ timeline))
}

#----    get_description    ----

get_description <- function(data){
  data %>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""))
}


#----    sanitize_links    ----

# Remove links from a text block and add to internal list
sanitize_links <- function(cv, text){
  if(cv$pdf_mode){
    link_titles <- stringr::str_extract_all(text, '(?<=\\[).+?(?=\\])')[[1]]
    link_destinations <- stringr::str_extract_all(text, '(?<=\\().+?(?=\\))')[[1]]

    n_links <- length(cv$links)
    n_new_links <- length(link_titles)

    if(n_new_links > 0){
      # add links to links array
      cv$links <- c(cv$links, link_destinations)

      # Build map of link destination to superscript
      link_superscript_mappings <- purrr::set_names(
        paste0("<sup>", (1:n_new_links) + n_links, "</sup>"),
        paste0("(", link_destinations, ")")
      )

      # Replace the link destination and remove square brackets for title
      text <- text %>%
        stringr::str_replace_all(stringr::fixed(link_superscript_mappings)) %>%
        stringr::str_replace_all('\\[(.+?)\\]', "\\1")
    }
  }

  list(cv = cv, text = text)
}


#----    get_bold_name    ----

get_bold_name <- function(data, my_name = "Zandonella Callegher, C."){
  data %>%
    mutate(title = gsub(my_name, paste0("<strong>", my_name, "</strong>"),
                        title))
}

#----

