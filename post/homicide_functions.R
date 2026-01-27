# This file contains common setup code for blog posts related to the national
# homicide problem profile

# PACKAGES ---------------------------------------------------------------------

pacman::p_load(
  broom,
  broom.mixed,
  fable,
  ggrepel,
  ggspatial,
  # ggstream,
  ggtext,
  gt,
  janitor,
  knitr,
  lme4,
  patchwork,
  readxl,
  scales,
  sf,
  sfhotspot,
  tidytext,
  tseries,
  tsibble,
  tidyverse
)


# Set data directory (since data is stored in a different project)
proj_dir <- "/Users/mattashby/Documents/Homicide problem profile"


colours <- c(
  darkgreen = "#555025",
  darkred = "#651D32",
  darkpurple = "#4B384C",
  darkblue = "#003D4C",
  darkbrown = "#4E3629",
  midgreen = "#8F993E",
  midred = "#93272C",
  midpurple = "#500778",
  midblue = "#002855",
  stone = "#D6D2C4",
  brightgreen = "#B5BD00",
  brightred = "#D50032",
  brightblue = "#0097A9",
  brightpink = "#AC145A",
  lightgreen = "#BBC592",
  lightred = "#E03C31",
  lightpurple = "#C6B0BC",
  lightblue = "#8DB9CA",
  yellow = "#F6BE00",
  orange = "#EA7600",
  grey = "#8C8279",
  blueceleste = "#A4DBE8",
  ioeblue = "#3255A4"
)


ucl_brand <- c(
  # a = "#361a54",
  # b = "#831e62",
  # c = "#c73259",
  # d = "#f3653d",
  # e = "#ffa600"
  a = "#002ea6", # Dark blue
  b = "#005e5c", # Dark teal green
  c = "#5487ff", # Blue
  d = "#ED367D", # Fuchsia
  e = "#E36C2A", # Orange
  f = "#57b444", # Teal green
  g = "#781c1c", # Dark orange
  h = "#9e1a54" # Dark fuchsia
)


# FUNCTIONS --------------------------------------------------------------------

n_from_table <- function(table, condition, accuracy = NULL, column = "n", ...) {
  table |>
    filter({{ condition }}) |>
    count(wt = .data[[column]]) |>
    pluck("n") |>
    comma(accuracy = accuracy, ...)
}

sum_from_table <- function(table, column = "n", ...) {
  table |>
    summarise(n = sum(.data[[column]])) |>
    pluck("n") |>
    comma(...)
}

prop_from_table <- function(
  table,
  condition,
  accuracy = 1,
  column = "n",
  drop_na = TRUE,
  ...
) {
  numerator <- table |>
    filter({{ condition }}) |>
    count(wt = .data[[column]]) |>
    pluck("n")
  if (drop_na) {
    denominator <- table |>
      drop_na() |>
      count(wt = .data[[column]]) |>
      pluck("n", 1)
  } else {
    denominator <- table |> count(wt = .data[[column]]) |> pluck("n", 1)
  }

  percent(numerator / denominator, accuracy = accuracy, ...)
}

prop_base <- function(data, condition, accuracy = 0.1, ...) {
  filtered_data <- filter(data, {{ condition }})

  percent(nrow(filtered_data) / nrow(data), accuracy = accuracy, ...)
}

theme_homicide <- function() {
  theme_minimal() %+replace%
    theme(
      axis.ticks = element_line(colour = "grey90"),
      axis.title.x.bottom = element_text(hjust = 1),
      axis.title.y.left = element_text(hjust = 1),
      legend.background = element_rect(colour = NA, fill = "white"),
      legend.justification = c(1, 1),
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.caption = element_textbox_simple(
        colour = "grey33",
        margin = margin(t = 11),
        size = 8
      ),
      plot.caption.position = "plot",
      plot.subtitle = element_textbox_simple(
        lineheight = 1,
        margin = margin(t = 3, b = 6),
        size = 8
      ),
      plot.title = element_textbox_simple(
        face = "bold",
        lineheight = 1,
        size = 14
      ),
      plot.title.position = "plot",
      strip.placement = "outside"
    )
}

add_prop <- function(data, criteria) {
  if (!"n" %in% names(data)) {
    cli::cli_abort(c(
      "Object must contain a column called `n`",
      "i" = "existing column names are: {str_flatten_comma(names(data))}"
    ))
  }

  if (".prop" %in% names(data)) {
    cli::cli_warn(c(
      "Object already contains a column called `.prop`",
      "i" = "the existing column values will be overwritten"
    ))
  }

  mutate(data, `.prop` = n / sum(n), .by = {{ criteria }})
}

# Replace `percent()` from scales with a version that specifies a whole number
# where no level of accuracy is specified
percent <- function(x, accuracy = 1, ...) {
  scales::percent(x, accuracy = accuracy, ...)
}

# Lookup table for HO sub-types
get_sub_type <- function(sub_type) {
  sub_types <- list(
    `1` = "female victims of domestic homicide",
    `2` = "male victims of domestic homicide",
    `3` = "male victims up to age 25 killed in a public place",
    `4` = "female victims of non-domestic homicide aged 16+",
    `5` = "male victims of non-domestic homicide aged 25+ killed in a public space",
    `6` = "male victims of non-domestic homicide aged 25+ killed in a house/dwelling",
    `7` = "victims under 16 killed in a non-public place",
    `other` = "other victims of homicide"
  )
  pluck(sub_types, sub_type)
}

social_figure <- function(
  figure,
  title,
  subtitle = "",
  caption = "",
  name,
  asp = 0.7,
  ...
) {
  this_figure <- figure +
    labs(
      title = title,
      subtitle = subtitle,
      caption = str_glue(
        "{caption}<br>Chart by Matt Ashby, UCL Security and Crime Science. ",
        "More details at lesscrime.info/post/homicide-profile"
      )
    )

  ggsave(
    filename = str_glue("{name}.png"),
    this_figure,
    width = 500 / 72,
    height = (500 / 72) * asp,
    units = "in",
    bg = "white",
    ...
  )
}

social_post <- function(..., image = "", alt = "", add = TRUE) {
  text <- stringr::str_c(...)
  if (stringr::str_length(text) > 260 && !stringr::str_detect(text, "http")) {
    cli::cli_abort(c(
      "Text exceeds 260-character limit",
      "i" = "text is:",
      ">" = text,
      "i" = "current length is {str_length(text)} characters"
    ))
  }
  if (!add) {
    text <- stringr::str_glue("{text}\n\n🧵")
  }
  text <- str_glue("{text} [{stringr::str_length(text)} chrs]")
  stringr::str_glue(
    "\n{text}\n\n",
    if_else(
      stringr::str_length(image) > 0,
      stringr::str_glue("[{image}.png]\n\n\n"),
      ""
    ),
    if_else(
      stringr::str_length(alt) > 0,
      stringr::str_glue("Alt text: {alt}\n\n\n"),
      ""
    ),
    "----\n\n"
  ) |>
    readr::write_lines(file = "social_posts.txt", append = add)
}


# LOAD DATA --------------------------------------------------------------------

homicides <- file.path(proj_dir, "analysis_data/homicides_final.csv.gz") |>
  read_csv() |>
  mutate(
    suspect_ethnicity = case_match(
      suspect_ethnicity,
      "Asian/Asian British" ~ "Asian",
      "Black/Black British" ~ "Black",
      .default = suspect_ethnicity
    ),
    victim_ethnicity = case_match(
      victim_ethnicity,
      "Asian/Asian British" ~ "Asian",
      "Black/Black British" ~ "Black",
      .default = victim_ethnicity
    )
  )

# For cases with more than one victim or suspect, the data contains multiple
# rows. We deal with this by creating separate objects to store victims and
# suspects.
victims <- filter(homicides, principal_suspect | is.na(principal_suspect))
suspects <- filter(homicides, victim_number == 1, suspect_charged)
suspects_convicted <- suspects |>
  filter(suspect_convicted_offence %in% c("murder", "manslaughter"))


## Historical homicide data ----
homicides_historic <- file.path(
  proj_dir,
  "analysis_data/historical_homicide_counts_2025.csv"
) |>
  read_csv()

# Population data ----

pop <- file.path(proj_dir, "analysis_data/population.csv.gz") |>
  read_csv() |>
  mutate(
    ethnic_group = case_match(
      ethnic_group,
      # "Asian" ~ "Asian/Asian British",
      # "Black" ~ "Black/Black British",
      "Mixed" ~ "Mixed/Multiple",
      .default = ethnic_group
    ),
    age10 = if_else(age > 90, 90, age),
    age10 = (ceiling((age + 1) / 10) * 10) - 10,
  )

pop_clusters_list <- list()
pop_clusters_list[["4"]] <- pop_clusters_list[["1"]] <- pop |>
  filter(sex == "Female", age >= 16) |>
  count(ethnic_group, wt = n)
pop_clusters_list[["2"]] <- pop |>
  filter(sex == "Male", age >= 16) |>
  count(ethnic_group, wt = n)
pop_clusters_list[["3"]] <- pop |>
  filter(sex == "Male", age < 25) |>
  count(ethnic_group, wt = n)
pop_clusters_list[["6"]] <- pop_clusters_list[["5"]] <- pop |>
  filter(sex == "Male") |>
  count(ethnic_group, wt = n)
pop_clusters_list[["7"]] <- pop |>
  filter(age < 16) |>
  count(ethnic_group, wt = n)
pop_clusters_list[["other"]] <- pop |>
  count(ethnic_group, wt = n)
pop_clusters <- pop_clusters_list |>
  bind_rows(.id = "cluster_id") |>
  rename(victim_ethnicity = ethnic_group, population = n)

pop_age_groups_ethnicity <- pop |>
  mutate(
    age = if_else(age > 90, 90, age),
    age_upper = ceiling((age + 1) / 10) * 10,
    age_lower = age_upper - 10
  ) |>
  count(ethnic_group, age_lower, wt = n, name = "population")

lad_pfa_lookup <- file.path(proj_dir, "original_data/lad_to_pfa_lookup.csv") |>
  read_csv() |>
  clean_names() |>
  select(lad23cd, pfa23cd, pfa23nm) |>
  summarise(across(everything(), first), .by = c(lad23cd, pfa23cd))

pop_pfa <- file.path(proj_dir, "original_data/nomisweb_RM032_2021.xlsx") |>
  excel_sheets() |>
  set_names() |>
  map(
    \(x) {
      file.path(proj_dir, "original_data/nomisweb_RM032_2021.xlsx") |>
        read_excel(sheet = x, skip = 9)
    }
  ) |>
  bind_rows(.id = "age_sex") |>
  separate(age_sex, into = c("age", "sex"), sep = "; ") |>
  mutate(
    sex = case_when(
      sex == "Female" ~ "female",
      sex == "Male" ~ "male",
      str_detect(sex, "^All") ~ "all persons",
      TRUE ~ NA_character_
    )
  ) |>
  rename(lad_name = 3, lad_code = `...2`) |>
  drop_na(lad_code) |>
  pivot_longer(
    cols = -c(age, sex, lad_name, lad_code),
    names_to = "ethnicity",
    values_to = "people"
  ) |>
  left_join(lad_pfa_lookup, by = c("lad_code" = "lad23cd")) |>
  mutate(
    ethnicity = case_match(
      ethnicity,
      "Asian, Asian British or Asian Welsh" ~ "Asian",
      "Black, Black British, Black Welsh, Caribbean or African" ~
        "Black",
      "Mixed or Multiple ethnic groups" ~ "Mixed/Multiple",
      "Other ethnic group" ~ "Other",
      .default = ethnicity
    )
  ) |>
  count(pfa23cd, pfa23nm, sex, age, ethnicity, wt = people, name = "people")

pop_lad <- file.path(proj_dir, "original_data/nomisweb_RM032_2021.xlsx") |>
  excel_sheets() |>
  set_names() |>
  map(
    \(x) {
      file.path(proj_dir, "original_data/nomisweb_RM032_2021.xlsx") |>
        read_excel(sheet = x, skip = 9)
    }
  ) |>
  bind_rows(.id = "age_sex") |>
  separate(age_sex, into = c("age", "sex"), sep = "; ") |>
  filter(age == "Total", sex == "All persons") |>
  select(lad_name = 3, lad_code = `...2`, people = Total) |>
  drop_na(lad_code)


econ_act <- file.path(proj_dir, "original_data/nomisweb_TS066_2021.csv") |>
  read_csv(skip = 6) |>
  slice(2:13) |>
  select(
    orig_status = `Economic activity status`,
    people = `England and Wales`
  ) |>
  mutate(
    people = as.numeric(people),
    status = case_when(
      orig_status %in%
        c(
          "Economically active and a full-time student",
          "Economically inactive: Student"
        ) ~ "Student",
      orig_status ==
        "Economically active (excluding full-time students):In employment" ~ "Employed",
      orig_status ==
        "Economically active (excluding full-time students): Unemployed" ~ "Unemployed",
      orig_status == "Economically inactive: Retired" ~ "Retired",
      orig_status ==
        "Economically inactive: Looking after home or family" ~ "Looking after family/home",
      orig_status ==
        "Economically inactive: Long-term sick or disabled" ~ "Long-term/temporarily sick/ill",
      orig_status ==
        "Economically inactive: Other" ~ "Other adults economically inactive",
      TRUE ~ NA_character_
    )
  ) |>
  drop_na(status) |>
  count(status, wt = people, name = "people") |>
  mutate(prop = people / sum(people))

## Historical population data ----
historical_population_raw <- file.path(
  proj_dir,
  "original_data/ukpopulationestimates18382022.xlsx"
) |>
  read_excel(sheet = "Table 7", skip = 3) |>
  clean_names() |>
  mutate(year = as.numeric(str_sub(year, start = -4))) |>
  drop_na(persons) |>
  select(year, persons)

historical_population_model <- lm(
  persons ~ year,
  data = historical_population_raw
)

historical_population <- historical_population_model |>
  broom::augment(
    newdata = expand_grid(
      year = (max(pull(historical_population_raw, "year")) + 1):2024
    )
  ) |>
  rename(persons = .fitted) |>
  bind_rows(historical_population_raw) |>
  arrange(year)


# Geographic data ----

pfa <- file.path(
  proj_dir,
  "original_data/Police_Force_Areas_December_2022_EW_BFE.gpkg"
) |>
  read_sf() |>
  clean_names() |>
  st_set_geometry("geometry") |>
  mutate(
    pfa22cd = case_match(
      pfa22cd,
      "E23000034" ~ "E23000001",
      .default = pfa22cd
    ),
    pfa22nm = case_match(
      pfa22nm,
      "London, City of" ~ "Metropolitan Police",
      .default = pfa22nm
    )
  ) |>
  select(pfa22cd, pfa22nm, geometry) |>
  summarise(pfa22nm = first(pfa22nm), across(geometry, st_union), .by = pfa22cd)

lad_reg_lookup <- file.path(
  proj_dir,
  "original_data/Local_Authority_District_to_Region_(December_2023)_Lookup_in_England.csv"
) |>
  read_csv() |>
  clean_names() |>
  select(lad23cd, rgn23nm)

lad <- file.path(
  proj_dir,
  "original_data/Local_Authority_Districts_December_2023_Boundaries_UK_BFE.gpkg"
) |>
  read_sf() |>
  clean_names() |>
  st_set_geometry("geometry") |>
  filter(str_detect(lad23cd, "^[EW]")) |>
  left_join(lad_reg_lookup, by = "lad23cd") |>
  replace_na(list(rgn23nm = "Wales"))

lsoa <- file.path(
  proj_dir,
  "original_data/Indices_of_Multiple_Deprivation_(IMD)_2019.geojson"
) |>
  read_sf() |>
  clean_names() |>
  st_transform("EPSG:27700")

london_stations <- file.path(proj_dir, "original_data/stations.gpkg") |>
  read_sf() |>
  st_transform("EPSG:27700") |>
  rownames_to_column(var = "id") |>
  mutate(id = as.numeric(id))

historical_homicides_rate <- homicides_historic |>
  filter(year >= 1900) |>
  left_join(historical_population, by = "year") |>
  drop_na() |>
  mutate(
    rate = homicides / (persons / 1000000),
    wartime = case_when(
      year %in% c(1914:1918, 1939:1945) ~ "war",
      year == 2003 ~ "shipman",
      TRUE ~ "peace"
    ),
    year = make_date(year)
  )

# Output area classification
oac <- file.path(proj_dir, "analysis_data/oac_2021.gpkg") |>
  read_sf() |>
  st_transform("EPSG:27700")


# Set up features data ---------------------------------------------------------
# Some of these objects are needed early on in the text

# Number of victims per week
victims_weekly_median_count <- victims |>
  mutate(week = yearweek(offence_date)) |>
  count(week) |>
  summarise(weekly_median = median(n)) |>
  pluck("weekly_median", 1)

# Range in number of homicides per week
# Source: https://www.instituteforgovernment.org.uk/data-visualisation/timeline-coronavirus-lockdowns
lockdowns <- tribble(
  ~start       , ~end         ,
  "2020-03-23" , "2020-06-15" ,
  "2020-11-05" , "2020-12-02" ,
  "2021-01-06" , "2021-03-29"
) |>
  mutate(across(everything(), ymd))

victims_trend_data <- victims |>
  count(offence_date = as_date(yearweek(offence_date))) |>
  mutate(
    lockdown = between(offence_date, ymd("2020-03-23"), ymd("2020-06-15")) |
      between(offence_date, ymd("2020-11-05"), ymd("2020-12-02")) |
      between(offence_date, ymd("2021-01-06"), ymd("2021-03-29")),
    grays = offence_date == as_date(yearweek(ymd("2019-10-23")))
  ) |>
  as_tsibble(index = offence_date)

# Victims with additional field on whether the incident was mental-health
# related
victims_mh <- homicides |>
  summarise(
    mh_linked = any(suspect_mentalhealthillness_new == "YES"),
    .by = case
  ) |>
  replace_na(list(mh_linked = FALSE)) |>
  right_join(victims, by = "case") |>
  rowwise() |>
  mutate(mh_linked = any(linked_to_mental_state_of_suspect, mh_linked)) |>
  ungroup()

# Suspects with additional field on whether the incident was mental-health
# related
suspects_mh <- homicides |>
  filter(number_of_suspects_including_zero > 0) |>
  summarise(
    mh_linked = any(suspect_mentalhealthillness_new == "YES") |
      any(linked_to_mental_state_of_suspect),
    .by = case
  ) |>
  replace_na(list(mh_linked = FALSE)) |>
  right_join(suspects, by = "case") |>
  filter(number_of_suspects_including_zero > 0)

# Date on which the gang-related field was removed
gang_related_last_date <- victims |>
  filter(!is.na(gang_related_removed)) |>
  slice_max(order_by = date_recorded_correct_format, n = 1) |>
  pluck("date_recorded_correct_format", 1)

# Date on which the OCG-related field was added
ocg_related_first_date <- victims |>
  filter(!is.na(ocg_related_new)) |>
  slice_min(order_by = date_recorded_correct_format, n = 1) |>
  pluck("date_recorded_correct_format", 1)

# Date on which the county-lines-related field was added
county_lines_related_first_date <- victims |>
  filter(!is.na(county_lines_related_new)) |>
  slice_min(order_by = date_recorded_correct_format, n = 1) |>
  pluck("date_recorded_correct_format", 1)
