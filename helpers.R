# this file contains objects and functions that are needed to generate CJ charts

# load core packages
library(ggtext)
library(httr)
library(httr2)
library(janitor)
library(patchwork)
library(scales)
library(tidyverse)

# define colour scheme, based on the UCL colour scheme defined at
# https://www.ucl.ac.uk/cam/brand/guidelines/colour
ucl_colours <- tribble(
  ~name, ~hex_code,
  "Dark Green", "#555025",
  "Dark Red", "#651D32",
  "Dark Purple", "#4B384C",
  "Dark Blue", "#003D4C",
  "Dark Brown", "#4E3629",
  "Mid Green", "#8F993E",
  "Mid Red", "#93272C",
  "Mid Purple", "#500778",
  "Mid Blue", "#002855",
  "Stone", "#D6D2C4",
  "Bright Green", "#B5BD00",
  "Bright Red", "#D50032",
  "Bright Blue", "#0097A9",
  "Bright Pink", "#AC145A",
  "Light Green", "#BBC592",
  "Light Red", "#E03C31",
  "Light Purple", "#C6B0BC",
  "Light Blue", "#8DB9CA",
  "Yellow", "#F6BE00",
  "Orange", "#EA7600",
  "Grey", "#8C8279",
  "Blue Celeste", "#A4DBE8"
)

ucl_colours_list <- ucl_colours$hex_code
names(ucl_colours_list) <- ucl_colours$name


# custom ggplot2 theme for the chart
theme_cjcharts <- function (...) {
  theme_minimal(base_family = "Arial", ...) %+replace%
    theme(
    	# axis.text.x = element_markdown(),
    	# axis.text.y = element_markdown(hjust = 1),
    	axis.ticks = element_line(colour = "grey92"),
      axis.title = element_text(size = 9, hjust = 1),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(10, "mm"),
      legend.position = "bottom",
      legend.spacing.x = unit(2, "mm"),
      legend.title = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9, colour = "grey33", hjust = 1,
      														margin = margin(t = 3)),
      plot.tag = element_text(size = 12, face = "bold", colour = "grey33",
                              hjust = 0),
      plot.tag.position = c(0.01, 0.01),
    	plot.subtitle = element_textbox_simple(
    	  lineheight = 1.1, 
    	  margin = margin(t = 3, b = 6),
    	  size = 10
    	 ),
      plot.title = element_textbox_simple(
        face = "bold", 
        size = 16, 
        hjust = 0, 
        lineheight = 1,
        margin = margin(t = 0, b = 0)
      ),
    	plot.title.position = "plot",
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
}


# common values for chart elements, which cannot be specified in a theme
elements <- list(
	linetype = c("solid", "11", "52"),
	label_arrow = arrow(length = unit(4, "points"), ends = "first"),
	label_line_colour = "grey40",
	label_line_curvature = 0.3,
	label_text_colour = "grey33",
	label_text_fill = "white",
	label_text_lineheight = 0.9,
	label_text_size = 9 / (14 / 5),
	average_line_colour = "grey50",
	average_line_linetype = "11",
	reference_line_colour = "grey50"
)


# format chart subtitle
format_subtitle <- function (..., .width = 100) {
  paste0("\n", str_wrap(glue::glue(..., .sep = " "), .width))
}


# format chart caption
format_caption <- function (chart_source, chart_id, chart_note = NA) {
	paste0(
		"\n\n",
		glue::glue(
			ifelse(!is.na(chart_note), paste0(chart_note, "\n"), ""),
			"Data: {chart_source} | ", "Details: lesscrime.info/post/{chart_id}",
			.sep = " "
		)
	)
}


# Add plot information
add_info <- function(chart, chart_source, chart_id) {
  chart + 
    patchwork::plot_annotation(
      caption = stringr::str_glue(
        "Data: {chart_source}  |  Details: lesscrime.info/post/{chart_id}",
        "<br>Author: Dr Matt Ashby, UCL Security and Crime Science  |  ",
        "Licence: Creative Commons Attribution "
      ),
      theme = ggplot2::theme(
        plot.caption = ggtext::element_textbox_simple(
          colour = "grey20", 
          family = "Arial", 
          fill = "grey95",
          hjust = 0,
          lineheight = 1.1, 
          margin = margin(t = 3),
          padding = margin(t = 5, r = 5, b = 3, l = 5),
          size = 9
        )
      )
    )
}


# add logo to chart
add_logo <- function (chart, chart_source, chart_id) {

	scs_logo <- here::here("files/UCL_logo_SCS_orange.png") %>% 
	  png::readPNG() %>% 
	  grid::rasterGrob(x = 0, hjust = 0)

	ggpubr::ggarrange(
		ggplot2::ggplotGrob(chart),
		ggpubr::ggarrange(
			scs_logo,
			grid::textGrob(
			  stringr::str_glue(
			    "Data: {chart_source} | Details: lesscrime.info/post/{chart_id}",
			    "\nAuthor: Matt Ashby, University College London | ",
			    "Licence: Creative Commons Attribution "
			  ),
				x = unit(1, "npc"),
				hjust = 1,
				gp = grid::gpar(col = "grey20", fontfamily = "Arial", fontsize = 8, 
				                lineheight = 1)
			),
			ncol = 2,
			nrow = 1,
			widths = c(0.25, 1)
		),
		ncol = 1,
		nrow = 2,
		heights = c(1, 0.075)
	)

}


# version of case_when() in which cases are returned as a factor in the order in
# which they are specified
fct_case_when <- function(...) {
	args <- as.list(match.call())
	levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
	levels <- levels[!is.na(levels)]
	factor(dplyr::case_when(...), levels=levels)
}


# function to split a string into approximately equal length lines
balance_lines <- Vectorize(function (str, lines) {
	width <- 1
	width_lines <- str_count(str_wrap(str, width), "\\n")
	while (width_lines > lines) {
		width <- width + 1
		width_lines <- str_count(str_wrap(str, width), "\\n") + 1
	}
	str_wrap(str, width)
})


# function to calculate percentage change
perc_change <- function (from, to, format = TRUE, ...) {
	change <- (to - from) / from
	if (format == TRUE) {
		scales::percent(change, ...)
	} else {
		change
	}
}


# Save original data file
download_data <- function(url) {
  
  ext <- tools::file_ext(url)
  
  temp_file <- tempfile(fileext = stringr::str_glue(".{ext}"))
  
  httr2::request(url) |> 
    httr2::req_progress() |> 
    httr2::req_timeout(60 * 5) |> 
    httr2::req_retry(max_tries = 3) |> 
    httr2::req_perform(path = temp_file)
  
  temp_file
  
}

