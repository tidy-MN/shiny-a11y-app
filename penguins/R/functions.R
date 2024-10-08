# author: Abby Stamm
# date: August 2024
# purpose: functions for sample dashboard

filter_penguins <- function(input, df) {
  if (!input$species == "All") {
    df <- df[df$species == input$species, ]
  }
  if (!input$island == "All") {
    df <- df[df$island == input$island, ]
  }
  if (!input$sex == "All") {
    df <- df[df$sex == input$sex, ]
  }
  if (!input$study_name == "All") {
    df <- df[df$study_name == input$study_name, ]
  }

  if (nrow(df) == 0) {
    df <- data.frame(Message = paste("There are no valid penguins for these", 
                                     "filters. Please select different filters."))
  }
  return(df)
}

table_penguins <- function(df) {
  if (ncol(df) == 1) { # no valid penguins
    dt <- DT::datatable(df, rownames = FALSE,
                        class = 'cell-border stripe',
                        options = list(paging = FALSE, searching = FALSE))
  } else {
    dt <- DT::datatable(df, extensions = c('Responsive'), escape = FALSE,
                        selection = "single", rownames = FALSE, 
                        options = list(responsive = TRUE, pageLength = 10,
                                       autoWidth = TRUE),
                        class = 'cell-border stripe') 
  }
  return(dt)
}

summarize_penguins <- function(df) {
  if (ncol(df) > 1) {
    df <- df |> dplyr::filter(!is.na(individual_id), !is.na(region)) |> 
      dplyr::group_by(region, island, species, sex) |> 
      dplyr::summarize(count = dplyr::n(), .groups = "drop")
  }
  return(df)
}

format_text <- function(input, message) {
    font_size <- gsub("[^0-9.]", "", input$font_size)
    letter_space <- gsub("[^0-9.]", "", input$letter_spacing)
    p_setting <- paste('<p style="font-size:', paste0(font_size, "px"), 
                       '; color:', input$text_color, 
                       '; letter-spacing:', paste0(letter_space, "px"), ';">')
    txt <- paste(p_setting, message, "</p>")
    return(HTML(txt))
}

font_size_color_matrix <- function() {
  # color contrast checker
  # https://webaim.org/resources/contrastchecker/
  d <- tibble::tibble(
    "Color Ratio 2:1" = c("12 pt", "18 pt", "24 pt"),
    "Color Ratio 3:1" = c("12 pt", "18 pt", "24 pt"),
    "Color Ratio 4.5:1" = c("12 pt", "18 pt", "24 pt")
  )
  
  DT::datatable(d, extensions = c('Responsive'), selection = "single",
                escape = FALSE, options = list(dom = 't', ordering = FALSE),
                class = 'cell-border stripe', rownames = FALSE) |>
    DT::formatStyle("Color Ratio 2:1", target = 'row', 
                    fontSize = DT::styleEqual("12 pt", '12px')) |>
    DT::formatStyle("Color Ratio 2:1", target = 'row',
                    fontSize = DT::styleEqual("18 pt", '18px')) |>
    DT::formatStyle("Color Ratio 2:1", target = 'row',
                    fontSize = DT::styleEqual("24 pt", '24px')) |>
    DT::formatStyle("Color Ratio 2:1", backgroundColor = "#FFA052", 
                    color = 'white') |>
    DT::formatStyle("Color Ratio 3:1", backgroundColor = "#F06C00", 
                    color = 'white') |>
    DT::formatStyle("Color Ratio 4.5:1", backgroundColor = "#C25700", 
                    color = 'white')
}


content_sources <- function() {
    sources <- paste(               
      '<img src="https://allisonhorst.github.io/palmerpenguins/logo.png"',
      'alt="palmer penguins R package logo">', "</br></br>", 
      "For more information, see: </br>", "<ul><li>",
      '<a href="https://allisonhorst.github.io/palmerpenguins/">',
      "palmerpenguins on GitHub</a>", "<li>", 
      '<a href="https://colorbrewer2.org/">', "ColorBrewer</a>", "<li>", 
      '<a href="https://www.w3.org/WAI/WCAG22/quickref/">',
      "WCAG guidelines at W3.org</a>", "</ul>")
    return(HTML(sources))
}

