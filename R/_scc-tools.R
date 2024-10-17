# Data checks ----

key_ok <- function(key) {
  if(is.null(key)) return(FALSE)
  if(!is.vector(key)) return(FALSE)
  if(length(key) == 0) return(FALSE)
  if(anyNA(key)) return(FALSE)
  if(length(key) != length(unique(key))) return(FALSE)
  return(TRUE)
}

missing_n <- function(df, var) {
  df |> 
    dplyr::filter(
      is.null(var) |
        is.na(var) |
        length(var) == 0
    ) |> 
    nrow()
}

dup_n <- function(df, var) {
  
  n_unique_values <- df |> 
    dplyr::distinct(!!as.name(var)) |> 
    nrow()
  
  nrow(df) - n_unique_values
}

# Table vis ----

gt_theme_scc <- function(gt_object, ...) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  
  gt_object  |> 
    gt::opt_all_caps(locations = "row_group") |>
    gt::opt_table_font(
      font = list(
        gt::google_font("Cairo"),
        gt::default_fonts()
      ),
      weight = 400
    ) |> 
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Chivo"),
        weight = 700
      )
    ) |> 
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Chivo"),
        weight = 300
      )
    ) |> 
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = gt::px(1)
      ),
      locations = gt::cells_row_groups()
    ) |> 
    gt::tab_options(
      column_labels.background.color = "white",
      data_row.padding = gt::px(3),
      heading.border.bottom.style = "none",
      table.border.top.width = gt::px(3),
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = "gray",
      row_group.border.top.color = "gray",
      row_group.border.bottom.color = "gray",
      row_group.background.color = "gray95",
      row_group.font.weight = "normal",
      row_group.font.size = 12,
      stub.border.color = "white",
      stub.border.width = gt::px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 12,
      heading.align = "left",
      ...
    ) |> 
    gt::opt_css(
      "tbody tr:last-child {border-bottom: 2px solid #ffffff00;}",
      add = TRUE
    ) |> 
    gt::opt_row_striping(row_striping = FALSE) |> 
    # Workaround for bootstrap css baked into Quarto but breaks table citations
    # https://github.com/quarto-dev/quarto-cli/issues/6945
    gt::tab_options(quarto.disable_processing = TRUE) 
}

display_tbl <- function(df, interactive = FALSE){
  df |>    
    gt::gt() |>   
    {\(.) if (interactive) gt::opt_interactive(., use_compact_mode = TRUE) else . }() |> 
    gt_theme_scc()
}

display_freq_tbl <- function(df, category_col, n_name, interactive = TRUE){
  df |> 
    dplyr::count({{ category_col }}, name = deparse(substitute(n_name))) |> 
    dplyr::mutate(percentage = {{ n_name }}/sum({{ n_name }})) |> 
    dplyr::arrange(dplyr::desc({{ n_name }})) |>
    display_tbl(interactive = interactive) |>
    gt::fmt_integer({{ n_name }}) |>
    gt::fmt_percent(percentage, decimals = 1)
}

# Plots ----

set_ggplot_defaults <- function(){
  
  ggplot2::theme_set(
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 10),
    )
  )
  
  ggplot2::update_geom_defaults("bar", list(fill = "steelblue"))
}

# Creates a data frame in a format ready to produce a sankey plot
# i.e. call before sankey_plot()
sankey_add_link <- function(links, source, target, value){
  
  # Instantiates the data frame if it's the first call of this function
  # i.e. if the data frame passed in the first argument is empty
  if(is.null(links) || nrow(links) == 0 ) {
    links <- tibble::tribble(~source, ~target, ~value)
  }
  
  # A connection data frame is a list of flows with intensity for each flow
  links <- tibble::add_row(
    .data = links,
    source = source, 
    target = target,
    value = value
  )
}

# Creates a D3 sankey diagram
# Note: First create a links data frame by using sankey_add_link()
sankey_plot <- function(links){
  
  # Source: https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html
  
  # From these flows we need to create a node data frame: 
  # it lists every entities involved in the flow
  nodes <- data.frame(
    name = c(as.character(links$source), 
             as.character(links$target)) |> unique()
  )
  
  # With networkD3, connection must be provided using id, 
  # not using real name like in the links dataframe. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  sankey <- networkD3::sankeyNetwork(
    Links = links, 
    Nodes = nodes,
    Source = "IDsource", 
    Target = "IDtarget",
    Value = "value", 
    NodeID = "name", 
    sinksRight = FALSE,
    nodeWidth = 30, 
    fontSize = 12, 
    fontFamily = "Arial",
    nodePadding = 10
  )
  
  # Display the node value on a 2nd line of the label
  sankey |>
    htmlwidgets::onRender(
    'function(el, x) {
      d3.select(el)
        .selectAll(".node text")
        .html(d =>
          d.name + "<tspan dy=\'1.2em\' x=\'60\'>" +
          "(" + d.value.toLocaleString() + ")"
        );
    }'
  )
}

# Apply to a Sankey plot
# i.e. call after sankey_plot()
sankey_add_columns <- function(sankey, col_names){
  
  # Source: https://rpubs.com/mjmillic/757990
  
  # Formatted string of column names ready for JavaScript
  cols_str <- col_names |> 
    purrr::map_chr(~ {stringr::str_c("'", .x, "', ")}) |> 
    stringr::str_flatten()
  
  # Build JavaScript
  js <- stringr::str_c(
    'function(el) { 
        var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
        var labels = [',
    cols_str,
    '];
        cols_x.forEach((d, i) => {
          d3.select(el).select("svg")
            .append("text")
            .attr("x", d)
            .attr("y", 12)
            .text(labels[i]);
        })
      } 
    '  
  )
  
  # Inject JavaScript  
  htmlwidgets::onRender(sankey, js)
}

# Time series ----

get_covid_events <- function() {
  tribble(
    ~ event,         ~ date,
    "1st\nlockdown", "2020-03-26",
    "2nd\nlockdown", "2020-11-05"
  ) |> 
    mutate(
      date = as_date(date),
      yr = round(year(date) + (yday(date)/365), 2)
    )
}

# Rounding to financial year, using a custom function
#  - from bottom of https://tsibble.tidyverts.org/reference/index-by.html
financial_year <- function(date) {
  year <- lubridate::year(date)
  if_else(lubridate::quarter(date) < 2, year - 1, year)
}

financial_year_name <- function(financial_year, reverse = FALSE) {
  if (!reverse) {
    stringr::str_c(financial_year, "/", (financial_year + 1 - 2000))
  } else {
    stringr::str_extract(financial_year, "^[^/]+")
  }
}

# Dev ----

# Saves all current workspace objects apart from connections
# (which are loaded in setup)
#  - assumes there's a data folder
save_here <- function (file_name){
  
  # Character vector of all current objects apart from our connections
  ws_objs <- ls(name = globalenv()) 
  # |> 
  #   quanteda::char_remove("tm_con") 
  
  save(
    list = ws_objs, 
    file = here::here("data", stringr::str_c(file_name, ".RData"))
  )
}

# Will load objects from earlier workspace that were saved using save_here()  
#  - can make your testing workflow quicker & easier 
load_here <- function(file_name){
  load(
    file = here::here("data", stringr::str_c(file_name, ".RData")),
    envir = globalenv(),
    verbose = TRUE
  )
}