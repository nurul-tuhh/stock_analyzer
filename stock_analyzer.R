
library(shiny)
library(shinyWidgets)
library(lubridate)

library(plotly)
library(tidyverse)
library(quantmod)

library(rvest)
library(glue)

# Helper functions ---
get_stock_list <- function(stock_index = "DAX") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker symbol", "Company")),
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}

get_symbol_from_user_input <- function(user_input) {
  user_input_array <- str_split(user_input, ", ")
  
  return(user_input_array[[1]][1])
}

get_stock_data <- function(
  stock_symbol,
  from = today() - days(180),
  to = today(),
  mavg_short = 20,
  mavg_long = 50
) {
  stock_symbol %>% quantmod::getSymbols(
    from = from,
    to = to,
    auto.assign = F
  ) %>% 
    timetk::tk_tbl(preserve_index = T,silent = T) %>% 
    mutate(currency = case_when(
      str_detect(names(.) %>% last(), "DE") ~ "EUR", TRUE ~ "USD")
    ) %>% 
    set_names(c("date", "open", "high", "low", "close", "volume", "adjusted", "currency")) %>%
    drop_na() %>%
    mutate(date = as.Date(date, "%d.%m.%y")) %>% 
    
    mutate(mavg_short = rollmean(adjusted, mavg_short,  fill = NA, align = "right")) %>%
    mutate(mavg_long = rollmean(adjusted, mavg_long,  fill = NA, align = "right")) %>%
    dplyr::select(date, adjusted, mavg_short, mavg_long, currency)
}

plot_stock_data <- function(stock_data) {
  g <- stock_data %>% 
    pivot_longer(
      cols = c("adjusted", "mavg_short", "mavg_long"),
      names_to = "legend",
      values_to = "value",
      names_ptypes = list(legend = factor())
    ) %>% 
    ggplot(aes(x = date, y = value, group = legend)) +
    geom_line(aes(linetype = legend)) +
    labs(y = "Adjusted Share Price", x = "")
  
  ggplotly(g)
}

currency_format <- function(currency) {
  
  if (currency == "dollar") 
  { x <- scales::dollar_format(largest_with_cents = 10) }
  if (currency == "euro")   
  { x <- scales::dollar_format(prefix = "", suffix = " €",
                               big.mark = ".", decimal.mark = ",",
                               largest_with_cents = 10)}
  return(x)
}

generate_commentary <- function(data, user_input) {
  warning_signal <- data %>%
    tail(1) %>% # Get last value
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
    pull(mavg_warning_flag)
  
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
  } else {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
    
  }
}

# Helper function end ---

# UI ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  div(
    h1("Stock Analyzer"),
    column(
      width = 4,
      
      h4("Stock Index"),
      pickerInput(
        inputId = "user_selected_server",
        choices = c("DAX", "SP500", "DOW", "NASDAQ")
      ),
      
      h4("Stocks"),
      uiOutput(outputId = "indices"),
      
      sliderInput(
        inputId = "short_moving_avg",
        label = h4("Short Moving Average"),
        min = 5,
        max = 40,
        value = 20,
        step = 1,
        round = TRUE
      ),
      
      sliderInput(
        inputId = "long_moving_avg",
        label = h4("Long Moving Average"),
        min = 50,
        max = 120,
        value = 50,
        step = 1,
        round = TRUE
      ),
    ),
    column(
      width = 8,
      div(
        div(h4(textOutput(outputId = "stock"))),
        div(textOutput(outputId = "server")),

        div(
          plotlyOutput(outputId = "stock_trend_plot")
        )
      )
    ),
    column(
      width = 12,
      div(h4("Analyst Commentary")),
      div(
        textOutput(outputId = "stock_commentary")
      )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  output$stock <- reactive(input$stock_selection)
  
  output$indices <- renderUI({
    choices = input$user_selected_server %>% get_stock_list() %>% purrr::pluck("label")
    pickerInput(
      inputId = "stock_selection",
      choices = choices
    )
  })
  
  output$stock_trend_plot <- renderPlotly(
    input$stock_selection %>%
      get_symbol_from_user_input() %>%
      get_stock_data(mavg_short = input$short_moving_avg, mavg_long = input$long_moving_avg) %>% 
      plot_stock_data()
  )

  output$stock_commentary <- renderPrint(
    input$stock_selection %>%
      get_symbol_from_user_input() %>%
      get_stock_data(mavg_short = input$short_moving_avg, mavg_long = input$long_moving_avg) %>%
      generate_commentary(user_input = input$stock_selection)
  )
}
# RUN APP ----
shinyApp(ui = ui, server = server)

