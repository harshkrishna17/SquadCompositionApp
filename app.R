# Libraries

library(tidyverse)
library(shiny)
library(ggtext)
library(glue)
library(MetBrewer)
library(grDevices)
library(shinyWidgets)
library(ggforce)

# Data 

data <- read.csv("CompositionSquadDataJanuary23.csv")

# UI

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #101313;
        color: white;
      }

      .set1 form.well { 
      background: transparent;
      border: 0px;
      }"))
  ),
  titlePanel("", windowTitle = "Squad Composition App"),
  sidebarLayout(
    div(class = "set1", 
        sidebarPanel(
          selectInput("squad", "Squad:", choices = data$squad, selected = "Manchester City"),
          selectInput("plot_type", "Plot Type:", choices = c("Squad Table", "Comet Plot"), 
                      selected = "Squad Table"),
          prettyRadioButtons("theme", "Background Theme:", choices = c("Dark", "Light"), 
                             selected = "Dark", shape = "curve", animation = "smooth",
                             status = "success"),
          downloadBttn("download", "Download Plot", 
                       color = "success", style = "gradient", size = "sm")
        )),
    mainPanel(h2("European Squads Composition", style = "color:#2ECC71"),
              h4("This easy to use Shiny app allows you to create visualizations depicting the composition of squads in the Top 5 European leagues.", style = "color:white"),
              h5("Created by Harsh Krishna (@veryharshtakes)", style = "color:white"),
              plotOutput("plot", height = "600px"))
  )
)

# Server

server <- function(input, output) {
  
  plot_fun <- reactive({
    
    req(input$squad)
    
    # Theme
    
    if(input$theme == "Dark") {
      fill_b <- "#0d1117"
      colorText <- "white"
      colorLine <- "white"
      gridline <- "#525252"
    }
    else if(input$theme == "Light") {
      fill_b <- "#F4F4F4"
      colorText <- "black"
      colorLine <- "black"
      gridline <- "#9E9E9E"
    }
    
    # Data Wrangling
    
    data1 <- data %>%
      filter(squad == input$squad)
    
    data1 <- data1 %>%
      mutate(age = as.numeric(difftime(Sys.Date(), as.Date(player_dob), units = "weeks"))/52.25,
             age = floor(age),
             contract_expiry = as.numeric(difftime(as.Date(contract_expiry), Sys.Date(), units = "weeks"))/52.25,
             contract_end = round(contract_expiry),
             contract_end = age + contract_end) %>%
      mutate(date_joined = as.numeric(difftime(as.Date(date_joined), as.Date(player_dob), units = "weeks"))/52.25,
             age_when_joined = floor(date_joined)) %>%
      select(comp_name, squad, age, age_when_joined, contract_expiry, contract_end, player_name, player_position) %>%
      na.omit()
    
    data1 <- data1 %>%
      mutate(contract_expiry = ifelse(contract_expiry < 0, -contract_expiry, contract_expiry)) %>%
      mutate(Contracts = case_when(contract_expiry > 0 & contract_expiry < 1 ~ "Less than a year",
                                   contract_expiry >= 1 & contract_expiry < 2 ~ "1 to 2 years",
                                   contract_expiry >= 2 & contract_expiry < 3 ~ "2 to 3 years",
                                   contract_expiry >= 3 & contract_expiry < 4 ~ "3 to 4 years",
                                   contract_expiry >= 4 ~ "More than 4 years")) %>%
      mutate(Ages = case_when(age <= 23 ~ "Youth (u23)",
                              age >= 24 & age <= 29 ~ "Peak (24-29)",
                              age >= 30 ~ "Veteran (+30)")) %>%
      mutate(Position = case_when(player_position == "Centre-Back" |
                                    player_position == "Left-Back" |
                                    player_position == "Right-Back" ~ "Defenders",
                                  player_position == "Defensive Midfield" |
                                    player_position == "Central Midfield" |
                                    player_position == "Attacking Midfield" ~ "Midfielders",
                                  player_position == "Left Winger" |
                                    player_position == "Right Winger" |
                                    player_position == "Centre-Forward" ~ "Forwards",
                                  player_position == "Goalkeeper" ~ "Goalkeepers")) %>%
      select(player_name, Ages, Position, Contracts, squad, comp_name, contract_expiry, contract_end, age_when_joined, age) %>%
      na.omit()
    
    if (input$plot_type == "Squad Table") {
      
      list_data <- split(data1, list(data1$Position, data1$Ages))
      list_data <- list_data[sapply(list_data, nrow) > 0]
      
      index_col <- function(x) {
        x <- x %>%
          arrange(desc(Contracts)) %>%
          mutate(index = 1:nrow(x))
      }
      
      final_data <- list_data %>%
        purrr::map(index_col) %>%
        purrr::reduce(full_join)
      
      final_data$AgeGroups <- factor(final_data$Ages, levels = c("Youth (u23)", "Peak (24-29)", "Veteran (+30)"))
      final_data$Position <- factor(final_data$Position, levels = c("Goalkeepers", "Defenders", "Midfielders", "Forwards"))
      
      contract_num <- length(unique(final_data$Contracts))
      
      # Theme Function
      
      theme_custom_1 <- function() {
        theme_minimal() +
          theme(plot.background = element_rect(colour = fill_b, fill = fill_b),
                panel.background = element_rect(colour = fill_b, fill = fill_b)) +
          theme(plot.title = element_text(colour = colorText, size = 28, hjust = 0.5, face = "bold"),
                plot.subtitle = element_markdown(colour = colorText, size = 20, hjust = 0.5),
                plot.caption = element_text(colour = colorText, size = 14, hjust = 1),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank()) +
          theme(strip.background = element_blank(),
                strip.text = element_text(colour = colorText, face = "bold", size = 18)) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          theme(panel.grid.major.x = element_blank(),
                panel.background = element_blank())
      }
      
      # Plot
      
      final_data %>%
        ggplot(aes(x = 1, y = index)) +
        geom_text(aes(label = player_name, colour = Contracts), fontface = "bold") +
        scale_colour_manual(values = met.brewer(name = "Homer2", n = contract_num, type = "discrete")) +
        geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = max(index) + 1), fill = NA, 
                  colour = gridline, linetype = "dashed", size = 1) +
        scale_y_reverse() +
        facet_grid(Position ~ AgeGroups, scales = "free") +
        theme_custom_1() +
        theme(legend.position = "top") +
        theme(legend.title = element_text(colour = colorText, size = 14),
              legend.text = element_text(colour = colorText, size = 12)) +
        labs(title = glue("{final_data$squad} Squad Composition"),
             subtitle = glue("{final_data$comp_name} 2022/23"),
             caption = "Data from Transfermrkt. Inspired by @Worville. Created by @veryharshtakes",
             colour = "Contract time left")
      
    }
    
    else if (input$plot_type == "Comet Plot") {
      
      df <- data1
      
      theme_custom_2 <- function() {
        theme_minimal() +
          theme(plot.background = element_rect(colour = fill_b, fill = fill_b),
                panel.background = element_rect(colour = fill_b, fill = fill_b)) +
          theme(plot.title = element_text(colour = colorText, size = 28, face = "bold", hjust = 0.5),
                plot.subtitle = element_markdown(colour = colorText, size = 20, hjust = 0.5),
                plot.caption = element_text(colour = colorText, size = 12, hjust = 1),
                axis.title.x = element_text(colour = colorText, size = 14),
                axis.title.y = element_blank(),
                axis.text.x = element_text(colour = colorText, size = 12),
                axis.text.y = element_text(colour = colorText, size = 12)) +
          theme(strip.text = element_blank()) +
          theme(panel.grid.major = element_line(colour = gridline, size = 0.3, linetype = "dashed"),
                panel.grid.minor = element_line(colour = gridline, size = 0.3, linetype = "dashed")) +
          theme(panel.grid.major.x = element_line(colour = gridline, size = 0.3, linetype = "dashed"),
                panel.background = element_blank()) +
          theme(legend.title = element_text(colour = colorText, size = 14),
                legend.text = element_text(colour = colorText, size = 12),
                legend.position = "top") +
          theme(panel.spacing = unit(0.5, "cm"))
      }
      
      df$Position <- factor(df$Position, levels = c("Goalkeepers", "Defenders", "Midfielders", "Forwards"))
      
      ggplot(df) +
        geom_link(aes(x = age_when_joined, y = player_name, xend = age, yend = player_name, 
                      alpha = stat(index), colour = Position), size = 4, show.legend = FALSE) +
        geom_point(aes(x = age, y = player_name, fill = Position), size = 4, shape = 21, stroke = 1) +
        geom_point(aes(x = contract_end, y = player_name, fill = Position), 
                   shape = 23, size = 3, stroke = 1, colour = "black") +
        scale_colour_manual(values = met.brewer(name = "Homer2", n = 4, type = "discrete")) +
        scale_fill_manual(values = met.brewer(name = "Homer2", n = 4, type = "discrete")) +
        scale_x_continuous(breaks = c(min(df$age):max(df$age)), labels = c(min(df$age):max(df$age))) +
        facet_grid(Position ~ ., scales = "free_y", space = "free_y", switch = "y") +
        theme_custom_2() +
        labs(title = glue("{df$squad} Squad Composition"),
             subtitle = glue("{df$comp_name} 2022/23"),
             x = "Age", 
             caption = "Comet Tails correspond to age at which players joined the club. Comet Points correspond to the player's current age\nDiamonds represent the age at which their contract expires\nCreated by Harsh Krishna")
      
    }
    
  })
  
  output$plot <- renderPlot({
    
    plot_fun()
    
  })
  
  # Download
  
  output$download <- downloadHandler(
    filename = function() { paste(input$squad, ".png", sep="") },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 4000, height = 2500, res = 300, units = "px")
      ggsave(file, plot = plot_fun(), device = device, bg = "#0d1117")
    }
  )
}

# Run App 

shinyApp(ui, server)
