load("35509-0001-Data.rda")  
df <- "35509-0001-Data.rda"
# change to the folder path of your data
library(labelled)
library(tibble)  
library(tidyverse) 
library(dplyr)  
# Suppose df is your data.frame

df<-da35509.0001
tb <- as_tibble(df)
col_names<-attributes(tb)
col_names_<-as.data.frame(col_names$variable.labels)

#tb %>% select(CIGEVER, CIGOFRSM, CIGWILYR, CIGTRY, CIGYFU, CIG30USE, CG30EST, CIG30AV, CIG30MEN,
 #                  CIGDLYMO, CIGAGE, CIG100LF, ALCEVER, ALCTRY, ALCYRTOT, ALDAYPYR, ALDAYPMO, ALDAYPWK, 
 #                  ALCDAYS, AL30EST, WORKBLAH, WORKDAYS, COLLENR, HSGED, HSDIPLMA, HEALTH2, SEXRACE, NEWRACE2, 
 #                  SEXAGE, HEALTH, COMBATPY, SERVICE, NOMARR2, POVERTY2, HLTINMNT, 
 #                  YODPREV, YODSCEV, YOLOSEV, YODPDISC, DYTXVST, CLINVST, THERVST, MHSUIPLN, MHSUITRY
 #             IRSEX, SEXAGE, SEXRACE)

as<-tb %>% select(CIGEVER, CIGDLYMO)

#HEALTH
#CIGTRY - AGE WHEN FIRST SMOKED A CIGARETTE
#CIGAGE - AGE WHEN FIRST STARTED SMOKING CIGARETTES EVERYDAY
#SNUFTRY - AGE WHEN FIRST USED SNUFF
#CHEWTRY - AGE WHEN FIRST USED CHEW
#COCUS30A -  DAYS USED COCAINE PAST 30 DAYS
#CRKUS30A - DAYS USED CRACK PAST 30 DAYS
#HAL30USE - DAYS USED HALLUCINOGEN PAST 30 DAYS
library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Substance Use Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Filters
      checkboxGroupInput(
        "health_sel", "Select HEALTH:", 
        choices = unique(tb$HEALTH), selected = unique(tb$HEALTH)
      ),
      
      # Age filters for first use
      sliderInput("cigtry_range", "CIGTRY - Age when first smoked a cigarette:",
                  min = min(tb$CIGTRY, na.rm = TRUE),
                  max = max(tb$CIGTRY, na.rm = TRUE),
                  value = c(min(tb$CIGTRY, na.rm = TRUE), max(tb$CIGTRY, na.rm = TRUE))),
      sliderInput("cigage_range", "CIGAGE - Age when first smoked every day:",
                  min = min(tb$CIGAGE, na.rm = TRUE),
                  max = max(tb$CIGAGE, na.rm = TRUE),
                  value = c(min(tb$CIGAGE, na.rm = TRUE), max(tb$CIGAGE, na.rm = TRUE))),
      
      # CRKEVER & SEXAGE filters for CIGTRY plot
      checkboxGroupInput("crk_groups",
                         "Select EVER USED CRACK groups:",
                         choices = unique(tb$CRKEVER),
                         selected = unique(tb$CRKEVER)),
      checkboxGroupInput("sexage_groups",
                         "Select COMBINED GENDER BY AGE CATEGORY (SEXAGE):",
                         choices = unique(tb$SEXAGE),
                         selected = unique(tb$SEXAGE))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Count Plot", plotOutput("countPlot")),
        tabPanel("Density by HEALTH",
                 plotOutput("cigtryDensity"),
                 plotOutput("cigageDensity"),
                 plotOutput("crkagePlot") # CRKAGE by HEALTH with mean lines
        ),
        tabPanel("CIGTRY by CRKEVER & SEXAGE", plotOutput("cigtryByCrkPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive: filter for HEALTH and numeric ranges
  filtered_data <- reactive({
    tb %>%
      filter(
        HEALTH %in% input$health_sel,
        CIGTRY >= input$cigtry_range[1] & CIGTRY <= input$cigtry_range[2],
        CIGAGE >= input$cigage_range[1] & CIGAGE <= input$cigage_range[2]
      )
  })
  
  # Count plot by HEALTH
  output$countPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = HEALTH)) +
      geom_bar(fill = "steelblue", color = "black") +
      labs(x = "Health Status", y = "Number of Subjects", title = "Number of Subjects by HEALTH (Filtered)") +
      theme_minimal()
  })
  
  # Density plots by HEALTH
  output$cigtryDensity <- renderPlot({
    ggplot(filtered_data(), aes(x = CIGTRY, fill = HEALTH)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age First Smoked a Cigarette", y = "Density", title = "Density of CIGTRY by HEALTH") +
      theme_minimal()
  })
  
  output$cigageDensity <- renderPlot({
    ggplot(filtered_data(), aes(x = CIGAGE, fill = HEALTH)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age First Smoked Every Day", y = "Density", title = "Density of CIGAGE by HEALTH") +
      theme_minimal()
  })
  
  # CRKAGE by HEALTH with mean lines
  filtered_health <- reactive({
    req(input$health_sel)
    tb %>% filter(HEALTH %in% input$health_sel, !is.na(CRKAGE))
  })
  
  mean_health <- reactive({
    filtered_health() %>%
      group_by(HEALTH) %>%
      summarise(mean_CRKAGE = mean(CRKAGE, na.rm = TRUE), .groups = "drop") %>%
      mutate(label = paste0(HEALTH, " (mean = ", round(mean_CRKAGE, 1), ")"))
  })
  
  output$crkagePlot <- renderPlot({
    mv <- mean_health()
    health_colors <- setNames(scales::hue_pal()(nrow(mv)), mv$HEALTH)
    
    ggplot(filtered_health(), aes(x = CRKAGE, color = HEALTH, fill = HEALTH)) +
      geom_density(alpha = 0.4) +
      geom_vline(data = mv, aes(xintercept = mean_CRKAGE, color = HEALTH),
                 linetype = "dashed", size = 1) +
      scale_color_manual(name = "Health status (mean CRKAGE)", values = health_colors, labels = mv$label) +
      scale_fill_manual(name = "Health status (mean CRKAGE)", values = health_colors, labels = mv$label) +
      labs(x = "Age when first used crack (CRKAGE)", y = "Density") +
      theme_minimal()
  })
  
  # CIGTRY by CRKEVER & SEXAGE
  filtered_cigtry <- reactive({
    req(input$crk_groups, input$sexage_groups)
    tb %>%
      filter(CRKEVER %in% input$crk_groups,
             SEXAGE %in% input$sexage_groups,
             !is.na(CIGTRY))
  })
  
  mean_cigtry <- reactive({
    filtered_cigtry() %>%
      group_by(CRKEVER) %>%
      summarise(mean_CIGTRY = mean(CIGTRY, na.rm = TRUE), .groups = "drop") %>%
      mutate(label = paste0(CRKEVER, " (mean = ", round(mean_CIGTRY, 1), ")"))
  })
  
  output$cigtryByCrkPlot <- renderPlot({
    mv <- mean_cigtry()
    crk_colors <- setNames(scales::hue_pal()(nrow(mv)), mv$CRKEVER)
    
    ggplot(filtered_cigtry(), aes(x = CIGTRY, color = CRKEVER, fill = CRKEVER)) +
      geom_density(alpha = 0.4) +
      geom_vline(data = mv, aes(xintercept = mean_CIGTRY, color = CRKEVER),
                 linetype = "dashed", size = 1) +
      scale_color_manual(name = "CRKEVER (mean CIGTRY)", values = crk_colors, labels = mv$label) +
      scale_fill_manual(name = "CRKEVER (mean CIGTRY)", values = crk_colors, labels = mv$label) +
      facet_wrap(~ SEXAGE) +
      labs(x = "Age when first smoked (CIGTRY)", y = "Density") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)

