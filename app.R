library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(bslib)
library(shinyWidgets)
library(ggplotlyExtra)
library(plotly)
library(gghalves)
library(ggforce)
library(ggdist)
library(shinycssloaders)
library(shinytitle)

# Loading libraries

data = read.csv("health_status_data.csv")
# Load csv as "data" variable

data = data %>% select(-SAMPLE1, -SAMPLE2, -SAMPLE3, -SAMPLE4, -SAMPLE5,
                       -RI_1, -RI_2, -RI_3, -RI_4, -RI_5, -RI_6, -RI_7, -RI_8,
                       -RACIALIZED_IDENTITY_SUM, -RG_AMERICANINDIAN, -RG_ASIAN,
                       -RG_BLACK, -RG_LATINE, -RG_MIDDLEEASTERN, -RG_NATIVEHAWAIIAN,
                       -RG_WHITE, -RG_OTHER, -RacializedGroups, -AGE_DICHOTOMOUS, -INCOME3, -PoorFairHealth, -ExcellentHealth, -RG)
# Unselecting unneeded columns

data = data %>% mutate(
  
  SEX = case_when(SEX == 0 ~ "Male",
                  SEX == 1 ~ "Female",
                  SEX == 2 ~ "Intersex",
                  SEX == 3 ~ "Other"),
  
  AmericanIndian = case_when(AmericanIndian == 0 ~ "",
                             AmericanIndian == 1 ~ "American Indian"),
  
  Asian = case_when(Asian == 0 ~ "",
                    Asian == 1 ~ "Asian"),
  
  Black = case_when(Black == 0 ~ "",
                    Black == 1 ~ "Black or African American"), 
  
  Latine = case_when(Latine == 0 ~ "",
                     Latine == 1 ~ "Latino or Hispanic"), 
  
  MiddleEastern = case_when(MiddleEastern == 0 ~ "",
                            MiddleEastern == 1 ~ "Middle Eastern or North African"), 
  
  NativeHawaiian = case_when(NativeHawaiian == 0 ~ "",
                             NativeHawaiian == 1 ~ "Native Hawaiian or Pacific Islander"),
  
  White = case_when(White == 0 ~ "",
                    White == 1 ~ "White"),
  
  Other = case_when(Other == 0 ~ "",
                    Other == 1 ~ "Other"),
  
  MINORITIZED_STATUS = case_when(RACIALIZED_IDENTITY == 0 ~ "Minoritized",
                                 RACIALIZED_IDENTITY == 1 ~ "Non-minoritized"),
  
  PERCEIVED_INCOME = factor(case_when(PERCEIVED_INCOME == 1 ~ "Lower",
                                      PERCEIVED_INCOME == 2 ~ "Lower Middle",
                                      PERCEIVED_INCOME == 3 ~ "Middle",
                                      PERCEIVED_INCOME == 4 ~ "Upper Middle",
                                      PERCEIVED_INCOME == 5 ~ "Upper"),
                            levels = c("Lower", "Lower Middle", "Middle", "Upper Middle", "Upper"), ordered = T),
  
  HEALTH_STATUS_LABEL = factor(case_when(HEALTH_STATUS == 5 ~ "Excellent",
                                         HEALTH_STATUS == 4 ~ "Very Good",
                                         HEALTH_STATUS == 3 ~ "Good",
                                         HEALTH_STATUS == 2 ~ "Fair",
                                         HEALTH_STATUS == 1 ~ "Poor"),
                               levels = c("Excellent", "Very Good", "Good", "Fair", "Poor"), ordered = T)
  
)
# Recoding variables according to codebook

data = data %>% unite("Race", AmericanIndian:Other, remove = TRUE, sep = "")
# Unite all race columns, getting rid of them after combining

data = data %>% mutate(
  RACIALIZED_IDENTITY = case_when(Race == "Middle Eastern or North AfricanWhite" ~ "Multiracial",
                                  Race == "Black or African AmericanLatino or HispanicWhite" ~ "Multiracial",
                                  Race == "Native Hawaiian or Pacific IslanderWhite" ~ "Multiracial",
                                  Race == "Latino or HispanicWhite" ~ "Multiracial",
                                  Race == "Black or African AmericanWhite" ~ "Multiracial",
                                  Race == "AsianLatino or Hispanic" ~ "Multiracial",
                                  Race == "AsianNative Hawaiian or Pacific Islander" ~ "Multiracial",
                                  Race == "AsianWhite" ~ "Multiracial",
                                  TRUE ~ Race) # keep other races the same
  
)

# Getting rid of multiple races, converting them to "Multiracial"

data = data %>% select(SAMPLE_ID, AGE, SEX, RACIALIZED_IDENTITY, MINORITIZED_STATUS, PERCEIVED_INCOME, SSS, HEALTH_STATUS, HEALTH_STATUS_LABEL)
#Reorder columns to make more sense


#Shiny app code

# Don't repeat yourself -- making ui code more neat 
# tell class this is a good way of building app if it gets too complicated

sidebar_filters =
  accordion(
    accordion_panel(
      "Study Information", icon = bsicons::bs_icon("info-circle"), #icons: https://icons.getbootstrap.com/
      selectInput("sample", "Select Sample(s)", choices = unique(data$SAMPLE_ID), selected = c(1:5), multiple = TRUE)),
    
    accordion_panel(
      "Sociodemographics", icon = bsicons::bs_icon("globe-americas"),
      sliderInput("age", "Age Range", min = min(data$AGE), max = max(data$AGE), value = c(18, 77)),
      checkboxGroupInput("sex", "Sex", choices = c("Male", "Female"), selected = c("Male", "Female")),
      selectizeInput("race", "Racialized Identity", choices = unique(data$RACIALIZED_IDENTITY), selected = unique(data$RACIALIZED_IDENTITY), multiple = TRUE),
      checkboxGroupInput("minoritize", "Minoritized Status", choices = unique(data$MINORITIZED_STATUS),
                         selected = unique(data$MINORITIZED_STATUS))),
    accordion_panel(
      "Socioeconomic Status", icon = bsicons::bs_icon("currency-dollar"),
      checkboxGroupInput("income", "Social Status Class", choices = levels(data$PERCEIVED_INCOME),
                         selected = levels(data$PERCEIVED_INCOME)),
      sliderInput("sss", "Subjective Social Status", min = min(data$SSS), max = max(data$SSS), value = c(1, 10))),
    br(),
    br(),
    div(class = "text-center",
        actionButton("update", "Filter Data", style = "color: #fff; background-color: #005A43; border-color: #003628"),
        br(),
        br(),
        actionButton("reset", "Reset Data", style = "color: #fff; background-color: #7f8c8d; border-color: #7f8c8d")),
    br(),
    br()
  )

#Ui code
ui <- page_fluid(
  
  theme = bs_theme(),
  title = "Health Status Explorer",
  use_shiny_title(),
  br(),
  titlePanel(div(tags$b("Health Status Explorer", style = "color:#005A43")), 
             tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"))), 
  
  # Sidebar outside of the tabs
  layout_sidebar(
    sidebar = sidebar_filters, # Sidebar shared across all pages
    navset_pill( 
      
      nav_menu(
        "About",
        nav_panel("The App",
                  card(
                    h3(tags$b("Welcome to the Health Status Explorer App!")),
                    p("This interactive app allows you to filter data and examine the health status of 343 people with different ages, sexes, racialized identities, and socioeconomic statuses."),
                    
                    tags$b(p("How to Use"), style = "margin-bottom: -10px;"),
                    tags$ul(
                      tags$li("Navigate through different views using the tabs at the top"),
                      tags$li("Use the left side panel to select your variables of interest"),
                      tags$li("Press the green button to apply your filters"),
                      tags$li("Use backspace to remove any unwanted filters")
                    ),
                    
                    tags$b(p("Sample"), style = "margin-bottom: -10px;"),
                    tags$ul(
                      tags$li("Five student research teams in the public health research stream in the First-Year Research Immersion (FRI) at Binghamton University conducted research for this study.")
                    ),
                    
                    tags$b(p("Variables"), style = "margin-bottom: -10px;"),
                    tags$ul(
                      tags$li(tags$b("Health Status:"),
                              tags$ul(
                                tags$li("Definition: ", tags$a(href = "https://www.cdc.gov/nchs/hus/topics/health-status.htm#definitions", "[Participant's perception of their health and well-being.]", target="_blank"), style = "margin-bottom: 10px;"),
                                tags$li("Item: 'Would you say your health in general is excellent, very good, good, fair, or poor?'", style = "margin-bottom: 10px;"),
                                tags$li("Response Options: 1 = Poor, 2 = Fair, 3 = Good, 4 = Very Good, 5 = Excellent")
                              )
                      ),
                      br(),
                      
                      tags$li(tags$b("Socioeconomic Status (SES):"),
                              tags$ul(
                                tags$li("Definition: ", tags$a(href = "https://www.apa.org/pi/ses/resources/class/definitions", 
                                                               "[Social class encompasses both socioeconomic status (SES) and subjective social status (SSS). 
                                         Although social class may often be included in psychological studies, it is often treated as a control variable as opposed to 
                                         a main variable, or a moderator or mediator of a relationship. As such, these practices do not allow us to examine the role of 
                                         social class in predictive relationships, nor potential class-related differences among constructs 
                                         (Diemer et al., 2013). SES and SSS can also be conceptualized in several ways, which has implications for 
                                         the types of measures included in your study.]", target="_blank"), style = "margin-bottom: 10px;"),
                                tags$li("Item: 'Think of ", tags$a(href = "https://www.researchgate.net/publication/361611162/figure/fig2/AS:11431281244691551@1715999869843/The-subjective-social-status-was-measured-using-the-MacArthur-scale-1-Here-is-a.tif", "this ladder", target="_blank"), "as representing where people stand in the United States. At the top of the ladder are the people who are the best off – those who have the most money, 
                                the most education, and the most respected jobs. 
                                        At the bottom are the people who are the worst off – those who have the least money, least education, the least respected jobs, or no job. The higher up you are on this ladder, the closer you are to the people at the very top; the lower you are, 
                                        the closer you are to the people at the very bottom.'", style = "margin-bottom: 10px;"),
                                tags$li("Response Options: 1 (Lowest) to 10 (Highest)")
                              )
                      ),
                      
                      br(),
                      tags$li(tags$b("Minoritized Status:"),
                              tags$ul(
                                tags$li("Definition: ", tags$a(href = "https://www.ama-assn.org/system/files/ama-aamc-equity-guide.pdf", 
                                                               "[Whether a participant belongs to a historically marginalized racial group.]", target="_blank"), style = "margin-bottom: 10px;"),
                                tags$li("Item: 'What is your race and ethnicity?'", style = "margin-bottom: 10px;"),
                                tags$li("Response Options: White = Non-Minoritized Status, All Non-White Racialized Identities = Minoritized Status")
                              ))
                    ))
        ),
        
        nav_panel("The Research and Development Team",
                  card(
                    h3(tags$b("About the Research and Development Team")),
                    p("This research was conducted by Cohort 10 (2023-2024) of the ",
                      tags$a(href = "https://www.binghamton.edu/first-year-research-immersion/research/public-health/index.html" , 
                             "First-year Research Immersion (FRI) Program at Binghamton University.", target="_blank")),
                    tags$ul(
                      tags$li(tags$b("R Shiny App Development:"), "Spiegel, Z."),
                      br(),
                      tags$li(tags$b("Ggplot2 Data Visualization:"), "Silhavy, A. & Spiegel, Z."),
                      br(),
                      tags$li(tags$b("Data Cleaning:"), "Papavangjeli, S., MacDuff, A., Volpe, M., Spiegel, Z."),
                      br(),
                      tags$li(tags$b("Data Collection:"), "Al-Aubaidy, S., Anemone, A., Baskar, S., Cheng., M., Estreich, M., Fumarola, G., Giallella, J., Goldberg, M., Guevarra, C., Hei, Z., John, J., Kang, M., Katz, O., Khan, A., Lewinson, I., Menon, P., Perez, K., Rualo, G., Santiago, J., Santos, A., Sava, E., Schaefer, B., Soler, A., Wayland-Smith, A., Wayne, H., Yousef, L."),
                      br(),
                      tags$li(tags$b("Research Mentors:"), "Peters, M. & McCarty, S."),
                      br(),
                      tags$li(tags$b("Citation for Shiny App:"), "Silhavy, A., Spiegel, Z. & McCarty, S. Health & Social Status Explorer [Shiny web application]. Retrieved from https://zachspiegel.shinyapps.io/HealthStatusExplorer/"),
                    ),
                    div(img(src = "Logo_FRI.png", width = "35%", height = "35%"), style = "text-align: center;")
                    
                  )
        )
      ),
      
      nav_panel("Plots", 
                card(
                  h5("Health Ratings Across Income Groups"),
                  plotlyOutput("percentPoor") %>% withSpinner(color = "#9FC1B0"),
                  h5("Health Distribution of Minoritized vs Non-Minoritized Groups"),
                  plotOutput("violinPlot") %>% withSpinner(color = "#9FC1B0"),
                  h5("Association between Subjective Social Status (SSS) and Health Status"),
                  p("Click the labels in the legend to show vs. hide data points and lines"),
                  plotlyOutput("sssHS") %>% withSpinner(color = "#9FC1B0")
                )
                
      ),
      
      nav_panel("Table", 
                card(
                  div(style = "overflow-x: scroll", DT::dataTableOutput("table") %>%
                        withSpinner(color = "#9FC1B0"))
                )
      ),
      
    )
  )
)

# Server code
server <- function(input, output) {
  
  reactive_data = reactiveVal(data)
  
  observeEvent(input$update, {
    filtered_data = data %>%
      filter(SAMPLE_ID %in% input$sample,
             AGE >= input$age[1] & AGE <= input$age[2],
             SEX %in% input$sex,
             RACIALIZED_IDENTITY %in% input$race,
             MINORITIZED_STATUS %in% input$minoritize,
             PERCEIVED_INCOME %in% input$income,
             SSS >= input$sss[1] & SSS <= input$sss[2])
    
    
    reactive_data(filtered_data)
  })
  
  observeEvent(input$reset, {
    reactive_data(data)
  })
  
  output$table <- DT::renderDataTable({
    reactive_data() %>% select(-HEALTH_STATUS) %>% 
      rename("Sample" = SAMPLE_ID, "Age" = AGE, "Racialized Identity" = RACIALIZED_IDENTITY,
             "Minoritized" = MINORITIZED_STATUS, "Sex" = SEX,
             "Perceived Income" = PERCEIVED_INCOME, "Social Status" = SSS, "Health" = HEALTH_STATUS_LABEL)
  })
  
  output$percentPoor = renderPlotly({
    
    filtered_data <- reactive_data()
    
    pfHS = filtered_data %>%
      count(PERCEIVED_INCOME, HEALTH_STATUS_LABEL) %>%
      group_by(PERCEIVED_INCOME) %>%
      mutate(percent = n / sum(n) * 100)
    
    total_per_income = filtered_data %>%
      group_by(PERCEIVED_INCOME) %>%
      summarise(total_count = n())
    
    pfHS = pfHS %>%
      left_join(total_per_income, by = "PERCEIVED_INCOME") %>%
      mutate(percent_of_total = n / total_count * 100)
    
    ggplotly(
      ggplot(pfHS, aes(x = PERCEIVED_INCOME, y = percent_of_total, fill = HEALTH_STATUS_LABEL,
                       text = paste("<b>Health:</b>", HEALTH_STATUS_LABEL, 
                                    "<br><b>Perceived Income:</b>" , PERCEIVED_INCOME,   # bold and breaks
                                    "<br><b>Percent:</b>" , round(percent_of_total, 2), "%")
      )) +
        geom_col() +
        scale_fill_manual(values = c("Poor" = "#750e13", "Fair" = "#fa4d56", 
                                     "Good" = "#e5f6ff", "Very Good" = "#1192e8", "Excellent" = "#003a6d")) +
        labs(fill= "Health Status") + 
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        xlab("Perceived Social Class") +
        ylab('Percentage Reported') +
        theme_minimal(),
      tooltip = "text")
  })
  
  output$violinPlot =
    renderPlot({
      ggplot(data = reactive_data(), aes(x = MINORITIZED_STATUS, y = HEALTH_STATUS, fill = MINORITIZED_STATUS)) +
        geom_violin() +
        geom_boxplot(width = 0.15, outlier.shape = NA, coef = 0) +
        scale_fill_manual(values = c("Minoritized" = "#be95ff", "Non-minoritized" = "#08bdba")) +
        guides(fill = "none") +
        stat_summary(fun = mean, geom = "crossbar", color = "firebrick", size = 0.25) +
        xlab("Minoritized Status") +
        ylab("Health Status") +
        scale_y_continuous(
          breaks = c(1, 2, 3, 4, 5),  # Define breaks for each level
          labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")) +
        theme_minimal()
    })
  
  output$sssHS = renderPlotly({
    
    # Had to chat gpt this to get tooltip to work correctly
    # Apparently there are some quirks with ggplotly when it converts over from ggplot2
    # It works great but for more complicated code it doesn't work too well
    
    # Create a combined dataset for smooth lines
    data_combined <- reactive_data() %>%
      mutate(Group = "Overall") %>%
      bind_rows(
        reactive_data() %>% filter(SEX == "Male") %>% mutate(Group = "Male"),
        reactive_data() %>% filter(SEX == "Female") %>% mutate(Group = "Female"),
        reactive_data() %>% filter(MINORITIZED_STATUS == "Minoritized") %>% mutate(Group = "Minoritized"),
        reactive_data() %>% filter(MINORITIZED_STATUS == "Non-minoritized") %>% mutate(Group = "Non-minoritized")
      )
    
    # Ensure `Group` is a factor to maintain consistent coloring
    data_combined$Group <- factor(data_combined$Group, levels = c("Overall", "Male", "Female", "Minoritized", "Non-minoritized"))
    
    p <- ggplot(data = reactive_data(), aes(x = SSS, y = HEALTH_STATUS, 
                                            text = paste("<b>Health:</b>", HEALTH_STATUS_LABEL, 
                                                         "<br><b>Social Status:</b>", SSS,
                                                         "<br><b>Minoritized:</b>", MINORITIZED_STATUS, 
                                                         "<br><b>Sex:</b>", SEX))) +
      geom_jitter(aes(color = SEX, fill = MINORITIZED_STATUS), width = 0.4, height = 0.4, alpha = 0.65) +
      
      # Smoothed lines using the combined dataset
      geom_smooth(data = data_combined, aes(x = SSS, y = HEALTH_STATUS, color = Group, group = Group), 
                  method = "lm", se = FALSE, linewidth = 1.2) +
      
      # Define colors for all groups
      scale_color_manual(values = c(
        "Overall" = "gray26",
        "Male" = "royalblue4",
        "Female" = "red3",
        "Minoritized" = "#be95ff",
        "Non-minoritized" = "#08bdba"
      )) +
      scale_fill_manual(values = c("Minoritized" = "#be95ff", "Non-minoritized" = "#08bdba")) +
      
      labs(color = "Sex & Minoritized Status", fill = "") +
      xlab("Subjective Social Status") +
      xlim(0, 10) +
      ylab("Health Status") +
      scale_y_continuous(
        breaks = c(1, 2, 3, 4, 5),
        labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")) +
      scale_x_continuous(breaks = 1:10) +
      theme_minimal()
    a
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)