# Libraries ----------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(tidyverse)
library(validate)
library(hrbrthemes)
library(waiter)
library(DT)

# Global -------------------------------------------------------------------------

source(file = "global.R")

# ui -----------------------------------------------------------------------------

ui <- dashboardPage(
        skin = "blue",

        # Header -----------------------------------------------------------------
        
        dashboardHeader(title = "Nobel Prize Winners", titleWidth = 275),
        
        # Sidebar ----------------------------------------------------------------
        
        dashboardSidebar(
                width = 275,
                sidebarMenu(
                        menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
                        menuItem("Overview", tabName = "overview", icon = icon("trophy")),
                        menuItem("Motivation and Paper Title Analysis", tabName = "motivationAndPaperTitleAnalysis", icon = icon("file-word")),
                        menuItem("Publication and Age Analysis", tabName = "pathToSuccess", icon = icon("bar-chart")),
                        menuItem("Additional Resources", icon = icon("book"),
                                menuSubItem("Sources", tabName = "sources"),
                                menuSubItem("Supplementary Materials", tabName = "supplementaryMaterials"))
                )
        ),

        # Body -------------------------------------------------------------------
        
        dashboardBody(
                tabItems(
                        # Introduction Body --------------------------------------
                        tabItem("introduction",
                                fluidRow(
                                        box(
                                                title = "Purpose", status = "primary", solidHeader = TRUE, width = 12,
                                                p("The purpose of this application is to provide insights to the Swedish and Norwegian institutions 
                                                responsible for awarding the Nobel Prize. In this application, analysis of past Nobel Prize winners is provided
                                                to help those institutions make informed decisions regarding future recipients of the Nobel Prize."),
                                            )
                                ),
                                fluidRow(
                                        box(
                                                title = "What You'll Find", status = "primary", solidHeader = TRUE, width = 12,
                                                h4(strong("Overview")),
                                                p(strong(em("Winners")),
                                                        tags$ul(
                                                                tags$li("Number of Nobel Prizes awarded as well as the number of all recipients and female recipients"),
                                                                tags$li("List of Nobel Prize winners")
                                                        ),
                                                ),
                                                p(strong(em("Journals and Organizations")),
                                                        tags$ul(
                                                                tags$li("Top journals and organizations"),
                                                                tags$li("List of all journals and organizations")
                                                        )
                                                ),
                                                h4(strong("Motivation and Paper Title Analysis")),
                                                p(strong(em("Motivation for Award")),
                                                        tags$ul(
                                                                tags$li("Most frequent words found in the stated motivation for a Nobel Prize to be awarded")
                                                        )
                                                ),
                                                p(strong(em("Paper Titles")),
                                                        tags$ul(
                                                                tags$li("Most frequent words found among the paper titles of publications by Nobel Prize winners")
                                                        )
                                                ),
                                                h4(strong("Publication and Age Analysis")),
                                                p(strong(em("Publication Analysis")),
                                                        tags$ul(
                                                                tags$li("Analysis of number of publications by category"),
                                                                tags$li("Average academic experience prior to winning the Nobel Prize")
                                                        )
                                                ),
                                                p(strong(em("Age Analysis")),
                                                        tags$ul(
                                                                tags$li("Average age of Nobel Prize winners by country"),
                                                                tags$li("Number of Nobel Prize winners by country")
                                                        )
                                                ),
                                                h4(strong("Additional Resources")),
                                                p("This section provides a list of resources referenced as well as links to supplimentary materials
                                                  that are related to this application."),
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Authors", status = "primary", solidHeader = TRUE, width = 12,
                                                p(
                                                        tags$ul(
                                                                tags$li("Dipin Kasana"),
                                                                tags$li("Kelly O'Shields"),
                                                                tags$li("Joshua Peterson")
                                                        )
                                                )
                                        )
                                )
                                
                        ),
                        # Overview Body -----------------------------------------------------------------
                        tabItem("overview",
                                fluidRow(
                                        tabsetPanel(
                                                tabPanel(strong("Winners"),
                                                         column(width = 3,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_winner", "Select Prize Category:", choices = c("All", cat_selection)),
                                                                        sliderInput("year_winner", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE))
                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section provides an overview of past Nobel Prize winners. The above filters can be
                                                                          used to filter by prize category and year to update the outputs on the right.")
                                                                )
                                                         ),
                                                         column(width = 9,
                                                                valueBoxOutput("total_awards"),
                                                                valueBoxOutput("total_winners"),
                                                                valueBoxOutput("total_female"),
                                                                box(
                                                                        title = "List of Nobel Prize Winners", status = "primary", solidHeader = TRUE, width = 12,
                                                                        DTOutput("winner_table")
                                                                )
                                                         )
                                                ),
                                                tabPanel(strong("Journals and Organizations"),
                                                         column(width = 3,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_journal_org", "Select Prize Category for Journals:", choices = c("All", cat_selection_lower)),
                                                                        selectInput("category_journal_org2", "Select Prize Category for Organizations:", choices = c("All", cat_selection)),
                                                                        sliderInput("year_journal_org", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE))
                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section provides an overview of journals and organizations that have
                                                                        produced Nobel Prize winners in the past. The above filters can be
                                                                          used to filter by prize category and year to update the outputs on the right.")
                                                                )
                                                         ),
                                                         column(width = 9,
                                                                tabBox(
                                                                        title = "", id = "tabset_motivation", width = 12, height = "500px",
                                                                        tabPanel("Top Journals", 
                                                                                 plotOutput("plot_journals"), 
                                                                                 height = "600px"),
                                                                        tabPanel("Top Organizations",
                                                                                 plotOutput("plot_organizations"),
                                                                                 height = "600px")
                                                                ),
                                                                tabBox(
                                                                        title = "", id = "tabset_motivation", width = 12, height = "500px",
                                                                        tabPanel("List of Journals", DTOutput("table_journals"), height = "500px"),
                                                                        tabPanel("List of Organizations", DTOutput("table_organizations"), height = "500px")
                                                                )
                                                         )
                                                )
                                        )
                                )
                        ),
                        # Paper and Motivation Analysis Body --------------------------------------------
                        tabItem("motivationAndPaperTitleAnalysis",
                                fluidRow(
                                        tabsetPanel(
                                                tabPanel(strong("Motivation for Award"),
                                                        column(width = 3,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_motivation", "Select Prize Category:", choices = c("All", cat_selection)), 
                                                                        sliderInput("year_motivation", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                        hr(),
                                                                        sliderInput(inputId = "maxwords_motivation", label = "Max Number of Words for Word Cloud:", min = 5, max = 50, value = 25)
                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section shows which words can be most commonly found within the 
                                                                          stated motivation for an award recipient recieving an award. Additionally, 
                                                                           a count of the most common words can be found on the Word Counts tab as well."),
                                                                )
                                                         ),
                                                         column(width = 9,
                                                                tabBox(
                                                                        title = strong("Motivation Analysis"), id = "tabset_motivation", width = 12,
                                                                        tabPanel("Word Cloud", plotOutput("motivation_wordcloud"), height = "500px", width = "500px"),
                                                                        tabPanel("Word Counts", plotOutput("motivation_freq"), height = "500px", width = "500px")
                                                                )
                                                         )
                                                ),
                                                tabPanel(strong("Paper Titles"),
                                                         column(width = 3,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_paper", "Select Prize Category:", choices = c("All", cat_selection_lower)), 
                                                                        selectInput("category_paper_winner", "Prize Winning Paper:", choices = prize_selection), 
                                                                        sliderInput("year_paper", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                        hr(),
                                                                        sliderInput(inputId = "maxwords_title", label = "Max Number of Words for Word Cloud:", min = 5, max = 50, value = 25)                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section shows which words can be most commonly found within the 
                                                                          titles of publications produced by Nobel Prize winners. Additionally, 
                                                                          a count of the most common words can be found on the Word Counts tab as well.")
                                                                )
                                                         ),
                                                         column(width = 9,
                                                                tabBox(
                                                                        title = strong("Paper Title Analysis"), id = "tabset_paper", width = 12,
                                                                        tabPanel("Word Cloud", plotOutput("title_wordcloud"), height = "500px", width = "500px"),
                                                                        tabPanel("Word Counts", plotOutput("title_freq"), height = "500px", width = "500px")
                                                                )
                                                         )
                                                )
                                        )
                                )
                        ),
                        # Analysis of Winners Body -----------------------------------
                        tabItem("pathToSuccess",
                                fluidRow(
                                        tabsetPanel(
                                                tabPanel(strong("Publication Analysis"),
                                                        column(width = 3,
                                                                 box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        sliderInput("prize_year_1", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                 ),
                                                                 box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("The top plot provides the distribution of the number of publications for Nobel Prize winners among three areas of study.
                                                                          You can select the points in this plot to find out the number of publications
                                                                          represented by each point. The output for your selection can be found on the Selection tab."),
                                                                        p("The bottom plot provides the average academic experience of the winners prior to receiving the Nobel Prize."),
                                                                 )
                                                         ),
                                                        column(width = 9,
                                                                 tabBox(
                                                                        title = strong("Publication Analysis"), id = "tabset_violin", width = 12, 
                                                                        tabPanel("Plot", plotOutput("exp1", brush = "violinBrush")),
                                                                        tabPanel("Selection", DTOutput("violinTable"))
                                                                 ),
                                                                 box(
                                                                        title = "Average Academic Experience Prior to Winning the Nobel Prize", status = "primary", solidHeader = TRUE, width = 12, 
                                                                        plotOutput("exp2")
                                                                 )
                                                        )
                                                ),
                                                tabPanel(strong("Age Analysis"),
                                                        column(width = 3,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_age", "Select Prize Category:", choices = c("All", cat_selection)), 
                                                                        hr(),
                                                                        sliderInput("prize_year_2", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                 ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This plot illustrates the average age of Nobel Prize recipients, according to country, since 1901.
                                                                          The number of individuals included in the average can be found in the table below."),
                                                                 )
                                                        ),
                                                        column(width = 9,
                                                                 box(
                                                                        title = "Average Age of the Nobel Prize Winners by Country", status = "primary", solidHeader = TRUE, width = 12,
                                                                        plotOutput("exp3")
                                                                 ),
                                                                 box(
                                                                        title = "Frequency of Nobel Prize Winners by Country", status = "primary", solidHeader = TRUE, width = 12,
                                                                        DT::dataTableOutput("dt_table")
                                                                 )
                                                        )
                                                )
                                        )
                                )
                        ),
                        # References Body ----------------------------------------
                        tabItem("sources",
                                box(width = 12,
                                        h3(strong(p("Sources"))),
                                        h4("Data Sources:"),
                                        h5(strong("Nobel Winners Dataset")),
                                        p("Kaggle, 2017: ", a(href = "https://www.kaggle.com/nobelfoundation/nobel-laureates#archive.csv", "https://www.kaggle.com/nobelfoundation/nobel-laureates#archive.csv")),
                                        h5(strong("Nobel Winners - All Publications Dataset")),
                                        p("Li, Jichao; Yin, Yian; Fortunato, Santo; Wang Dashun, 2018, A dataset of publication records for Nobel laureates,
                                        https://doi.org/10.7910/DVN/6NJ5RN, Harvard Dataverse, V1, UNF:6:/Mr84aTKPhJytkmsz1tgZQ== [fileUNF]"),
                                        p("Harvard Dataverse, 2018: ", a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6NJ5RN", "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6NJ5RN")),
                                        h4("Information Source:"),
                                        h5(strong("TidyTuesday")),
                                        p("TidyTuesday, 2019: ", a(href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-14", "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-14"))
                                )
                        ),
                        # Supplementary Materials Body ---------------------------
                        tabItem("supplementaryMaterials",
                                box(width = 12,
                                        h3(strong(p("Supplimentary Materials"))),
                                        h4("The Code for this Application is Available at GitHub:"), 
                                        a(href = "https://github.com/joshapeterson/dsba5122-class-project", "https://github.com/joshapeterson/dsba5122-class-project")
                                )
                        )
                        
                ),
                use_waiter(include_js = FALSE),
                show_waiter_on_load(html = tagList(
                        spin_circle(),
                        span("Loading...", style = "color:white")
                ))
        )
)

# server -------------------------------------------------------------------------

server <- function(input, output, session) {
        
        # Waiter -----------------------------------------------------------------
        
        Sys.sleep(3)
        hide_waiter()
        
        # Winner Reactive --------------------------------------------------------
        
        output$total_awards <- renderValueBox({
                if (input$category_winner != "All"){
                        sum_of_prize <- nobel_winners %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize) %>%
                                count(prize)      
                } else {
                        sum_of_prize <- nobel_winners %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize) %>%
                                count(prize)                  
                }

                
                valueBox(
                        paste0(sum(sum_of_prize$n)),
                        "Nobel Prize Awards Given",
                        icon = icon("trophy"),
                        color = "blue"
                )
        })
        
        output$total_winners <- renderValueBox({
                if (input$category_winner != "All"){
                        sum_of_prize <- nobel_winners %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize)
                } else {
                        sum_of_prize <- nobel_winners %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize)
                }
                
                
                valueBox(
                        paste0(sum(sum_of_prize$n)),
                        "Award Recipients",
                        icon = icon("user"),
                        color = "maroon"
                )
        })
        
        output$total_female <- renderValueBox({
                if (input$category_winner != "All"){
                        sum_of_prize <- nobel_winners %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                filter(gender == "Female") %>%
                                count(prize)
                } else {
                        sum_of_prize <- nobel_winners %>%
                                filter(gender == "Female") %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize)
                }
                
                
                valueBox(
                        paste0(sum(sum_of_prize$n)),
                        "Female Award Recipients",
                        icon = icon("venus"),
                        color = "light-blue"
                )
        })
        
        output$winner_table <- renderDT(
                if (input$category_winner != "All"){
                        winner_data <- nobel_winners %>%
                                count(laureate_id)
                        
                        winner_data <- inner_join(nobel_winners, winner_data, by = "laureate_id") %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                arrange(desc(laureate_id)) %>%
                                arrange(desc(n)) %>%
                                select("prize", "motivation", "full_name", "gender", "organization_name", "n")
                        
                        winner_data2 <- winner_data %>%
                                select("full_name", everything())
                        
                        return(winner_data2)
                        
                } else {
                        winner_data <- nobel_winners %>%
                                count(laureate_id)
                        
                        winner_data <- inner_join(nobel_winners, winner_data, by = "laureate_id") %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                arrange(desc(laureate_id)) %>%
                                arrange(desc(n)) %>%
                                select("prize", "motivation", "full_name", "gender", "organization_name", "n")
                        
                        winner_data2 <- winner_data %>%
                                select("full_name", everything())
                        
                        return(winner_data2)
                },
                options = list(
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15)
                )
        )
        
        output$table_journals <- renderDT(
                if (input$category_journal_org != "All"){
                        journal_data <- nobel_winner_all_pubs %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(category == input$category_journal_org) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        return(journal_data)
                        
                } else {
                        journal_data <- nobel_winner_all_pubs %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        return(journal_data)
                },
                options = list(
                        pageLength = 10,
                        lengthMenu = c(5, 10, 15)
                )
        )
        
        output$table_organizations <- renderDT(
                if (input$category_journal_org2 != "All"){
                        organization_data <- nobel_winners %>%
                                filter(!is.na(organization_name)) %>% 
                                filter(category == input$category_journal_org2) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        organization_data <- inner_join(nobel_winners, organization_data, by = "organization_name") %>%
                                select("organization_name", "organization_city", "organization_country", "n") %>%
                                arrange(desc(n))
                        
                        return(organization_data)
                        
                } else {
                        organization_data <- nobel_winners %>%
                                filter(!is.na(organization_name)) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        organization_data <- inner_join(nobel_winners, organization_data, by = "organization_name") %>%
                                select("organization_name", "organization_city", "organization_country", "n") %>%
                                arrange(desc(n))
                        
                        return(organization_data)
                },
                options = list(
                        pageLength = 10,
                        lengthMenu = c(5, 10, 15)
                )
        )
        
        output$plot_journals <- renderPlot({
                if (input$category_journal_org != "All"){
                        text <- nobel_winner_all_pubs %>%
                                filter(category == input$category_journal_org) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(!is.na(journal)) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        validate(
                                need(text$n != "", "No Data Available")
                        )
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(journal, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Winners by Journal",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white") 
                } else {
                        text <- nobel_winner_all_pubs %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(!is.na(journal)) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        validate(
                                need(text$n != "", "No Data Available")
                        )
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(journal, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Winners by Journal",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white") 
                }
        })
        
        output$plot_organizations <- renderPlot({
                if (input$category_journal_org2 != "All"){
                        text <- nobel_winners %>%
                                filter(category == input$category_journal_org2) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(!is.na(organization_name)) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        validate(
                                need(text$n != "", "No Data Available")
                        )
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(organization_name, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Winners by Organization",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white") 
                } else {
                        text <- nobel_winners %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(!is.na(organization_name)) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        validate(
                                need(text$n != "", "No Data Available")
                        )
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(organization_name, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Winners by Organization",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white")
                }
        })

        # Motivation Reactive ----------------------------------------------------
        
        motivation_freq <- reactive(
                withProgress({
                        setProgress(message = "Processing text...")
                        getFreq_motivation(motivation_text, input$category_motivation, input$year_motivation[1], input$year_motivation[2])
                })
        )
        
        output$motivation_wordcloud <- renderPlot({
                validate(
                        need(motivation_freq()$n != "", "No Data Available")
                )
                
                text <- motivation_freq()
                
                text %>%
                        with(
                                wordcloud(
                                        words = word,
                                        freq = n,
                                        random.order = FALSE,
                                        max.words = input$maxwords_motivation,
                                        scale = c(3.5, 0.75),
                                        colors = brewer.pal(8, "Set2")
                                )
                        )
        })
        
        output$motivation_freq <- renderPlot({
                validate(
                        need(motivation_freq()$n != "", "No Data Available")
                )
                
                text <- motivation_freq()
                
                text %>%
                        top_n(5) %>%
                        ggplot(aes(reorder(word, n), n)) +
                        geom_col(fill = "#01579b") +
                        coord_flip() +
                        labs(
                                title = "Top Words in Motivation for the Award",
                                subtitle = "These are the most common words among the described motivations for awarding the Nobel Prize",
                                x = "",
                                y = "Count of Word"
                        ) +
                        theme_minimal() +
                        theme(text = element_text(size = 14)) +
                        geom_text(aes(label = n), hjust = 2, color = "white")
        })
        
        # Paper Title Reactive --------------------------------------------------------
        
        title_freq <- reactive(
                withProgress({
                        setProgress(message = "Processing text...")
                        getFreq_paper_titles(paper_title_text, input$category_paper, input$year_paper[1], input$year_paper[2], input$category_paper_winner)
                })
        )
        
        output$title_wordcloud <- renderPlot({
                validate(
                        need(title_freq()$n != "", "No Data Available")
                )
                
                text <- title_freq()
                
                text %>%
                        with(
                                wordcloud(
                                        words = word,
                                        freq = n,
                                        random.order = FALSE,
                                        max.words = input$maxwords_title,
                                        scale = c(3.5, 0.75),
                                        colors = brewer.pal(8, "Set2")
                                )
                        )
        })
        
        output$title_freq <- renderPlot({
                validate(
                        need(title_freq()$n != "", "No Data Available")
                )
                
                text <- title_freq()
                
                text %>%
                        top_n(5) %>%
                        ggplot(aes(reorder(word, n), n)) +
                        geom_col(fill = "#01579b") +
                        coord_flip() +
                        labs(
                                title = "Top Words among Paper Titles",
                                subtitle = "These are the most common words among titles of published papers",
                                x = "",
                                y = "Count of Word"
                        ) +
                        theme_minimal() +
                        theme(text = element_text(size = 14)) +
                        geom_text(aes(label = n), hjust = 2, color = "white")
        })
        
        # Analysis of Winners Reactive ------------------------------------------------
        
        data1 <- reactive(
                if (input$category_age != "All"){
                        by_nobel %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(!is.na(prize_year)) %>% 
                                filter(!is.na(birth_date)) %>% 
                                filter(category == input$category_age) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                mutate(years_exp = prize_year - year_of_birth) %>% 
                                group_by(organization_country) %>% 
                                summarize(avg_exp = mean(years_exp))     
                } else {
                        by_nobel %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(!is.na(prize_year)) %>% 
                                filter(!is.na(birth_date)) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                mutate(years_exp = prize_year - year_of_birth) %>% 
                                group_by(organization_country) %>% 
                                summarize(avg_exp = mean(years_exp)) 
                }

        )
        
        output$exp3 <- renderPlot({
                validate(
                        need(data1()$avg_exp != "", "No Data Available")
                )
                
                ggplot(data1()) + 
                        geom_col(aes(reorder(organization_country, avg_exp), avg_exp), width = 0.5, fill="#01579b") + 
                        theme_minimal() + 
                        labs(y= "Average Age of the Winner", x = "", title = "") + 
                        coord_flip() +
                        theme(text = element_text(size = 14)) +
                        theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
        })
        
        output$dt_table <- DT::renderDataTable(DT::datatable({
                if (input$category_age != "All"){
                        dt <- nobel_winners %>% 
                                select(category, birth_country, death_country, organization_country, prize_year) %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(category == input$category_age) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                count(organization_country) %>%
                                arrange(desc(n))
                } else {
                        dt <- nobel_winners %>% 
                                select(category, birth_country, death_country, organization_country, prize_year) %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                count(organization_country) %>%
                                arrange(desc(n))   
                }
        })
        )
        data2 <- reactive(
                grouped_nobel_5 %>% 
                        filter(!is.na(category)) %>% 
                        filter(!is.na(academic_experience)) %>% 
                        filter(is_prize_winning_paper == "YES") %>%
                        filter(prize_year >= input$prize_year_1[1] & prize_year <= input$prize_year_1[2]) %>%
                        group_by(category) %>% 
                        summarize(avg_aca_exp = mean(academic_experience))
        )
        
        output$exp2 <- renderPlot({
                ggplot(data2()) + 
                        geom_col(aes(reorder(category, avg_aca_exp), avg_aca_exp), width = 0.5, fill="#01579b") + 
                        theme_minimal() + 
                        labs(y= "Years of Experience", x = "", title = "") + 
                        coord_flip() + 
                        scale_x_discrete(labels=c("chemistry" = "Chemistry", "medicine" = "Medicine", "physics" = "Physics")) + 
                        scale_y_continuous(limits=c(0, 50)) +
                        theme(text = element_text(size = 14)) + 
                        theme(axis.line = element_line(colour = "black", size = 0.3, linetype = "solid"))
        })
        
        data3 <- reactive(
                by_nobel_5 %>% 
                        select(laureate_id, category, is_prize_winning_paper, prize_year) %>%
                        filter(prize_year >= input$prize_year_1[1] & prize_year <= input$prize_year_1[2]) %>%
                        group_by(laureate_id, category) %>% 
                        summarise(freq = n())
        )
        
        output$exp1<- renderPlot({
                ggplot(data3()) + 
                        geom_violin(aes(category, freq), fill="#01579b", alpha = 0.8) + 
                        geom_jitter(aes(category, freq), width = 0.1, height = 0.1, alpha = 0.4) + 
                        theme_minimal() + 
                        labs(title = "Number of Nobel Prize Winner Publications by Category", x="" , y="Number of Publications") + 
                        scale_x_discrete(labels=c("chemistry" = "Chemistry", "medicine" = "Medicine", "physics" = "Physics")) +
                        theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid")) + scale_y_continuous(limits=c(0, 2000)) + 
                        theme(text = element_text(size = 14))
        })
        
        output$violinTable <- renderDT({
                output <- brushedPoints(data3(), input$violinBrush) %>%
                        inner_join(nobel_winner_all_pubs, data3(), by = "laureate_id") %>%
                        select("laureate_name", "affiliation") %>%
                        count(laureate_name)
        })
}

# app build ----------------------------------------------------------------------

shinyApp(ui, server)
