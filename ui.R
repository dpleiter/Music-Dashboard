shinyUI(
    fluidPage(
        title = "Music Dashboard",
        
        # CSS
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
        ),
        
        fluidRow(
            div(
                class = "title-panel-outside",
                div(
                    class = "title-panel-inside",
                    "Dylan Pleiter's Music Dashboard"
                )
            )
        ),
        fluidRow(
            column(
                3,
                id = "sidebar",
                HTML("<h4><b>Dashboard Filters</b></h4>"),
                h5("View"),
                selectInput(
                    "view",
                    NULL,
                    choices = c(
                        "Artist",
                        "First Letter",
                        "Year",
                        "Country"
                    ),
                    width = "100%"
                ),
                hr(),
                h5("First Letter"),
                selectInput(
                    "first_letter_filter",
                    NULL,
                    choices = c("(ALL)", str_sort(unique(album_listens$letter))),
                    width = "100%"
                ),
                h5("Year"),
                selectInput(
                    "year_filter",
                    NULL,
                    choices = c("(ALL)", str_sort(unique(album_listens$year))),
                    width = "100%"
                ),
                h5("Country"),
                selectInput(
                    "country_filter",
                    NULL,
                    choices = c("(ALL)", str_sort(unique(album_listens$country))),
                    width = "100%"
                )
            ),
            column(
                9,
                plotlyOutput("main_chart", height = "800px")
            )
        )
    ))
