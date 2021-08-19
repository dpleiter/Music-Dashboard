library(utf8)
library(magrittr)

shinyServer(function(input, output) {
    data_filtering <- reactive({
        df <- album_listens
        
        df %<>% mutate_if(is.character, utf8_encode)
        
        if (input$first_letter_filter != "(ALL)") {
            df %<>% filter(letter == input$first_letter_filter)
        }
        
        if (input$year_filter != "(ALL)") {
            df %<>% filter(year == input$year_filter)
        }
        
        if (input$country_filter != "(ALL)") {
            df %<>% filter(country == input$country_filter)
        }
        
        df <- df[order(df$year),]
        
        df
    })
    
    artist_chart <- reactive({
        df <- data_filtering()
        
        plot_ly(
            data = df,
            x = ~artist,
            y = ~ plays,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Artist: ", df$artist, "<br>Album: ", df$album, "<br>Year: ", df$year, "<br>Album Plays: ", df$plays, "<br> Last Played: ", df$last_played)
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(
                    title = "",
                    tickangle = -90
                ),
                yaxis = list(
                    title = "Total Album Plays",
                    showgrid = FALSE
                )
            )
    })
    
    letter_chart <- reactive({
        df <- data_filtering()
        
        plot_ly(
            data = df,
            x = ~letter,
            y = ~ plays,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Artist: ", df$artist, "<br>Album: ", df$album, "<br>Year: ", df$year, "<br>Album Plays: ", df$plays, "<br> Last Played: ", df$last_played)
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(
                    title = "",
                    tickangle = -90
                ),
                yaxis = list(
                    title = "Total Album Plays",
                    showgrid = FALSE
                )
            )
    })
    
    year_chart <- reactive({
        df <- data_filtering()
        
        plot_ly(
            data = df,
            x = ~year,
            y = ~ plays,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Artist: ", df$artist, "<br>Album: ", df$album, "<br>Year: ", df$year, "<br>Album Plays: ", df$plays, "<br> Last Played: ", df$last_played)
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(
                    title = "",
                    tickangle = -90
                ),
                yaxis = list(
                    title = "Total Album Plays",
                    showgrid = FALSE
                )
            )
    })
    
    country_chart <- reactive({
        df <- data_filtering()
        
        plot_ly(
            data = df,
            x = ~country,
            y = ~ plays,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Artist: ", df$artist, "<br>Album: ", df$album, "<br>Year: ", df$year, "<br>Album Plays: ", df$plays, "<br> Last Played: ", df$last_played)
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(
                    title = "",
                    tickangle = -90
                ),
                yaxis = list(
                    title = "Total Album Plays",
                    showgrid = FALSE
                )
            )
    })
    
    observe({
        if (input$view == "Artist") {
            output$main_chart <- renderPlotly(artist_chart())
        } else if (input$view == "First Letter") {
            output$main_chart <- renderPlotly(letter_chart())
        } else if (input$view == "Year") {
            output$main_chart <- renderPlotly(year_chart())
        } else if (input$view == "Country") {
            output$main_chart <- renderPlotly(country_chart())
        }
    })
})
