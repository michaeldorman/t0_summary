# Hamaarag bird species data viewer app 
# Version 0.1

library(shiny)
library(RSQLite)
library(leaflet)
library(magrittr)
library(ggplot2)
library(plyr)
library(scales)
library(DT)

# Load data
dat_all = read.csv("t0_summary.csv", stringsAsFactors = FALSE)

cols = data.frame(
  unit_heb = c(
    "בתות עשבוניות ושיחניות",
    "חורש ים-תיכוני",
    "יער מחטני נטוע",
    "חולות מישור החוף",
    "אזורי לס בצפון הנגב",
    "אזור הספר הים תיכוני", 
    "הר הנגב",
    "דרום צחיח"
  ), 
  col = c(
    "#1B9E77", 
    "#D95F02", 
    "#7570B3", 
    "#8DD3C7", 
    "#E7298A", 
    "#66A61E", 
    "#E6AB02", 
    "#A6761D")
  )

dat_all = join(dat_all, cols, "unit_heb")

ui = fluidPage(
  titlePanel("המארג - סיכום נתוני סבב ניטור ראשון"),
  p("בחרו קבוצה טקסונומית ← התמקדו על אזור במפה ← הטבלה מציגה סיכום התצפיות ← ניתן להוריד את הטבלה בעזרת כפתור ההורדה"),
  # p("ניתן לסנן את הטבלה בעזרת חלון החיפוש; שימו לב - הסינון אינו משפיע על הקובץ שמורידים"),
  fluidRow(
    column(7, 
      leafletOutput("mymap", height = 300)
      ),
    column(5, 
      selectInput(
        "group", 
        "קבוצה", 
        choices = unique(dat_all$group_heb), 
        selected = "זוחלים"
        ),
      downloadButton(
        "downloadData", 
        "הורדה"
        ),
      plotOutput("sampling_events", height = 300)
      )
  ),
  tags$head(tags$style("#dat_extent_table  {white-space: nowrap;  }")),
  DT::dataTableOutput("dat_extent_table", height = 300)
)

# SERVER
server = function(input, output, session) {
  
  dat_group = reactive({
    dat_all[
      dat_all$group_heb == input$group, 
      ]
  })
  
  dat_extent = reactive({
    dat_group()[
      dat_group()$lat <= input$mymap_bounds$north &
        dat_group()$lat >= input$mymap_bounds$south &
        dat_group()$lon <= input$mymap_bounds$east &
        dat_group()$lon >= input$mymap_bounds$west,
      ]
  })
  
  output$mymap = renderLeaflet({
    leaflet(dat_group()) %>% 
      addProviderTiles("Stamen.Toner") %>%
      addCircleMarkers(
        lng = ~ lon, 
        lat = ~ lat, 
        color = ~ col, 
        popup = ~ paste(unit_heb, site_heb, sep = " - "),
        group = "all"
        )
  })
  
  output$sampling_events = renderPlot({
    
    if(nrow(dat_extent()) > 0) {
    
    plot_data =
      dat_extent() %>% 
      ddply(
        c("unit_heb", "col"),
        summarize,
        events = length(unique(paste(year, lon, lat)))
      )
    
    plot_data$unit_heb %<>% 
      factor(
        levels = cols$unit_heb
      )
    
    ggplot() +
      geom_bar(data = plot_data,
        aes(x = unit_heb, y = events, fill = col), 
        stat = "identity",
        colour = "black", size = 0.25) +
      geom_text(
        data = plot_data,
        aes(x = unit_heb, label = events, y = events), 
        position ="stack", size = 3, vjust = -0.5
      ) +
      scale_x_discrete(breaks = levels(plot_data$unit_heb), drop = FALSE) +
      scale_y_continuous(
        name = 
          "מספר ארועי דיגום"
      ) +
      scale_fill_identity() +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
        axis.text.x = element_text(
          angle = 45, 
          hjust = 1, 
          vjust = 1
        )
      )
    
    }
    
  })
  
  output$dat_extent_table = DT::renderDataTable({
    
    tab = dat_extent()
    tab$col = NULL
    
    DT::datatable(tab, rownames = FALSE, style = "bootstrap", options = list(paging = FALSE))
  })
  
  output$downloadData = downloadHandler(
    
    filename = function() {
      't0_summary.csv'
    },
    
    content = function(file) {
      tab = dat_extent()
      tab$col = NULL
      write.csv(tab, file, row.names = FALSE, fileEncoding = "iso8859-8")
    }
  )
    
}

shinyApp(server = server, ui = ui)