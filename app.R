library('shiny')
library('scales')
library('ggplot2')
source('data.R')

# Define UI
ui <- navbarPage('Barcelona City Data',
                 tabPanel('Demographics',
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(
                                "pop_type", 
                                h4("Show As"), 
                                choices = pop_list,
                                selected = 'all')
                            ),
                            mainPanel(
                              h3('Population by Year'),
                              plotOutput(outputId = "pop_year")
                            )
                          )),
                 tabPanel('Unemployment',
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(
                                "unempl_years", 
                                h4("Years Included"),
                                choices = year_list,
                                selected = years)
                            ),
                            mainPanel(
                              h3('Total Unemployment by Year'),
                              plotOutput(outputId = "unempl_year"),
                              h3('Average Unemployment by Month'),
                              plotOutput(outputId = "unempl_month"),
                              h3('Total Unemployment by District'),
                              plotOutput(outputId = "unempl_district")
                            )
                          )),
                 tabPanel('Accidents',
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(
                                "acc_districts", 
                                h4("Districts Included"),
                                choices = district_list,
                                selected = districts)
                            ),
                            mainPanel(
                              h3('Total Accidents by Month (2017)'),
                              plotOutput(outputId = "acc_month"),
                              h3('Average Accidents by Day of Week (2017)'),
                              plotOutput(outputId = "acc_week"),
                              h3('Average Accidents by Day of Month (2017)'),
                              plotOutput(outputId = "acc_day"),
                              h3('Average Accidents by Hour of Day (2017)'),
                              plotOutput(outputId = "acc_time")
                            )
                          ))
)

# Define server logic
server <- function(input, output) {
  
  # Population
  output$pop_year <- renderPlot({
    if (input$pop_type == 'all') {
      ggplot(data=population, aes(x=year, y=number)) +
        geom_bar(stat="identity") + xlab('Year') + ylab('Population') +
        scale_y_continuous(label=comma)
    } else {
      ggplot(data=population, aes_string(x='year', y='number', fill=input$pop_type)) +
        geom_bar(stat="identity") + xlab('Year') + ylab('Population') +
        scale_y_continuous(label=comma)
    }
  })
  
  # Unemployment
  output$unempl_year <- renderPlot({
    ggplot(data=unempl_by_year[unempl_by_year$year %in% input$unempl_years,], aes(x=year, y=number)) + 
      geom_bar(stat="identity") + xlab('Year') + ylab('Unemployment Claims') +
      scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 1500000))
  })  
  output$unempl_month <- renderPlot({
      ggplot(data=unempl_by_month[unempl_by_month$year %in% input$unempl_years,], aes(x=month)) + 
        xlab('Month') + ylab('Unemployment Claims') +
        stat_summary_bin(aes(y = number), fun.y = "mean", geom = "bar") + 
        scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 125000))
  })  
  output$unempl_district <- renderPlot({
    ggplot(data=unempl_by_district[unempl_by_district$year %in% input$unempl_years,], aes(x=reorder(district.name, -number))) + 
      stat_summary_bin(aes(y = number), fun.y = "sum", geom = "bar") + xlab('District') + ylab('Unemployment Claims') +
      scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 1250000))
  })
  
  # Accidents
  output$acc_month <- renderPlot({
    ggplot(data=accidents_2017[accidents_2017$district.name %in% input$acc_districts,], aes(x=month)) + 
      geom_bar(stat="count") + xlab('Month') + ylab('Number of Accidents') +
      scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 1000))
  })  
  output$acc_week <- renderPlot({
    ggplot(data=acc_by_week[acc_by_week$district.name %in% input$acc_districts,], aes(x=weekday)) + 
      stat_summary_bin(aes(y = n), fun.y =sum, geom="bar") + xlab('Day of Week') + ylab('Number of Accidents') +
      scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 50))
  })
  output$acc_day <- renderPlot({
    ggplot(data=acc_by_day[acc_by_day$district.name %in% input$acc_districts,], aes(x=day)) + 
      stat_summary(aes(y = n, group=1), fun.y=mean, geom="line", group=1) +
      xlab('Day') + ylab('Number of Accidents') +
      scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 50))
  })  
  output$acc_time <- renderPlot({
    ggplot(data=acc_by_time[acc_by_time$district.name %in% input$acc_districts,], aes(x=hour)) + 
      stat_summary(aes(y = n, group=1), fun.y=mean, geom="line", group=1) +
      xlab('Hour') + ylab('Number of Accidents') +
      scale_y_continuous(label=comma) + coord_cartesian(ylim=c(0, 5))
  })  
}

# Run the app ----
shinyApp(ui = ui, server = server)