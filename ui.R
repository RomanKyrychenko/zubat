shinyUI(fluidPage(
  titlePanel("Зубко без реєстрації та смс"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Оберіть ексельку із Зубочком',
                accept = c(".xlsx")),
      tags$hr(),
      downloadButton('downloadPlot',"Дайте мені Зубка в pdf!"),
      downloadButton('download',"Дайте мені Зубка в png!")
    ),
    mainPanel(
      plotOutput('plot'),
      tableOutput('contents'))
  )
))