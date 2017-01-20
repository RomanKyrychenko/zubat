shinyUI(fluidPage(
  titlePanel("ZUBAT без реєстрації та смс"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Оберіть ексельку із Зубочком',
                accept = c(".xlsx")),
      tags$hr(),
      downloadButton('downloadPlot',"Дайте мені Зубка в pdf!"),
      downloadButton('download',"Дайте мені Зубка в png!")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Карта",plotOutput('plot', width = "100%", height = "750px")),
      tabPanel("Таблиця",tableOutput('contents'))
    )
      )
  )
))