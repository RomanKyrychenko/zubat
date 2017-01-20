shinyUI(fluidPage(
  titlePanel("ZUBAT без реєстрації та смс"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      tags$hr(),
      downloadButton('downloadPlot',"Завантажити мапу в pdf!"),
      downloadButton('download',"Завантажити мапу в png!")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Карта",plotOutput('plot', width = "100%", height = "750px")),
      tabPanel("Таблиця",tableOutput('contents'))
    )
      )
  )
))