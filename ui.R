shinyUI(fluidPage(
  titlePanel("ZUBAT без реєстрації та смс"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      tags$hr(),
      radioButtons("typ","Оберіть тип карти",choices = c("Зубата!","Без пимпочек","Кастом")),
      
      colourInput("mincol", label = "Колір для мінімуму:", value = "#e7e7e8"),
      #textInput("mincol","Колір для мінімуму","#f0f0f0"),
      colourInput("maxcol", label = "Колір для максимуму:", value = "#adc5e6"),
      #textInput("maxcol","Колір для максимуму","#4d738a"),
      colourInput("nacol", label = "Колір для NA:", value = "white"),
      
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