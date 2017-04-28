library(shiny)

shinyUI(fluidPage(

  titlePanel(div(h1("Migracje Polaków pomiędzy powiatami"),
             p("powiaty docelowe przy wyjeździe z wybranego powiatu")),
             windowTitle ="Migracje Polaków"),

  fluidRow(
       column(2,
              wellPanel(
             	p("Wybierz powiat"),
             	selectInput("wojewodztwo", "Województwo:", c()),
             	selectInput("powiat", "Powiat:", c()),
             	actionButton("action", "Aktualizuj wykresy")
              )
       ),
       column(5, plotOutput("mapaPlot")),
       column(5, plotOutput("slupkiPlot"))
    )
  )
)

