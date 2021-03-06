# RUN:
# http://35.163.57.248/shiny/lemur/migracje/


library(shiny)

shinyUI(fluidPage(

   # kody Google Analytics
  tags$head(includeScript("google-analytics.js")),

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

