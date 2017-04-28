library(tidyverse)

powiaty_nazwy <- readRDS("powiaty_nazwy.RDS")
powiaty_migracje <- readRDS("powiaty_migracje.RDS")
powiaty_mapa <- readRDS("powiaty_mapa.RDS")


migracje_z_powiatu <- function(pow_teryt) {
   return( filter(powiaty_migracje, Z==pow_teryt) %>% select(-Z) %>% unique() )
}


migracje_mapa <- function(migracje_df, powiat_teryt) {
   migracje_df <- left_join(powiaty_mapa, migracje_df, by=c("id"="DO"))
   plot <- ggplot() +
      geom_polygon(data = migracje_df,
                   aes(long, lat, group=group, fill=log10(Liczba)),
                   color="gray", show.legend = FALSE) +
      geom_path(data = filter(migracje_df, id==powiat_teryt),
                aes(long, lat, group=group),
                color="blue", size=1) +
      scale_fill_gradient(low="#eeee00", high="#ee0000", na.value = "#eeffee") +
      coord_equal() +
      theme_void()

   return(plot)
}


migracje_slupki <- function(migracje_df) {
   plot <- migracje_df %>%
      mutate(procent = round(100*Liczba/sum(Liczba), 1)) %>%
      top_n(10, wt = Liczba) %>%
      arrange(Liczba) %>%
      left_join(powiaty_nazwy, by=c("DO"="TERYT")) %>%
      mutate(NAZWA_POW = factor(NAZWA_POW, levels=NAZWA_POW))%>%
      ggplot() +
      geom_bar(aes(NAZWA_POW, Liczba, fill=NAZWA_WOJ), stat="identity") +
      geom_text(aes(NAZWA_POW, Liczba, label=paste0(procent, "%"),
                    hjust=ifelse(procent<5, -0.2, 1.1))) +
      coord_flip() +
      theme_minimal() +
      labs(x="", y="Liczba osób", fill="Województwo")

   return(plot)
}


powiat_teryt <- function(woj, pow) {
   teryt <- powiaty_nazwy %>%
      filter(NAZWA_WOJ == woj, NAZWA_POW == pow) %>%
      .[1,3] %>%
      as.character()

   return(teryt)
}


powiaty_woj <- function(woj) {
   nazwy <- powiaty_nazwy %>% filter(NAZWA_WOJ == woj) %>%
      select(NAZWA_POW) %>%
      unique() %>%
      arrange(NAZWA_POW)

   return(nazwy$NAZWA_POW)
}

shinyServer(function(input, output, session) {

   fun_init <- reactive({
      wojewodztwa_lista <- unique(powiaty_nazwy$NAZWA_WOJ)
      updateSelectInput(session, "wojewodztwo", choices = wojewodztwa_lista)
   })

   fun_wojewodztwo <- reactive({
      # zmienilo sie wojewodztwo
      powiaty_lista <- powiaty_woj(input$wojewodztwo)
      updateSelectInput(session, "powiat", choices = powiaty_lista)
   })

#    fun_powiat <- reactive({
#       # zmienil sie powiaty
#       powiat <- input$powiat
#
#       # znajdz TERYT powiatu
#       powiat_t <- powiat_teryt(wojewodztwo, powiat)
#
#       # przygotuj dane z powiatu
#       migracje <- migracje_z_powiatu(powiat_t)
#    })

   main <- observe({
      fun_init()
      fun_wojewodztwo()
   })

   observeEvent(input$action, {
      # zmienil sie powiaty
      wojewodztwo <- input$wojewodztwo
      powiat <- input$powiat

      # znajdz TERYT powiatu
      powiat_t <- powiat_teryt(wojewodztwo, powiat)

      # przygotuj dane z powiatu
      migracje <- migracje_z_powiatu(powiat_t)


      output$mapaPlot <- renderPlot({
         p <- migracje_mapa(migracje, powiat_t)
         print(p)
      })

      output$slupkiPlot <- renderPlot({
         p <- migracje_slupki(migracje)
         print(p)
      })
   })
})

