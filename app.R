if (interactive()) {
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(plotly)
  library(leaflet)
  library(shinythemes)
  library(styler)
  library(tidyverse)
  library(dplyr)

  source("code/data_processing.R")

  ui <- fluidPage(
    setBackgroundColor(color = "ghostwhite"),
    useShinydashboard(),
    navbarPage(
      theme = shinytheme("flatly"),
      HTML('<a style = "text-decoration: none; cursor: default; color: #FFFFFF;" class= "active" href= "https://saude.rs.gov.br/inicial">
           <img src = "marca_governo_RS.png" width="45px" height="30px" >
           </a>'), id = "nav",
      windowTitle = "RS",
      tabPanel(
        "Monitor",
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Os dados reportados são de responsabilidade do Estado do Rio Grande do Sul.")), style = "color:#045a8d"),
            "O Aedes costuma ter sua circulação intensificada no verão, em virtude da combinação da temperatura mais quente e chuvas. Para se reproduzir, ele precisa de locais com água parada. Por isso, o cuidado para evitar a sua proliferação busca eliminar esses possíveis criadouros, impedindo o nascimento do mosquito.",
            "Para visualizar o gráfico do total de casos ao longo dos anos:",
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            selectInput("Municipio_monitor", "Selecione o município",
              choices = unique(casos_por_sexo$Municipio),
              selected = "Porto Alegre"
            )
          ),
          mainPanel(
            fluidRow(
              infoBox(
                "Total de casos", uiOutput("totais_casos2"), tags$br(),
                color = "navy", icon = icon("viruses")
              ),
              infoBox(
                "Média de Casos", uiOutput("media_casos2"), tags$br(),
                color = "navy", icon = icon("virus")
              ),
              infoBox(
                "CRS com maior n° de casos", uiOutput("top_crs"), tags$br(),
                color = "navy", icon = icon("map-marked-alt")
              ),
              leaflet::leafletOutput("map3"),
              tags$br(), tags$br(), tags$br(),
              fluidRow(
                column(6, plotlyOutput("pizza_casos_sexo")),
                column(6, plotlyOutput("barplot_faixa_etaria"))
              ),
              tags$br(), tags$br(),
              fluidRow(
                column(6, plotlyOutput("barplot_evolucao")),
                column(6, plotlyOutput("barplot_gestante"))
              ),
              tags$br(), tags$br(), tags$br(),
              plotlyOutput("barplot_zona")
            )
          )
        ),
        tags$div(
          class = "outer",
          absolutePanel(
            id = "controls", class = "panel panel-default",
            top = 75, left = 55, width = "auto", fixed = TRUE,
            draggable = TRUE, height = "auto"
          )
        )
      ),
      tabPanel(
        "Dados Históricos da Dengue",
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Os dados reportados são de responsabilidade do Estado do Rio Grande do Sul.")), style = "color:#045a8d"),
            "Selecione o resultado, as regiões e a data de início do gráfico nos menus suspensos para atualizar a tabela.",
            tags$br(), tags$br(),
            selectInput("Ano", "Selecione o ano", choices = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            "Para visualizar o gráfico do total de casos ao longo dos anos:",
            selectInput("Município", "Selecione o município",
              choices = unique(df$Município), selected = "Porto Alegre"
            ),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br(), tags$br(),
            tags$br(), tags$br()
          ),
          mainPanel(
            infoBox(
              "Total de casos", uiOutput("totais_casos"), tags$br(),
              color = "navy", icon = icon("viruses")
            ),
            infoBox(
              "Média de Casos por cidade", uiOutput("media_casos"), tags$br(),
              color = "navy", icon = icon("virus")
            ),
            infoBox(
              "CRS com maior n° de casos", uiOutput("data_city_cases"), tags$br(),
              color = "navy", icon = icon("map-marked-alt")
            ),
            leaflet::leafletOutput("map"),
            tags$br(), tags$br(),
            tags$br(), tags$br(),
            plotlyOutput("barplot")
          )
        ),
        tags$div(
          class = "outer",
          absolutePanel(
            id = "controls", class = "panel panel-default",
            top = 75, left = 55, width = "auto", fixed = TRUE,
            draggable = TRUE, height = "auto"
          )
        )
      ),
      tabPanel(
        "Dados Históricos do Mosquito",
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Os dados reportados são de responsabilidade do Estado do Rio Grande do Sul.")), style = "color:#045a8d"),
            "Selecione o resultado, as regiões e a data de início do gráfico nos menus suspensos para atualizar a tabela.",
            tags$br(),
            tags$br(),
            selectInput("year", "Selecione o ano",
              choices = c(1997, 2002, 2003, 2004, 2010, 2013, 2019, 2020, 2021),
            )
          ),
          mainPanel(
            infoBox(
              "Total de casos", "100", tags$br(),
              color = "navy", icon = icon("viruses")
            ),
            infoBox(
              "Média de Casos", "100", tags$br(),
              color = "navy", icon = icon("virus")
            ),
            infoBox(
              "CRS com maior n° de casos", "100", tags$br(),
              color = "navy", icon = icon("map-marked-alt")
            ),
            leaflet::leafletOutput("map2"),
            tags$br(), tags$br(),
            plotlyOutput("occ_plot")
          )
        ),
        tags$div(
          class = "outer",
          absolutePanel(
            id = "controls", class = "panel panel-default",
            top = 75, left = 55, width = "auto", fixed = TRUE,
            draggable = TRUE, height = "auto"
          )
        )
      ),
      tabPanel(
        "Cenário de casos",
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Os dados reportados são de responsabilidade do Estado do Rio Grande do Sul.")), style = "color:#045a8d"),
            "Selecione o resultado, as regiões e a data de início do gráfico nos menus suspensos para atualizar a tabela."
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Casos", plotlyOutput("plot_casos")),
              tabPanel("Mosquito", plotlyOutput("plot_mosquito")),
              tabPanel("Medidas preventivas", plotlyOutput("lista_medidas"))
            )
          )
        )
      ),
      tabPanel(
        "Mais",
        wellPanel(
          tags$h4("O Estado do Rio Grande do Sul"),
          tags$p(
            style = "text-align: justify;",
            "Rio Grande do Sul tem extensão territorial de 281.730,2 km², ocupando mais de 3% do território brasileiro, sendo o nono maior Estado brasileiro . 
          Dividido em 497 municípios, tem 11,3 milhões de habitantes, conforme dados do Instituto Brasileiro de Geografia e Estatística (IBGE), 
          o que corresponde a 6% da população nacional. O volume populacional fica atrás apenas de São Paulo, Minas Gerais, Rio de Janeiro e Bahia. 
          A densidade demográfica é de 39,8 habitantes/km². A capital, Porto Alegre, é o município mais populoso com 1,4 milhão de pessoas."
          ),
          tags$br(), tags$br(),
          tags$h4("Sobre a secretária do Estado do RS"),
          tags$br(), tags$br(),
          tags$h4("Últimas notícias"),
          tags$br(), tags$br(),
          tags$h3("Background"),
          tags$h4("O que é dengue?"),
          tags$br(), tags$br(),
          tags$p(
            style = "text-align: justify;",
            "Dengue é uma doença febril grave causada por um arbovírus. Arbovírus são vírus transmitidos por picadas de insetos, especialmente os mosquitos. 
           Existem quatro tipos de vírus de dengue (sorotipos 1, 2, 3 e 4). Cada pessoa pode ter os 4 sorotipos da doença, mas a infecção por um sorotipo 
           gera imunidade permanente para ele.

           O transmissor (vetor) da dengue é o mosquito Aedes aegypti, que precisa de água parada para se proliferar. O período do ano com maior transmissão 
           são os meses mais chuvosos de cada região, mas é importante manter a higiene e evitar água parada todos os dias, porque os ovos do mosquito podem 
           sobreviver por um ano até encontrar as melhores condições para se desenvolver.

           Todas as faixas etárias são igualmente suscetíveis, porém as pessoas mais velhas têm maior risco de desenvolver dengue grave e outras complicações
           que podem levar à morte. O risco de gravidade e morte aumenta quando a pessoa tem alguma doença crônica, como diabetes e hipertensão, mesmo tratada."
          ),
          tags$br(), tags$br(),
          tags$h3("Transmissão da dengue"),
          tags$br(),
          tags$p(
            style = "text-align: justify;",
            "A dengue é transmitida pela picada do mosquito Aedes aegypti. Após picar uma pessoa infectada com um dos quatro sorotipos do vírus, a fêmea pode  
          transmitir o vírus para outras pessoas. Há registro de transmissão por transfusão sanguínea.

          Não há transmissão da mulher grávida para o feto, mas a infecção por dengue pode levar a mãe a abortar ou ter um parto prematuro, além da gestante estar 
          mais exposta para desenvolver o quadro grave da doença, que pode levar à morte. Por isso, é importante combater o mosquito da dengue, fazendo limpeza 
          adequada e não deixando água parada em pneus, vasos de plantas, garrafas, pneus ou outros recipientes que possam servir de reprodução do mosquito Aedes Aegypti.

          Em populações vulneráveis, como crianças e idosos com mais de 65 anos, o vírus da dengue pode interagir com doenças pré-existentes e levar ao quadro grave ou gerar 
          maiores complicações nas condições clínicas de saúde da pessoa."
          ),
          HTML('<center><img src="https://s3-sa-east-1.amazonaws.com/agencia-radio-arb/1759/content_CICLO-AEDES-AEGYPTI.png"></center>')
        )
      ),
      tabPanel(
        "Sobre nós",
        box(
          tags$img(" descrição",
            src = "amanda.jpeg",
            width = 200, height = 200
          ),
          solidHeader = F, status = "info"
        ),
        box(
          tags$img(" descrição",
            src = "grati.jpeg",
            width = 200, height = 200
          ),
          solidHeader = F, status = "info"
        ),
        HTML('<center><img src="lepav.png" width="150px" height="150px"></center>')
      )
    )
  )

  server <- function(input, output) {

    ## Monitor
    # Total de casos e media de casos por cidade

    data_summary_cases_2001 <- function() {
      casos_autoctones_2021_pop %>%
        summarise(
          soma_casos = sum(Frequência),
          media_casos = mean(Frequência)
        )
    }

    # Cidades com maior numero de casos

    crs_casos_2021 <- function() {
      casos_crs <- casos_autoctones_2021_pop %>%
        group_by(CRS, `Frequência`) %>%
        summarise(soma_casos = sum(`Frequência`)) %>%
        arrange(desc(soma_casos))
    }

    output$top_crs <- renderText({
      prettyNum(crs_casos_2021()$CRS[1], big.mark = ",")
    })

    # mapa

    output$map3 <- leaflet::renderLeaflet({
      raster_2021@data <- left_join(casos_autoctones_2021_pop, raster_2021@data, by = c("Municipio" = "Label_N"))

      pal <- colorNumeric(c("darkblue", "moccasin", "red4"),
        domain = raster_2021@data$casos_1000_hab
      )

      leaflet(raster_2021) %>%
        addTiles() %>%
        addPolygons(
          stroke = FALSE, fillOpacity = 0.5,
          smoothFactor = 0.5,
          color = ~ pal(casos_1000_hab)
        ) %>%
        addLegend("bottomright",
          values = ~casos_1000_hab,
          pal = pal,
          title = paste0("Casos de Dengue em 2021"),
          opacity = 1
        )
    })

    # Box de total de casos

    output$totais_casos2 <- renderText({
      prettyNum(data_summary_cases_2001()$soma_casos, big.mark = ",")
    })

    # Box de media de casos

    output$media_casos2 <- renderText({
      prettyNum(round(data_summary_cases_2001()$media_casos, 0), big.mark = ",")
    })

    # Casos por sexo

    dados_por_sexo <- function() {
      casos_por_sexo %>%
        dplyr::filter(Municipio == input$Municipio_monitor)
    }

    output$pizza_casos_sexo <- renderPlotly({
      fig <- plot_ly(dados_por_sexo(), labels = ~categoria, values = ~total, type = "pie")
      fig <- fig %>% layout(
        title = "Casos por sexo",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )

      fig
    })

    # Casos por faixa etaria

    dados_por_faixa_etaria <- function() {
      casos_por_faixa_etaria %>%
        dplyr::filter(Municipio == input$Municipio_monitor)
    }

    output$barplot_faixa_etaria <- renderPlotly({
      ggplotly(ggplot(dados_por_faixa_etaria(), aes(x = categoria, y = total)) +
        geom_bar(fill = rgb(0.1, 0.4, 0.5, 0.7), stat = "identity") +
        theme_classic() +
        ggtitle(" \n Distribuição de casos por faixa etária \n\n") +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.border = element_blank(),
          line = element_blank()
        ) +
        labs(
          x = "\n \n Faixa Etária",
          y = "Total de casos"
        ))
    })

    # Casos por gestante

    dados_por_gestante <- function() {
      casos_por_gestante %>%
        dplyr::filter(Municipio == input$Municipio_monitor)
    }

    output$barplot_gestante <- renderPlotly({
      ggplotly(ggplot(dados_por_gestante(), aes(x = total, y = categoria)) +
        geom_bar(fill = rgb(0.1, 0.4, 0.5, 0.7), stat = "identity") +
        ggtitle("\n Casos por o periodo gestacional") +
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          line = element_blank()
        ) +
        labs(
          x = "Total de casos",
          y = "Período Gestacional"
        ))
    })

    # Evoluação dos casos

    dados_por_evolucao <- function() {
      casos_por_evolucao %>%
        dplyr::filter(Municipio == input$Municipio_monitor)
    }

    output$barplot_evolucao <- renderPlotly({
      ggplotly(ggplot(dados_por_evolucao(), aes(x = 1, y = total, fill = categoria)) +
        geom_bar(stat = "identity") +
        ggtitle("\n Evolução dos casos") +
        theme_classic() +
        theme(
          legend.position = "none", axis.text.x = element_blank(),
          line = element_blank()
        ) +
        labs(
          x = " ",
          y = "Total de casos"
        ))
    })

    # Casos por Residência

    dados_por_zona <- function() {
      casos_por_zona %>%
        dplyr::filter(Municipio == input$Municipio_monitor)
    }

    output$barplot_zona <- renderPlotly({
      ggplotly(ggplot(dados_por_zona(), aes(x = 1, y = total, fill = categoria)) +
        geom_bar(stat = "identity") +
        ggtitle("\n Distribuição de casos de acordo com a zona de Residência \n\n") +
        coord_flip() +
        theme_classic() +
        theme(
          legend.position = "none", axis.text.y = element_blank(),
          line = element_blank()
        ) +
        labs(
          x = "",
          y = "Total de casos"
        ))
    })


    ## Casos de dengue históricos

    # DF de casos de dengue

    data_year <- function() {
      df %>%
        filter(Ano == input$Ano) %>%
        mutate(`Casos 1000 hab` = round(`Casos 1000 hab`, 2))
    }

    # DF de casos de dengue por município

    data_year_municipe <- function() {
      df %>%
        filter(Município == input$Município) %>%
        mutate(`Casos 1000 hab` = round(`Casos 1000 hab`, 2))
    }

    # Render Table para os top casos -- não estamos utilizando

    output$table_top_cases <- DT::renderDT(
      {
        data_year() %>%
          arrange(desc(`Casos 1000 hab`)) %>%
          top_n(30, `Casos 1000 hab`) %>%
          dplyr::select(Município, `Casos 1000 hab`)
      },
      rownames = FALSE
    )

    # Total de casos

    data_summary_cases <- function() {
      df %>%
        filter(Ano == input$Ano) %>%
        summarise(
          soma_casos = sum(Casos),
          media_casos = mean(Casos)
        )
    }

    # Box de total de casos

    output$totais_casos <- renderText({
      prettyNum(data_summary_cases()$soma_casos, big.mark = ",")
    })

    # Box de media de casos

    output$media_casos <- renderText({
      prettyNum(round(data_summary_cases()$media_casos, 0), big.mark = ",")
    })

    # Cidades com maior numero de casos

    data_city_cases <- function() {
      casos_crs <- df %>%
        group_by(CRS, Ano, Casos) %>%
        summarise(soma_casos = sum(Casos)) %>%
        arrange(desc(soma_casos))

      casos_crs %>%
        filter(Ano == input$Ano)
    }

    output$data_city_cases <- renderText({
      prettyNum(data_city_cases()$CRS[1], big.mark = ",")
    })

    # Mapa de calor com os casos de dengue por cidade

    output$map <- leaflet::renderLeaflet({
      raster_historico@data <- left_join(data_year(), raster_historico@data, by = c("cod_ibge" = "cod"))

      pal <- colorNumeric(c("darkblue", "moccasin", "red4"),
        domain = raster_historico@data$`Casos 1000 hab`
      )

      leaflet(raster_historico) %>%
        addTiles() %>%
        addPolygons(
          stroke = FALSE, fillOpacity = 0.5,
          smoothFactor = 0.5,
          color = ~ pal(`Casos 1000 hab`)
        ) %>%
        addLegend("bottomright",
          values = ~`Casos 1000 hab`,
          pal = pal,
          title = paste0("Casos de Dengue ", input$Ano),
          opacity = 1
        )
    })

    # Plot de casos de uma cidade selecionada por ano

    output$barplot <- renderPlotly({
      ggplotly(ggplot(data_year_municipe(), aes(x = as.factor(Ano), y = Casos)) +
        geom_bar(fill = rgb(0.1, 0.4, 0.5, 0.7), stat = "identity") +
        theme_classic() +
        theme(line = element_blank()) +
        labs(
          x = "Ano",
          y = "Total de Casos"
        ))
    })

    ## Ocerrência ae. aegypty histórico

    # DF de ocorrências de Ae. aegypti

    data_occ <- function() {
      occ %>%
        filter(year == input$year)
    }

    # Mapa de ocorrência de Ae. aegypti

    output$map2 <- leaflet::renderLeaflet({
      leaflet(data_occ()) %>%
        addTiles() %>%
        setView(lng = -52.0986, lat = -30.5, zoom = 6) %>%
        addMarkers(
          icon = fly, lng = ~longitude, lat = ~latitude
        )
    })

    # Ocorrência do vetor
    occ_aaegypti <- function() {
      occ %>%
        group_by(year) %>%
        count()
    }

    output$occ_plot <- renderPlotly({
      ggplotly(ggplot(data = occ_aaegypti(), aes(x = year, y = n)) +
        geom_line(linetype = "dashed", color = rgb(0.1, 0.4, 0.5, 0.7)) +
        geom_point(color = rgb(0.1, 0.4, 0.5, 0.7)) +
        theme_classic() +
        theme(line = element_blank()) +
        ggtitle("\n Quantidade de mosquitos ao longo dos anos \n\n") +
        labs(
          x = "Ano",
          y = "Total de Mosquitos"
        ))
    })
  }

  # Roda a aplicação

  shinyApp(ui = ui, server = server)
}