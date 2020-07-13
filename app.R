library(shinythemes); library(shinyBS)
library(tidyverse)
library(chorddiag)
# source("kdrisk.R")
# source("kdData.R")


ui <- navbarPage("Residency and Fellowship Paths", theme = shinytheme("flatly"), collapsible = T,
                 header = 
                   tags$head(
                     includeHTML("google-analytics.js"),
                     tags$style(HTML("
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "))
                   ),
                 
                 tabPanel("Plot", id="test", 
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         bsCollapse(open = "panel",
                                                    bsCollapsePanel(
                                                      p(icon("bars"),HTML('&nbsp;'), "Viewing Format"), value = "panel",
                                                      radioButtons("viewFormat", 
                                                                   label = NULL, 
                                                                   c("Desktop/Table" = "large",
                                                                     "Smartphone" = "small")
                                                      )
                                                    )
                                         )
                            ),
                            mainPanel(
                              chorddiagOutput('chorddiag', height = '50vmax')
                            )
                          )
                 )
)

server <- function(input, output) {
  df <- read_csv("residency_fellowships.csv")
  df.3 <- df %>% mutate(count = 1) %>% pivot_wider(names_from = Residency, values_from = count, values_fill = 0)
  
  residencies <- names(df.3)[-1]
  fellowships <- df.3$Fellowship
  
  df.3 <- df.3 %>% 
    select(-Fellowship) %>% 
    mutate_all(as.numeric) %>% 
    as.matrix()
  dimnames(df.3) <- list(Fellowships = fellowships, Residencies = residencies)
  
  output$chorddiag <- renderChorddiag({
    diag_large <- chorddiag(df.3,
              type = "bipartite",
              showTicks = F,
              groupnameFontsize = 11,
              groupnamePadding = 14,
              groupPadding = 1,
              margin = 185,
              groupColors = c(
                rep("#ef8a62", nrow(df.3)),
                rep("#67a9cf", ncol(df.3))
              ),
              categoryNames = c("","")
              )
    diag_small<- chorddiag(df.3,
              type = "bipartite",
              showTicks = F,
              groupnameFontsize = 4,
              groupnamePadding = 7,
              groupPadding = 2,
              margin = 95,
              groupThickness = 0.1,
              groupColors = c(
                rep("#ef8a62", nrow(df.3)),
                rep("#67a9cf", ncol(df.3))
              ),
              categoryNames = c("","")
              )
    plot <- switch(input$viewFormat,
                   large = diag_large,
                   small = diag_small)
    plot
  })
}

shinyApp(ui, server)
