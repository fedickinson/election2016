library(shiny)

shinyUI(navbarPage("2016 Election Matrix",

# Tab 1: Map ####
                                      
        tabPanel("Map",
                 htmlOutput('text_tabIntro1a'),
                 htmlOutput('text_tabIntro1b'),
                 htmlOutput('text_tabIntro1c'),
                 br(),
                 fluidRow(column(width=6,offset=2,img(src = "map.png", align = "center"))),
                 br(),
                 textOutput('text_tabIntro2'),
                 br(),
                 htmlOutput('text_tabIntro4'),
                 br(),
                 htmlOutput('text_tabIntro5'),
                 br(),
                 htmlOutput('text_tabIntro6'),
                 htmlOutput('text_tabIntro7'),
                 br(),
                 htmlOutput('text_tabIntro8'),                 
                 fluidRow(column(width=8,offset=1,htmlOutput('text_tabIntroRed'))),
                 br(),
                 fluidRow(column(width=8,offset=1,htmlOutput('text_tabIntroOrange'))),
                 br(),
                 fluidRow(column(width=8,offset=1,htmlOutput('text_tabIntroPurple'))),
                 br(),
                 fluidRow(column(width=8,offset=1,htmlOutput('text_tabIntroBlue'))),
                 br(),
                 htmlOutput('text_tabIntro9'),
                 br()
                 ),
# Tab 2: Scatterplot & Sankey ####

        tabPanel("Scatter",
                 br(),
                 textOutput('text_tabScatter1'),
                 br(),
                 fluidRow(column(width=6,offset=2, htmlOutput('scatter'))),
                 br(),
                 textOutput('text_tabScatter4'),
                 br(),
                 fluidRow(column(width=6,offset=3,htmlOutput('CD_Sankey_plot'))),
                 br(),
                 textOutput('text_tabScatter5'),
                 br()
                 ),
 
# Tab 3: Income ####

        tabPanel('Income',
                 textOutput('text_tabIncome1'),
                 br(),
                 htmlOutput('gvis_income_mean'),
                 br(),
                 htmlOutput('gvis_scatter_income')),

# Tab 4: Education ####

        tabPanel('Education',
                 textOutput('text_tabEducation1'),
                 fluidRow(
                   column(6,htmlOutput('gvisCD_education_D_T_mean')),
                   column(6,htmlOutput('gvisCD_education_R_T_mean'))),
                   
                 fluidRow(
                   column(6,htmlOutput('gvisCD_education_D_C_mean')),
                   column(6,htmlOutput('gvisCD_education_R_C_mean'))),
                 br(),
                 selectizeInput(
                   inputId = 'scatter_education',
                   label = 'View distribution of selected education level:',
                   choices = c('No Degree','High School','Bachelors/Associates','Professional')),
                 br(),
                 htmlOutput('scatter_education_plot')
                 ),

# Tab 5: Race ####

          tabPanel('Race',
                   textOutput('text_tabRace1'),
                   fluidRow(
                     column(6,htmlOutput('gvisCD_race_D_T_mean')),
                     column(6,htmlOutput('gvisCD_race_R_T_mean'))),
                   
                   fluidRow(
                     column(6,htmlOutput('gvisCD_race_D_C_mean')),
                     column(6,htmlOutput('gvisCD_race_R_C_mean'))),
                   br(),
                   selectizeInput(
                     inputId = 'scatter_race',
                     label = 'View distribution of selected race/ethnic group:',
                     choices = c('White','Black','Native','Asian','Latino')),
                   br(),
                   htmlOutput('scatter_race_plot')
          )
))