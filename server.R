
  library(shiny)
  
  shinyServer(function(input, output, session) {
  
  
  # Tab 1: Map ####
    
    output$text_tabIntro1a <- renderText({paste("<h4>","So who did you vote for?: ","</h4>")})
    output$text_tabIntro1b <- renderText({paste("<b>","An examination of congressional and national party misalignment in the 2016 election.","</b>")})
    output$text_tabIntro1c <- renderText({paste("by Franklin. E. Dickinson")})
    
    
    
    output$text_tabIntro2 <- renderText({paste("Within the United States 2-Party system, a range of political identities exist within both the Republican and Democrat parties, each competing in the primaries to represent the party in the general election. In the presidential election, candidates compete to represent their party at the national level. Meanwhile candidates at the Congressional level seek to represent the party identificiation of their much more localized district.")})
    
    output$text_tabIntro4 <- renderText({paste("When a district-party's locally prefered Presidential candidate loses in the primary, the party's winner still usually best represents their views in comparison to the other party's candidate. However, this is not always the case, and sometimes the general election candidate of the other party more represent's the district-party on their most salient issues.")})
    
    output$text_tabIntro5 <- renderText({paste("Therefore, Presidential elections where the majority Congressional and Presidential vote winners of a district come from opposing parties signify in the short term, discord between local and national party identities, and in the long term, changing dominance in national party identity.")})
    
    output$text_tabIntro6 <- renderText({paste("In the 2016 election, party misalignment occurred in 35 of the 435 districts, and the following analysis compares the demographic features of these districts compared to their non-misaligned counterparts.")})
      
    output$text_tabIntro7 <- renderText({paste("Demographic data comes via the U.S. Census Bureau's 2016 American Community Survey.")})
    
    output$text_tabIntro8 <- renderText({paste('<i>',"I use",'<b>','color',"</b>"," to distinguish the four outcomes:",'</i>')})
    
    output$text_tabIntro9 <- renderText({paste("In the following midterm elections, these districts are marked for close contest, and whether the incumbent can hold on makes clear whether these changes have been concretized.")}) 
    
    output$text_tabIntroRed <- renderText({paste('<font color=\"#FF0000\"><b>','Red','</b></font>',' for districts won by a ','<font color=\"#FF0000\"><b>',"Republican Congressman and Donald Trump.",'</b></font>')})
    
    output$text_tabIntroOrange <- renderText({paste('<font color=\"#FF8C00\"><b>','Orange','</b></font>',' for districts won by a ','<font color=\"#FF8C00\"><b>',"Democratic Congressman and Donald Trump.",'</b></font>')})
    
    output$text_tabIntroPurple <- renderText({paste('<font color=\"#7913C1\"><b>','Purple','</b></font>',' for districts won by a ','<font color=\"#7913C1\"><b>',"Republican Congressman and Hilary Clinton.","</b></font>")})
    
    output$text_tabIntroBlue <- renderText({paste('<font color=\"#0034E2\"><b>','Blue','</b></font>',' for districts won by a ','<font color=\"#0034E2\"><b>',"Democratic Congressman and Hilary Clinton.",'</b></font>')})
    
    
    
  # Tab 2: Scatterplot & Sankey ####
  
    output$text_tabScatter1 <- renderText({paste('This scatterplot reformulates the map, demonstrate the Congressional versus Presidential vote margin in the 2016 election for each Congressional District.')})
    
    output$text_tabScatter4 <- renderText({paste('The Sankey plot below shows the 2016 results in relation to the previous 2014 Congress:')})
    
    output$text_tabScatter5 <- renderText({paste('While Republicans mantained their district majority following the 2016 election, they lost more districts to Democrats than vice versa.')})
    
    output$scatter <- renderGvis(scatter_margin)
    
    output$CD_Sankey_plot <- renderGvis(gvisSankey(CD_Sankey, from='From',to='To',weight='Weight',
                                 options = list(
                                   sankey= "{node: { colors: ['red', 'red', 'purple', 'orange', 'blue', 'blue']}}" )))
  
  
  # Tab 3: Income ####
  
  output$text_tabIncome1 <- renderText({paste("The side-by-side bar chart below presents the average district population percent falling within each income bracket.")})
    
  
  output$gvis_income_mean <- renderGvis({gvisColumnChart(data=income_mean,
                                                         options = list(
                                                           colors= "['red','purple','orange','blue']",
                                                           legend="{ position: 'top', maxLines: 2 }",
                                                           bar.groupWidth = '100%',
                                                           vAxis="{title: 'Mean Percent of District Population'}",
                                                           hAxis="{title: 'Income Bracket (USD$)'}",
                                                           height=300
                                                         ))})
  
  
  output$gvis_scatter_income <- renderGvis({gvisScatterChart(data=CD_income_scatter,
                                                            options = list(
                                                              legend = 'top',
                                                                pointSize = 1.5,
                                                                colors= "['red','purple','orange','blue']",
                                                                vAxis="{title: 'Pres. Vote margin in favor of Trump'}",
                                                                hAxis="{title: 'Mean household income (USD$)'}",
                                                                height = 400
                                                            ))})
                                 
  # Tab 4: Education ####
  
  output$text_tabEducation1 <- renderText({paste("The pie charts below present the average district population breakdown among various education levels.")})
  
  
  output$gvisCD_education_R_T_mean <- renderGvis({gvisPieChart(data=CD_education_R_T_mean,
                                                               options = list(
                                                                 title= 'House: Republican; Pres: Trump',
                                                                 colors= "['ffd4d4','ff7c7c','d41b1b','a10808']"
                                                               ))})
  
  output$gvisCD_education_R_C_mean <- renderGvis({gvisPieChart(data=CD_education_R_C_mean,
                                                               options = list(
                                                                 title= 'House: Republican; Pres: Clinton',
                                                                 colors= "['f3d9fd','df85ff','9a33bf','6a2a81']"
                                                               ))})
  output$gvisCD_education_D_T_mean <- renderGvis({gvisPieChart(data=CD_education_D_T_mean,
                                                               options = list(
                                                                 title= 'House: Democrat; Pres: Trump',
                                                                 colors= "['ffdeab','ffbb53','fb9800','b26c00']"
                                                               ))})
  output$gvisCD_education_D_C_mean <- renderGvis({gvisPieChart(data=CD_education_D_C_mean,
                                                               options = list(
                                                                 title= 'House: Democrat; Pres: Clinton',
                                                                 colors= "['c5d7ff','77a0fd','135cff','003aba']"
                                                               ))})
  
  scatter_education_data <- reactive(CD_data %>% 
                                       select(.,input$scatter_education) %>% 
                                       mutate(.,repTrump = ifelse(CD_data$margin_republican > 0 & CD_data$margin_Trump > 0, CD_data$margin_Trump, NA)) %>% 
                                       mutate(.,repClinton = ifelse(CD_data$margin_republican > 0 & CD_data$margin_Trump < 0, CD_data$margin_Trump, NA)) %>% 
                                       mutate(.,demTrump = ifelse(CD_data$margin_republican < 0 & CD_data$margin_Trump > 0, CD_data$margin_Trump, NA)) %>% 
                                       mutate(.,demClinton = ifelse(CD_data$margin_republican < 0 & CD_data$margin_Trump < 0, CD_data$margin_Trump, NA)) %>%
                                       mutate(.,CD1.tooltip = CD_data$CD) %>% 
                                       mutate(.,CD2.tooltip = CD_data$CD) %>% 
                                       mutate(.,CD3.tooltip = CD_data$CD) %>% 
                                       mutate(.,CD4.tooltip = CD_data$CD) %>%
                                       select(input$scatter_education, repTrump, CD1.tooltip, repClinton, CD2.tooltip, demTrump, CD3.tooltip, demClinton, CD4.tooltip))
  
  output$scatter_education_plot <- renderGvis(gvisScatterChart(scatter_education_data(),
                                                               options = list(
                                                                 legend='top',
                                                                 pointSize= 1.25,
                                                                 title="Distribution of Support for Trump by Selected Education Level",
                                                                 colors= "['red','purple','orange','blue']",
                                                                 vAxis="{title: 'Presidential vote margin in favor of Trump'}",
                                                                 hAxis="{title: '% of District achieving selected degree'}")))
  
  
  # Tab 5: Race ####
  
  output$text_tabRace1 <- renderText({paste("The pie charts below present the average district population breakdown among racial/ethnic identification.")})
  
  
  output$gvisCD_race_R_T_mean <- renderGvis({gvisPieChart(data=CD_race_R_T_mean,
                                                          options = list(
                                                            title= 'House: Republican; Pres: Trump',
                                                            colors= "['ffd4d4','760202','fd8484','a10808','d41b1b']",
                                                            legend.position = 'bottom'
                                                          ))})
  output$gvisCD_race_R_C_mean <- renderGvis({gvisPieChart(data=CD_race_R_C_mean,
                                                          options = list(
                                                            title= 'House: Republican; Pres: Clinton',
                                                            colors= "['f3d9fd','3f0b5c','bd71ff','6a2a81','9a33bf']",
                                                            legend.position = 'bottom'
                                                          ))})
  output$gvisCD_race_D_T_mean <- renderGvis({gvisPieChart(data=CD_race_D_T_mean,
                                                          options = list(
                                                            title= 'House: Democrat; Pres: Trump',
                                                            colors= "['ffdeab','923b01','ffc071','b26c00','fb9800']",
                                                            legend.position = 'bottom'
                                                            ))})
  output$gvisCD_race_D_C_mean <- renderGvis({gvisPieChart(data=CD_race_D_C_mean,
                                                          options = list(
                                                            title= 'House: Democrat; Pres: Clinton',
                                                            colors= "['c5d7ff','121c6b','719eff','003aba','135cff']"
                                                          ))})
  
  
  scatter_race_data <- reactive(CD_data %>% 
                                  select(.,input$scatter_race) %>% 
                                  mutate(.,repTrump = ifelse(CD_data$margin_republican > 0 & CD_data$margin_Trump > 0, CD_data$margin_Trump, NA)) %>% 
                                  mutate(.,repClinton = ifelse(CD_data$margin_republican > 0 & CD_data$margin_Trump < 0, CD_data$margin_Trump, NA)) %>% 
                                  mutate(.,demTrump = ifelse(CD_data$margin_republican < 0 & CD_data$margin_Trump > 0, CD_data$margin_Trump, NA)) %>% 
                                  mutate(.,demClinton = ifelse(CD_data$margin_republican < 0 & CD_data$margin_Trump < 0, CD_data$margin_Trump, NA)) %>%
                                  mutate(.,CD1.tooltip = CD_data$CD) %>% 
                                  mutate(.,CD2.tooltip = CD_data$CD) %>% 
                                  mutate(.,CD3.tooltip = CD_data$CD) %>% 
                                  mutate(.,CD4.tooltip = CD_data$CD) %>%
                                  select(input$scatter_race, repTrump, CD1.tooltip, repClinton, CD2.tooltip, demTrump, CD3.tooltip, demClinton, CD4.tooltip))
                                  
  output$scatter_race_plot <- renderGvis(gvisScatterChart(scatter_race_data(),
                                                          options = list(
                                                            legend='top',
                                                            pointSize= 1.25,
                                                            title="Distribution of Support for Trump by Selected Racial/Ethnic Group",
                                                            colors= "['red','purple','orange','blue']",
                                                            vAxis="{title: 'Presidential vote margin in favor of Trump'}",
                                                            hAxis="{title: '% of District identifying as Race/Ethnicity'}")))
    
  
  })
