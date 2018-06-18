
# Load Base Packages #### 

# install.packages("shiny")

# install.packages("tidyverse")

#install.packages("googleVis")

library(shiny)

library(tidyverse)

library(googleVis)

library(data.table)

# Load downloaded datasets ####

# 1) MIT Election Lab dataset on Congressional Election Results 

  # See source 'US House 1976-2016': https://electionlab.mit.edu/data

MIT = read.csv('MIT_House.csv', stringsAsFactors = FALSE) 

# 2) DailyKos dataset on 2008, 2012, & 2016 Presidential Election Results

  # See source https://www.dailykos.com/stories/2012/11/19/1163009/-Daily-Kos-Elections-presidential-results-by-congressional-district-for-the-2012-2008-elections

DK = read.csv('DK_Pres_2016.csv', stringsAsFactors = FALSE, skip=1)


colnames(DK)[6] <- 'Obama.2012'

colnames(DK)[8] <- 'Obama.2008'


# Download Census API data ####


# install.packages("censusapi")

  # See source https://github.com/hrecht/censusapi

library(censusapi)


  # 1) Select variables for further download 

      # Specifically analyzing the 2016 American Community Survey (ACS)
      #variables -  https://api.census.gov/data/2016/acs/acs1/profile/variables.html


variables_education = c('DP02_0059PE','DP02_0060PE','DP02_0061PE','DP02_0062PE','DP02_0063PE','DP02_0064PE','DP02_0065PE') 

variables_income = c('DP03_0052PE','DP03_0053PE','DP03_0054PE','DP03_0055PE','DP03_0056PE','DP03_0057PE','DP03_0058PE','DP03_0059PE','DP03_0060PE','DP03_0061PE','DP03_0063E')

variables_race = c('DP05_0059PE','DP05_0060PE','DP05_0061PE','DP05_0062PE','DP05_0063PE','DP05_0066PE')

variables_occupation = c('DP03_0027PE','DP03_0028PE','DP03_0029PE','DP03_0030PE','DP03_0031PE')

# Removing variables_industry for final output
  # variables_industry = c('DP03_0033PE','DP03_0034PE','DP03_0035PE','DP03_0036PE','DP03_0037PE','DP03_0038PE','DP03_0039PE','DP03_0040PE','DP03_0041PE','DP03_0042PE','DP03_0043PE','DP03_0044PE','DP03_0045PE')

variables_selected = c(variables_education,variables_income,variables_race,variables_occupation) #<- variables industry removed


  # 2) Download selected variables from API

key = 'cc4151c170141a6c1f7b3f63803719fd74c78357'  

acs_variables  <- getCensus(name = 'acs/acs1/profile', vintage = 2016, 
                            vars = c("NAME",variables_selected), 
                            region = "congressional district:*",
                            key = key)

  # Clean Datasets ####

  # Merge will be based on one row for each congressional district. 
  # Formats of Congressional Districts standardized to DailyKos format ('NY-04','DE-AL') 

# MIT Congressional Votes Dataset

  # get state and state abbreviation pairs

MIT_state_list <- MIT %>% select(.,state, state_po) %>%
  unique(.)

  # Clean data

    # Party data for Wyoming not filled in... need to fill in republican and democrat manually

MIT <- MIT %>% mutate(party=replace(party, candidate=='Liz Cheney', 'republican'))
MIT <- MIT %>% mutate(party=replace(party, candidate=='Ryan Greene', 'democrat'))

    # 'FL-24' had no general election, want to represent via giving 100 votes
  
MIT <- MIT %>% mutate(candidatevotes=replace(candidatevotes, candidate=='Frederica S. Wilson', 100))
MIT <- MIT %>% mutate(totalvotes=replace(totalvotes, candidate=='Frederica S. Wilson', 100))

    # Filter out candidates who weren't republican or democratic
MIT <- MIT %>% filter(party %in% c('republican','democrat'))

    # Filter out candidates who got very low votes
MIT <- MIT %>% mutate(percent_total = candidatevotes/totalvotes)
MIT <- MIT %>% filter(percent_total >.1)

    # Drop unusable columns
MIT_drop = c('stage','special','version','writein')
MIT <- MIT %>% select(-one_of(MIT_drop))

    # Filter for each of the 2012, 2014, and 2016 election data

MIT_2012 <- MIT %>% filter(year==2012)

candidates_to_remove <- c(42,65,131,75,89,130,77,92)

MIT_2012 <- MIT_2012[-candidates_to_remove,]

MIT_2012 <- MIT_2012 %>% 
  unite(.,'candidate_w_votes',candidate,candidatevotes,percent_total,sep="~%~") %>% 
  spread(., party, candidate_w_votes) %>%
  separate(., democrat, c('democrat_candidate','democrat_vote', 'democrat_vote_pot'),sep='~%~',remove=TRUE,extra='warn',fill='warn') %>% 
  separate(., republican, c('republican_candidate','republican_vote', 'republican_vote_pot'),sep='~%~',remove=TRUE,extra='warn',fill='warn')

MIT_2012$CD <- formatC(as.numeric(MIT_2012$district), width = 2, flag='0')

MIT_2012$CD <- paste(MIT_2012$state_po,MIT_2012$CD,sep='-')

MIT_2012$CD <- gsub('00','AL',MIT_2012$CD,fixed=TRUE) 

MIT_2012$CD <- gsub('98','AL',MIT_2012$CD,fixed=TRUE)

MIT_2014 <- MIT %>% filter(year==2014)

candidates_to_remove <- c(103,131,115,93,106,334,129,331,330)

MIT_2014 <- MIT_2014[-candidates_to_remove,]

MIT_2014 <- MIT_2014 %>% 
  unite(.,'candidate_w_votes',candidate,candidatevotes,percent_total,sep="~%~") %>% 
  spread(., party, candidate_w_votes) %>%
  separate(., democrat, c('democrat_candidate','democrat_vote', 'democrat_vote_pot'),sep='~%~',remove=TRUE,extra='warn',fill='warn') %>% 
  separate(., republican, c('republican_candidate','republican_vote', 'republican_vote_pot'),sep='~%~',remove=TRUE,extra='warn',fill='warn')

MIT_2014$CD <- formatC(as.numeric(MIT_2014$district), width = 2, flag='0')

MIT_2014$CD <- paste(MIT_2014$state_po,MIT_2014$CD,sep='-')

MIT_2014$CD <- gsub('00','AL',MIT_2014$CD,fixed=TRUE) 

MIT_2014$CD <- gsub('98','AL',MIT_2014$CD,fixed=TRUE)

MIT_2014_merge <- MIT_2014[,c('CD','democrat_candidate','democrat_vote','democrat_vote_pot','republican_candidate','republican_vote','republican_vote_pot')]
colnames(MIT_2014_merge) <- c('CD','democrat_candidate_2014','democrat_vote_2014','democrat_vote_pot_2014','republican_candidate_2014','republican_vote_2014','republican_vote_pot_2014')

MIT_2016 <- MIT %>% filter(year==2016)
MIT_2016[59,'candidate'] <- 'Tony Cardenas'
MIT_2016[45,'candidate'] <- 'Nanette Barragan'

    # Clean up candidates where mutliple candidates of same party are in general election.
    # Occurs for states with a 'top-two' primary system

candidates_to_remove <- c(62, 122, 95, 119, 101, 76, 331, 339, 788, 330, 327, 335, 324, 328)

MIT_2016 <- MIT_2016[-candidates_to_remove,]

  # Utilize the tidyr package to unite, spread, and separate data into one row per district format

MIT_2016 <- MIT_2016 %>% 
  unite(.,'candidate_w_votes',candidate,candidatevotes,percent_total,sep="~%~") %>% 
  spread(., party, candidate_w_votes) %>%
  separate(., democrat, c('democrat_candidate','democrat_vote', 'democrat_vote_pot'),sep='~%~',remove=TRUE,extra='warn',fill='warn') %>% 
  separate(., republican, c('republican_candidate','republican_vote', 'republican_vote_pot'),sep='~%~',remove=TRUE,extra='warn',fill='warn')


  #Create CD format to match DailyKos for merge

MIT_2016$CD <- formatC(as.numeric(MIT_2016$district), width = 2, flag='0')

MIT_2016$CD <- paste(MIT_2016$state_po,MIT_2016$CD,sep='-')

MIT_2016$CD <- gsub('00','AL',MIT_2016$CD,fixed=TRUE) 

MIT_2016$CD <- gsub('98','AL',MIT_2016$CD,fixed=TRUE)

  # Fix missing data

MIT_2016$democrat_candidate[MIT_2016$CD == 'AZ-08'] <- "Hiral Tipirneni"
MIT_2016$democrat_vote[MIT_2016$CD == 'AZ-08'] <- 82318
MIT_2016$democrat_vote_pot[MIT_2016$CD == 'AZ-08'] <- .4739
MIT_2016$republican_vote_pot[MIT_2016$CD == 'AZ-08'] <- .5261

MIT_2016$Party[MIT_2016$CD == 'PA-17'] <- '(D)'

#Merge MIT_2016 with MIT_2014_merge

MIT_2016 <- merge(MIT_2016,MIT_2014_merge, by='CD',all=TRUE) 

#Ensure vote columns are set to numeric

MIT_cols_to_numeric <- c("democrat_vote","democrat_vote_pot","republican_vote","republican_vote_pot","democrat_vote_2014","democrat_vote_pot_2014","republican_vote_2014","republican_vote_pot_2014")
MIT_2016[,MIT_cols_to_numeric] = apply(MIT_2016[,MIT_cols_to_numeric], 2, function(x) as.numeric(x))


# ACS Census Data

census <- acs_variables

    # Match states with districts and change format to match DailyKos

district_list = census[,'NAME']
district_numeric_vector = str_extract(district_list, "\\d+|(at Large)")
district_state_vector = str_extract(district_list, ", \\w+ ?\\w+ ?\\w+")  %>% 
  substring(.,3)

census <- census %>% mutate(state_name = district_state_vector)

state_name_factor <- as.factor(census$state_name)
census$state_abrev <- c(state.abb, 'DC')[match(state_name_factor, c(state.name, 'District of Columbia'))]

census$CD <- paste0(census$state_abrev,'-',census$congressional_district)

census$CD <- gsub("00","AL",census$CD, fixed=TRUE)

    # Remove Puerto Rico

census <- census[1:436,]


# Merge Datasets ####


# Before merging, check datasets against each other to make sure CD there for all

filter(MIT_2016, !(CD %in% census$CD))

filter(census, !(CD %in% MIT_2016$CD))

  # DC is not in MIT_2016 data... going to remove

filter(MIT_2016, !(CD %in% DK$CD))

filter(DK, !(CD %in% MIT_2016$CD))

filter(DK, !(CD %in% census$CD))

filter(census, !(CD %in% DK$CD))


  # Perform merge

CD_data <- merge(DK,MIT_2016,by='CD',all=FALSE) %>% 
  merge(.,census, by='CD',all=FALSE)

!complete.cases(CD_data$CD)

  # Clean final dataset of unecessary columns

CD_columns_remove <- c('office','year','state_cen','state_ic','state.y',
                       'state_po')


CD_data <- CD_data[ , -which(colnames(CD_data) %in% CD_columns_remove)]

rownames(CD_data) <- NULL

CD_data$democrat_vote_2014[is.na(CD_data$democrat_vote_2014)] <- 0
CD_data$republican_vote_2014[is.na(CD_data$republican_vote_2014)] <- 0
CD_data$democrat_vote_pot_2014[is.na(CD_data$democrat_vote_pot_2014)] <- 0
CD_data$republican_vote_pot_2014[is.na(CD_data$republican_vote_pot_2014)] <- 0

    # Now all datasets merged, and app now ready to build

# Tab 1: Map ####

  # Create axis columns

      # X-axis: Congressional Candidate Margin: Republican - Democrat

CD_data$margin_Trump <- CD_data$Trump - CD_data$Clinton
CD_data$margin_republican <- (CD_data$republican_vote_pot - CD_data$democrat_vote_pot) * 100 
CD_data$margin_republican_2014 <- (CD_data$republican_vote_pot_2014 - CD_data$democrat_vote_pot_2014) * 100 



CD_data$color[CD_data$Party.x == '(R)' & CD_data$margin_Trump > 0] <- 'House: R; Pres: Trump'
CD_data$color[CD_data$Party.x == '(R)' & CD_data$margin_Trump < 0] <- 'House: R; Pres: Clinton'
CD_data$color[CD_data$Party.x == '(D)' & CD_data$margin_Trump > 0] <- 'House: D; Pres: Trump'
CD_data$color[CD_data$Party.x == '(D)' & CD_data$margin_Trump < 0] <- 'House: D; Pres: Clinton'


# Map US Congressional Districts
# install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")
# 
# library(USAboundaries)
# 
# cont_48 <- state.name[state.name != c('Hawaii','Alaska')]
# 
# plot_states_data <- us_congressional(resolution = 'low',states=cont_48)
# dummy <- sample(1:30, nrow(plot_states_data), replace=T)
# 
# 
# plot_states_data$CD <- paste0(plot_states_data$state_abbr,"-",plot_states_data$cd115fp)
# plot_states_data$CD <- gsub("00","AL",plot_states_data$CD, fixed=TRUE)
# 
# 
# CD_scatter_map <- CD_data[ ,c('CD','Party.x','margin_Trump','Party.x','color')]
# 
# # Check for incomplete cases
# 
# CD_scatter_map[!complete.cases(CD_scatter_map),]
# 
# #merge datasets
# CD_scatter_map_merged <- merge(plot_states_data, CD_scatter_map, by='CD',all=FALSE) %>% 
#   st_transform("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") %>%
#   st_simplify(TRUE, dTolerance = 10000)
# 
# CD_scatter_map_merged$"2016 result breakdown" <- CD_scatter_map_merged$color
# 
# CD_scatter_ggplot <- ggplot(data = CD_scatter_map_merged) +
#   geom_sf(aes(fill=CD_scatter_map_merged$"2016 result breakdown")) +
#   scale_fill_manual(values=c("blue2", "#E69F00",'purple4','red2')) +
#   ggtitle("Breakdown Map of Congressional versus Presidential Winner in 2016 election") +
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
#                      legend.position = "top", legend.direction = "horizontal") + labs(fill = "Presidential/Congressional Winner Breakdown")




# Tab 2: Scatter & Sankey ####

CD_data <- CD_data[rowSums(is.na(CD_data))!=ncol(CD_data),]

test <- CD_data[CD_data$color == 'House: R; Pres: Clinton',]
d2014_rc2016 <-  CD_data[CD_data$color == 'House: R; Pres: Clinton' & CD_data$margin_republican_2014 < 0,]
r2014_rc2016 <-  CD_data[CD_data$color == 'House: R; Pres: Clinton' & CD_data$margin_republican_2014 > 0,]
d2014_dt2016 <-  CD_data[CD_data$color == 'House: D; Pres: Trump' & CD_data$margin_republican_2014 < 0,]
r2014_dt2016 <-  CD_data[CD_data$color == 'House: D; Pres: Trump' & CD_data$margin_republican_2014 > 0,]
d2014_rt2016 <-  CD_data[CD_data$color == 'House: R; Pres: Trump' & CD_data$margin_republican_2014 < 0,]
r2014_rt2016 <-  CD_data[CD_data$color == 'House: R; Pres: Trump' & CD_data$margin_republican_2014 > 0,]
d2014_dc2016 <-  CD_data[CD_data$color == 'House: D; Pres: Clinton' & CD_data$margin_republican_2014 < 0,]
r2014_dc2016 <-  CD_data[CD_data$color == 'House: D; Pres: Clinton' & CD_data$margin_republican_2014 > 0,]

CD_scatter <- CD_data[,c('margin_republican','margin_Trump')]
CD_scatter$repTrump <- ifelse(CD_scatter$margin_republican > 0 & CD_scatter$margin_Trump > 0, CD_scatter$margin_Trump, NA)
CD_scatter$repClinton <- ifelse(CD_scatter$margin_republican > 0 & CD_scatter$margin_Trump < 0, CD_scatter$margin_Trump, NA)
CD_scatter$demTrump <- ifelse(CD_scatter$margin_republican < 0 & CD_scatter$margin_Trump > 0, CD_scatter$margin_Trump, NA)
CD_scatter$demClinton <- ifelse(CD_scatter$margin_republican < 0 & CD_scatter$margin_Trump < 0, CD_scatter$margin_Trump, NA)
CD_scatter$margin_Trump <- NULL
CD_scatter$CD1.tooltip <- CD_data$CD
CD_scatter$CD2.tooltip <- CD_data$CD
CD_scatter$CD3.tooltip <- CD_data$CD
CD_scatter$CD4.tooltip <- CD_data$CD
CD_scatter <- CD_scatter[,c("margin_republican","repTrump","CD1.tooltip","repClinton","CD2.tooltip","demTrump","CD3.tooltip","demClinton","CD4.tooltip")]

# Create plot for GoogleVis

scatter_margin <- gvisScatterChart(CD_scatter,
                                   options = list(
                                     legend='right',
                                     pointSize= 1.75,
                                     title="House versus Presidential Margins in 2016 Election",
                                     colors= "['red','purple','orange','blue']",
                                     vAxis="{title: 'Presidential vote margin in favor of Trump'}",
                                     hAxis="{title: 'House vote margin in favor of Republican'}",
                                     width = 600,
                                     height = 500
                                   ))

  # Sankey

CD_Sankey <- data.frame(
  From = c(rep('2014 House: R',4),rep('2014 House: D',4)),
  To = c(rep(c('2016 House: R; Pres: Trump','2016 House: R; Pres: Clinton','2016 House: D; Pres: Trump','2016 House: D; Pres: Clinton'),2)),
  Weight = c(nrow(r2014_rt2016),nrow(r2014_rc2016),nrow(r2014_dt2016),nrow(r2014_dc2016),nrow(d2014_rt2016),nrow(d2014_rc2016),nrow(d2014_dt2016),nrow(d2014_dc2016)))
  


# Tab 3: Income ####

  # Name Columns

CD_data$income_Under10k <- CD_data$DP03_0052PE
CD_data$income_10to15k <- CD_data$DP03_0053PE
CD_data$income_15to25k <- CD_data$DP03_0054PE
CD_data$income_25to35k <- CD_data$DP03_0055PE
CD_data$income_35to50k <- CD_data$DP03_0056PE
CD_data$income_50to75k <- CD_data$DP03_0057PE
CD_data$income_75to100k <- CD_data$DP03_0058PE
CD_data$income_100to150k <- CD_data$DP03_0059PE
CD_data$income_150to200k <- CD_data$DP03_0060PE
CD_data$income_Over200k <- CD_data$DP03_0061PE

CD_data$income_median <- CD_data$DP03_0063E

  
  # Side-by-side Bar Chart
    
    # House: R; Pres: Trump 

CD_income_R_T = CD_data %>%
  select(c("CD","Trump","income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")) %>%
  filter(CD_data$color == "House: R; Pres: Trump")  %>%
  gather("income_level","income_percent_of_pop",3:12) %>%
  mutate(
    income_level = factor(income_level, levels = c("income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")))


CD_income_R_T_mean <- CD_income_R_T %>% group_by(income_level) %>% 
  summarise(R_T_mean_percent_of_pop = mean(income_percent_of_pop))

    # House: R; Pres: Clinton

CD_income_R_C = CD_data %>%
  select(c("CD","Trump","income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")) %>%
  filter(CD_data$color == "House: R; Pres: Clinton")  %>%
  gather("income_level","income_percent_of_pop",3:12) %>%
  mutate(
    income_level = factor(income_level, levels = c("income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")))


CD_income_R_C_mean <- CD_income_R_C %>% group_by(income_level) %>% 
  summarise(R_C_mean_percent_of_pop = mean(income_percent_of_pop))

    # House: D; Pres: Trump

CD_income_D_T = CD_data %>%
  select(c("CD","Trump","income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")) %>%
  filter(CD_data$color == "House: D; Pres: Trump")  %>%
  gather("income_level","income_percent_of_pop",3:12) %>%
  mutate(
    income_level = factor(income_level, levels = c("income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")))

CD_income_D_T_mean <- CD_income_D_T %>% group_by(income_level) %>% 
  summarise(D_T_mean_percent_of_pop = mean(income_percent_of_pop))


    # House: D; Pres: Clinton

CD_income_D_C = CD_data %>%
  select(c("CD","Trump","income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")) %>%
  filter(CD_data$color == "House: D; Pres: Clinton")  %>%
  gather("income_level","income_percent_of_pop",3:12) %>%
  mutate(
    income_level = factor(income_level, levels = c("income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k")))

CD_income_D_C_mean <- CD_income_D_C %>% group_by(income_level) %>% 
  summarise(D_C_mean_percent_of_pop = mean(income_percent_of_pop))
  
    # Stack bar charts side by side

income_mean <- merge(CD_income_R_T_mean,CD_income_R_C_mean,by='income_level') %>% 
  merge(.,CD_income_D_T_mean, by= 'income_level') %>% 
  merge(.,CD_income_D_C_mean, by= "income_level") %>% 
  mutate(
    income_level = factor(income_level, levels = c("income_Under10k","income_10to15k","income_15to25k","income_25to35k","income_35to50k","income_50to75k","income_75to100k","income_100to150k","income_150to200k","income_Over200k"))) 

colnames(income_mean) <- c('income_level',' "House: R; Trump"',' "House: R; Clinton"',' "House: D; Trump"',' "House: D; Clinton"')
income_mean <- income_mean[c(10,2,4,5,6,7,8,1,3,9),]
rownames(income_mean) <- NULL

  # Mean Income scatterplot 

CD_income_scatter <- CD_data[,c('income_median','margin_republican','margin_Trump')]
CD_income_scatter$repTrump <- ifelse(CD_income_scatter$margin_republican > 0 & CD_income_scatter$margin_Trump > 0, CD_income_scatter$margin_Trump, NA)
CD_income_scatter$repClinton <- ifelse(CD_income_scatter$margin_republican > 0 & CD_income_scatter$margin_Trump < 0, CD_income_scatter$margin_Trump, NA)
CD_income_scatter$demTrump <- ifelse(CD_income_scatter$margin_republican < 0 & CD_income_scatter$margin_Trump > 0, CD_income_scatter$margin_Trump, NA)
CD_income_scatter$demClinton <- ifelse(CD_income_scatter$margin_republican < 0 & CD_income_scatter$margin_Trump < 0, CD_income_scatter$margin_Trump, NA)
CD_income_scatter$margin_Trump <- NULL
CD_income_scatter$CD1.tooltip <- CD_data$CD
CD_income_scatter$CD2.tooltip <- CD_data$CD
CD_income_scatter$CD3.tooltip <- CD_data$CD
CD_income_scatter$CD4.tooltip <- CD_data$CD
CD_income_scatter$margin_republican <- NULL
CD_income_scatter <- CD_income_scatter[,c("income_median","repTrump","CD1.tooltip","repClinton","CD2.tooltip","demTrump","CD3.tooltip","demClinton","CD4.tooltip")]


# Tab 4: Education ####

  # Name Columns

CD_data$'No Degree' <- CD_data$DP02_0059PE + CD_data$DP02_0060PE
CD_data$'High School' <- CD_data$DP02_0061PE +  CD_data$DP02_0062PE
CD_data$'Bachelors/Associates' <- CD_data$DP02_0063PE +  CD_data$DP02_0064PE
CD_data$'Professional' <- CD_data$DP02_0065PE

  # Side by Side Bar Chart

    # House: R; Pres: Trump

CD_education_R_T = CD_data %>% 
  select(c('CD','Trump','No Degree','High School','Bachelors/Associates','Professional')) %>% 
  filter(CD_data$color == "House: R; Pres: Trump")  %>%
  gather('education_level','education_percent_of_pop', 3:6) %>% 
  mutate(
    education_level = factor(education_level, levels = c('No Degree','High School','Bachelors/Associates','Professional')))

CD_education_R_T_mean <- CD_education_R_T %>% group_by(education_level) %>% 
  summarise(mean_percent_of_pop = mean(education_percent_of_pop))

    # House: R; Pres: Clinton

CD_education_R_C = CD_data %>% 
  select(c('CD','Trump','No Degree','High School','Bachelors/Associates','Professional')) %>% 
  filter(CD_data$color == "House: R; Pres: Clinton")  %>%
  gather('education_level','education_percent_of_pop', 3:6) %>% 
  mutate(
    education_level = factor(education_level, levels = c('No Degree','High School','Bachelors/Associates','Professional')))

CD_education_R_C_mean <- CD_education_R_C %>% group_by(education_level) %>% 
  summarise(mean_percent_of_pop = mean(education_percent_of_pop))

    # House: D; Pres: Trump

CD_education_D_T = CD_data %>% 
  select(c('CD','Trump','No Degree','High School','Bachelors/Associates','Professional')) %>% 
  filter(CD_data$color == "House: D; Pres: Trump")  %>%
  gather('education_level','education_percent_of_pop', 3:6) %>% 
  mutate(
    education_level = factor(education_level, levels = c('No Degree','High School','Bachelors/Associates','Professional')))

CD_education_D_T_mean <- CD_education_D_T %>% group_by(education_level) %>% 
  summarise(mean_percent_of_pop = mean(education_percent_of_pop))

    # House: D; Pres: Clinton

CD_education_D_C = CD_data %>% 
  select(c('CD','Trump','No Degree','High School','Bachelors/Associates','Professional')) %>% 
  filter(CD_data$color == "House: D; Pres: Clinton")  %>%
  gather('education_level','education_percent_of_pop', 3:6) %>% 
  mutate(
    education_level = factor(education_level, levels = c('No Degree','High School','Bachelors/Associates','Professional')))

    # Stack bar charts side by side

CD_education_D_C_mean <- CD_education_D_C %>% group_by(education_level) %>% 
  summarise(mean_percent_of_pop = mean(education_percent_of_pop))

# Tab 5: Race ####

  # Name Columns

CD_data$White <- CD_data$DP05_0059PE
CD_data$Black <- CD_data$DP05_0060PE
CD_data$Native <- CD_data$DP05_0061PE
CD_data$Asian <- CD_data$DP05_0062PE
CD_data$Latino <- CD_data$DP05_0066PE

  # Side-by-side bar charts

    # House: R; Pres: Trump

CD_race_R_T = CD_data %>% 
  select(c('CD','Trump','White','Black','Native','Asian','Latino')) %>% 
  filter(CD_data$color == "House: R; Pres: Trump")  %>%
  gather('race_group','race_percent_of_pop', 3:7)

CD_race_R_T_mean <- CD_race_R_T %>% group_by(race_group) %>% 
  summarise(mean_percent_of_pop = mean(race_percent_of_pop))

    # House: R; Pres: Clinton

CD_race_R_C = CD_data %>% 
  select(c('CD','Trump','White','Black','Native','Asian','Latino')) %>% 
  filter(CD_data$color == "House: R; Pres: Clinton")  %>%
  gather('race_group','race_percent_of_pop', 3:7)

CD_race_R_C_mean <- CD_race_R_C %>% group_by(race_group) %>% 
  summarise(mean_percent_of_pop = mean(race_percent_of_pop))

    # House: D; Pres: Trump

CD_race_D_T = CD_data %>% 
  select(c('CD','Trump','White','Black','Native','Asian','Latino')) %>% 
  filter(CD_data$color == "House: D; Pres: Trump")  %>%
  gather('race_group','race_percent_of_pop', 3:7)

CD_race_D_T_mean <- CD_race_D_T %>% group_by(race_group) %>% 
  summarise(mean_percent_of_pop = mean(race_percent_of_pop))

    # House: D; Pres: Clinton

CD_race_D_C = CD_data %>% 
  select(c('CD','Trump','White','Black','Native','Asian','Latino')) %>% 
  filter(CD_data$color == "House: D; Pres: Clinton")  %>%
  gather('race_group','race_percent_of_pop', 3:7)

    # Stack bar charts side by side 

CD_race_D_C_mean <- CD_race_D_C %>% group_by(race_group) %>% 
  summarise(mean_percent_of_pop = mean(race_percent_of_pop))



