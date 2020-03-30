#Author : Navya Mote
#Date   : 10/13/2019
#Title  : Healthy Places, Healthy Lives
#---------------------------------------------------------------------------------------------
#Libraries
library(expss)
library(dplyr)
library(data.table)
library(openxlsx)
library(knitr)
# library(kableExtra)
library(DT)
library(grDevices)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(maps)
library(leaflet)
library(tidycensus)
library(randomcoloR)
library(reshape2)
# setwd("C:/Users/Navya/Desktop/HPHL_1/HPHL")
census_api_key("674be763fef3c5492edcc4fc10c59f4b437f2958", overwrite=TRUE)
state <- get_acs(state = "VA",
                 # county = "Henrico",
                 geography = "county",
                 variables = c(medincome = "B19013_001"),
                 year = 2016,
                 geometry = TRUE)
countygrp<-read.csv("County_LHD_Group.csv",fileEncoding="UTF-8-BOM")
data_input<-merge(state, countygrp, by=c("NAME"))
data_input <- data_input[with(data_input, order(Group)), ]
data_input$Group <- as.factor(data_input$Group)

factpal <- colorFactor(c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', 
                         '#808000', '#ffd8b1', '#000075', '#808080'), data_input$Group)
# -------------------------------------------------------------------------------------------------
master<-read.csv("Master_HDL.csv",fileEncoding="UTF-8-BOM",check.names = FALSE)
leb<-read.csv("LEB_Regression.csv",fileEncoding="UTF-8-BOM",check.names = FALSE)
# library(Hmisc) # cut2
# library(latticeExtra)
master <- master[order(master$HOI),]
qs <- quantile(master$HOI, seq(0, 1, length.out = 6))
master$hoi_grp <- as.numeric(cut(master$HOI, unique(qs), include.lowest = TRUE, na.rm = TRUE))
master_leb<-merge(master, leb, by=c("Group_LHD"))
# write.csv(master_leb, "mastest.csv")
data_mas<-group_by(master,hoi_grp)
df_mas<-summarise(data_mas, Mean = round(mean(LEB_Imputed),digits =1))
a <- c("All Census Tracts", round(mean(master$LEB_Imputed),digits = 1),"High Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==4],digits =1),"Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==2],digits =1))
b <- c("Very High Opportunity Tracts", round(df_mas$Mean[df_mas$hoi_grp==5],digits =1),"Average Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==3],digits =1),"Very Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==1],digits =1))
df_first<- data.frame(a,b)
# print(df_first)
data_mas_leb<-group_by(master,Group_LHD)
df_mas_leb<-summarise(data_mas_leb, Mean_leb = round(mean(LEB_Imputed),1))
df_dist<-distinct(master,Group_LHD, `Access to Care`, `Employment Access`, Affordability,
                  `Air Quality`, `Population Churning`, Education, `Food Accessibility`,
                  `Income Inequality`, `Job Participation`, `Population Density`,
                  `Spatial Segregation`, `Material Deprevation`, Walkability)
df_dist<-melt(data = df_dist, id.vars = "Group_LHD")
df_dist<-df_dist[with(df_dist, order(Group_LHD, -value)), ]
df_sub<-subset(df_dist, Group_LHD == first(data_input$Group))
df_head<-head(df_sub,5)
dropdwn2 <-unique(df_head$variable)
levels(dropdwn2) <- c(levels(dropdwn2), "Overall Health Opportunity Index")    # add new level
dropdwn2[6] <- "Overall Health Opportunity Index"
# master_2 <- subset(master_leb, Group_LHD == first(data_input$Group))
master_2 = master_leb
master_2$Access_to_Healthcare_Index_new = (master_2$Access_to_Healthcare_Index - mean(master_2$Access_to_Healthcare_Index))/sd(master_2$Access_to_Healthcare_Index)
master_2$Access_to_Employment_Index_new = (master_2$Access_to_Employment_Index - mean(master_2$Access_to_Employment_Index))/sd(master_2$Access_to_Employment_Index)
master_2$Affordability_Index_new = (master_2$Affordability_Index - mean(master_2$Affordability_Index))/sd(master_2$Affordability_Index)
master_2$Air_Quality_Index_new = (master_2$Air_Quality_Index - mean(master_2$Air_Quality_Index))/sd(master_2$Air_Quality_Index)
master_2$Pop_Churning_Index_new = (master_2$Pop_Churning_Index - mean(master_2$Pop_Churning_Index))/sd(master_2$Pop_Churning_Index)
master_2$Education_Index_new = (master_2$Education_Index - mean(master_2$Education_Index))/sd(master_2$Education_Index)
master_2$Food_Access_Index_new = (master_2$Food_Access_Index - mean(master_2$Food_Access_Index))/sd(master_2$Food_Access_Index)
master_2$Income_Inequality_Index_new = (master_2$Income_Inequality_Index - mean(master_2$Income_Inequality_Index))/sd(master_2$Income_Inequality_Index)
master_2$Job_Participation_Index_new = (master_2$Job_Participation_Index - mean(master_2$Job_Participation_Index))/sd(master_2$Job_Participation_Index)
master_2$Pop_Density_Index_new = (master_2$Pop_Density_Index - mean(master_2$Pop_Density_Index))/sd(master_2$Pop_Density_Index)
master_2$Segregation_Index_new = (master_2$Segregation_Index - mean(master_2$Segregation_Index))/sd(master_2$Segregation_Index)
master_2$Townsend_Index_new = (master_2$Townsend_Index - mean(master_2$Townsend_Index))/sd(master_2$Townsend_Index)
master_2$Walkability_Index_new = (master_2$Walkability_Index - mean(master_2$Walkability_Index))/sd(master_2$Walkability_Index)
master_2$HOI_new<- (master_2$Access_to_Healthcare_Index_new * master_2$`Access to Care`) +
  (master_2$Access_to_Employment_Index_new * master_2$`Employment Access`) +
  (master_2$Affordability_Index_new * master_2$Affordability) +
  (master_2$Air_Quality_Index_new * master_2$`Air Quality`) +
  (master_2$Pop_Churning_Index_new * master_2$`Population Churning`) +
  (master_2$Education_Index_new * master_2$Education) +
  (master_2$Food_Access_Index_new * master_2$`Food Accessibility`) +
  (master_2$Income_Inequality_Index_new * master_2$`Income Inequality`) +
  (master_2$Job_Participation_Index_new * master_2$`Job Participation`) +
  (master_2$Pop_Density_Index_new * master_2$`Population Density`) +
  (master_2$Segregation_Index_new * master_2$`Spatial Segregation`) +
  (master_2$Townsend_Index_new * master_2$`Material Deprevation`) +
  (master_2$Walkability_Index_new * master_2$Walkability)
# print(slider_data)
master_2$LEB_new<- (master_2$HOI_new * master_2$Slope) + master_2$Intercept
master_2$residual <- master_2$LEB_Imputed - master_2$LEB_new
master_2$Intercept = master_2$Intercept + master_2$residual
month<-as.numeric(format(as.Date(Sys.Date(), "%Y-%m-%d"), "%m"))
year<-as.numeric(format(as.Date(Sys.Date(), "%Y-%m-%d"), "%Y"))
up_dt<-paste(month,year, sep ="/")
updated_dt<-paste("Last updated on", up_dt, sep = " ")
footer<-paste ("Created on 2/2020", updated_dt, sep = ", ", collapse = NULL)
dropdwn <-unique(data_input$Group)
levels(dropdwn) <- c(levels(dropdwn), "All of Virginia")    # add new level
dropdwn[21] <- "All of Virginia"
dropdwn<-ordered(dropdwn)
# dropdwn<-sort(dropdwn)
# print(dropdwn)
# -----------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel(title=(div(tags$em("Healthy Places, Healthy Lives"), img(src = "vdh.jpg",class = "pull-right"),img(src = "wh.jpg",class = "pull-right"), img(src = "Official OHE Letterhead1.jpg",class = "pull-right"))), windowTitle = "Healthy Places, Healthy Lives"),
  # Main panel for displaying outputs ----
  div(tabsetPanel(tabPanel("About", br(), tags$b(tags$em(textOutput("txtOutput_about1"))),
                           br(),
                           textOutput("txtOutput_about2"),
                           br(),
                           uiOutput("txtOutput_about3"),
                           br(),
                           uiOutput("txtOutput_about4"),
                           br(),
                           uiOutput("txt_about5")),
                  tabPanel("Explore the Data", br(),
                           textOutput("explore"), br(),
                           uiOutput("explore1"), br(),
                           textOutput("explore2"), br(),
                           uiOutput("exp2"), br(),
                           splitLayout(sidebarPanel(leafletOutput("mapPlot", width = "100%"), width = "100%"),
                                       
                                       
                                       # ),
                                       tags$div(sidebarPanel(selectInput("cty", "Local Health District", unique(dropdwn), selected = dropdwn[21], multiple = FALSE, width = "100%"),
                                                             
                                                             
                                                             tags$b(textOutput("txtOutput_2"), align = "center"),
                                                             tags$strong(tableOutput('table')),
                                                             # tags$strong(tableOutput('table1')),
                                                             # tags$strong(tableOutput('table2')),
                                                             width = "100%"))),
                           # )
                           
                           # )),
                           conditionalPanel(condition = "input.cty != 'All of Virginia'", uiOutput("exp4")),
                           br(),
                           conditionalPanel(condition = "input.cty != 'All of Virginia'", uiOutput("exp3")),
                           br(),
                           conditionalPanel(condition = "input.cty != 'All of Virginia'", splitLayout(sidebarPanel(selectInput("ind", "Choose an Indicator :", unique(dropdwn2), selected = dropdwn2[6], multiple = FALSE, width = "100%"),
                                                                                                                   leafletOutput("mapPlot1", width = "100%"), width = "100%"),
                  
                                       # tableOutput('table4'),
                                       tags$div(sidebarPanel(
                                         tags$b(textOutput("txtOutput_3"), align = "center"),
                                         tags$strong(tableOutput('exp_val')),
                                         textOutput("txtOutput"),
                                         br(),
                                        sliderInput("slider1", label = nth(df_head$variable, 1), min = -25, 
                                                     max = 25, value = 0, post = "%", ticks = FALSE, step = 1),
                                         sliderInput("slider2", label = nth(df_head$variable, 2), min = -25, 
                                                     max = 25, value = 0, post = "%", ticks = FALSE),
                                         sliderInput("slider3", label = nth(df_head$variable, 3), min = -25, 
                                                     max = 25, value = 0, post = "%", ticks = FALSE),
                                         sliderInput("slider4", label = nth(df_head$variable, 4), min = -25, 
                                                     max = 25, value = 0, post = "%", ticks = FALSE),
                                         sliderInput("slider5", label = nth(df_head$variable, 5), min = -25, 
                                                     max = 25, value = 0, post = "%", ticks = FALSE),width = "100%"
                                         )
                                       )),
                           )),
                  
                  # leafletOutput("mapPlot1", width = 800, height = 500)),
                  tabPanel(title = HTML("Small-Area Life<br/>Expectancy Estimates"), br(), uiOutput("txtOutput_sm1"), br(), uiOutput("txtOutput_sm2"),br(),uiOutput("txtOutput_sm3")),
                  tabPanel("Health Opportunity Index", br(), uiOutput("txtOutput_hoi1"), br(), textOutput("txtOutput_hoi2"), br(), textOutput("txtOutput_hoi3"),
                           br(), uiOutput("txtOutput_hoi4"), br(), uiOutput("txtOutput_hoi5"), br(), uiOutput("txtOutput_hoi6"),
                           br(), uiOutput("txtOutput_hoi7")),
                  tabPanel("Solutions", br(),uiOutput("sol1"), br(),uiOutput("sol2")),
                  # tabPanel("Methodology", DT::dataTableOutput("mytable6")),
                  tabPanel("Contacts", uiOutput("contact")),
                  selected = "Explore the Data"
  ),
  hr(),
  print(footer), textOutput("counter")
  )
)
server <- function(input, output, session) {
  # ---------------------------------------
  # output$table3 <- renderTable(df_fin3, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
  # output$table5 <- renderTable(df_fin4, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
  # output$table6 <- renderTable(df_fin5, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
  print("test")
  output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in ",input$cty,sep=" ")})
  output$txtOutput_3 = renderText({ paste0("Expected Average Life Expectancy in ",input$cty,sep=" ")})
  output$counter<-renderText({
    if(!file.exists("counter.Rdata"))
      counter<-0
    else
      load(file = "counter.Rdata")
    counter<- counter + 1
    save(counter, file = "counter.Rdata")
    paste0("No. of visits:", counter)
  })
  output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "100%",align = 'c', bordered = TRUE, digits = 1)
  # output$table1 <- renderTable(df_fin1, striped = TRUE, bordered = TRUE, width = "600", align = 'c', digits = 1)
  # output$table2 <- renderTable(df_fin2, striped = TRUE, bordered = TRUE, width = "600", align = 'c', digits = 1)
  output$txtOutput = renderText({
    "Adjust these factors to see the expected average change in life expectancy :"
  })
  output$txtOutput_about1 = renderText({
    "Healthy Places, Healthy Lives"
  })
  output$txtOutput_about2 = renderText({
    "For many Americans the neighborhood in which they live has a significant impact on their opportunity for optimal health. However, most measures of community characteristics and health outcomes are made at the county or metro level. These often hide vulnerable populations, diluting their impact in mapping and analysis."
  })
  output$txtOutput_about3 = renderUI(HTML(paste(
    em("Healthy Places, Healthy Lives")," bridges this gap by combining robust, Census Tract-level, measures into a single visualization. For health outcomes, the visualization uses life expectancy estimates created by the United States Small-Area Life Expectancy Estimates Project (USALEEP). For neighborhood characteristics the visualization incorporates  the Health Opportunity Index (HOI), a Census Tract-level index comprising 13 Social Determinant of Health (SDOH) indicators, created by the Virginia Department of Health, Office of Health Equity."
  )))
  output$txtOutput_about4 = renderUI(HTML(paste(
    "Using Weighted-Quantile Squares regression performed by partners at the Virginia Commonwealth University Department of Biostatistics, the visualization identifies the HOI indicators that have the most impact on life expectancy regionally, and the expected effect changes in these indicators may have on life expectancy. The goal of the project is to assist local communities to identify programs and practices likely to improve life expectancy and the neighborhoods in which these programs will have the largest impact. Importantly, it will demonstrate how targeting disparities and the SDOH at the neighborhood level is the best approach to improving health outcomes."
  ,"</br>","</br>","<b>Healthy Places, Healthy Lives Regions</b>","</br>","</br>","The Healthy Places, Healthy Lives regions are based on",
  "<u><a href='http://www.vdh.virginia.gov/local-health-districts/'>Virginia's 35 Local Health Districts.</a></u>","Some of these health districts were combined to provide a large enough area or population for analysis, resulting in 18 Healthy Places, Healthy Lives regions:",
  "</br>","</br>","<li><b>Alleghany-Roanoke</b></li>","Alleghany","</br>", "Roanoke","</br>",
  "<li><b>Central Shenandoah</b></li>","<li><b>Central Virginia</b></li>","<li><b>Chesapeake</b></li>",
  "Eastern Shore","</br>","Three Rivers","</br>","<li><b>Greater Richmond</b></li>",
  "Chesterfield","</br>","Henrico","</br>","Richmond City","<li><b>Hampton Roads</b></li>",
  "Chesapeake","</br>","Hampton","</br>","Norfolk","</br>","Portsmouth","</br>","Virginia Beach",
  "</br>","<li><b>Lord Fairfax</b></li>","<li><b>New River</b></li>","<li><b>Northern Virginia</b></li>",
  "Alexandria","</br>","Arlington","</br>","Fairfax","</br>","<li><b>Loudoun</b></li>",
  "<li><b>Peninsula</b></li>","<li><b>Piedmont-Southside</b></li>",
  "Piedmont","</br>","Southside","</br>","<li><b>Pittsylvania-Danville</b></li>","<li><b>Prince William</b></li>",
  "<li><b>Southwest Virginia</b></li>","Cumberland-Plateau","</br>","Lenowisco","</br>",
  "Mount Rogers","</br>","<li><b>Thomas Jefferson</b></li>","<li><b>Upper Rappahannock River</b></li>",
  "Rappahannock","</br>","Rappahannock Rapidan","</br>","<li><b>West Piedmont</b></li>",
  "<li><b>Western Tidewater</b></li>")))
  output$txt_about5 = renderUI(HTML(paste("<b>Acknowledgement of Support</b>","</br>",
                                          "This project is based upon work supported by the Urban Institute through funds provided by the Robert Wood Johnson Foundation. We thank them for their support but acknowledge that the findings and conclusions presented in these results are those of the author(s) alone, and do not necessarily reflect the opinions of the Urban Institute or the Robert Wood Johnson Foundation.")))
  output$txtOutput_sm1 = renderUI(HTML(paste(
    "The United States Small-Area Life Expectancy Estimates Project (USALEEP) is the first public health outcome measure available nationwide at the census tract-level measuring life expectancy at birth for nearly every census tract in the country. A joint effort of",
    "<a href='https://www.rwjf.org/en/library/interactives/whereyouliveaffectshowlongyoulive.html'>The Robert Wood Johnson Foundation (RWJF),</a>",
    "<a href='https://www.naphsis.org/usaleep'>National Association for Public Health Statistics and Information Systems (NAPHSIS),</a>"," and the",
    "<a href='https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html'>National Center for Health Statistics (NCHS) at the Centers for Disease Control (CDC),</a>"," USALEEP data provide unparalleled insights into community health and demonstrate that not everyone has the same opportunity to be healthy where they live."
  )))
  output$txtOutput_sm2 = renderUI(HTML(paste(
    "For more information visit: ",
    "<a href='https://www.naphsis.org/usaleep'>naphsis.org/usaleep</a>")))
  output$txtOutput_sm3 = renderUI(HTML(paste(
    "For details on how these census tract level estimates were developed, see:","</br>",
    "Arias E, Escobedo LA, Kennedy J, Fu C, Cisewski J. U.S.,","<a href='https://www.cdc.gov/nchs/data/series/sr_02/sr02_181.pdf'>Small-area Life Expectancy Estimates Project: Methodology and Results Summary.</a>","National Center for Health Statistics. Vital Health Stat 2(181). 2018."
  )))
  output$txtOutput1 = renderText({
    "Average Life Expectancy in Virginia Census Tracts"
  })
  output$txtOutput_hoi1 = renderUI(HTML(paste(
    "The VDH Office of Health Equity developed the",
    "<a href='https://www.vdh.virginia.gov/omhhe/hoi/'>Virginia Health Opportunity Index (HOI)</a>",
    "to identify vulnerable populations at the Census Tract level and to help communities, providers and policymakers understand how neighborhood level factors influence the opportunity residents have to live long and healthy lives. Originally developed for internal use at the VDH, the HOI has been embraced by a broad array of stakeholders in Virginia and has been replicated in Ohio. The HOI can help communities examine their health opportunity landscape, target limited resources and, most importantly, begin conversations around health and public policy."
  )))
  output$txtOutput_hoi2 = renderText({
    "The Virginia HOI is a group of indicators that provide broad insight into the overall opportunity Virginians have to live long and healthy lives based on the Social Determinants of Health. It is a hierarchical index that allows users to examine social determinants of health at multiple levels of detail in Virginia. It is made up of over 30 variables, combined into 13 indicators, grouped into four profiles, which are aggregated into a single Health Opportunity Index. The HOI is reported at both the census tract and county/independent city level."
  })
  output$txtOutput_hoi3 = renderText({
    "The HOI is a composite measure of the Social Determinants of Health (the social, economic, educational, demographic, and environmental factors that relate to a community's well-being and the health status of a population). It is comprised of 13 indicators, organized into four profiles, which reflect a broad array of social determinants of health."
  })
  output$txtOutput_hoi4 = renderUI(HTML(paste(
    "<b>The Community Environmental Profile:</b>", "<br/>",
    
    "(1) Air Quality", "<br/>",
    
    "(2) Population Churning", "<br/>",
    
    "(3) Population Weighted Density", "<br/>",
    
    "(4) Walkability"
  )))
  output$txtOutput_hoi5 = renderUI(HTML(paste(
    "<b>The Consumer Opportunity Profile:</b>", "<br/>",
    
    "(5) Affordability", "<br/>",
    
    "(6) Education", "<br/>",
    
    "(7) Food Accessibility", "<br/>",
    
    "(8) Material Deprivation"
  )))
  output$txtOutput_hoi6 = renderUI(HTML(paste(
    "<b>The Economic Opportunity Profile:</b>", "<br/>",
    
    "(9) Employment Access", "<br/>",
    
    "(10) Income Inequality", "<br/>",
    
    "(11) Job Participation"
  )))
  output$txtOutput_hoi7 = renderUI(HTML(paste(
    "<b>The Wellness Disparity Profile:</b>", "<br/>",
    
    "(12) Access to Care", "<br/>",
    
    "(13) Spatial Segregation"
  )))
  output$explore = renderText({
    "Although individuals may have different experiences, the opportunity to live a long and healthy life can vary widely depending on where you live. Most people in Northern Virginia, for instance, live in areas where economic and consumer resources, the built environment and access to care provide residents with ample opportunity to be healthy. In other areas of Virginia, however, people may face additional challenges living healthy lives and making healthy choices."
  })
  output$explore1 = renderUI(HTML(paste(
    "Life expectancy at birth is one of the best measures of overall health outcomes. Use the dashboards below to explore the impact of place on life expectancy at birth within different regions of Virginia. These dashboards compare",
    "<a href='https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_13'>Census Tract</a>", "level life expectancy estimates, provided by the CDC, to the",
    "<a href='https://www.vdh.virginia.gov/omhhe/hoi/what-is-the-hoi'>Virginia Health Opportunity Index</a>",", a 13-indicator index of the social and economic factors that have a strong influence on health outcomes."
  )))
  output$explore2 = renderText({
    "The table below shows the average life expectancy for all Census Tracts in Virginia, and for Census Tracts categorized by opportunity level as determined by the Health Opportunity Index.  Average life expectancy differs by six-years between the highest opportunity tracts and the lowest opportunity tracts in Virginia."
  })
  output$exp2 = renderUI(HTML(paste("Use the map or the drop down menu to select a Healthy Places, Healthy Lives (HPHL) region. HPHL regions are based on Virginia's Local Health Districts, but have been combined in some cases for analysis. Once selected, the table below will show the average life expectancy for all Census Tracts in the HPHL region, and for Census Tracts categorized by opportunity level, while the map shows the opportunity level by Census Tract.",
                                    "<br/>",
                                    "<br/>")))
  output$exp4 = renderUI(HTML(paste("The slider bars below represent the five HOI factors that have the most impact on life expectancy in the selected HPHL region. These factors vary by region. Use these sliders to explore how changes in these factors may affect life expectancy in your region. The solutions tab provides a list of potential partners and best practices to improve each of the 13 HOI indicators."
  )))
  output$exp3 = renderUI(HTML(paste("Moving the sliders causes equivalent changes in each Census Tract regardless of opportunity level. Targeting resources at Census Tracts with lower opportunity levels associated with each factor is the most cost effective way to improve life-expectancy overall in your region. Use the dropdown menu above the map to show the opportunity level by factor in each Census Tract."
  )))
  output$sol1 = renderUI(HTML(paste("The list below includes a variety of organizations, resources and best practices that may be available to assist your community in improving the Health Opportunity Index indicators affecting health in your community. This list is by no means comprehensive. If you are aware of other Virginia resources, please let us know.",
                                    "</br>","</br>", "<i>Disclaimer: These resources are listed for informational purposes only. Neither the Virginia Department of Health, Office of Health Equity nor other project partners are directly affiliated with any resource or organization. Listing does not indicate endorsement of any organization, resource or best practice.</i>",
                                    "</br>","</br>","<b>Cross-Cutting Resources</b>", "</br>","</br>",
                                    "<u><a href= 'https://www.collectiveimpactforum.org/'>Collective Impact Forum</a></u>","</br>",
                                    "Too many organizations are working in isolation from one another. Collective impact brings people together, in a structured way, to achieve social change. The Collective Impact Forum exists to support the efforts of those who are practicing collective impact in the field. While the rewards of collective impact can be great, the work is often demanding. Those who practice it must keep themselves and their teams motivated and moving forward.",
                                    "</br>","</br>","<u><a href = 'https://ofn.org/cdfi-locator'>Community Development Financial Institutions (CDFIs)</a></u>"," are specialized financial institutions operating in markets that are underserved by traditional financial institutions. This section highlights the Richmond Fed's work on CDFIs and includes a dedicated section on CDFI banks, also known as Community Development Banks (CDBs).",
                                    "</br>","</br>","<u><a href='http://www.communityfoundationsva.org/'>Community Foundations of Virginia</a></u>",
                                    " serve residents of the Commonwealth who want to invest in their communities. Through community foundations caring citizens give, volunteer, network and learn. Community foundations also bring donors, nonprofits, governments and businesses together to solve problems.",
                                    "</br>","</br>","<u><a href='https://www.vacommunitycapital.org/'>Virginia Community Capital (VCC)</a></u>",
                                    "</br>","VCC was established in 2006 as a Community Development Financial Institution (CDFI). The 
                                    goal was to leverage that initial investment for an economic return to underserved areas. Through the power of partnerships and leverage,VCC has turned that $15 million seed investment into over $900 million of impact in Virginia. Over the years, VCC has become known for its ability to combine the resources of national, state, and local social investors into successfully funded projects.",
                                    "</br>","</br>","<u><a href='https://211.getcare.com/consumer/index.php'>The Virginia Department of Social Services (VDSS)</a></u>","fosters community collaboration by providing services and sponsoring various grants, funds, programs and initiatives throughout the Commonwealth. Whether you need help or want to provide help, 2-1-1 is the fast, free and confidential way to locate hundreds of services in your community."
                                    ,"</br>","</br>","<b>Air Quality:</b>","Includes EPA measures of air pollution, including on-road, non-road and nonpoint pollution, and EPA measures of neurological, cancer and respiration risk.",
                                    "</br>","</br>","<u><a href='https://environmentvirginia.org/'>Environment Virginia</a></u>","</br>",
                                    "With Environment Virginia, we protect the places that all of us love and promote core environmental values, such as clean air to breathe, clean water to drink, and clean energy to power our lives. Environment Virginia focuses on timely, targeted action that wins tangible improvements in the quality of our environment and our lives.",
                                    "</br>","</br>","<b>Best Practices</b>","</br>","Use the ","<a href='http://www.drpt.virginia.gov/commuter-programs/major-initiatives/bike-to-work-week/bike-share-services/'>bike-sharing programs</a>"," if your city or town has them.","</br>","Take ","<a href='https://www.apta.com/research-technical-resources/public-transportation-links/virginia/#a2'>public transit</a>",
                                    " when possible.","</br>","Use ","<a href='http://rideshareinfo.org/'>ride-sharing</a>"," services if your city or town has them.", "</br>", "</br>",
                                    "<b>Population Churning:</b>"," The amount of population turnover within a community. It measures the rate at which people both move into a community and move out of a community. Specific solutions include building social capital and assisting community members through periods of unemployment.","</br>","</br>",
                                    "<u><a href='https://www2.deloitte.com/content/dam/Deloitte/us/Documents/about-deloitte/us-community-guide-engage-your-community.pdf'>Engage Your Community: A Local Guide to Addressing the Long-Term Unemployment Challenge</a></u>"," was created for community leaders - in the public, private or non-profit sectors - to mobilize your community to decrease long-term unemployment. 
                                    It includes actionable steps to understand your community-specific LTU challenges, identify key stakeholders, and lead efforts to create and implement sustainable solutions."
  )))
  output$sol2 = renderUI(HTML(paste("<u><a href='https://www.a2ru.org/wp-content/uploads/2018/10/skbuildingtoolkitversion1.2.pdf'>Social Capital Building Toolkit</a></u>","</br>"," A central challenge for those desiring more local social capital is how to build it. The goal of this Toolkit is to briefly describe the social capital concept and its dimensions, and then outline and illustrate some effective ways 
                                    to build social capital among individuals and groups.We've included examples about supportive settings, venues and activities for building social capital, and when possible some","smart bets"," or best guesses about its different purposes and outcomes", "</br>","</br>","<u><a href='https://www.aecf.org/resources/the-role-of-social-capital-in-building-healthy-communities/'>Social Capital in Community Development</a></u>","</br>"," Social capital for communities refers to establishing trust-based networks. That means not just establishing strong 
                                    connections, but reinforcing the quality of those relationships among families, communities and organizations. This is that important, underlying ingredient that determines healthy families and building social capital in communities. Using four cities as case studies and examples of social capital in the community, this report reflects the various aspects of examples of social capital in the community as it pertains to immigrant neighborhoods and communities of color, showing ways that social capital can help or hinder community development.",
                                    "</br>","</br>","<b>Population Weighted Density:</b>"," A measure of population density that takes into account the density levels most people in the community experience. Population density allows for broad comparison of settlement intensity across geographic areas.","</br>","</br>",
                                    "<u><a href='https://www.rural-design.org/'>Citizens Institute on Rural Design</a></u>"," is a leadership initiative of the National Endowment for the Arts in partnership with the Housing Assistance Council, along with buildingcommunityWORKSHOP. Focusing on communities with populations of 50,000 or less, CIRD's goal is to enhance the quality of life and economic viability of rural America through planning, design, and creative placemaking.",
                                    "</br>","</br>","CIRD is intended to empower local citizens to capitalize on unique local and regional assets in order to guide the civic development and future design of their own communities. The CIRD program goals include:",
                                    "</br>","</br>","<li>Building capacity in rural communities to plan comprehensive revitalization strategies;</li>",
                                    "<li>Introducing creative placemaking, arts, culture, and design strategies as drivers of economic development in rural America;</li>",
                                    "<li>Facilitating a network of rural communities for idea exchanges and peer learning; and</li>",
                                    "<li>Preparing communities to be ready and competitive for state and federal funding opportunities.</li>",
                                    "</br>","<u><a href='https://americas.uli.org/research/centers-initiatives/building-healthy-places-initiative/building-healthy-places-toolkit/'>The Urban Land Institute's Building Healthy Places Toolkit: Strategies for Enhancing Health in the Built Environment</a></u>"," outlines evidence-supported opportunities for enhancing health outcomes in real estate developments. Developers, owners, property managers, designers, investors, and others involved in real estate decision making can use the report's recommendations and strategies to create places that contribute to healthier people and communities, and to enhance and preserve value by meeting growing desires for health-promoting places.",
                                    "</br>","</br>", "<b>Walkability:</b>"," A measure of how walkable a community is based on residential and employment density, land use (destination) diversity, street connectivity and public transit accessibility.",
                                    "</br>","</br>","<u><a href='https://www.livingstreetsalliance.org/our-work/programsservices/neighborhood-walking-assessments/'>Living Streets Alliance</a></u>"," has developed a Neighborhood Walkability Assessment Program for the Tucson region to address pedestrian safety & comfort issues, while helping area residents identify walkability improvements needed in their neighborhoods. The program engages the participants in an open discussion about the walking conditions in their neighborhoods and provides an opportunity to: discuss what","walkability"," means and what makes a good place for walking, share ideas about walkability improvements you'd like to see in your neighborhood, connect with fellow neighbors, explore and document the current neighborhood walking conditions and gain an additional perspective on your neighborhood streets.",
                                    "</br>","</br>","<u><a href='https://smartgrowthamerica.org/resources/'>National Complete Streets Coalition</a></u>","</br>"," Streets are a vital part of livable, attractive communities. Everyone, regardless of age, ability, income, race, or ethnicity, ought to have safe, comfortable, and convenient access to community destinations and public places-whether walking, driving, bicycling, or taking public transportation.",
                                    "</br>","</br>","<u><a href='http://americawalks.org/wp-content/uploads/2014/12/261463434-Steps-to-a-Walkable-Community.pdf'>Steps to a Walkable Community</a></u>"," compiles multidisciplinary tactics that readers can assemble into custom strategies designed for their community's circumstances. The guide contains tactics for building or rebuilding cities and suburbs in ways that encourage walking. The guide is also about making walking in cities safer, and it provides traffic-engineering techniques to achieve that. Steps to a Walkable Community also describes methods of organizing advocacy to reach these goals.",
                                    "</br>","</br>","<u><a href='https://www.cdc.gov/obesity/downloads/UrbanDesignPolicies.pdf'>Urban Design and Transportation Policies and Practices</a></u>","</br>", " Strategies related to urban design and transportation policies and practices that promote physical activity include street-scale urban design and land-use policies and practices; community-scale urban design and land-use policies and practices; and transportation and travel policies and practices. These types of policies and practices can encourage active transportation by facilitating walking, bicycling, and public transportation use.",
                                    "</br>","</br>","<b>Affordability:</b>"," The proportion of a community's income spent on housing and transportation. This indicates how much income remains for other priorities, including food, health care and social activities.",
                                    "</br>","</br>","<u><a href='https://www.betterhousingcoalition.org/find-housing/'>Better Housing Coalition's (BHC) Home Ownership Program</a></u>","</br>"," The affordable high-quality homes BHC constructs and rehabilitates provide first-time homeowners with low to moderate income access to equity and wealth building. Home ownership helps stabilize neighborhoods and is an important component of healthy communities. By embracing green building practices by building to EarthCraft standards, a certification program which serves as a blueprint for comfortable, energy efficient homes, homeowners have lower maintenance and utility bills. While BHC single-family homes incorporate modern green features, such as solar hot water heating systems, they are designed with classic architectural details which blend seamlessly with the surrounding neighborhood. ",
                                    "</br>","</br>","<u><a href='https://www.dhcd.virginia.gov/housing'>DHCD Housing Programs</a></u>","</br>"," The Virginia Department of Housing and Community Development offers many resources to assist with housing needs. From housing rehabilitation to homebuyer resources, check out the many programs and resources available to Virginians. Multiple programs offer state and federal funding sources to provide safe and affordable housing throughout Virginia, and to reduce homelessness.",
                                    "</br>","</br>","<u><a href='https://www.housingvirginia.org/'>Housing Virginia</a></u>"," is a broad-based, statewide partnership of public and private organizations and committed individuals. We believe that all Virginians should have access to high quality, affordable housing in suitable locations.",
                                    "</br>","</br>","<u><a href='https://www.rrha.com/revitalize-rva/properties-available/homeownership/'>The Urban Homestead Homeownership</a></u>"," program provided homeownership opportunities for more than 300 families, from the mid 1980's through 2002. For this program, RRHA acquired vacant homes, created loan and grant packages for low- to moderate-income purchasers, and managed rehabilitation.  The program used a mixture of federal and private funds to finance the projects.",
                                    "</br>","</br>","<u><a href='https://urbanhoperva.org/'>Urban Hope</a></u>"," is a 501(c)(3) community development corporation committed to the social, economic and spiritual health of Metro Richmond. The primary focus of Urban Hope is to partner with low income families who seek affordable rental housing and/or home ownership. Urban Hope oversees the rental and homeownership processes from beginning to end. Staff and volunteers supervise the construction and renovation of affordable housing, provide financial counseling, and support qualifying families as they develop an ownership stake in their community. Practical, emotional, and spiritual support is provided through Urban Neighbors and financial mentors - who play an integral role in the Urban Hope model. Urban Hope is working in the geographic area of the East End. By supporting families through affordable rental housing and the homeownership process, communities of distress are transformed into environments where families can thrive.",
                                    "</br>","</br>","<u><a href='https://www.vehiclesforchange.org/'>Vehicles for Change</a></u>","</br>"," Vehicles for Change refurbishes vehicles that have been donated by the public and sells them at a reduced cost to eligible recipients. To be eligible to purchase a car from them, you must be employed at least 30 hours per week and have funds available to pay the taxes, title, and tags on the vehicle. The vehicles available are donated by the public and then refurbished for use. Recipients are referred to the program through social services.",
                                    "</br>","</br>", "Families that live in Northern Virginia (north of Fredericksburg, Virginia) should work with staff at VFC's headquarters.","<u><a href='https://www.vehiclesforchange.org/marylandvirginia/need-a-car/'> Learn more and find out if you qualify.</a></u>",
                                    "</br>","Families that live in Southern Virginia (south of Fredericksburg) should work with staff at VFC's Virginia location.","<u><a href='https://www.vehiclesforchange.org/need-a-car/southern-virginia/'> Learn more and find out if you qualify.</a></u>",
                                    "</br>","</br>","<u><a href='https://www.vhda.com/Programs/Pages/Programs.aspx'>Virginia Housing Development Authority (VHDA)</a></u>"," carries out their mission to help Virginians attain quality, affordable housing by working in public-private partnerships with local governments, community service organizations, lenders, Realtors, developers and many others. They provide mortgages for first-time homebuyers, as well as financing for apartment communities and neighborhood revitalization efforts. They offer free homebuyer classes, support housing counseling, and help people with disabilities and the elderly make their homes more livable. They also administer the federal Housing Choice Voucher and Housing Credit programs in Virginia.",
                                    "</br>","</br>","<u><a href='http://www.virginiasupportivehousing.org/'>Virginia Supportive Housing</a></u>","  is the largest and oldest provider of permanent supportive housing (PSH) in Virginia. The PSH program model, which combines affordable housing and individualized supportive services, is widely recognized as an evidence-based solution to homelessness.",
                                    "</br>","</br>","<b>Education:</b>"," The average number of years of schooling among adults in the community. It can range from zero (those with no formal schooling) to 20 (those with a doctorate/professional degree).",
                                    "</br>","</br>","<u><a href='https://evidencebasedprograms.org/document/h-and-r-block-college-financial-aid-assistance-evidence-summary/'>College Financial Aid Application Assistance</a></u>","</br>"," Streamlined personal assistance in completing a college financial aid application provided to low and moderate-income families with a dependent child at or near college age. Evaluation Methods: A large, multi-site randomized controlled trial (RCT). Key Findings: A sizable increase in college attendance and persistence over the 3.5-4 years following the program - e.g., 29% greater likelihood of attending college for two consecutive years",
                                    "</br>","</br>","<u><a href='http://www.pluggedinva.com/'>PluggedInVA: Expanding through Partnerships</a></u>","</br>"," PluggedInVA is a career pathways program that provides motivated adult learners with a contextualized high school equivalency curriculum integrated with industry-specific technical training as a means to develop essential workplace skills for entry-level jobs in targeted industries.",
                                    "</br>","</br>","<u><a href='https://valrc.org/'>The Virginia Adult Learning Resource Center</a></u>","</br>"," A division of The Literacy Institute at VCU, the Virginia Adult Learning Resource Center (VALRC) strengthens Virginia's adult education and literacy system through professional development, resources, and collaborative projects. VALRC provides face-to-face and online training for Virginia adult education teachers and program staff; manages the GED Virginia website and GED Helpline; and supports the development and expansion of the PluggedInVA career pathways program for adults.",
                                    "</br>","</br>","<b>Food Accessibility:</b>"," A measure of access to food by low-income people within a community. It measures the proportion of the low-income community that has a large grocery store within 1 mile in urban areas or 10 miles in rural areas.",
                                    "</br>","</br>","<u><a href='https://vafoodbanks.org/'>The Federation of Virginia Food Banks</a></u>"," is a 501(c)(3) nonprofit state association of food banks affiliated with Feeding America and is the largest hunger-relief network in the state. Composed of the seven regional Virginia/Washington DC food banks, the Federation supports the food banks in providing food, funding, and education throughout the Commonwealth.",
                                    "</br>","</br>","The Federation's mission is to grow the collective capacity of Virginia's food banks and engage partners to end hunger in the Commonwealth. Last year, the Federation's network distributed almost 120 million pounds of food and grocery products through 1,500 member agencies that directly serve those in need.  These agencies operate programs such as soup kitchens, after school programs, senior centers and elderly feeding programs, the Kids Cafe Program, Head Start, transitional housing, mental health programs, homeless and domestic violence shelters, and individual household distribution.",
                                    "</br>","</br>","<u><a href='https://www.richmondfed.org/publications/community_development/community_scope/2019/community_scope_2019_issue2'>Food Hubs: Mission-Driven Local Food Systems in the Fifth District</a></u>","</br>"," Regional food systems are food supply chains that operate within local geographies and have an explicit mission to serve community farmers and consumers. Food hubs are one type of regional food system that strive to improve local communities not only by supporting small businesses, but also by offering community-strengthening services. Food hubs tend to participate in activities targeted to low- and moderate-income (LMI) populations, like improving individuals' food security and increasing their access to farm-fresh produce. This issue of Community Scope seeks to better understand the economic impact of regional food systems through an examination of the food hub model. The article also offers examples of how regional organizations impact Fifth District communities.",
                                    "</br>","</br>","<u><a href='http://www.vdh.virginia.gov/healthy-corner-store-initiative/'>The Healthy Corner Store Initiative (HCSI)</a></u>","</br>"," Funded in part by the Virginia Department of Health, HCSI is a program that is aimed at improving access to healthy foods within underserved communities across the Commonwealth. Through this initiative community partners, corner store owners, and local farmers work together to bring fresh fruits and vegetables into neighborhood stores within areas that are designated by the USDA as","food deserts.",
                                    "</br>","</br>","<u><a href='http://www.richmondgov.com/CommunityGarden/'>Richmond Grows Gardens Program (RGG)</a></u>","</br>"," Under the leadership of Mayor Levar M. Stoney, our City Administration recognizes that community gardens provide economic benefits, increase social equity, strengthen our community, and promote environmental stewardship. RGG supports the development of community gardens throughout Richmond. RGG defines a community garden as: A portion of city-owned property used to grow fruits, vegetables, flowers, herbs, wood products, native or ornamental plants for non-commercial purposes, i.e. where there is no exchange of goods for monetary value. RGG offers city property for community gardens to incorporated organizations, unincorporated organizations and governmental organizations via an online application process.",
                                    "</br>","</br>","<u><a href='https://blogs.ext.vt.edu/virginia-community-garden-network/2018/04/02/membership-resources/'>The Virginia Community Garden Network</a></u>"," is a new platform to engage and share info, resources, news, and events related to community gardens across the commonwealth. The budding network is building an interactive listserv, shared resources folder, and a statewide community gardening mapping component.",
                                    "</br>","</br>","<b>Material Deprivation Indicator:</b>","Based on the Townsend Material Deprivation Index, it examines the private material resources available to households in a community. 4 indicators makeup Townsend Index:",
                                    "</br>","</br>","1. Overcrowding (>2 persons per room)",
                                    "</br>","2. Unemployment",
                                    "</br>","3. Percentage of persons with no vehicle or car",
                                    "</br>","4. Percent of persons who rent",
                                    "</br>","</br>","<u><a href='https://movingforwardusa.com/credit-counseling/'>Moving Forward Empowerment Services</a></u>",
                                    "</br>","Moving Forward Empowerment Services is a Virginia-based comprehensive financial and housing services company. They specialize in credit, money management, personal finances, and homeownership counseling.",
                                    "</br>","</br>","<b>Virginia Community Action Partnership and Virginia CASH Campaign</b>",
                                    "</br>","The Virginia Community Action Partnership (VACAP) Earned Income Tax Credit (EITC) Initiative supports community groups and local coalitions throughout the Commonwealth as they provide free tax preparation services to low and moderate income working individuals and families. VACAP works with coalitions to promote the EITC through the Virginia CASH Campaign. CASH stands for Creating Assets, Savings and Hope. The mission is to raise awareness of the EITC among all taxpayers and to educate them of the importance of obtaining all tax credits to which they are entitled. Along with claiming valuable tax credits, even more valuable is obtaining free tax preparation services through the Internal Revenue Service Volunteer Income Tax Assistance (VITA) Program.",
                                    "</br>","CASH:","<u><a href='http://www.vaeitc.org/'>http://www.vaeitc.org/</a></u>",
                                    "</br>","VITA:","<u><a href='https://irs.treasury.gov/freetaxprep/'>https://irs.treasury.gov/freetaxprep/</a></u>",
                                    "</br>","</br>","<b>Employment Accessibility Indicator:</b>","A measure of the number of jobs accessible to members of the community. Accessibility is determined by distance: close jobs are more accessible than jobs farther away.",
                                    "</br>","</br>","<u><a href='https://www.dhcd.virginia.gov/communities'>DHCD Community Revitalization Programs</a></u>",
                                    "</br>","The Virginia Department of Housing and Community Development offers financial resources, technical assistance, and learning opportunities to help communities reinvest in their places, invigorate local economies, and improve quality in the lives of their residents. DHCD also offers community-based technical assistance programs to support Virginia's entrepreneurs.",
                                    "</br>","</br>","<u><a href='https://govirginia.org/'>GO Virginia</a></u>"," is a bipartisan, business-led economic development initiative that is changing the way Virginia's diverse regions collaborate on economic and workforce development activities. GO Virginia supports programs to create more high-paying jobs through incentivized collaboration between business, education, and government to diversify and strengthen the economy in every region of the Commonwealth.",
                                    "</br>","</br>","<u><a href='https://www.richmondfed.org/community_development/investment_connection'>Investment Connection</a></u>",
                                    ", a program of the Federal Reserve Bank of Richmond, brings together bankers, nonprofit organizations, foundations, and public and private funders to meet and explore community development opportunities in the areas they serve, especially those in low- and moderate-income, distressed and underserved communities.",
                                    "</br>","</br>","<u><a href='https://virginiacareerworks.com/'>Virginia Career Works</a></u>",
                                    "</br>","Our mission is to advance economic stability and growth by preparing and connecting people who want to work with employers who need to hire through its training providers and network of professional partners. Virginia Career Works is Virginia's vital link between meaningful employment and growing businesses, changing lives; and advancing economic prosperity.",
                                    "</br>","</br>","<u><a href='https://www.vedp.org/incentive/virginia-jobs-investment-program-vjip'>The Virginia Jobs Investment Program (VJIP)</a></u>",
                                    "</br>","The VJIP is a discretionary program that provides consultative services and funding to companies creating new jobs or experiencing technological change to reduce the human resource development costs for new companies, expanding companies, and companies retraining their employees.",
                                    "</br>","</br>","<b>Income Inequality Indicator:</b>","The Gini Index, a common measure of income inequality, measures whether the income earned within a community is distributed broadly or concentrated within the hands of a small number of households.",
                                    "</br>","</br>","<u><a href='https://www.housingvirginia.org/deconcentrating-poverty/'>Deconcentrating Poverty Toolkit</a></u>",
                                    "</br>","From Housing Virginia, this toolkit includes examples of best practices and multi-pronged approaches for reducing and deconcentrating poverty. In Virginia, efforts to deconcentrate poverty have guided policies in resource allocations in certain municipalities to improve workforce development, early childhood education, and neighborhood revitalization.",
                                    "</br>","</br>","<u><a href='http://www.richmondgov.com/CommunityWealthBuilding/Neighborhood.aspx'>Office of Community Wealth Building (OCWB) Neighborhood Transformation</a></u>",
                                    "</br>","The Office of Community Wealth Building (OCWB), in collaboration with Richmond Redevelopment & Housing Authority (RRHA), community partners, and other City agencies, is helping to develop a strategy for the redevelopment of these communities, using a public policy strategy that empowers and expands choices for residents while assuring no one is involuntarily displaced.",
                                    "</br>","<u><a href='http://www.richmondgov.com/CommunityWealthBuilding/Neighborhood.aspx'>http://www.richmondgov.com/CommunityWealthBuilding/Neighborhood.aspx</a></u>",
                                    "</br>","</br>","<b>Job Participation Indicator:</b>","The percentage of individuals 16-64 years of age active in the civilian labor force. It includes both those currently working and those seeking work.",
                                    "</br>","</br>","<u><a href='https://vadars.org/drs/cpid/#'>Career Pathways</a></u>",
                                    "</br>","Career Pathways connects Virginians with disabilities to the education, training and industry-recognized credentials needed to qualify for high-demand jobs.",
                                    "</br>","</br>","<u><a href='https://www.fastforwardva.org/'>FastForward</a></u>","was created to bolster its workforce and make it easier for you to access careers by offering a low-cost training option. FastForward is a short-term workforce credential program to train Virginians for top, in-demand jobs across the Commonwealth. Most programs take between six and 12 weeks and are built so students can get their education while they work. At Virginia's Community Colleges, we want students to gain the skills they need quickly and affordably, while creating more opportunities for their future.",
                                    "</br>","</br>","<u><a href='http://www.vats.org/'>Virginia Assistive Technology System (VATS)</a></u>","- VATS can help you learn about the range of available technology, select the most appropriate device, receive training on how to use the selected device, and find resources to pay for the device.",
                                    "</br>","</br>","<u><a href='https://www.vadars.org/'>Virginia Department for Aging & Rehabilitative Services (DARS)</a></u>",
                                    "- provides and advocates for resources and services to improve the employment, quality of life, security, and independence of older Virginians, Virginians with disabilities, and their families.",
                                    "</br>","</br>","<u><a href='https://www.benefits.va.gov/vocrehab/index.asp'>Virginia's Department of Veterans Affairs</a></u>",
                                    "</br>","Veterans may receive Vocational Rehabilitation and Employment (VR&E) services to help with job training, employment accommodations, resume development, and job seeking skills coaching.",
                                    "</br>","</br>","<u><a href='https://virginiareentry.org/re-entry-councils/'>Re-Entry Councils</a></u>",
                                    "</br>","Re-entry councils consist of a collection of representatives of agencies and organizations that deliver services to persons who were previously incarcerated. Representatives meet to address the needs and barriers of returning citizens. Councils are an excellent resource to access community resources and learn more about community capabilities.",
                                    "</br>","</br>","<b>Access to Care Indicator:</b>","Whether community members have access to a primary care physician and the means to pay for care. It includes the proportion of uninsured residents and the number of physicians within 30 miles of the community.",
                                    "</br>","</br>","<u><a href='https://thehealthwagon.org/'>The Health Wagon</a></u>",
                                    "is a mobile clinic that provides free healthcare in 11 rural Virginia communities. The Health Wagon's mobile clinic visits 11 rural communities in southwest Virginia's Buchanan, Dickenson, Lee, Russell, Scott, and Wise Counties to provide free healthcare to those in need.",
                                    "</br>","</br>","<u><a href='vafreeclinics.org'>Virginia Association of Free and Charitable Clinics</a></u>",
                                    "</br>","We believe that every person deserves access to basic healthcare services. Therefore, VAFCC supports Virginia's network of free and charitable clinics to adapt to Virginia's changing healthcare landscape in order to serve those who fall through the gaps of the healthcare system with compassionate and comprehensive healthcare services regardless of their insurance status or ability to pay.",
                                    "</br>","</br>","<u><a href='https://vacommunityhealth.org/'>Virginia Community Healthcare Association (VaCHA)</a></u>",
                                    "</br>","VaCHA's mission is Assuring Access to Primary Care for All Virginians. We believe that regardless of geographic location or income, all Virginians should have convenient, appropriate, and affordable primary care. We are an integral part of Virginia's Health Safety Net which includes Virginia's Community Health Centers (CHCs), nonprofit rural health clinics, community-based providers of primary care, and health departments providing primary care services, and other similar organizations.",
                                    "</br>","Over the years, The Association has been a driving force in establishing and maintaining both local and statewide efforts to improve primary care. Among our efforts we have been a founder or original sponsor of Virginia's Five Point Plan to Strengthen the Primary Care System; Area Health Education Centers (AHEC); Virginia Center for the Advancement of Generalist Medicine; and the Practice Sights Initiative.",
                                    "</br>","</br>","<u><a href='https://www.vhcf.org/'>The Virginia Health Care Foundation (VHCF)</a></u>",
                                    "</br>","A public/private partnership, VHCF helps uninsured Virginians and those who live in underserved communities receive medical, dental and mental health care. VHCF support helps free clinics, community health centers and others to expand both the types of care offered and the number of patients cared for each year. In addition to grants, VHCF programs help make prescription medications available to those who can't afford them. And VHCF outreach helps uninsured children receive medical and dental coverage.",
                                    "</br>","</br>","<u><a href='http://www.vdh.virginia.gov/health-equity/incentive-programs/'>Workforce Incentive Programs</a></u>",
                                    "</br>","The Commonwealth of Virginia offers several incentive programs to attract primary health professionals to Virginia's underserved areas. These programs not only assist with paying down outstanding medical education debt but also afford individuals an opportunity to become part of a community and provide care to deserving populations.",
                                    "</br>","</br>","<b>Segregation Indicator:</b>","A measure of whether and how much people of different racial and ethnic backgrounds live together in diverse communities. It includes measures of both community diversity and the distance between communities with different racial or ethnic profiles.",
                                    "</br>","</br>","<u><a href='https://inclusiveva.org/'>Virginia Center for Inclusive Communities</a></u>",
                                    "</br>","The Virginia Center for Inclusive Communities is increasingly called upon to help schools, businesses, and communities across Virginia achieve success through inclusion. Through workshops, retreats, and customized programs that raise knowledge, motivation, and skills, VCIC develops leaders who work together to achieve success throughout the Commonwealth.",
                                    "</br>","</br>","<b>Virginia Department of Small Business and Supplier Diversity</b>",
                                    "</br>","The Department of Small Business and Supplier Diversity (SBSD) is the state agency dedicated to enhancing the participation of our small, women- and minority-owned businesses in Virginia's procurement opportunities. SBSD is responsible for the administration of two certification programs: the Small, Women- and Minority-owned Businesses under Virginia's SWaM Procurement Initiative and the federal U.S. Department of Transportation's Disadvantaged Business Enterprise (DBE) Program.",
                                    "</br>","<u><a href='https://www.sbsd.virginia.gov/certification-division/swam/'>https://www.sbsd.virginia.gov/certification-division/swam/</a></u>",
                                    "</br>","<u><a href='https://www.sbsd.virginia.gov/certification-division/dbe/'>https://www.sbsd.virginia.gov/certification-division/dbe/</a></u>")))
  output$contact = renderUI(HTML(paste("</br>","<u><a href='http://www.vdh.virginia.gov/health-equity/'>Virginia Department of Health Office of Health Equity</a></u>",
                                       "</br>","</br>","Justin Crow, MPA","</br>","Director, Division of Social Epidemiology","</br>","<u><a href='justin.crow@vdh.virginia.gov'>justin.crow@vdh.virginia.gov</a></u>",
                                       "</br>","</br>","Rexford Anson-Dwamena, MPH","</br>","Senior Epidemiologist, Division of Social Epidemiology","</br>","<u><a href='rexford.dwamena@vdh.virginia.gov'>rexford.dwamena@vdh.virginia.gov</a></u>",
                                       "</br>","</br>","Sarah O'Connor, MPH","</br>","Healthy People, Healthy Lives Project Coordinator, Division of Social Epidemiology","</br>","<u><a href='sarah.oconnor@vdh.virginia.gov'>sarah.oconnor@vdh.virginia.gov</a></u>",
                                       "</br>","</br>","Navya Mote","</br>","R Visualization Specialist","</br>","<u><a href='navya.mote@vdh.virginia.gov'>navya.mote@vdh.virginia.gov</a></u>",
                                       "</br>","</br>","Partner at the Virginia Commonwealth University, Department of Biostatistics:","</br>","Dr. Roy Sabo")))
  output$value <- renderPrint({ input$slider1 })
  data_input$id <- 1:nrow(data_input)
  output$mapPlot <- renderLeaflet({
    county<-paste0("County Name: ", gsub(",.*$", "", data_input$NAME))
    # lhd<-paste0(", LHD Name: ", gsub(",.*$", "", data_input$LHD))
    # label<-cat(paste0(county,lhd, "\n"))
    leaflet(data = data_input) %>% addTiles() %>%
      # setView(-78.02, 37.92, zoom = 6.25) %>%
      addPolygons(data = data_input, layerId = data_input$NAME, stroke = TRUE,
                  color = "black", weight = 1.5, fillOpacity = 0.7, fillColor = ~factpal(Group),
                  label= county,
                  # ~paste0(county,lhd),
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               bringToFront = TRUE))
    
  })
  observeEvent(input$cty, { # update the location selectInput on map clicks
    # print(input$cty)
    print("dropdown")
    p <- input$cty
    if(p == "All of Virginia"){
      output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in Virginia Census Tracts")})
      a <- c("All Census Tracts", round(mean(master$LEB_Imputed),digits = 1),"High Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==4],digits =1),"Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==2],digits =1))
      b <- c("Very High Opportunity Tracts", round(df_mas$Mean[df_mas$hoi_grp==5],digits =1),"Average Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==3],digits =1),"Very Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==1],digits =1))
      df_first<- data.frame(a,b)
      output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "100%",align = 'c', bordered = TRUE, digits = 1)
      output$exp_val<-renderTable(round(mean(master$LEB_Imputed),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }else{
      output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in ",p,sep=" ")})
      output$txtOutput_3 = renderText({ paste0("Expected Average Life Expectancy in ",p,sep=" ")})
    master_2_new <- subset(master_2, Group_LHD == input$cty)
    master_2_new$LEB_new<- (master_2_new$HOI_new * master_2_new$Slope) + master_2_new$Intercept
    data_mas_2<-group_by(master_2_new,hoi_grp)
    df_mas_2<-summarise(data_mas_2, Mean = round(mean(LEB_new),digits =1))
    # print(df_mas_2$hoi_grp)
    if(is.element(3, df_mas_2$hoi_grp) == FALSE){
      df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = NA, check.names = FALSE)
    }else{
      df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==3],digits =1), check.names = FALSE)
    }
    if(is.element(5, df_mas_2$hoi_grp) == FALSE){
      df_fin3 <- data.frame("All Census Tracts" = round(mean(master_2_new$LEB_new),digits = 1), "Very High Opportunity Tracts" = NA , check.names = FALSE)
      output$exp_val<-renderTable(round(mean(master_2_new$LEB_new),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }else{
      df_fin3 <- data.frame("All Census Tracts" = round(mean(master_2_new$LEB_new),digits = 1), "Very High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==5],digits =1), check.names = FALSE)
      output$exp_val<-renderTable(round(mean(master_2_new$LEB_new),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }
    # print(df_fin3)
    # df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==3],digits =1), check.names = FALSE)
    df_fin5 <- data.frame("Low Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==2],digits =1), "Very Low Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==1],digits =1), check.names = FALSE)
    a <- c("All Census Tracts", df_fin3$`All Census Tracts`,"High Opportunity Tracts",df_fin4$`High Opportunity Tracts`,"Low Opportunity Tracts",df_fin5$`Low Opportunity Tracts`)
    b <- c("Very High Opportunity Tracts", df_fin3$`Very High Opportunity Tracts`,"Average Opportunity Tracts",df_fin4$`Average Opportunity Tracts`,"Very Low Opportunity Tracts",df_fin5$`Very Low Opportunity Tracts`)
    df_first<- data.frame(a,b)
    output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "100%",align = 'c', bordered = TRUE, digits = 1)
    output$exp_val<-renderTable(df_fin3$`All Census Tracts`,colnames = FALSE,digits = 1,width = "100%",align = 'c')
    # output$table3 <- renderTable(df_fin3, striped = TRUE, bordered = TRUE, spacing = c("m"), width = "350", align = 'c', digits = 1)
    # output$table5 <- renderTable(df_fin4, striped = TRUE, bordered = TRUE, spacing = c("m"), width = "350", align = 'c', digits = 1)
    # output$table6 <- renderTable(df_fin5, striped = TRUE, bordered = TRUE, spacing = c("m"), width = "350", align = 'c', digits = 1)
    group <- gsub(",.*$", "", data_input$NAME[data_input$Group == p]) 
    df_sub<-subset(df_dist, Group_LHD == input$cty)
    df_head<-head(df_sub,5)
    dropdwn2 <-unique(df_head$variable)
    levels(dropdwn2) <- c(levels(dropdwn2), "Overall Health Opportunity Index")    # add new level
    dropdwn2[6] <- "Overall Health Opportunity Index"
    updateSelectInput(session, "ind",
                      choices = gsub(",.*$", "", dropdwn2),
                      selected = dropdwn2[6]
    )
    updateSliderInput(session, "slider1", label = nth(df_head$variable, 1), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider2", label = nth(df_head$variable, 2), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider3", label = nth(df_head$variable, 3), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider4", label = nth(df_head$variable, 4), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider5", label = nth(df_head$variable, 5), min = -25, 
                      max = 25, value = 0)
    # group = 
    tract <- get_acs(state = "VA",
                     county = group,
                     geography = "tract",
                     variables = c(medincome = "B19013_001"),
                     year = 2016,
                     geometry = TRUE)
    tract$NAME <- as.factor(tract$NAME)
    tract<-merge(tract, data_mas_2,by = c("GEOID"))
    tract$hoi_index[tract$hoi_grp==1] = "Very Low"
    tract$hoi_index[tract$hoi_grp==2] = "Low"
    tract$hoi_index[tract$hoi_grp==3] = "Average"
    tract$hoi_index[tract$hoi_grp==4] = "High"
    tract$hoi_index[tract$hoi_grp==5] = "Very High"
    factpal1 <- colorFactor(palette = c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),domain = tract$hoi_grp)
    # addPolygons(data = data_input, layerId = data_input$NAME, stroke = TRUE,
    #             color = "black", weight = 1.5, fillOpacity = 0.7, fillColor = ~factpal(Group),
    #             label= ~paste0("County Name: ", gsub(",.*$", "", data_input$NAME)),
    #             highlight = highlightOptions(weight = 3,
    #                                          color = "black",
    #                                          bringToFront = TRUE))
    # print(tract)
    output$mapPlot1 <- renderLeaflet({
      tract<-separate(tract, NAME, c('census','county','state'), sep = ",", remove = FALSE)
      # ct_nm<-paste0("County Name: ", tract$county)
      # tr_nm<-paste0(", Tract FIPS: ", tract$GEOID)
      leb_bir<-paste0("Life Expectancy at Birth:",tract$LEB_new)
      leaflet(data = tract) %>% addTiles() %>%
        # addPolygons(fillColor = topo.colors(40, alpha = NULL), stroke = FALSE)
        addPolygons(data = tract, stroke = TRUE,
                    # weight = 1, fillColor = topo.colors(40, alpha = NULL),
                    color = "black", weight = 1,fillOpacity = 0.7, fillColor = ~factpal1(hoi_grp),
                    label= leb_bir,
                    # ~paste0(ct_nm,tr_nm),
                    highlight = highlightOptions(weight = 3,
                                                 color = "black",
                                                 bringToFront = TRUE))%>%
        addLegend(data = tract, "bottomright",
                  values = ~hoi_grp,
                  colors =c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),
                  labels = c("Very Low", "Low", "Average","High","Very High"),
                  title = "Opp. Index",
                  # labFormat = labelFormat(prefix = "$"),
                  opacity = 1
        )
    })
    }
  })
  observeEvent(input$ind, {
    print("drop2")
    if(input$ind == " " & input$cty != "All of Virginia"){
      # master_2_new <- subset(master_2, Group_LHD == input$cty)
      # master_2_new$LEB_new<- (master_2_new$HOI_new * master_2_new$Slope) + master_2_new$Intercept
      # data_mas_2<-group_by(master_2,hoi_grp)
      data_mas_2<-master_2
      data_mas_2 <- data_mas_2[order(data_mas_2$HOI),]
      qs <- quantile(data_mas_2$HOI, seq(0, 1, length.out = 6))
      data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$HOI, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      # df_mas_2<-summarise(data_mas_2, Mean = round(mean(LEB_new),digits =1))
      # group <- gsub(",.*$", "", data_input$NAME[data_input$Group == input$cty])
      # tract <- get_acs(state = "VA",
      #                  county = group,
      #                  geography = "tract",
      #                  variables = c(medincome = "B19013_001"),
      #                  year = 2016,
      #                  geometry = TRUE)
      # tract$NAME <- as.factor(tract$NAME)
      # data_mas_2 <- subset(data_mas_2, Group_LHD == input$cty)
      # tract<-merge(tract, data_mas_2,by = c("GEOID"))
      group <- gsub(",.*$", "", data_input$NAME[data_input$Group == input$cty])
      tract <- get_acs(state = "VA",
                       county = group,
                       geography = "tract",
                       variables = c(medincome = "B19013_001"),
                       year = 2016,
                       geometry = TRUE)
      tract$NAME <- as.factor(tract$NAME)
      data_mas_2 <- subset(data_mas_2, Group_LHD == input$cty)
      tract<-merge(tract, data_mas_2,by = c("GEOID"))
      tract$hoi_index[tract$hoi_grp==1] = "Very Low"
      tract$hoi_index[tract$hoi_grp==2] = "Low"
      tract$hoi_index[tract$hoi_grp==3] = "Average"
      tract$hoi_index[tract$hoi_grp==4] = "High"
      tract$hoi_index[tract$hoi_grp==5] = "Very High"
      factpal1 <- colorFactor(palette = c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),domain = tract$hoi_grp)
      # addPolygons(data = data_input, layerId = data_input$NAME, stroke = TRUE,
      #             color = "black", weight = 1.5, fillOpacity = 0.7, fillColor = ~factpal(Group),
      #             label= ~paste0("County Name: ", gsub(",.*$", "", data_input$NAME)),
      #             highlight = highlightOptions(weight = 3,
      #                                          color = "black",
      #                                          bringToFront = TRUE))
      # print(tract)
      output$mapPlot1 <- renderLeaflet({
        tract<-separate(tract, NAME, c('census','county','state'), sep = ",", remove = FALSE)
        # ct_nm<-paste0("County Name: ", tract$county)
        # tr_nm<-paste0(", Tract FIPS: ", tract$GEOID)
        leb_bir<-paste0("Life Expectancy at Birth:",round(tract$LEB_new, digits = 1))
        leaflet(data = tract) %>% addTiles() %>%
          # addPolygons(fillColor = topo.colors(40, alpha = NULL), stroke = FALSE)
          addPolygons(data = tract, stroke = TRUE,
                      # weight = 1, fillColor = topo.colors(40, alpha = NULL),
                      color = "black", weight = 1,fillOpacity = 0.7, fillColor = ~factpal1(hoi_grp),
                      label= leb_bir,
                      # ~paste0(ct_nm,tr_nm),
                      highlight = highlightOptions(weight = 3,
                                                   color = "black",
                                                   bringToFront = TRUE))%>%
          addLegend(data = tract, "bottomright",
                    values = ~hoi_grp,
                    colors =c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),
                    labels = c("Very Low", "Low", "Average","High","Very High"),
                    title = "Opp. Index",
                    # labFormat = labelFormat(prefix = "$"),
                    opacity = 1
          )
      })
    }else if(input$ind != " " & input$cty != "All of Virginia"){
      # master_2_new <- subset(master_2, Group_LHD == input$cty)
      # master_2_new$LEB_new<- (master_2_new$HOI_new * master_2_new$Slope) + master_2_new$Intercept
      # data_mas_2<-group_by(master_2_new,hoi_grp)
      # df_mas_2<-summarise(data_mas_2, Mean = round(mean(LEB_new),digits =1))
      data_mas_2<-master_2
      if (input$ind == "Access to Care"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Access_to_Healthcare_Index),]
        qs <- quantile(data_mas_2$Access_to_Healthcare_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Access_to_Healthcare_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Employment Access"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Access_to_Employment_Index),]
        qs <- quantile(data_mas_2$Access_to_Employment_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Access_to_Employment_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Affordability"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Affordability_Index),]
        qs <- quantile(data_mas_2$Affordability_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Affordability_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Air Quality"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Air_Quality_Index),]
        qs <- quantile(data_mas_2$Air_Quality_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Air_Quality_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Population Churning"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Pop_Churning_Index),]
        qs <- quantile(data_mas_2$Pop_Churning_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Pop_Churning_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Education"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Education_Index),]
        qs <- quantile(data_mas_2$Education_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Education_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Food Accessibility"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Food_Access_Index),]
        qs <- quantile(data_mas_2$Food_Access_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Food_Access_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Income Inequality"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Income_Inequality_Index),]
        qs <- quantile(data_mas_2$Income_Inequality_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Income_Inequality_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Job Participation"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Job_Participation_Index),]
        qs <- quantile(data_mas_2$Job_Participation_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Job_Participation_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Population Density"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Pop_Density_Index),]
        qs <- quantile(data_mas_2$Pop_Density_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Pop_Density_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Spatial Segregation"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Segregation_Index),]
        qs <- quantile(data_mas_2$Segregation_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Segregation_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Material Deprevation"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Townsend_Index),]
        qs <- quantile(data_mas_2$Townsend_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Townsend_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }else if(input$ind == "Walkability"){
        data_mas_2 <- data_mas_2[order(data_mas_2$Walkability_Index),]
        qs <- quantile(data_mas_2$Walkability_Index, seq(0, 1, length.out = 6))
        data_mas_2$hoi_grp <- as.numeric(cut(data_mas_2$Walkability_Index, unique(qs), include.lowest = TRUE, na.rm = TRUE))
      }
      group <- gsub(",.*$", "", data_input$NAME[data_input$Group == input$cty])
      tract <- get_acs(state = "VA",
                       county = group,
                       geography = "tract",
                       variables = c(medincome = "B19013_001"),
                       year = 2016,
                       geometry = TRUE)
      tract$NAME <- as.factor(tract$NAME)
      data_mas_2 <- subset(data_mas_2, Group_LHD == input$cty)
      tract<-merge(tract, data_mas_2,by = c("GEOID"))
      tract$hoi_index[tract$hoi_grp==1] = "Very Low"
      tract$hoi_index[tract$hoi_grp==2] = "Low"
      tract$hoi_index[tract$hoi_grp==3] = "Average"
      tract$hoi_index[tract$hoi_grp==4] = "High"
      tract$hoi_index[tract$hoi_grp==5] = "Very High"
      factpal1 <- colorFactor(palette = c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),domain = tract$hoi_grp)
      # addPolygons(data = data_input, layerId = data_input$NAME, stroke = TRUE,
      #             color = "black", weight = 1.5, fillOpacity = 0.7, fillColor = ~factpal(Group),
      #             label= ~paste0("County Name: ", gsub(",.*$", "", data_input$NAME)),
      #             highlight = highlightOptions(weight = 3,
      #                                          color = "black",
      #                                          bringToFront = TRUE))
      # print(tract)
      output$mapPlot1 <- renderLeaflet({
        tract<-separate(tract, NAME, c('census','county','state'), sep = ",", remove = FALSE)
        # ct_nm<-paste0("County Name: ", tract$county)
        # tr_nm<-paste0(", Tract FIPS: ", tract$GEOID)
        leb_bir<-paste0("Life Expectancy at Birth:",round(tract$LEB_new, digits = 1))
        leaflet(data = tract) %>% addTiles() %>%
          # addPolygons(fillColor = topo.colors(40, alpha = NULL), stroke = FALSE)
          addPolygons(data = tract, stroke = TRUE,
                      # weight = 1, fillColor = topo.colors(40, alpha = NULL),
                      color = "black", weight = 1,fillOpacity = 0.7, fillColor = ~factpal1(hoi_grp),
                      label= leb_bir,
                      # ~paste0(ct_nm,tr_nm),
                      highlight = highlightOptions(weight = 3,
                                                   color = "black",
                                                   bringToFront = TRUE))%>%
          addLegend(data = tract, "bottomright",
                    values = ~hoi_grp,
                    colors =c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),
                    labels = c("Very Low", "Low", "Average","High","Very High"),
                    title = "Opp. Index",
                    # labFormat = labelFormat(prefix = "$"),
                    opacity = 1
          )
      })
    }
  })
  observeEvent(input$mapPlot_shape_click, { # update the location selectInput on map clicks
    print("map")
    p <- input$mapPlot_shape_click
    county <- gsub(",.*$", "", p$id)
    group <- data_input$Group[data_input$NAME == p$id]
    # df_leb <- data.frame( "name" = df_mas_leb$Mean_leb[df_mas_leb$Group_LHD == group])
    # names(df_leb)[1] <- paste0("Mean Life Expectancy in ",group,sep="")
    # output$table4 <- renderTable(df_leb, striped = TRUE, bordered = TRUE, width = "600", align = 'c')
    updateSelectInput(session, "cty",
                      choices = gsub(",.*$", "", dropdwn),
                      selected = group
    )
    if(input$cty == "All of Virginia"){
      output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in Virginia Census Tracts")})
      a <- c("All Census Tracts", round(mean(master$LEB_Imputed),digits = 1),"High Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==4],digits =1),"Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==2],digits =1))
      b <- c("Very High Opportunity Tracts", round(df_mas$Mean[df_mas$hoi_grp==5],digits =1),"Average Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==3],digits =1),"Very Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==1],digits =1))
      df_first<- data.frame(a,b)
      output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "100%",align = 'c', bordered = TRUE, digits = 1)
      output$exp_val<-renderTable(round(mean(master$LEB_Imputed),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }else{
    output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in",input$cty,sep=" ")})
    output$txtOutput_3 = renderText({ paste0("Expected Average Life Expectancy in ",input$cty,sep=" ")})
    master_2_new <- subset(master_2, Group_LHD == input$cty)
    master_2_new$LEB_new<- (master_2_new$HOI_new * master_2_new$Slope) + master_2_new$Intercept
    data_mas_2<-group_by(master_2_new,hoi_grp)
    df_mas_2<-summarise(data_mas_2, Mean = round(mean(LEB_new),digits =1))
    if(is.element(3, df_mas_2$hoi_grp) == FALSE){
      df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = NA, check.names = FALSE)
    }else{
      df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==3],digits =1), check.names = FALSE)
    }
    if(is.element(5, df_mas_2$hoi_grp) == FALSE){
      df_fin3 <- data.frame("All Census Tracts" = round(mean(master_2_new$LEB_new),digits = 1), "Very High Opportunity Tracts" = NA , check.names = FALSE)
      output$exp_val<-renderTable(round(mean(master_2_new$LEB_new),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }else{
      df_fin3 <- data.frame("All Census Tracts" = round(mean(master_2_new$LEB_new),digits = 1), "Very High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==5],digits =1), check.names = FALSE)
      output$exp_val<-renderTable(round(mean(master_2_new$LEB_new),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }
    # df_fin3 <- data.frame("All Census Tracts" = round(mean(master_2_new$LEB_new),digits = 1), "Very High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==5],digits =1), check.names = FALSE)
    df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==3],digits =1), check.names = FALSE)
    df_fin5 <- data.frame("Low Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==2],digits =1), "Very Low Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==1],digits =1), check.names = FALSE)
    a <- c("All Census Tracts", df_fin3$`All Census Tracts`,"High Opportunity Tracts",df_fin4$`High Opportunity Tracts`,"Low Opportunity Tracts",df_fin5$`Low Opportunity Tracts`)
    b <- c("Very High Opportunity Tracts", df_fin3$`Very High Opportunity Tracts`,"Average Opportunity Tracts",df_fin4$`Average Opportunity Tracts`,"Very Low Opportunity Tracts",df_fin5$`Very Low Opportunity Tracts`)
    df_first<- data.frame(a,b)
    output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "100%",align = 'c', bordered = TRUE, digits = 1)
    output$exp_val<-renderTable(df_fin3$`All Census Tracts`,colnames = FALSE,digits = 1,width = "100%",align = 'c')
    # output$table3 <- renderTable(df_fin3, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
    # output$table5 <- renderTable(df_fin4, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
    # output$table6 <- renderTable(df_fin5, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
    df_sub<-subset(df_dist, Group_LHD == group)
    df_head<-head(df_sub,5)
    dropdwn2 <-unique(df_head$variable)
    levels(dropdwn2) <- c(levels(dropdwn2), "Overall Health Opportunity Index")    # add new level
    dropdwn2[6] <- "Overall Health Opportunity Index"
    updateSelectInput(session, "ind",
                      choices = gsub(",.*$", "", dropdwn2),
                      selected = dropdwn2[6]
    )
    updateSliderInput(session, "slider1", label = nth(df_head$variable, 1), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider2", label = nth(df_head$variable, 2), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider3", label = nth(df_head$variable, 3), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider4", label = nth(df_head$variable, 4), min = -25, 
                      max = 25, value = 0)
    updateSliderInput(session, "slider5", label = nth(df_head$variable, 5), min = -25, 
                      max = 25, value = 0)
    county <- gsub(",.*$", "", data_input$NAME[data_input$Group == group])
    tract <- get_acs(state = "VA",
                     county = county,
                     geography = "tract",
                     variables = c(medincome = "B19013_001"),
                     year = 2016,
                     geometry = TRUE)
    tract$NAME <- as.factor(tract$NAME)
    tract<-merge(tract, data_mas_2,by = c("GEOID"))
    tract$hoi_index[tract$hoi_grp==1] = "Very Low"
    tract$hoi_index[tract$hoi_grp==2] = "Low"
    tract$hoi_index[tract$hoi_grp==3] = "Average"
    tract$hoi_index[tract$hoi_grp==4] = "High"
    tract$hoi_index[tract$hoi_grp==5] = "Very High"
    # print(tract)
    factpal1 <- colorFactor(palette = c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),domain = tract$hoi_grp)
    output$mapPlot1 <- renderLeaflet({
      tract<-separate(tract, NAME, c('census','county','state'), sep = ",", remove = FALSE)
      # ct_nm<-paste0("County Name: ", tract$county)
      # tr_nm<-paste0(", Tract FIPS: ", tract$GEOID)
      leb_bir<-paste0("Life Expectancy at Birth:",tract$LEB_new)
      leaflet(data = tract) %>% addTiles() %>%
        # addPolygons(fillColor = topo.colors(40, alpha = NULL), stroke = FALSE)
        addPolygons(data = tract, stroke = TRUE,
                    # weight = 1, fillColor = topo.colors(40, alpha = NULL),
                    color = "black", weight = 1,fillOpacity = 0.7, fillColor = ~factpal1(hoi_grp),
                    label= leb_bir,
                    # ~paste0(ct_nm,tr_nm),
                    highlight = highlightOptions(weight = 3,
                                                 color = "black",
                                                 bringToFront = TRUE))%>%
        addLegend(data = tract, "bottomright",
                  values = ~hoi_grp,
                  colors =c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),
                  labels = c("Very Low", "Low", "Average","High","Very High"),
                  title = "Opp. Index",
                  # labFormat = labelFormat(prefix = "$"),
                  opacity = 1
        )
    })
    }
  })
  observeEvent(slider_lst <- c(input$slider1,input$slider2,input$slider3,input$slider4,input$slider5), {
    slider1<-(input$slider1/100) + 1
    slider2<-(input$slider2/100) + 1
    slider3<-(input$slider3/100) + 1
    slider4<-(input$slider4/100) + 1
    slider5<-(input$slider5/100) + 1
    print("slider")
    if( input$slider1 == 0 & input$slider2 == 0 & input$slider3 == 0 & input$slider4 == 0 & input$slider5 == 0)
    {
      if(input$cty == "All of Virginia"){
        output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in Virginia Census Tracts")})
        a <- c("All Census Tracts", round(mean(master$LEB_Imputed),digits = 1),"High Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==4],digits =1),"Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==2],digits =1))
        b <- c("Very High Opportunity Tracts", round(df_mas$Mean[df_mas$hoi_grp==5],digits =1),"Average Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==3],digits =1),"Very Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==1],digits =1))
        df_first<- data.frame(a,b)
        output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "100%",align = 'c', bordered = TRUE, digits = 1)
        output$exp_val<-renderTable(round(mean(master$LEB_Imputed),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
        }else{
      output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in ",input$cty,sep=" ")})
      output$txtOutput_3 = renderText({ paste0("Expected Average Life Expectancy in ",input$cty,sep=" ")})
      slider_data <- subset(master_2, Group_LHD == input$cty)
      slider_data$LEB_new<- slider_data$HOI_new * slider_data$Slope
      slider_data$LEB_new<- slider_data$LEB_new + slider_data$Intercept
      df_slider_leb<-summarise(slider_data, Mean_leb = round(mean(LEB_new),1))
      df_leb_1 <- data.frame( "name" = df_slider_leb$Mean_leb)
      names(df_leb_1)[1] <- paste0("Average Life Expectancy in ",input$cty,sep=" ")
      output$table4 <- renderTable(df_leb_1, striped = TRUE, bordered = TRUE, width = "100%", align = 'c')
      slider_data_mas<-group_by(slider_data,hoi_grp)
      df_slider_mas<-summarise(slider_data_mas, Mean = round(mean(LEB_new),1))
      df_fin3 <- data.frame("All Census Tracts" = round(mean(slider_data$LEB_new),1), "Very High Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==5], check.names = FALSE
      )
      df_fin4 <- data.frame("High Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==4], "Average Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==3], check.names = FALSE)
      df_fin5 <- data.frame("Low Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==2], "Very Low Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==1], check.names = FALSE)
      a <- c("All Census Tracts", df_fin3$`All Census Tracts`,"High Opportunity Tracts",df_fin4$`High Opportunity Tracts`,"Low Opportunity Tracts",df_fin5$`Low Opportunity Tracts`)
      b <- c("Very High Opportunity Tracts", df_fin3$`Very High Opportunity Tracts`,"Average Opportunity Tracts",df_fin4$`Average Opportunity Tracts`,"Very Low Opportunity Tracts",df_fin5$`Very Low Opportunity Tracts`)
      df_first<- data.frame(a,b)
      # output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "600",align = 'c', bordered = TRUE, digits = 1)
      output$exp_val<-renderTable(round(mean(slider_data$LEB_new),1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
      } 
    }else{
      if(input$cty == "All of Virginia"){
        output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in Virginia Census Tracts")})
        a <- c("All Census Tracts", round(mean(master$LEB_Imputed),digits = 1),"High Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==4],digits =1),"Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==2],digits =1))
        b <- c("Very High Opportunity Tracts", round(df_mas$Mean[df_mas$hoi_grp==5],digits =1),"Average Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==3],digits =1),"Very Low Opportunity Tracts",round(df_mas$Mean[df_mas$hoi_grp==1],digits =1))
        df_first<- data.frame(a,b)
        output$exp_val<-renderTable(round(mean(master$LEB_Imputed),digits = 1),colnames = FALSE,digits = 1,width = "100%",align = 'c')
        }else{
        output$txtOutput_2 = renderText({ paste0("Average Life Expectancy in ",input$cty,sep=" ")})
        output$txtOutput_3 = renderText({ paste0("Expected Average Life Expectancy in ",input$cty,sep=" ")})
      slider_data <- subset(master_2, Group_LHD == input$cty)
        if (nth(df_head$variable, 1) == "Access to Care"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Healthcare_Index),
                          SD = sd(Access_to_Healthcare_Index))
        slider_data$Access_to_Healthcare_Index_new = (slider_data$Access_to_Healthcare_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Healthcare_Index_new = slider_data$Access_to_Healthcare_Index_new/sl_sum$SD}
      }else if(nth(df_head$variable, 1) == "Employment Access"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Employment_Index),
                          SD = sd(Access_to_Employment_Index))
        slider_data$Access_to_Employment_Index_new = (slider_data$Access_to_Employment_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Employment_Index_new = slider_data$Access_to_Employment_Index_new/sl_sum$SD}
        # slider_data$Access_to_Employment_Index_Z_Score = slider_data$Access_to_Employment_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Affordability"){
        sl_sum<-summarise(slider_data, Mean = mean(Affordability_Index),
                          SD = sd(Affordability_Index))
        slider_data$Affordability_Index_new = (slider_data$Affordability_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Affordability_Index_new = slider_data$Affordability_Index_new/sl_sum$SD}
        # slider_data$Affordability_Index_Z_Score = slider_data$Affordability_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Air Quality"){
        sl_sum<-summarise(slider_data, Mean = mean(Air_Quality_Index),
                          SD = sd(Air_Quality_Index))
        slider_data$Air_Quality_Index_new = (slider_data$Air_Quality_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Air_Quality_Index_new = slider_data$Air_Quality_Index_new/sl_sum$SD}
        # slider_data$Air_Quality_Index_Z_Score = slider_data$Air_Quality_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Population Churning"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Churning_Index),
                          SD = sd(Pop_Churning_Index))
        slider_data$Pop_Churning_Index_new = ( slider_data$Pop_Churning_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Churning_Index_new = slider_data$Pop_Churning_Index_new/sl_sum$SD}
        # slider_data$Pop_Churning_Index_Z_Score = slider_data$Pop_Churning_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Education"){
        sl_sum<-summarise(slider_data, Mean = mean(Education_Index),
                          SD = sd(Education_Index))
        slider_data$Education_Index_new = (slider_data$Education_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Education_Index_new = slider_data$Education_Index_new/sl_sum$SD}
        # slider_data$Education_Index_Z_Score = slider_data$Education_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Food Accessibility"){
        sl_sum<-summarise(slider_data, Mean = mean(Food_Access_Index),
                          SD = sd(Food_Access_Index))
        slider_data$Food_Access_Index_new = (slider_data$Food_Access_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Food_Access_Index_new = slider_data$Food_Access_Index_new/sl_sum$SD}
        # slider_data$Food_Access_Index_Z_Score = slider_data$Food_Access_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Income Inequality"){
        sl_sum<-summarise(slider_data, Mean = mean(Income_Inequality_Index),
                          SD = sd(Income_Inequality_Index))
        slider_data$Income_Inequality_Index_new = ( slider_data$Income_Inequality_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Income_Inequality_Index_new = slider_data$Income_Inequality_Index_new/sl_sum$SD}
        # slider_data$Income_Inequality_Index_Z_Score = slider_data$Income_Inequality_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Job Participation"){
        sl_sum<-summarise(slider_data, Mean = mean(Job_Participation_Index),
                          SD = sd(Job_Participation_Index))
        slider_data$Job_Participation_Index_new = (slider_data$Job_Participation_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Job_Participation_Index_new = slider_data$Job_Participation_Index_new/sl_sum$SD}
        # slider_data$Job_Participation_Index_Z_Score = slider_data$Job_Participation_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Population Density"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Density_Index),
                          SD = sd(Pop_Density_Index))
        slider_data$Pop_Density_Index_new = (slider_data$Pop_Density_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Density_Index_new = slider_data$Pop_Density_Index_new/sl_sum$SD}
        # slider_data$Pop_Density_Index_Z_Score = slider_data$Pop_Density_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Spatial Segregation"){
        sl_sum<-summarise(slider_data, Mean = mean(Segregation_Index),
                          SD = sd(Segregation_Index))
        slider_data$Segregation_Index_new = (slider_data$Segregation_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Segregation_Index_new = slider_data$Segregation_Index_new/sl_sum$SD}
        # slider_data$Segregation_Index_Z_Score = slider_data$Segregation_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Material Deprevation"){
        sl_sum<-summarise(slider_data, Mean = mean(Townsend_Index),
                          SD = sd(Townsend_Index))
        slider_data$Townsend_Index_new = (slider_data$Townsend_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Townsend_Index_new = slider_data$Townsend_Index_new/sl_sum$SD}
        # slider_data$Townsend_Index_Z_Score = slider_data$Townsend_Index_Z_Score * slider1
      }else if(nth(df_head$variable, 1) == "Walkability"){
        sl_sum<-summarise(slider_data, Mean = mean(Walkability_Index),
                          SD = sd(Walkability_Index))
        slider_data$Walkability_Index_new = (slider_data$Walkability_Index * slider1) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Walkability_Index_new = slider_data$Walkability_Index_new/sl_sum$SD}
        # slider_data$Walkability_Index_Z_Score = slider_data$Walkability_Index_Z_Score * slider1
      }
      # }
      # if (slider2 >= 1){
      if (nth(df_head$variable, 2) == "Access to Care"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Healthcare_Index),
                          SD = sd(Access_to_Healthcare_Index))
        slider_data$Access_to_Healthcare_Index_new = (slider_data$Access_to_Healthcare_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Healthcare_Index_new = slider_data$Access_to_Healthcare_Index_new/sl_sum$SD}
      }else if(nth(df_head$variable, 2) == "Employment Access"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Employment_Index),
                          SD = sd(Access_to_Employment_Index))
        slider_data$Access_to_Employment_Index_new = (slider_data$Access_to_Employment_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Employment_Index_new = slider_data$Access_to_Employment_Index_new/sl_sum$SD}
        # slider_data$Access_to_Employment_Index_Z_Score = slider_data$Access_to_Employment_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Affordability"){
        sl_sum<-summarise(slider_data, Mean = mean(Affordability_Index),
                          SD = sd(Affordability_Index))
        slider_data$Affordability_Index_new = (slider_data$Affordability_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Affordability_Index_new = slider_data$Affordability_Index_new/sl_sum$SD}
        # slider_data$Affordability_Index_Z_Score = slider_data$Affordability_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Air Quality"){
        sl_sum<-summarise(slider_data, Mean = mean(Air_Quality_Index),
                          SD = sd(Air_Quality_Index))
        slider_data$Air_Quality_Index_new = (slider_data$Air_Quality_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Air_Quality_Index_new = slider_data$Air_Quality_Index_new/sl_sum$SD}
        # slider_data$Air_Quality_Index_Z_Score = slider_data$Air_Quality_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Population Churning"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Churning_Index),
                          SD = sd(Pop_Churning_Index))
        slider_data$Pop_Churning_Index_new = ( slider_data$Pop_Churning_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Churning_Index_new = slider_data$Pop_Churning_Index_new/sl_sum$SD}
        # slider_data$Pop_Churning_Index_Z_Score = slider_data$Pop_Churning_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Education"){
        sl_sum<-summarise(slider_data, Mean = mean(Education_Index),
                          SD = sd(Education_Index))
        slider_data$Education_Index_new = (slider_data$Education_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Education_Index_new = slider_data$Education_Index_new/sl_sum$SD}
        # slider_data$Education_Index_Z_Score = slider_data$Education_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Food Accessibility"){
        sl_sum<-summarise(slider_data, Mean = mean(Food_Access_Index),
                          SD = sd(Food_Access_Index))
        slider_data$Food_Access_Index_new = (slider_data$Food_Access_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Food_Access_Index_new = slider_data$Food_Access_Index_new/sl_sum$SD}
        # slider_data$Food_Access_Index_Z_Score = slider_data$Food_Access_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Income Inequality"){
        sl_sum<-summarise(slider_data, Mean = mean(Income_Inequality_Index),
                          SD = sd(Income_Inequality_Index))
        slider_data$Income_Inequality_Index_new = ( slider_data$Income_Inequality_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Income_Inequality_Index_new = slider_data$Income_Inequality_Index_new/sl_sum$SD}
        # slider_data$Income_Inequality_Index_Z_Score = slider_data$Income_Inequality_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Job Participation"){
        sl_sum<-summarise(slider_data, Mean = mean(Job_Participation_Index),
                          SD = sd(Job_Participation_Index))
        slider_data$Job_Participation_Index_new = (slider_data$Job_Participation_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Job_Participation_Index_new = slider_data$Job_Participation_Index_new/sl_sum$SD}
        # slider_data$Job_Participation_Index_Z_Score = slider_data$Job_Participation_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Population Density"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Density_Index),
                          SD = sd(Pop_Density_Index))
        slider_data$Pop_Density_Index_new = (slider_data$Pop_Density_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Density_Index_new = slider_data$Pop_Density_Index_new/sl_sum$SD}
        # slider_data$Pop_Density_Index_Z_Score = slider_data$Pop_Density_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Spatial Segregation"){
        sl_sum<-summarise(slider_data, Mean = mean(Segregation_Index),
                          SD = sd(Segregation_Index))
        slider_data$Segregation_Index_new = (slider_data$Segregation_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Segregation_Index_new = slider_data$Segregation_Index_new/sl_sum$SD}
        # slider_data$Segregation_Index_Z_Score = slider_data$Segregation_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Material Deprevation"){
        sl_sum<-summarise(slider_data, Mean = mean(Townsend_Index),
                          SD = sd(Townsend_Index))
        slider_data$Townsend_Index_new = (slider_data$Townsend_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Townsend_Index_new = slider_data$Townsend_Index_new/sl_sum$SD}
        # slider_data$Townsend_Index_Z_Score = slider_data$Townsend_Index_Z_Score * slider2
      }else if(nth(df_head$variable, 2) == "Walkability"){
        sl_sum<-summarise(slider_data, Mean = mean(Walkability_Index),
                          SD = sd(Walkability_Index))
        slider_data$Walkability_Index_new = (slider_data$Walkability_Index * slider2) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Walkability_Index_new = slider_data$Walkability_Index_new/sl_sum$SD}
        # slider_data$Walkability_Index_Z_Score = slider_data$Walkability_Index_Z_Score * slider2
      }
      # }
      # if (slider3 >= 1){
      if (nth(df_head$variable, 3) == "Access to Care"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Healthcare_Index),
                          SD = sd(Access_to_Healthcare_Index))
        slider_data$Access_to_Healthcare_Index_new = (slider_data$Access_to_Healthcare_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Healthcare_Index_new = slider_data$Access_to_Healthcare_Index_new/sl_sum$SD}
      }else if(nth(df_head$variable, 3) == "Employment Access"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Employment_Index),
                          SD = sd(Access_to_Employment_Index))
        slider_data$Access_to_Employment_Index_new = (slider_data$Access_to_Employment_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Employment_Index_new = slider_data$Access_to_Employment_Index_new/sl_sum$SD}
        # slider_data$Access_to_Employment_Index_Z_Score = slider_data$Access_to_Employment_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Affordability"){
        sl_sum<-summarise(slider_data, Mean = mean(Affordability_Index),
                          SD = sd(Affordability_Index))
        slider_data$Affordability_Index_new = (slider_data$Affordability_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Affordability_Index_new = slider_data$Affordability_Index_new/sl_sum$SD}
        # slider_data$Affordability_Index_Z_Score = slider_data$Affordability_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Air Quality"){
        sl_sum<-summarise(slider_data, Mean = mean(Air_Quality_Index),
                          SD = sd(Air_Quality_Index))
        slider_data$Air_Quality_Index_new = (slider_data$Air_Quality_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Air_Quality_Index_new = slider_data$Air_Quality_Index_new/sl_sum$SD}
        # slider_data$Air_Quality_Index_Z_Score = slider_data$Air_Quality_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Population Churning"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Churning_Index),
                          SD = sd(Pop_Churning_Index))
        slider_data$Pop_Churning_Index_new = ( slider_data$Pop_Churning_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Churning_Index_new = slider_data$Pop_Churning_Index_new/sl_sum$SD}
        # slider_data$Pop_Churning_Index_Z_Score = slider_data$Pop_Churning_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Education"){
        sl_sum<-summarise(slider_data, Mean = mean(Education_Index),
                          SD = sd(Education_Index))
        slider_data$Education_Index_new = (slider_data$Education_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Education_Index_new = slider_data$Education_Index_new/sl_sum$SD}
        # slider_data$Education_Index_Z_Score = slider_data$Education_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Food Accessibility"){
        sl_sum<-summarise(slider_data, Mean = mean(Food_Access_Index),
                          SD = sd(Food_Access_Index))
        slider_data$Food_Access_Index_new = (slider_data$Food_Access_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Food_Access_Index_new = slider_data$Food_Access_Index_new/sl_sum$SD}
        # slider_data$Food_Access_Index_Z_Score = slider_data$Food_Access_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Income Inequality"){
        sl_sum<-summarise(slider_data, Mean = mean(Income_Inequality_Index),
                          SD = sd(Income_Inequality_Index))
        slider_data$Income_Inequality_Index_new = ( slider_data$Income_Inequality_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Income_Inequality_Index_new = slider_data$Income_Inequality_Index_new/sl_sum$SD}
        # slider_data$Income_Inequality_Index_Z_Score = slider_data$Income_Inequality_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Job Participation"){
        sl_sum<-summarise(slider_data, Mean = mean(Job_Participation_Index),
                          SD = sd(Job_Participation_Index))
        slider_data$Job_Participation_Index_new = (slider_data$Job_Participation_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Job_Participation_Index_new = slider_data$Job_Participation_Index_new/sl_sum$SD}
        # slider_data$Job_Participation_Index_Z_Score = slider_data$Job_Participation_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Population Density"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Density_Index),
                          SD = sd(Pop_Density_Index))
        slider_data$Pop_Density_Index_new = (slider_data$Pop_Density_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Density_Index_new = slider_data$Pop_Density_Index_new/sl_sum$SD}
        # slider_data$Pop_Density_Index_Z_Score = slider_data$Pop_Density_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Spatial Segregation"){
        sl_sum<-summarise(slider_data, Mean = mean(Segregation_Index),
                          SD = sd(Segregation_Index))
        slider_data$Segregation_Index_new = (slider_data$Segregation_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Segregation_Index_new = slider_data$Segregation_Index_new/sl_sum$SD}
        # slider_data$Segregation_Index_Z_Score = slider_data$Segregation_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Material Deprevation"){
        sl_sum<-summarise(slider_data, Mean = mean(Townsend_Index),
                          SD = sd(Townsend_Index))
        slider_data$Townsend_Index_new = (slider_data$Townsend_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Townsend_Index_new = slider_data$Townsend_Index_new/sl_sum$SD}
        # slider_data$Townsend_Index_Z_Score = slider_data$Townsend_Index_Z_Score * slider3
      }else if(nth(df_head$variable, 3) == "Walkability"){
        sl_sum<-summarise(slider_data, Mean = mean(Walkability_Index),
                          SD = sd(Walkability_Index))
        slider_data$Walkability_Index_new = (slider_data$Walkability_Index * slider3) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Walkability_Index_new = slider_data$Walkability_Index_new/sl_sum$SD}
        # slider_data$Walkability_Index_Z_Score = slider_data$Walkability_Index_Z_Score * slider3
      }
      # }
      # if (slider4 >= 1){
      if (nth(df_head$variable, 4) == "Access to Care"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Healthcare_Index),
                          SD = sd(Access_to_Healthcare_Index))
        slider_data$Access_to_Healthcare_Index_new = (slider_data$Access_to_Healthcare_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Healthcare_Index_new = slider_data$Access_to_Healthcare_Index_new/sl_sum$SD}
      }else if(nth(df_head$variable, 4) == "Employment Access"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Employment_Index),
                          SD = sd(Access_to_Employment_Index))
        slider_data$Access_to_Employment_Index_new = (slider_data$Access_to_Employment_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Employment_Index_new = slider_data$Access_to_Employment_Index_new/sl_sum$SD}
        # slider_data$Access_to_Employment_Index_Z_Score = slider_data$Access_to_Employment_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Affordability"){
        sl_sum<-summarise(slider_data, Mean = mean(Affordability_Index),
                          SD = sd(Affordability_Index))
        slider_data$Affordability_Index_new = (slider_data$Affordability_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Affordability_Index_new = slider_data$Affordability_Index_new/sl_sum$SD}
        # slider_data$Affordability_Index_Z_Score = slider_data$Affordability_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Air Quality"){
        sl_sum<-summarise(slider_data, Mean = mean(Air_Quality_Index),
                          SD = sd(Air_Quality_Index))
        slider_data$Air_Quality_Index_new = (slider_data$Air_Quality_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Air_Quality_Index_new = slider_data$Air_Quality_Index_new/sl_sum$SD}
        # slider_data$Air_Quality_Index_Z_Score = slider_data$Air_Quality_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Population Churning"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Churning_Index),
                          SD = sd(Pop_Churning_Index))
        slider_data$Pop_Churning_Index_new = ( slider_data$Pop_Churning_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Churning_Index_new = slider_data$Pop_Churning_Index_new/sl_sum$SD}
        # slider_data$Pop_Churning_Index_Z_Score = slider_data$Pop_Churning_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Education"){
        sl_sum<-summarise(slider_data, Mean = mean(Education_Index),
                          SD = sd(Education_Index))
        slider_data$Education_Index_new = (slider_data$Education_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Education_Index_new = slider_data$Education_Index_new/sl_sum$SD}
        # slider_data$Education_Index_Z_Score = slider_data$Education_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Food Accessibility"){
        sl_sum<-summarise(slider_data, Mean = mean(Food_Access_Index),
                          SD = sd(Food_Access_Index))
        slider_data$Food_Access_Index_new = (slider_data$Food_Access_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Food_Access_Index_new = slider_data$Food_Access_Index_new/sl_sum$SD}
        # slider_data$Food_Access_Index_Z_Score = slider_data$Food_Access_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Income Inequality"){
        sl_sum<-summarise(slider_data, Mean = mean(Income_Inequality_Index),
                          SD = sd(Income_Inequality_Index))
        slider_data$Income_Inequality_Index_new = ( slider_data$Income_Inequality_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Income_Inequality_Index_new = slider_data$Income_Inequality_Index_new/sl_sum$SD}
        # slider_data$Income_Inequality_Index_Z_Score = slider_data$Income_Inequality_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Job Participation"){
        sl_sum<-summarise(slider_data, Mean = mean(Job_Participation_Index),
                          SD = sd(Job_Participation_Index))
        slider_data$Job_Participation_Index_new = (slider_data$Job_Participation_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Job_Participation_Index_new = slider_data$Job_Participation_Index_new/sl_sum$SD}
        # slider_data$Job_Participation_Index_Z_Score = slider_data$Job_Participation_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Population Density"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Density_Index),
                          SD = sd(Pop_Density_Index))
        slider_data$Pop_Density_Index_new = (slider_data$Pop_Density_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Density_Index_new = slider_data$Pop_Density_Index_new/sl_sum$SD}
        # slider_data$Pop_Density_Index_Z_Score = slider_data$Pop_Density_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Spatial Segregation"){
        sl_sum<-summarise(slider_data, Mean = mean(Segregation_Index),
                          SD = sd(Segregation_Index))
        slider_data$Segregation_Index_new = (slider_data$Segregation_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Segregation_Index_new = slider_data$Segregation_Index_new/sl_sum$SD}
        # slider_data$Segregation_Index_Z_Score = slider_data$Segregation_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Material Deprevation"){
        sl_sum<-summarise(slider_data, Mean = mean(Townsend_Index),
                          SD = sd(Townsend_Index))
        slider_data$Townsend_Index_new = (slider_data$Townsend_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Townsend_Index_new = slider_data$Townsend_Index_new/sl_sum$SD}
        # slider_data$Townsend_Index_Z_Score = slider_data$Townsend_Index_Z_Score * slider4
      }else if(nth(df_head$variable, 4) == "Walkability"){
        sl_sum<-summarise(slider_data, Mean = mean(Walkability_Index),
                          SD = sd(Walkability_Index))
        slider_data$Walkability_Index_new = (slider_data$Walkability_Index * slider4) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Walkability_Index_new = slider_data$Walkability_Index_new/sl_sum$SD}
        # slider_data$Walkability_Index_Z_Score = slider_data$Walkability_Index_Z_Score * slider3
      }
      # }
      # if (slider5 >= 1){
      if (nth(df_head$variable, 5) == "Access to Care"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Healthcare_Index),
                          SD = sd(Access_to_Healthcare_Index))
        slider_data$Access_to_Healthcare_Index_new = (slider_data$Access_to_Healthcare_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Healthcare_Index_new = slider_data$Access_to_Healthcare_Index_new/sl_sum$SD}
      }else if(nth(df_head$variable, 5) == "Employment Access"){
        sl_sum<-summarise(slider_data, Mean = mean(Access_to_Employment_Index),
                          SD = sd(Access_to_Employment_Index))
        slider_data$Access_to_Employment_Index_new = (slider_data$Access_to_Employment_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Access_to_Employment_Index_new = slider_data$Access_to_Employment_Index_new/sl_sum$SD}
        # slider_data$Access_to_Employment_Index_Z_Score = slider_data$Access_to_Employment_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Affordability"){
        sl_sum<-summarise(slider_data, Mean = mean(Affordability_Index),
                          SD = sd(Affordability_Index))
        slider_data$Affordability_Index_new = (slider_data$Affordability_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Affordability_Index_new = slider_data$Affordability_Index_new/sl_sum$SD}
        # slider_data$Affordability_Index_Z_Score = slider_data$Affordability_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Air Quality"){
        sl_sum<-summarise(slider_data, Mean = mean(Air_Quality_Index),
                          SD = sd(Air_Quality_Index))
        slider_data$Air_Quality_Index_new = (slider_data$Air_Quality_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Air_Quality_Index_new = slider_data$Air_Quality_Index_new/sl_sum$SD}
        # slider_data$Air_Quality_Index_Z_Score = slider_data$Air_Quality_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Population Churning"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Churning_Index),
                          SD = sd(Pop_Churning_Index))
        slider_data$Pop_Churning_Index_new = ( slider_data$Pop_Churning_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Churning_Index_new = slider_data$Pop_Churning_Index_new/sl_sum$SD}
        # slider_data$Pop_Churning_Index_Z_Score = slider_data$Pop_Churning_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Education"){
        sl_sum<-summarise(slider_data, Mean = mean(Education_Index),
                          SD = sd(Education_Index))
        slider_data$Education_Index_new = (slider_data$Education_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Education_Index_new = slider_data$Education_Index_new/sl_sum$SD}
        # slider_data$Education_Index_Z_Score = slider_data$Education_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Food Accessibility"){
        sl_sum<-summarise(slider_data, Mean = mean(Food_Access_Index),
                          SD = sd(Food_Access_Index))
        slider_data$Food_Access_Index_new = (slider_data$Food_Access_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Food_Access_Index_new = slider_data$Food_Access_Index_new/sl_sum$SD}
        # slider_data$Food_Access_Index_Z_Score = slider_data$Food_Access_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Income Inequality"){
        sl_sum<-summarise(slider_data, Mean = mean(Income_Inequality_Index),
                          SD = sd(Income_Inequality_Index))
        slider_data$Income_Inequality_Index_new = ( slider_data$Income_Inequality_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Income_Inequality_Index_new = slider_data$Income_Inequality_Index_new/sl_sum$SD}
        # slider_data$Income_Inequality_Index_Z_Score = slider_data$Income_Inequality_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Job Participation"){
        sl_sum<-summarise(slider_data, Mean = mean(Job_Participation_Index),
                          SD = sd(Job_Participation_Index))
        slider_data$Job_Participation_Index_new = (slider_data$Job_Participation_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Job_Participation_Index_new = slider_data$Job_Participation_Index_new/sl_sum$SD}
        # slider_data$Job_Participation_Index_Z_Score = slider_data$Job_Participation_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Population Density"){
        sl_sum<-summarise(slider_data, Mean = mean(Pop_Density_Index),
                          SD = sd(Pop_Density_Index))
        slider_data$Pop_Density_Index_new = (slider_data$Pop_Density_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Pop_Density_Index_new = slider_data$Pop_Density_Index_new/sl_sum$SD}
        # slider_data$Pop_Density_Index_Z_Score = slider_data$Pop_Density_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Spatial Segregation"){
        sl_sum<-summarise(slider_data, Mean = mean(Segregation_Index),
                          SD = sd(Segregation_Index))
        slider_data$Segregation_Index_new = (slider_data$Segregation_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Segregation_Index_new = slider_data$Segregation_Index_new/sl_sum$SD}
        # slider_data$Segregation_Index_Z_Score = slider_data$Segregation_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Material Deprevation"){
        sl_sum<-summarise(slider_data, Mean = mean(Townsend_Index),
                          SD = sd(Townsend_Index))
        slider_data$Townsend_Index_new = (slider_data$Townsend_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Townsend_Index_new = slider_data$Townsend_Index_new/sl_sum$SD}
        # slider_data$Townsend_Index_Z_Score = slider_data$Townsend_Index_Z_Score * slider5
      }else if(nth(df_head$variable, 5) == "Walkability"){
        sl_sum<-summarise(slider_data, Mean = mean(Walkability_Index),
                          SD = sd(Walkability_Index))
        slider_data$Walkability_Index_new = (slider_data$Walkability_Index * slider5) - sl_sum$Mean
        if (sl_sum$SD != 0){
          slider_data$Walkability_Index_new = slider_data$Walkability_Index_new/sl_sum$SD}
        # slider_data$Walkability_Index_Z_Score = slider_data$Walkability_Index_Z_Score * slider3
      }
      # }
      # if(input$slider1 != 0 | input$slider2 != 0 | input$slider3 != 0 | input$slider4 != 0 | input$slider5 != 0)
      # {
      slider_data$HOI_new<- (slider_data$Access_to_Healthcare_Index_new * slider_data$`Access to Care`) +
        (slider_data$Access_to_Employment_Index_new * slider_data$`Employment Access`) +
        (slider_data$Affordability_Index_new * slider_data$Affordability) +
        (slider_data$Air_Quality_Index_new * slider_data$`Air Quality`) +
        (slider_data$Pop_Churning_Index_new * slider_data$`Population Churning`) +
        (slider_data$Education_Index_new * slider_data$Education) +
        (slider_data$Food_Access_Index_new * slider_data$`Food Accessibility`) +
        (slider_data$Income_Inequality_Index_new * slider_data$`Income Inequality`) +
        (slider_data$Job_Participation_Index_new * slider_data$`Job Participation`) +
        (slider_data$Pop_Density_Index_new * slider_data$`Population Density`) +
        (slider_data$Segregation_Index_new * slider_data$`Spatial Segregation`) +
        (slider_data$Townsend_Index_new * slider_data$`Material Deprevation`) +
        (slider_data$Walkability_Index_new * slider_data$Walkability)
      # print(slider_data)
      slider_data$LEB_new<- slider_data$HOI_new * slider_data$Slope
      slider_data$LEB_new<- slider_data$LEB_new + slider_data$Intercept
      df_slider_leb<-summarise(slider_data, Mean_leb = round(mean(LEB_new),1))
      slider_data_mas<-group_by(slider_data,hoi_grp)
      df_slider_mas<-summarise(slider_data_mas, Mean = round(mean(LEB_new),1))
      df_fin3 <- data.frame("All Census Tracts" = round(mean(slider_data$LEB_new),1), "Very High Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==5], check.names = FALSE
      )
      df_fin4 <- data.frame("High Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==4], "Average Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==3], check.names = FALSE)
      df_fin5 <- data.frame("Low Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==2], "Very Low Opportunity Tracts" = df_slider_mas$Mean[df_slider_mas$hoi_grp==1], check.names = FALSE)
      a <- c("All Census Tracts", df_fin3$`All Census Tracts`,"High Opportunity Tracts",df_fin4$`High Opportunity Tracts`,"Low Opportunity Tracts",df_fin5$`Low Opportunity Tracts`)
      b <- c("Very High Opportunity Tracts", df_fin3$`Very High Opportunity Tracts`,"Average Opportunity Tracts",df_fin4$`Average Opportunity Tracts`,"Very Low Opportunity Tracts",df_fin5$`Very Low Opportunity Tracts`)
      df_first<- data.frame(a,b)
      # output$table <- renderTable(df_first, colnames = FALSE, striped = TRUE, width = "600",align = 'c', bordered = TRUE, digits = 1)
      output$exp_val<-renderTable(df_fin3$`All Census Tracts`,colnames = FALSE,digits = 1,width = "100%",align = 'c')
      }
    }
    # df_fin3 <- knitr::kable(df_fin3, "html")
    # column_spec(df_fin3, 1:2, width = "20em", bold = TRUE, italic = TRUE)
    # output$table3 <- renderTable(df_fin3, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
    # output$table5 <- renderTable(df_fin4, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
    # output$table6 <- renderTable(df_fin5, striped = TRUE, bordered = TRUE, width = "350", align = 'c', digits = 1)
    #   }else{
    #   # df_leb <- data.frame( "name" = df_mas_leb$Mean_leb[df_mas_leb$Group_LHD == input$cty])
    #   # names(df_leb)[1] <- paste0("Mean Life Expectancy in ",input$cty,sep="")
    #   # output$table4 <- renderTable(df_leb, striped = TRUE, bordered = TRUE, width = "600", align = 'c')
    #   master_2 <- subset (master, Group_LHD == input$cty)
    #   master_2 <- master_2[order(master_2$HOI),]
    #   qs <- quantile(master_2$HOI, seq(0, 1, length.out = 6))
    #   master_2$hoi_grp <- as.numeric(cut(master_2$HOI, unique(qs), include.lowest = TRUE, na.rm = TRUE))
    #   data_mas_2<-group_by(master_2,hoi_grp)
    #   df_mas_2<-summarise(data_mas_2, Mean = round(mean(LEB_Imputed),digits =1))
    #   df_fin3 <- data.frame("All Tracts" = round(mean(master_2$LEB_Imputed),digits = 1), "Very High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==5],digits =1), check.names = FALSE)
    #   df_fin4 <- data.frame("High Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==4],digits =1), "Average Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==3],digits =1), check.names = FALSE)
    #   df_fin5 <- data.frame("Low Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==2],digits =1), "Very Low Opportunity Tracts" = round(df_mas_2$Mean[df_mas_2$hoi_grp==1],digits =1), check.names = FALSE)
    #   output$table3 <- renderTable(df_fin3, striped = TRUE, bordered = TRUE, width = "350", align = 'c')
    #   output$table5 <- renderTable(df_fin4, striped = TRUE, bordered = TRUE, width = "350", align = 'c')
    #   output$table6 <- renderTable(df_fin5, striped = TRUE, bordered = TRUE, width = "350", align = 'c')
    # }
  })
  # ------------------------------------------------
}
shinyApp(ui, server)