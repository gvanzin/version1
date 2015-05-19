library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer) 
library(tidyr)
library(svgPanZoom)

blocks<-toupper(c("KACHCHH","DEVAS","KHANDWA","MAHBUBNAGAR","MALKANGIRI","PALAMU","BANKURA"))
crop_categories<-toupper(c("Cereals","Pulses","Oilseeds","Fibre","Fruit","Vegetable","Others"))
live_categories<-toupper(c("agriculture","tree based livelihood","sheep/goat rearing","dairy cattle","other livestocks","fishing/aquaculture","home production(small-scale processing)","trade/business/services","ntfp collection","salaried job","pension / remittances","local labor (agri,skilled,unskilled)","migratory labor(agri,skilled,unskilled)"))
shg_categories<-toupper(c("Public Institutions","Community initiated Civic Institutions","NGO initiated Civic Institutions","Government initiated Civic Institutions","Self-Help Micro finance Groups"))
water_categories<-toupper(c("Pond","Tank","Ahar","Check dam","Spring","Stream or River","Canal"))

shinyUI(
dashboardPage(
  dashboardHeader(title = "DataVisualisation"),
  dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Intra CP Site", tabName = "tab1", icon = icon("th")),
                menuItem("Inter CP Site", tabName = "tab2", icon = icon("th"))
    )
    
  ),
  dashboardBody(
    
      tabItems(
        tabItem(tabName="tab1",
                fluidRow(
                  tabBox(
                    title=tagList(shiny::icon("table"),"Block"),
                    id="block",
                    width=8,
                    selected=blocks[1],
                    tabPanel(blocks[1],plotOutput("plot1",width="100%")),
                    tabPanel(blocks[2],plotOutput("plot2")),
                    tabPanel(blocks[3],plotOutput("plot3")),
                    tabPanel(blocks[4],plotOutput("plot4")),
                    tabPanel(blocks[5],plotOutput("plot5")),
                    tabPanel(blocks[6],plotOutput("plot6")),
                    tabPanel(blocks[7],plotOutput("plot7"))
                  ),#end tabbox
                  
                  box(status="primary",width=4,
                    selectInput("option","Select the category",choices=list("Crops"=1,"Livelihood"=2,"Self Help Groups"=3,"Water"=4,"none"=5),selected=1),
                    conditionalPanel(condition="input.option==1",
                                     selectInput("view1",label="Select the way you want to look : ",choices=list("Stacked"=2,"Faceted"=1),selected=2),
                                     checkboxGroupInput("relation1",label="Select sub-categories :",choices=crop_categories,selected=head(crop_categories,n=2))
                    ),
                    #livelihood
                    conditionalPanel(condition="input.option==2",
                                     selectInput("view2",label="Select the way you want to look : ",
                                                 choices=list("Stacked"=2,"Faceted"=1),
                                     ),
                                     checkboxGroupInput("relation2",label="Select sub-categories :",
                                                        choices=live_categories,selected=head(live_categories,n=2))
                                     
                    ),
                    #self_help_groups
                    conditionalPanel(condition="input.option==3",
                                     selectInput("xx","Select the sub-category 1 ",choices=shg_categories,selected=head(shg_categories,n=1)),selectInput("yy","Select the sub-category 2 ",choices=shg_categories,selected=tail(shg_categories,n=1))),
                    
                    #surface.water
                    conditionalPanel(condition="input.option==4",
                                     selectInput("view4",label="Select the way you want to look : ",
                                                 choices=list("stacked"=2,"Faceted"=1),
                                     ),
                                     checkboxGroupInput("relation4",label="Select sub-categories :",
                                                        choices=water_categories,selected=head(water_categories,n=2))
                                     
                    )
                    
                    )
                  
                ),#end row
              fluidRow(
                box(width=12,height="600px",plotOutput("PLOT_3",height="600px"))  
                
                )
                
        ),
        
        tabItem(tabName = "tab2",
                fluidRow(column(width=9,
                box(width=12,title="Select the blocks",collapsible=TRUE,solidHeader=TRUE,status="primary",
                                        checkboxGroupInput("checkGroup",label="",choices =(blocks),
                                                              selected=blocks[1:4],inline=TRUE),
                    checkboxInput("All",label="All Blocks together ",value=FALSE)
                    
                    ),
                box(width=12,height="700px",plotOutput("PLOT_1",width="100%",height="600px")),
                box(width=12,dataTableOutput("table"))
                ),
                column(width=3,
                box(width=12,title="Select",collapsible=TRUE,solidHeader=TRUE,status="primary",
                    selectInput("option3","Select to compare : ",choices=list("CP Site vs count (each category)"=1,"Category vs count (each cpsite)"=2)),
                    selectInput("option2","Select Category",choices=list("Crops"=1,"Livelihood"=2,"Self Help Groups"=3,"Water"=4,"none"=5),selected=1),
                    selectInput("option5","Select View :",choices=list("Stacked"=1,"Faceted"=2,"Dodged"=3)))
                ))
                
                
                  
                #column(width=5,box(width=25,height="1000px",plotOutput("PLOT_2",width="100%",height="1000px")))
                
                
               
                
                
        )
      )#end tabitems
      
      
    )#end dashboard
    
    
    )
  
  )



