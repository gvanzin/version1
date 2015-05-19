
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer) 
library(tidyr)


melt_crops<-as.data.frame(read.table("./data/crops2.txt",sep=",",header = T),row.names=NULL)
melt_live<-as.data.frame(read.table("./data/live2.txt",sep=",",header = T),row.names=NULL)
melt_shg<-as.data.frame(read.table("./data/shg2.txt",sep=",",header = T),row.names=NULL)
melt_water<-as.data.frame(read.table("./data/water2.txt",sep=",",header = T),row.names=NULL)


melt_crops$cpid<-as.character(melt_crops$cpid)
melt_live$cpid<-as.character(melt_live$cpid)
melt_shg$cpid<-as.character(melt_shg$cpid)
melt_water$cpid<-as.character(melt_water$cpid)

blocks<-toupper(c("KACHCHH","DEVAS","KHANDWA","MAHBUBNAGAR","MALKANGIRI","PALAMU","BANKURA"))
crop_categories<-toupper(c("Cereals","Pulses","Oilseeds","Fibre","Fruit","Vegetable","Others"))
live_categories<-toupper(c("agriculture","tree based livelihood","sheep/goat rearing","dairy cattle","other livestocks","fishing/aquaculture","home production(small-scale processing)","trade/business/services","ntfp collection","salaried job","pension / remittances","local labor (agri,skilled,unskilled)","migratory labor(agri,skilled,unskilled)"))
shg_categories<-toupper(c("Public Institutions","Community initiated Civic Institutions","NGO initiated Civic Institutions","Government initiated Civic Institutions","Self-Help Micro finance Groups"))
water_categories<-toupper(c("Pond","Tank","Ahar","Check dam","Spring","Stream or River","Canal"))

shinyServer(function(input, output) {

  output$text1<-renderText({
    input$block
    
    
  })

  plot1<-function(){
    if(input$option==1)
    {
      if(input$view1==1)
      {    
        data<-subset(subset(melt_crops,cpid==input$block),crop.category %in% input$relation1)
        ggplot(data,aes(variable,fill=..count..))+geom_bar()+facet_wrap(~crop.category)+scale_x_discrete()
      }
      else
      {
        data<-subset(subset(melt_crops,cpid==input$block),crop.category %in% input$relation1)
        data<-data[order(as.numeric(data$variable)),]
        ggplot(data,aes(variable,fill=crop.category))+geom_bar(position="fill")+scale_fill_brewer(palette="Set2",name="Crop Categories")+scale_x_discrete()
      }
      
    }
    
    
    
    else if(input$option==2)
    {
      if(input$view2==1)
      {    data<-subset(subset(melt_live,cpid==input$block),livelihood.category %in% input$relation2)
           data<-data[order(as.numeric(data$variable)),]
           ggplot(data,aes(variable,fill=..count..))+geom_bar()+facet_wrap(~livelihood.category)+scale_x_discrete()           
      }
      else
      {
        data<-subset(subset(melt_live,cpid==input$block),livelihood.category %in% input$relation2)
        data<-data[order(as.numeric(data$variable)),]
        ggplot(data,aes(variable,fill=livelihood.category))+geom_bar(position="fill")+scale_fill_brewer(palette="Set3",name="livelihood Categories")+scale_x_discrete() 
      }
      
    }
    
    else if(input$option==3)
    {
      
      
    }
    
    else if(input$option==4)
    {
      if(input$view4==1)
      {    data<-subset(subset(melt_water,cpid==input$block),water.unit.type %in% input$relation4)
           data<-data[order(as.numeric(data$variable)),]
           ggplot(data,aes(variable,fill=..count..))+geom_bar()+facet_wrap(~water.unit.type)+scale_x_discrete()
           
      }
      else
      {
        data<-subset(subset(melt_water,cpid==input$block),water.unit.type %in% input$relation4)
        data<-data[order(as.numeric(data$variable)),]
        ggplot(data,aes(variable,fill=water.unit.type))+geom_bar(position="fill")+scale_fill_brewer(palette="Set2",name="Water body Categories")+scale_x_discrete()
      }
    }
    
    
  }
  output$plot1 <- renderPlot({
    plot1()
  
  })
  
  output$plot2 <- renderPlot({
    plot1()
    
  })
  
  output$plot3 <- renderPlot({
    plot1()
    
  })

  output$plot4 <- renderPlot({
    plot1()
    
  })
  
  output$plot5 <- renderPlot({
    plot1()
    
  })

  output$plot6 <- renderPlot({
    plot1()
    
  })
  output$plot7 <- renderPlot({
    plot1()
    
  })
  
#######################################################
  output$PLOT_1<-renderPlot({

  
  if(input$All){selected<-blocks}
  else{selected<-input$checkGroup}

  if(input$option3==1){ ## cp site vs category
  if(input$option2==1){
    if(input$option5==1)
      ggplot(subset(melt_crops,cpid %in% selected),aes(cpid,fill=as.character(crop.category)))+geom_bar(width=.5,position="fill")+scale_fill_brewer(palette="Set2",name="Crop Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")+coord_flip()
    else if(input$option5==3)
      ggplot(subset(melt_crops,cpid %in% selected),aes(cpid,fill=as.character(crop.category)))+geom_bar(width=.8,position="dodge")+scale_fill_brewer(palette="Set2",name="Crop Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")+coord_flip()
    else
      ggplot(subset(melt_crops,cpid %in% selected),aes(cpid))+geom_bar(width=.3)+facet_wrap(~crop.category)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")
  }
  
  else if(input$option2==2){
    if(input$option5==1)
    ggplot(subset(melt_live,cpid %in% selected),aes(cpid,fill=as.character(livelihood.category)))+geom_bar(width=.5,position="fill")+scale_fill_brewer(palette="Set3",name="Livelihood Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")+coord_flip()
    else if(input$option5==3)
      ggplot(subset(melt_live,cpid %in% selected),aes(cpid,fill=as.character(livelihood.category)))+geom_bar(width=.8,position="dodge")+scale_fill_brewer(palette="Set3",name="Livelihood Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")+coord_flip()  
    else
      ggplot(subset(melt_live,cpid %in% selected),aes(cpid))+geom_bar(width=.3)+facet_wrap(~livelihood.category)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of units")
  }
  
  else if(input$option2==3){
    if(input$option5==1)
    ggplot(subset(melt_shg,cpid %in% selected),aes(cpid))+geom_bar()
    else{}
  }
  
  else if(input$option2==4){
    if(input$option5==1)
    ggplot(subset(melt_water,cpid %in% selected),aes(cpid,fill=as.character(water.unit.type)))+geom_bar(width=.5,position="fill")+scale_fill_brewer(palette="Set2",name="Water Body Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")+coord_flip()
    else if(input$option5==3)
      ggplot(subset(melt_water,cpid %in% selected),aes(cpid,fill=as.character(water.unit.type)))+geom_bar(width=.8,position="dodge")+scale_fill_brewer(palette="Set2",name="Water Body Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of crops")+coord_flip()  
    else
      ggplot(subset(melt_water,cpid %in% selected),aes(cpid))+geom_bar(width=.3)+facet_wrap(~water.unit.type)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("CP Site")+ylab("Number of water units")
  }
}

else { ## category vs cp site
  
  if(input$option2==1){ # crops
    if(input$option5==1)
    ggplot(subset(melt_crops,cpid %in% selected),aes(crop.category,fill=as.character(cpid)))+geom_bar(width=.5,position="fill")+scale_fill_brewer(palette="Set2",name="CP Site")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Crop categories")+ylab("Number of crops")+coord_flip()
    else if(input$option5==3)
      ggplot(subset(melt_crops,cpid %in% selected),aes(crop.category,fill=as.character(cpid)))+geom_bar(width=.8,position="dodge")+scale_fill_brewer(palette="Set2",name="CP Site")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Crop categories")+ylab("Number of crops")+coord_flip() 
    else 
      ggplot(subset(melt_crops,cpid %in% selected),aes(crop.category))+geom_bar(width=.5)+facet_wrap(~cpid)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Crop Categories")+ylab("Number of crop units")
  }
  
  else if(input$option2==2){# livelihood
    if(input$option5==1)
    ggplot(subset(melt_live,cpid %in% selected),aes(livelihood.category,fill=as.character(cpid)))+geom_bar(width=.5,position="fill")+scale_fill_brewer(palette="Set2",name="CP Site")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Livelihood Categories")+ylab("Number of units")+coord_flip()
    else if(input$option5==3)
      ggplot(subset(melt_live,cpid %in% selected),aes(livelihood.category,fill=as.character(cpid)))+geom_bar(width=.8,position="dodge")+scale_fill_brewer(palette="Set2",name="CP Site")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Livelihood categories")+ylab("Number of units")  
    else
      ggplot(subset(melt_live,cpid %in% selected),aes(livelihood.category))+geom_bar(width=.5)+facet_wrap(~cpid)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("Livelihood Categories")+ylab("Number of units")
    
  }
  
  else if(input$option2==3){# shg
    
    ggplot(subset(melt_shg,cpid %in% selected),aes(cpid))+geom_bar()
  }
  
  else if(input$option2==4){# surface water units
    if(input$option5==1)
    ggplot(subset(melt_water,cpid %in% selected),aes(water.unit.type,fill=as.character(cpid)))+geom_bar(width=.5,position="fill")+scale_fill_brewer(palette="Set2",name="CP Site")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Categories")+ylab("Number of water units")+coord_flip()
    else if(input$option5==3)
      ggplot(subset(melt_water,cpid %in% selected),aes(water.unit.type,fill=as.character(cpid)))+geom_bar(width=.8,position="dodge")+scale_fill_brewer(palette="Set2",name="CP Site")+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Categories")+ylab("Number of water units")+coord_flip() 
    else 
      ggplot(subset(melt_water,cpid %in% selected),aes(water.unit.type))+geom_bar(width=.5)+facet_wrap(~cpid)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+xlab("Surface Water Categories")+ylab("Number of water units")
    
  }
}


})


####################################################################
output$table <- DT::renderDataTable({
  if(input$All){selected<-blocks}
  else{selected<-input$checkGroup}
  
  if(input$option2==1)
  {tab<-spread(as.data.frame(table(subset(select(melt_crops,cpid,crop.category),cpid %in% selected))),crop.category,Freq)
   DT::datatable(tab,options=list(scrollX=TRUE))
  }
  else if(input$option2==2)
  {
    tab<-spread(as.data.frame(table(subset(select(melt_live,cpid,livelihood.category),cpid %in% selected))),livelihood.category,Freq)
    DT::datatable(tab,options=list(scrollX=TRUE)) 
  }
  else if(input$option2==3){
    
  }
  
  else if(input$option2==4){
    tab<-spread(as.data.frame(table(subset(select(melt_water,cpid,water.unit.type),cpid %in% selected))),water.unit.type,Freq)
    DT::datatable(tab,options=list(scrollX=TRUE)) 
  }
  
  
  
})


output$PLOT_3<-renderPlot({
  if(input$option==1)
  {tab<-as.data.frame(table(select(melt_crops,cpid,crop.category)))
  ggplot(tab,aes((crop.category),Freq))+geom_boxplot(aes(fill=factor(crop.category)))+scale_fill_brewer(palette="Set2",name="Crop Categories")
  }
  else if(input$option==2)
  {
    tab<-as.data.frame(table(select(melt_live,cpid,livelihood.category)))
    ggplot(tab,aes((livelihood.category),Freq))+geom_boxplot(aes(fill=factor(livelihood.category)))+scale_fill_brewer(palette="Set2",name="Livelihood Categories")+scale_fill_brewer(palette="Set3",name="Livelihood Categories")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
  }
  else if(input$option==3)
  {
    
  }
  else if(input$option==4)
  {
    tab<-as.data.frame(table(select(melt_water,cpid,water.unit.type)))
    ggplot(tab,aes((water.unit.type),Freq))+geom_boxplot(aes(fill=factor(water.unit.type)))+scale_fill_brewer(palette="Set2",name="surface water Categories")
  }
})



})

