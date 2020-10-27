library(flexdashboard)
library(readr)
library(readxl)
library(kableExtra)
library(ggplot2)
library(DT)
library(dplyr)
library(shiny)
library(lubridate)
library(stringr)
library(leaflet)
library(stats)
library(tidyverse)
library(d3treeR)
library(treemap)
library(treemapify)
library(geojsonio)
library(stats)
library(directlabels)
library(shinyWidgets)
library(shinythemes)
states <- geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson",  what = "sp")
dados=readRDS("total1.rds")
mortalidade1 <- read_excel("mortalidade1.xlsx")
tentativa <- read_excel("tentativa.xlsx")

dados$horas=substring(dados$horario,1,2)

wom <- function(date) { 
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}
teste=data.frame(data_inversa=unique(dados$data_inversa))


tentativa$Total=as.numeric(tentativa$Total)

ui = tagList(
  navbarPage(
    theme = shinytheme("united"),
    tabPanel("Navbar 2","Acidente de Transito")    ,
    tabPanel("Home Page",
             inputPanel(
               selectInput("ano4","Ano",choices =sort(unique(tentativa$ano)),selected = 2018),
               pickerInput("uf4","Uf",choices= list("Norte" = list("Acre","Amazonas","Amapá","Tocantins","Pará","Roraima","Rondônia"),
                                                    "Nordeste" = list("Ceará","Alagoas","Bahia","Maranhão","Paraíba","Pernambuco",
                                                                      "Piauí","Rio Grande do Norte","Sergipe"),
                                                    "Centro-Oeste" = list("Goiás","Mato Grosso","Mato Grosso do Sul","Distrito Federal"),
                                                    "Sudeste"=list("Espírito Santo","Minas Gerais","Rio de Janeiro","São Paulo"),
                                                    "Sul"=list("Paraná","Santa Catarina","Rio Grande do Sul")),selected = "Distrito Federal",
                           multiple = T, options = list(`actions-box` = TRUE))
             ),
             tabsetPanel(tabPanel("Home",mainPanel(
               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/UttRHB2cT4o" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
               tabPanel("Mortalidade",mainPanel(splitLayout( cellWidths = c("70%", "70%"),plotOutput("plot1"), plotOutput("plot2")) ))
             )),
    tabPanel("Mapa",
             inputPanel(selectInput("ano3","ano",choices =c(2018,2019)),
                        
                        selectInput("tipo","tipo de acidente",choices =unique(dados$tipo_acidente)),
                        selectInput("uf","uf",choices= list("Norte" = list("AC","AM","AP","TO","PA","RR","RO"),
                                                            "Nordeste" = list("CE","AL","BA","MA","PA","PE",
                                                                              "PI","RN","SE"),
                                                            "Centro-Oeste" = list("GO","MT","MS","DF"),
                                                            "Sudeste"=list("ES","MG","RJ","SP"),
                                                            "Sul"=list("PR","SC","RS")),selected = "DF")),
             
             mainPanel(splitLayout(cellWidths = c("70%","80%"),plotOutput("plot4"),leafletOutput("plot3")), column(8,plotOutput("plot5")))),
    tabPanel("Classificação dos Acidentes",
             inputPanel(
               selectInput("ano1","ano",choices =unique(dados$ano)),
               selectInput("causa","causa do acidente",choices =unique(dados$causa_acidente))),
             mainPanel(splitLayout(cellWidths = c("100%","50%"),plotOutput("plot6"), uiOutput("plot7")) )
    ),
    tabPanel("Condição Meteoroliga",
             inputPanel(selectInput("ano1","ano",choices =unique(dados$ano)),
                        selectInput("causa","causa do acidente",choices =unique(dados$causa_acidente))
             ))
  )
)
server=function(input,output){
  output$plot1=renderPlot({
    pyramid<-tentativa%>%
      filter(uf %in% input$uf4 & ano %in% input$ano4)
    pyramid$n=ifelse(pyramid$sexo=="masculino",-pyramid$n,pyramid$n)
    ggplot(pyramid, aes(x = idade, y = n, fill = sexo)) + 
      geom_bar(subset = (pyramid$sexo == "feminino"), stat = "identity") + 
      geom_bar(subset = (pyramid$sexo == "masculino"), stat = "identity") + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1") + 
      theme_bw()
  })
  
  output$plot2=renderPlot({
    y=mortalidade1%>%filter(uf %in% input$uf4)
    ggplot(y,aes(x=ano,y=taxa))+geom_line(size=1.2,color="#1B998B")+
      scale_x_continuous("Ano", 
                         breaks = c(2018,2019))+
      labs(y="Taxa de Mortalidade")+
      geom_dl(aes(label = round(taxa,3)), method = list(dl.trans(x = x + .1), "last.points"))+
      geom_dl(aes(label = round(taxa,3)), method = list(dl.trans(x = x - .1), "first.points"))
  })
  output$plot3=renderLeaflet({
    datatran2019=dados%>%filter(ano %in% input$ano3)
    states2<-merge(x=states,y=as.data.frame(table(datatran2019$uf)),by.x='sigla',by.y='Var1')
    bins <- c(0, 50, 200, 500, 1000, 2000, 5000, Inf)
    pal <- colorBin("PuBu", domain =states2$Freq, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%s Acidentes </sup>",
      states$name, states2$Freq
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states) %>%
      setView(-47, -15.8, 3) %>%
      addProviderTiles("Stamen.Watercolor", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(states2$Freq),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~regiao_id, opacity = 0.7, title = NULL,
                position = "bottomleft")%>%
      addMeasure(position = "bottomleft")
  })
  
  output$plot4=renderPlot({
    line=dados%>%
      filter(uf %in% input$uf & ano %in% input$ano3 & tipo_acidente %in% input$tipo)%>%
      group_by(horas)%>%summarise(n=n())
    line$horas<-as.numeric(line$horas)
    ggplot(line,aes(x=horas,y=n))+geom_line(size=1.2,color="#1B998B")+
      scale_x_continuous("horas", labels = as.character(paste(line$horas,"h",sep="")), 
                         breaks = line$horas)+
      labs(y="Frequência")+
      theme_classic()+
      xlab("Horário")
    
  })  
  
  
  output$plot5=renderPlot({
    barras=dados%>%
      filter(uf %in% input$uf & ano %in% input$ano3 & tipo_acidente %in% input$tipo)%>%
      group_by(dia_semana)%>%summarise(n=n())%>%mutate(proporcao = (n/sum(n))*100)
    
    names(barras)<-c("dia","Acidentes","proporcao")
    barras$dia=gsub("-feira","",barras$dia)
    barras$dia<-  factor(barras$dia,
                         levels=c("domingo","segunda","terça","quarta",
                                  "quinta","sexta","sábado"))
    
    barras=barras[order(barras$dia), ]
    
    ggplot() + geom_bar(aes(x = dia, y = Acidentes),fill="#1B998B", 
                        data = barras, stat = 'identity')+
      labs(x="Dia da semana",y="Frequência")+
      geom_text(data=barras,aes(x = dia, y = Acidentes,label=paste0(round(proporcao,3),"%")),
                vjust=-0.25,check_overlap = T,position = position_dodge(width = 0.9))+
      theme(axis.text.x = element_text(angle = 30))
    
  })
  
  
  output$plot6=renderPlot({
    
    
    p=dados%>%filter(ano %in% input$ano1 & causa_acidente %in% input$causa)%>%
      group_by(data_inversa)%>%summarise(counts=sum(ponto))
    
    
    missing<- full_join(p,teste,by="data_inversa")
    missing$counts[is.na(missing$counts)]=0
    p=missing
    
    p$cdow=wday(as.Date(p$data_inversa),label=T)
    p$week  <- wom(as.Date(p$data_inversa))
    p$day   <- mday(as.Date(p$data_inversa))
    p$cmonth<- month(as.Date(p$data_inversa),label=T)
    p$ano=year(as.Date(p$data_inversa))
    p=p[complete.cases(p), ]
    p=p[p$ano %in% input$ano1,]
    
    ggplot(p, aes(x=cdow,y=-week))+
      geom_tile(aes(fill=counts,colour="grey50"))+
      geom_text(aes(label=day),size=3,colour="grey20")+
      facet_wrap(~cmonth, ncol=4)+
      scale_fill_gradient(low = "white", high = "firebrick3", na.value="white")+
      scale_color_manual(guide=F,values="grey50")+
      scale_x_discrete(labels=c("Sab","Seg","Ter","Quar","Qui","Sex","Dom"))+
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(panel.grid=element_blank())+
      labs(x="",y="")+
      coord_fixed()
  })
  
  output$plot7=renderUI({
    table=dados%>%filter(causa_acidente %in% input$causa & ano %in% input$ano1)%>%
      select(tipo_acidente) %>%
      group_by(tipo_acidente) %>%
      summarise(frequencia = n())%>%arrange(desc(frequencia))
    table$porcentagem=round(table$frequencia/sum(table$frequencia),3)
    colnames(table)=c("Tipo de Acidente","Frequência","Porcentagem")
    HTML(
      kable(table))
  })
}  


shinyApp(ui,server)
