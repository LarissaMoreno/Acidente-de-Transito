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
library(shinydashboard)
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
  navbarPage(   tags$style(HTML("
.navbar { background-color: #1B998B;}
.navbar-default .navbar-nav > li > a {color:black;}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:focus,
 .navbar-default .navbar-nav > li > a:hover {color: black;background-color:#BAF3EC;text-decoration:underline;}
 ")),
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
##############################################tabset1             
             tabsetPanel(tabPanel("Home",sidebarPanel(h3("Acidente de Trânsito"),
                                                      h6("No Brasil, nos últimos anos, em decorrência da estabilidade econômica e ao aumento da população,
                                                         o volume de carros tem aumentado significativamente. Como existe poucos estudos a respeito dos acidentes
                                                         de trânsito, esse aplicativo visa contribuir para o desenvolvimento dessa área de estudo em relação as rodovias
                                                         federais do Brasil."),
                                                      p(
                                                        h6("Os acidentes de trânsito se configuram como grave problema de saúde pública no País pois, aumenta a quantidade de
                                                           atendimentos em hospitais e uma boa quantidade de recursos da área médica devem ser direcionados a esses atendimentos.
                                                           Essas emergências têm, porém, um aspecto particular: a maioria delas é evitável"))),
                                  mainPanel(column(6,HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/UttRHB2cT4o" frameborder="0" allow="accelerometer; 
                                                          autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                                  ),
                                  mainPanel(column(6,imageOutput("image")),
                                            sidebarPanel(h5("Um estudo realizado pelo “perguntar para o professor” mostrou que 57% dos acidentes são causados 
                                                            por falha humana, 3% devido as condições da via e 2% por causa de problemas nos veículos. "),
                                                         h5("A “falha humana”, portanto, é a principal fator dos acidentes nas estradas e ruas brasileiras, e ela é causada principalmente pela 
                                                            imprudência dos motoristas, infração das leis de trânsito e falta de atenção do condutor.")))
                                                         ),
####################################################tabset2
                         tabPanel("Mortalidade",box(h3("Mortalidade"),
                                                    h5("o	De acordo com a OMS 1,25 milhão de pessoas morrem, no mundo,
                                                       por ano em acidentes de trânsito, e dessa total metade das vítimas são pedestres, ciclistas e motociclistas."),
                                                    h5("A cada dia, ocorre 14 mortes no trânsito no Brasil"),
                                                    h5("A cada dia, o Brasil registra 190 acidentes nas rodovias federais."),
                                                    h5("São 82 acidentes com vítimas a cada 100 km de rodovia federal no Brasil "),
                                                    h5("Colisão é o tipo mais comum de acidentes com vítimas no Brasil"),
                                                    h5("Sudeste e Sul concentram os maiores índices de acidentes com vítimas"),
                                                    h5("As rodovias do Nordeste são as que mais matam no Brasil "),
                                                    h5("Nordeste, Norte e Centro-Oeste registram acidentes mais graves")
                                                    ),
                                  box( h3("Mortalidade"),h5("Minas Gerais é campeã em número de mortes e de acidentes nas rodovias federais "),
                                       h5("Minas Gerais também está à frente do ranking de custos com acidente"),
                                       h5("DF registra quatro vezes mais acidentes do que a média nacional  "),
                                       h5("Maranhão, Amazonas, Alagoas, Tocantins, e Bahia registram os acidentes mais graves "),
                                       h5("As rodovias do Paraná concentram mais mortes de ciclistas "),
                                       h5("A maior parte das mortes por atropelamento também ocorre no Paraná "),
                                       h5("Nordeste é a região com maior número de mortes de motociclistas "),
                                       h5("Goiás concentra 40% dos acidentes com motos nas rodovias federais do Centro-Oeste")),
                                  mainPanel(splitLayout( cellWidths = c("70%", "70%"),plotOutput("plot1"), plotOutput("plot2")) ))
                                  )),
###########################################2 Painel
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
############################################3 Painel
    tabPanel("Classificação dos Acidentes",
             inputPanel(
               selectInput("ano1","ano",choices =unique(dados$ano)),
               selectInput("causa","causa do acidente",choices =unique(dados$causa_acidente))),
             mainPanel(splitLayout(cellWidths = c("100%","50%"),plotOutput("plot6"), uiOutput("plot7")) )
    ),
############################################4 Painel
    tabPanel("Condição Meteoroliga",
             inputPanel(selectInput("ano1","ano",choices =unique(dados$ano)),
                        selectInput("causa","causa do acidente",choices =unique(dados$causa_acidente))
             ))
             )
  )
server=function(input,output){
  output$image=renderImage({
    list(src="elementos.png",contentType="elementos/png",width = 700,
         height = 400)
  },deleteFile = F)
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

shinyApp(ui, server)
######################
###################
######################
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
library(geojsonio)
library(stats)
library(directlabels)
library(shinyWidgets)
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

library(shinydashboard)
note = expression(paste(italic("Note: "), "Fonte: MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM"))

ui = tagList(
  navbarPage(tags$style(HTML("
.navbar { background-color: #1B998B;}
.navbar-default .navbar-nav > li > a {color:black;}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:focus,
 .navbar-nav > li > a, .navbar {min-height:60px !important;}
body{
  background-image: url(https://icetran.com.br/blog/wp-content/uploads/2016/05/legislacao-001-768x405.jpg);
  background-size: cover;
  background-position: center;
  background-attachment: fixed;
  background-repeat: no-repet;
}
.shiny-input-panel{padding: 0px 0px !important;}


")),
    tabPanel("Home Page",
             inputPanel(style = 'background-color: #B5BCB3;',
                        selectInput("ano4","Ano",choices =sort(unique(tentativa$ano)),selected = 2018),
                        pickerInput("uf4","Uf",choices= list("Norte" = list("Acre","Amazonas","Amapá","Tocantins","Pará","Roraima","Rondônia"),
                                                             "Nordeste" = list("Ceará","Alagoas","Bahia","Maranhão","Paraíba","Pernambuco",
                                                                               "Piauí","Rio Grande do Norte","Sergipe"),
                                                             "Centro-Oeste" = list("Goiás","Mato Grosso","Mato Grosso do Sul","Distrito Federal"),
                                                             "Sudeste"=list("Espírito Santo","Minas Gerais","Rio de Janeiro","São Paulo"),
                                                             "Sul"=list("Paraná","Santa Catarina","Rio Grande do Sul")),selected = "Distrito Federal",
                                    multiple = T, options = list(`actions-box` = TRUE))
             ),
##############################################tabset1             
             tabsetPanel(tabPanel("Home",sidebarPanel(h3("Acidente de Tr?nsito"),
                                                      style = "color: white; background-color:#B5BCB3",
                                                      h6("No Brasil, nos últimos anos, em decorrência da estabilidade econômica e ao aumento da população,
                                                         o volume de carros tem aumentado significativamente. Como existe poucos estudos a respeito dos acidentes
                                                         de trânsito, esse aplicativo visa contribuir para o desenvolvimento dessa área de estudo em relação as rodovias
                                                         federais do Brasil."),
                                                      p(
                                                        h6("Os acidentes de trânsito se configuram como grave problema de saúde pública no País pois, aumenta a quantidade de
                                                           atendimentos em hospitais e uma boa quantidade de recursos da área médica devem ser direcionados a esses atendimentos.
                                                           Essas emergências têm, porém, um aspecto particular: a maioria delas é evitável"))),
                                  mainPanel(column(6,HTML('<iframe width="500" height="300" src="https://www.youtube.com/embed/UttRHB2cT4o" frameborder="0" allow="accelerometer; 
                                                          autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                                  mainPanel(column(5,imageOutput("image")),
                                            sidebarPanel(h5("Um estudo realizado pelo “perguntar para o professor” mostrou que 57% dos acidentes são causados 
                                                            por falha humana, 3% devido as condições da via e 2% por causa de problemas nos veículos. "),
                                                         h5("A “falha humana”, portanto, é a principal fator dos acidentes nas estradas e ruas brasileiras, e ela é causada principalmente pela 
                                                            imprudência dos motoristas, infração das leis de trânsito e falta de atenção do condutor."),
                                                         width = 7,
                                                         style = "color: white; background-color:#B5BCB3"))
                                                         ),
####################################################tabset2
                         tabPanel("Mortalidade",
                                  sidebarPanel(h3("Mortalidade"),width = 6,
                                                             style = "color: white; background-color:#B5BCB3",
                                               h5("o	De acordo com a OMS 1,25 milhão de pessoas morrem, no mundo,
                                                       por ano em acidentes de trânsito, e dessa total metade das vítimas são pedestres, ciclistas e motociclistas."),
                                               h5("A cada dia, ocorre 14 mortes no trânsito no Brasil"),
                                               h5("A cada dia, o Brasil registra 190 acidentes nas rodovias federais."),
                                               h5("São 82 acidentes com vítimas a cada 100 km de rodovia federal no Brasil "),
                                               h5("Colisão é o tipo mais comum de acidentes com vítimas no Brasil"),
                                               h5("Sudeste e Sul concentram os maiores índices de acidentes com vítimas"),
                                               h5("As rodovias do Nordeste são as que mais matam no Brasil "),
                                               h5("Nordeste, Norte e Centro-Oeste registram acidentes mais graves")
                                                    ),
                                  sidebarPanel( width = 6,style = "color: white; background-color:#B5BCB3",
                                                h3("Mortalidade"),h5("Minas Gerais é campeã em número de mortes e de acidentes nas rodovias federais "),
                                                h5("Minas Gerais também está à frente do ranking de custos com acidente"),
                                                h5("DF registra quatro vezes mais acidentes do que a média nacional  "),
                                                h5("Maranhão, Amazonas, Alagoas, Tocantins, e Bahia registram os acidentes mais graves "),
                                                h5("As rodovias do Paraná concentram mais mortes de ciclistas "),
                                                h5("A maior parte das mortes por atropelamento também ocorre no Paraná "),
                                                h5("Nordeste é a região com maior número de mortes de motociclistas "),
                                                h5("Goiás concentra 40% dos acidentes com motos nas rodovias federais do Centro-Oeste")),
                                  mainPanel(splitLayout( cellWidths = c("70%", "70%"),plotOutput("plot1"), plotOutput("plot2")) ))
                                  )),
###########################################2 Painel
    tabPanel("Mapa",
             inputPanel(style = 'background-color: #B5BCB3;',
                        selectInput("ano3","ano",choices =c(2018,2019)),
                        selectInput("tipo","tipo de acidente",choices =unique(dados$tipo_acidente)),
                        selectInput("uf","uf",choices= list("Norte" = list("AC","AM","AP","TO","PA","RR","RO"),
                                                            "Nordeste" = list("CE","AL","BA","MA","PA","PE",
                                                                              "PI","RN","SE"),
                                                            "Centro-Oeste" = list("GO","MT","MS","DF"),
                                                            "Sudeste"=list("ES","MG","RJ","SP"),
                                                            "Sul"=list("PR","SC","RS")),selected = "DF")),
             
             mainPanel(splitLayout(cellWidths = c("70%","80%"),plotOutput("plot4"),leafletOutput("plot3")), column(8,plotOutput("plot5")))),
############################################3 Painel
    tabPanel("Classificação dos Acidentes",
             inputPanel(style = 'background-color: #B5BCB3;',
               selectInput("ano1","ano",choices =unique(dados$ano)),
               selectInput("causa","causa do acidente",choices =unique(dados$causa_acidente))),
             mainPanel(splitLayout(cellWidths = c("100%","50%"),plotOutput("plot6",width = "100%",height = "600px"), 
                                   uiOutput("plot7")) )
    ),
############################################4 Painel
    tabPanel("Condição Meteoroliga",
             
             inputPanel(style = 'background-color: #B5BCB3;',
                        selectInput("ano1","ano",choices =unique(dados$ano)),
                        selectInput("causa","causa do acidente",choices =unique(dados$causa_acidente))
             ))


             )
  )


shinyApp(ui, server)

server=function(input,output){
  output$image=renderImage({
    list(src="elementos.png",contentType="elementos/png",width = "100%",
         height = 300)
  },deleteFile = F)
  output$plot1=renderPlot({
    pyramid<-tentativa%>%
      filter(uf %in% input$uf4 & ano %in% input$ano4)
    pyramid$n=ifelse(pyramid$sexo=="masculino",-pyramid$n,pyramid$n)
    ggplot(pyramid, aes(x = idade, y = n, fill = sexo)) + 
      geom_bar(subset = (pyramid$sexo == "feminino"), stat = "identity") + 
      geom_bar(subset = (pyramid$sexo == "masculino"), stat = "identity") + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1") + 
      theme_bw()+ggtitle("Pir?mide Et?ria para mortalidade")+
      labs(caption = note)
  })
  
  output$plot2=renderPlot({
   
    y=mortalidade1%>%filter(uf %in% input$uf4)
    ggplot(y,aes(x=ano,y=taxa))+geom_line(size=1.2,color="#1B998B")+
      scale_x_continuous("Ano", 
                         breaks = c(2018,2019))+
      labs(y="Taxa de Mortalidade",caption = note)+
      geom_dl(aes(label = round(taxa,3)), method = list(dl.trans(x = x + .1), "last.points"))+
      geom_dl(aes(label = round(taxa,3)), method = list(dl.trans(x = x - .1), "first.points"))+
      ggtitle("Taxa de Mortalidade")
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
      labs(y="Frequ?ncia")+
      theme_classic()+
      xlab("Hor?rio")+ggtitle("Número de Acidentes por hor?rio")
    
  })  
  
  
  output$plot5=renderPlot({
    barras=dados%>%
      filter(uf %in% input$uf & ano %in% input$ano3 & tipo_acidente %in% input$tipo)%>%
      group_by(dia_semana)%>%summarise(n=n())%>%mutate(proporcao = (n/sum(n))*100)
    
    names(barras)<-c("dia","Acidentes","proporcao")
    barras$dia=gsub("-feira","",barras$dia)
    barras$dia<-  factor(barras$dia,
                         levels=c("domingo","segunda","ter?a","quarta",
                                  "quinta","sexta","s?bado"))
    
    barras=barras[order(barras$dia), ]
    
    ggplot() + geom_bar(aes(x = dia, y = Acidentes),fill="#1B998B", 
                        data = barras, stat = 'identity')+
      labs(x="Dia da semana",y="Frequ?ncia")+
      geom_text(data=barras,aes(x = dia, y = Acidentes,label=paste0(round(proporcao,3),"%")),
                vjust=-0.25,check_overlap = T,position = position_dodge(width = 0.9))+
      theme(axis.text.x = element_text(angle = 30))+
      ggtitle("N?mero de Acidentes por dia da Semana")
    
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
      coord_fixed()+
      ggtitle("Calend?rio do N?mero de Acidentes por Causa")
  })
  
  output$plot7=renderUI({
    table=dados%>%filter(causa_acidente %in% input$causa & ano %in% input$ano1)%>%
      select(tipo_acidente) %>%
      group_by(tipo_acidente) %>%
      summarise(frequencia = n())%>%arrange(desc(frequencia))
    table$porcentagem=round(table$frequencia/sum(table$frequencia),3)
    colnames(table)=c("Tipo de Acidente","Frequ?ncia","Porcentagem")
    HTML(
      kable(table,caption = "Tabela N?mero de Acidentes por Tipo")
      )
  })
}


shinyApp(ui, server)
