library(shiny)
library(shinyWidgets)
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)
library(tidyverse)
library(geojsonio)
library(stats)
library(directlabels)
library(DT)
library(plotly)
library(formattable)
require(data.table)
detach("package:data.table")

states <- geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson",  what = "sp")
dados=readRDS("total.rds")
mortalidade1 <- read_excel("mortalidade1.xlsx")
tentativa <- read_excel("tentativa.xlsx")
df1 <- read.csv("fase_dia.csv", sep=",")

dados$horas=substring(dados$horario,1,2)
dados$uso_solo[dados$uso_solo=="Sim"]="Urbano"
dados$uso_solo[dados$uso_solo=="Não"]="Rural"
wom <- function(date) { 
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}
teste=data.frame(data_inversa=unique(dados$data_inversa))


tentativa$Total=as.numeric(tentativa$Total)


note = expression(paste(italic("Note: "), "Fonte: MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM"))





ui = tagList(
  navbarPage(id= "navibar",
             tags$style(HTML("
                             
                             
                             .navbar { background-color: #1B998B;}
                             .navbar-default .navbar-nav > li > a {color:white;}
                             .navbar-default .navbar-nav > .active > a,
                             .navbar-default .navbar-nav > .active > a:focus,
                             .navbar-nav > li > a, .navbar {min-height:60px !important;}
                             
                             .shiny-input-panel{padding: 0px 0px !important;}
                             .tabbable > .nav > li > a[data-value='Mortalidade'] {background-color: #1B998B; color:white}
                             .tabbable > .nav > li > a[data-value='Home'] {background-color: #1B998B; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='home'] {margin-left:500px; width: 10%;}
                             
                             
                             
                             ")),
             tabPanel("Home Page",icon = icon("home", lib =  "glyphicon"),
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
                      tabsetPanel(tabPanel("Home",
                                           
                                           column(4,wellPanel(tags$style(type="text/css", '#leftPanel { width:500px; float:left;position: relative;left: 250px; bottom: -30px}'),
                                                              id = "leftPanel",
                                                              h3("Acidente de Trânsito"),
                                                              style = "color: black; background-color:#B5BCB3 ",
                                                              h6("No Brasil, nos últimos anos, em decorrência da estabilidade econômica e ao aumento da população,
                                                                 o volume de carros tem aumentado significativamente. Como existe poucos estudos a respeito dos acidentes
                                                                 de trânsito, esse aplicativo visa contribuir para o desenvolvimento dessa área de estudo em relação as rodovias
                                                                 federais do Brasil."),
                                                              p(
                                                                h6("Os acidentes de trânsito se configuram como grave problema de saúde pública no País pois, aumenta a quantidade de
                                                                   atendimentos em hospitais e uma boa quantidade de recursos da área médica devem ser direcionados a esses atendimentos.
                                                                   Essas emergências têm, porém, um aspecto particular: a maioria delas é evitável"))
                                                                )),
                                           column(2,mainPanel(style = "position: relative;left: 350px;bottom:-30px;",
                                                              HTML('<iframe width="500" height="250" src="https://www.youtube.com/embed/UttRHB2cT4o" frameborder="0" allow="accelerometer; 
                                                                   autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')))
                                           
                                           ,
                                           fluidRow( mainPanel(style = "position: relative;left: 250px;padding: 35px;",column(5,imageOutput("image")),
                                                               sidebarPanel(style = "position: relative;left: 200px;padding: 35px;",h5("Um estudo realizado pelo “perguntar para o professor” mostrou que 57% dos acidentes são causados 
                                                                                                                                       por falha humana, 3% devido as condições da via e 2% por causa de problemas nos veículos. "),
                                                                            h5("A “falha humana”, portanto, é a principal fator dos acidentes nas estradas e ruas brasileiras, e ela é causada principalmente pela 
                                                                               imprudência dos motoristas, infração das leis de trânsito e falta de atenção do condutor."),
                                                                            width = 7,
                                                                            style = "color: black; background-color:#B5BCB3"))),
                                           mainPanel(style = "height:1px;position: relative;left: -20px;bottom:700px;",imageOutput("image1"))
                                           
                                           ),
                                  ####################################################tabset2
                                  tabPanel("Mortalidade",
                                           sidebarPanel(h3("Mortalidade"),width = 6,
                                                        style = "color: black; background-color:#B5BCB3",
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
                                           sidebarPanel( width = 6,style = "color: black; background-color:#B5BCB3",
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
            #tabPanel("Mapa",icon = icon("map-marked-alt"),
            #         inputPanel(style = 'background-color: #B5BCB3;',
            #                    selectInput("ano3","ano",choices =c(2018,2019)),
            #                    selectInput("tipo","tipo de acidente",choices =unique(dados$tipo_acidente)),
            #                    selectInput("uf","uf",choices= list("Norte" = list("AC","AM","AP","TO","PA","RR","RO"),
            #                                                        "Nordeste" = list("CE","AL","BA","MA","PA","PE",
            #                                                                          "PI","RN","SE"),
            #                                                        "Centro-Oeste" = list("GO","MT","MS","DF"),
            #                                                        "Sudeste"=list("ES","MG","RJ","SP"),
            #                                                        "Sul"=list("PR","SC","RS")),selected = "DF")),
            #         
            #         mainPanel(splitLayout(cellWidths = c("70%","80%"),plotlyOutput("plot4"),leafletOutput("plot3"))),
            #         column(8,mainPanel(style = "position: relative;left: -30px;padding: 35px;",plotlyOutput("plot5",width = "120%")) )),
            #
            #   
            
           tabPanel("Mapa",icon = icon("map-marked-alt"),
                    inputPanel(style = 'background-color: #B5BCB3;',
                               selectInput("ano3","Ano",choices =c(2018,2019)),
                               selectInput("tipo","Tipo de Acidente",choices =unique(dados$tipo_acidente)),
                               selectInput("uf","UF",choices= list("Norte" = list("AC","AM","AP","TO","PA","RR","RO"),
                                                                   "Nordeste" = list("CE","AL","BA","MA","PA","PE",
                                                                                     "PI","RN","SE"),
                                                                   "Centro-Oeste" = list("GO","MT","MS","DF"),
                                                                   "Sudeste"=list("ES","MG","RJ","SP"),
                                                                   "Sul"=list("PR","SC","RS")),selected = "DF")),
                     fluidRow(mainPanel( style="position: relative;left: -30px;padding: 35px;",plotlyOutput("plot4",width = "75%")),
                              mainPanel(style="position: relative;left: 700px;bottom:500px;padding: 35px;",leafletOutput("plot3",width = "75%"))),
                     fluidRow(  mainPanel(style="height:1px;position: relative;left: 700px;bottom:500px;padding: 35px;",leafletOutput("plot99",width = "75%")),
                                mainPanel(style="height:1px;position: relative;left: -30px;bottom:570px;padding: 35px;",plotlyOutput("plot5",width = "75%"))),
                     #fluidRow(mainPanel(style="height:300px;position: relative;left: 250px;bottom:200px;padding: 35px;",plotlyOutput("solo")))
            ),            

             
             ############################################3 Painel
             tabPanel("Classificação dos Acidentes",icon = icon("car"),
                      inputPanel(style = 'background-color: #B5BCB3;',
                                 selectInput("ano1","Ano",choices =unique(dados$ano)),
                                 selectInput("causa","Causa do Acidente",choices =unique(dados$causa_acidente))),
                      mainPanel(splitLayout(cellWidths = c("90%","60%"),plotOutput("plot6",width = "100%",height = "630px"), 
                                            DT::dataTableOutput("plot7")) )
             ),
             ############################################4 Painel
             tabPanel("Condição Meteoroliga",icon = icon("umbrella"),
                      
                      tags$style(type="text/css", 'body {overflow-y: scroll;overflow-x: hidden;
                             }
                                 '),
                      
                      inputPanel(style = 'background-color: #B5BCB3;',
                                 selectInput("ano2","ano",choices =unique(dados$ano)),
                                 selectInput("cond","condição",choices = unique(dados$condicao_metereologica))
                      ),
                      fluidRow(mainPanel( style="position: relative;left: 100px;padding: 35px;",plotlyOutput("p1",width = "50%")),
                               mainPanel(style="position: relative;left: 700px;bottom:475px;padding: 35px;",plotlyOutput("p2",width = "50%"))),
                      fluidRow(  mainPanel(style="height:1px;position: relative;left: 700px;bottom:500px;padding: 35px;",plotlyOutput("p3",width = "50%")),
                                 mainPanel(style="height:1px;position: relative;left: 100px;bottom:570px;padding: 35px;",plotlyOutput("p4",width = "50%"))),
                      fluidRow(mainPanel(style="height:300px;position: relative;left: 250px;bottom:200px;padding: 35px;",plotlyOutput("solo")))
                      ),
             #########################################5 painel
             tabPanel("",icon = icon("fab fa-info-circle"),value = "home",
                      wellPanel(style = "color: black; background-color:#B5BCB3;width:80%;
                                position: relative;left: 125px;",
                                h1("Dicas de transito",style="position: relative;left: 380px;"),
                                h3("Sempre use o cinto de segurança"),
                                h5("O cinto de segurança é um dispositivo de proteção para os ocupantes de um veículo e é de 
                                   uso obrigatório. O principal objetivo é evitar consequências mais sérias no caso de haver uma 
                                   batida, por exemplo."),
                                h5("Colocá-lo deve ser uma das primeiras coisas a fazer antes de começar a dirigir — apesar do fato 
                                   de alguns motoristas ignorarem a regra. Além do risco de trafegar sem o cinto, vale lembrar que
                                   pode haver a aplicação de multa, visto que se trata de uma infração."),
                                
                                h5("Colocar a manutenção em dia, calibrar os pneus de maneira correta e fazer a troca de óleo periodicamente 
                                   são algumas medidas que devem ser praticadas pelos motoristas. Um problema inesperado pode fazer com que 
                                   algum componente essencial, como freios ou direção, não funcione corretamente em um momento crítico,
                                   causando acidentes"),
                                h3("Tenha cuidado com pedestres, ciclistas e motoqueiros"),
                                h5("A regra é simples: o maior sempre tem responsabilidade pela segurança do menor. Assim, pedestres e ciclistas 
                                   devem ter a preferência, bem como motociclistas. Afinal, em uma colisão, os riscos são maiores para quem está 
                                   em um veículo menor ou a pé."),
                                h3("Não usar o celular"),
                                h5("Diversos estudos comprovam que o uso do celular ao volante, mesmo que por alguns segundos, é tão perigoso quanto
                                   dirigir embriagado. Portanto, o aparelho deve ser esquecido enquanto estiver guiando um veículo! Deixe-o no bolso 
                                   ou guardado no console ou no porta-luvas para não correr o risco de “dar uma olhadinha rápida” a cada notificação que chega."),
                                h3("Respeite os limites de velocidade"),
                                h5("As diferentes vias possuem limites de velocidade distintos, mas eles são determinados pensando no tamanho, tráfego, fluxo de pessoas 
                                   e outras variáveis. Eles servem para garantir uma condução segura e que o veículo tenha condições de frear e parar totalmente antes de causar
                                   um acidente mais grave."),
                                h3("Sinalize antes de realizar uma ação"),
                                h5("É muito comum encontrar casos em que o motorista vai realizar uma conversão, 
                                   mas não dá seta antes de virar, por exemplo. Esse tipo de situação aumenta os riscos de
                                   haver uma batida ou mesmo um acidente mais grave — como quando se trata de mudar de faixa 
                                   em uma estrada."),
                                h3("Manter a distância"),
                                h5("Manter a distância é a regra básica para a direção defensiva, logo, torna o trânsito mais seguro 
                                   para todos. Essa atitude dá mais tempo para a reação a uma situação de risco e maior espaço para 
                                   manobras. É também essencial para a prevenção de acidentes e para minimizar as consequências de 
                                   distrações.")),
                      wellPanel(style = "color: black; background-color:#B5BCB3;width:50%;",
                                h2("O que fazer quando ocorre um acidente ",style="position: relative;left: 30px;"),
                                h5("Em primeiro lugar, verifique se alguém se feriu. Se houver uma vítima no acidente de trânsito, independentemente da gravidade, ligue para o 
                                   Serviço de Atendimento Móvel de Urgência (SAMU), por meio do telefone 192, e sinalize a batida. A polícia deve ser acionada sempre que alguém
                                   se machucar. Nesses casos, o boletim de ocorrência (B.O.) para acidentes de trânsito é realizado pelo agente no local.")))
             
             
                                )
                      )


server=function(input,output){
  output$image1=renderImage({
    list(src="imagem2.png",contentType="imagem2/png",width = "250px",
         height = 550)
  },deleteFile = F)
  
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
      theme_bw()+ggtitle("Pirâmide Etária para mortalidade")+
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
  
 
  ##############################################
  
  output$plot99=renderLeaflet({
    datatran2019=dados%>%filter(ano %in% input$ano3)
    datatran2019$ponto<-1
    states2<-merge(x=states,y=as.data.frame(table(datatran2019$uf)),by.x='sigla',by.y='Var1')
    datatran2019%>%leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(
        lng = datatran2019$long, 
        lat = datatran2019$lat, 
        radius = log(datatran2019$ponto), 
        label = datatran2019$datedate, 
        clusterOptions = markerClusterOptions())%>% 
      addMeasure(position = "bottomleft")
  })
  
  ###############################################
  
   
  
  
  
  output$plot4=renderPlotly({
    line=dados%>%
      filter(uf %in% input$uf & ano %in% input$ano3 & tipo_acidente %in% input$tipo)%>%
      group_by(horas)%>%summarise(n=n())
    line$horas<-as.numeric(line$horas)
    #ggplot(line,aes(x=horas,y=n))+geom_line(size=1.2,color="#1B998B")+
    #  scale_x_continuous("horas", labels = as.character(paste(line$horas,"h",sep="")), 
    #                     breaks = line$horas)+
    #  labs(y="Frequ?ncia")+
    #  theme_classic()+
    #  xlab("Horário")+ggtitle("Número de Acidentes por horário")
    plot_ly(x = ~line$horas, y = ~line$n, mode = 'lines',
            hoverinfo = "text",
            text = ~paste("UF:", input$uf, "<br>",
                          "Ano:",input$ano3,"<br>",
                          "Tipo:",input$tipo,"<br>",
                          "Acidentes:", line$n, "<br>",
                          "Horário:", line$horas))%>%
      layout(title='Número de Acidentes por horário',xaxis=list(title='Horário'), 
             yaxis=list(title='Frequência',type='category'))
    
  })  
  
  
  output$plot5=renderPlotly({
    barras=dados%>%
      filter(uf %in% input$uf & ano %in% input$ano3 & tipo_acidente %in% input$tipo)%>%
      group_by(dia_semana)%>%summarise(n=n())%>%mutate(proporcao = (n/sum(n))*100)
    
    names(barras)<-c("dia","Acidentes","proporcao")
    barras$dia=gsub("-feira","",barras$dia)
    barras$dia<-  factor(barras$dia,
                         levels=c("domingo","segunda","terça","quarta",
                                  "quinta","sexta","sábado"))
    
    barras=barras[order(barras$dia), ]
    
    #ggplot() + geom_bar(aes(x = dia, y = Acidentes),fill="#1B998B", 
    #                    data = barras, stat = 'identity')+
    #  labs(x="Dia da semana",y="Porcentagem")+
    #  geom_text(data=barras,aes(x = dia, y = Acidentes,label=paste0(round(proporcao,3),"%")),
    #            vjust=-0.25,check_overlap = T,position = position_dodge(width = 0.9))+
    #  theme(axis.text.x = element_text(angle = 30))+
    #  ggtitle("Número de Acidentes por dia da Semana")+
    #  theme_bw() +
    #  theme(panel.border = element_blank(),
    #        axis.line = element_line(colour = "black"))
    barras%>%plot_ly(x=~dia, y=~Acidentes,hoverinfo = "text",text = ~paste("UF:", input$uf, "<br>","Ano:",input$ano3,"<br>","Tipo:",input$tipo,"<br>","Acidentes:",Acidentes, "<br>","Dia:", dia))%>%
                     add_bars()%>%
                     layout(title='Número de Acidentes por Dia da Semana',xaxis=list(title='Dia da Semana'),yaxis=list(title='Frequência'))
    
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
      ggtitle("Calendário do Número de Acidentes por Causa")
  })
  
  output$plot7=DT::renderDataTable({
    table=dados%>%filter(causa_acidente %in% input$causa & ano %in% input$ano1)%>%
      select(tipo_acidente) %>%
      group_by(tipo_acidente) %>%
      summarise(frequencia = n())%>%arrange(desc(frequencia))
    table$porcentagem=round(table$frequencia/sum(table$frequencia),3)
    colnames(table)=c("Tipo de Acidente","Frequência","Porcentagem")
#    datatable(table,
#              rownames = FALSE, 
#              options = list(pageLength = 16,dom = 't',
#                             headerCallback = JS(
#                               "function( thead, data, start, end, display ) {
#                               $(thead).closest('thead').find('th').eq(3).css('color', 'red');
#                               $(thead).closest('thead').find('th').eq(4).css('color', 'red');
#                               $(thead).closest('thead').find('th').eq(5).css('color', 'blue');
#                               $(thead).closest('thead').find('th').eq(6).css('color', 'blue');
#  }"
#                             ),
#                             initComplete = JS(
#                               "function(settings, json) {",
#                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
#                               "}")
#                             ))
    
    require(data.table)
    as.datatable(formattable(table, align = "l", list(
      area(col = c(1)) ~ formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
      area(col = 2) ~ color_tile("#DeF7E9", "#71CA97"),
      area(col = 3) ~ color_tile("#FFFFFF", "orange")
    )))
    
    
    
})
  
  react=reactive({
    ex1=df1%>%filter(condicao_metereologica %in% input$cond & ano %in% input$ano2)
    
  })
  output$p1=renderPlotly({
    
    #ggplot(subset(react(),fase_dia == "Amanhecer"), aes(x = feridos , y= n)) +
    #  geom_bar(position="dodge", stat = "identity",fill="#1B998B")+
    #  ggtitle("Número de Acidentes no Amanhecer")
    subset(react(),fase_dia == "Amanhecer")%>%plot_ly(x=~n, y=~feridos,hoverinfo = "text",text = ~paste("UF:", input$uf, "<br>","Ano:",input$ano3,"<br>","Tipo:",input$tipo,"<br>",feridos,":",n))%>%
      add_bars(opacity = .5)%>%
      layout(title='Número de Acidentes no Amanhecer',xaxis=list(title='Frequência'),yaxis=list(title=''))
  })  
  output$p2=renderPlotly({
    
    #ggplot(subset(react(),fase_dia == "Anoitecer"), aes(x = feridos , y= n)) +
    #  geom_bar(position="dodge", stat = "identity",fill="#1B998B")+
    #  ggtitle("Número de Acidentes no Anoitecer")
    subset(react(),fase_dia == "Anoitecer")%>%plot_ly(x=~n, y=~feridos,hoverinfo = "text",text = ~paste("UF:", input$uf, "<br>","Ano:",input$ano3,"<br>","Tipo:",input$tipo,"<br>",feridos,":",n))%>%
      add_bars(color=I('#1B998B'),opacity = .5)%>%
      layout(title='Número de Acidentes no Anoitecer',xaxis=list(title='Frequência'),yaxis=list(title=''))
  })
  output$p3=renderPlotly({
    
    #ggplot(subset(react(),fase_dia == "Plena Noite"), aes(x = feridos , y= n)) +
    #  geom_bar(position="dodge", stat = "identity",fill="#1B998B")+
    #  ggtitle("Número de Acidentes em Plena Noite")
    subset(react(),fase_dia == "Plena Noite")%>%plot_ly(x=~n, y=~feridos,hoverinfo = "text",text = ~paste("UF:", input$uf, "<br>","Ano:",input$ano3,"<br>","Tipo:",input$tipo,"<br>",feridos,":",n))%>%
      add_bars(color=I('#1B998B'),opacity = 1)%>%
      layout(title='Número de Acidentes em Plena Noite',xaxis=list(title='Frequência'),yaxis=list(title=''))
  })
  output$p4=renderPlotly({
    
    #ggplot(subset(react(),fase_dia == "Pleno dia"), aes(x = feridos , y= n)) +
    #  geom_bar(position="dodge", stat = "identity",fill="#1B998B")+
    #  ggtitle("Número de Acidentes em Pleno dia")
    subset(react(),fase_dia == "Pleno dia")%>%plot_ly(x=~n, y=~feridos,hoverinfo = "text",text = ~paste("UF:", input$uf, "<br>","Ano:",input$ano3,"<br>","Tipo:",input$tipo,"<br>",feridos,":",n))%>%
      add_bars(opacity = 1)%>%
      layout(title='Número de Acidentes em Pleno dia',xaxis=list(title='Frequência'),yaxis=list(title=''))
  })
  
  
  
  output$solo=renderPlotly({
    #df2=dados%>%group_by(ano,condicao_metereologica,uso_solo,horas)%>%summarise(n=n())%>%
    #  filter(condicao_metereologica %in% input$cond & ano %in% input$ano2)
    #ggplot(df2,aes(x=horas,y=n, group = uso_solo ,colour=uso_solo))+geom_line()+
    #  labs(colour="Local",y="FrequÊncia",x="Horas")+
    #  scale_color_manual(values=c("blue", "red"))+
    #  ggtitle("Número de Acidente de Acordo com o Horário")
    
    dados%>%group_by(ano,condicao_metereologica,uso_solo,horas)%>%summarise(n=n())%>%
      filter(condicao_metereologica %in% input$cond & ano %in% input$ano2)%>%
      plot_ly(x = ~horas, y = ~n, color = ~uso_solo,hoverinfo = "text",text = ~paste("Brasil","<br>","Ano:",input$ano3,"<br>","Horário:",horas,"<br>","Acidentes:",n))%>%
      add_lines()%>%
      layout(title='Número de Acidente de Acordo com o Horário',xaxis=list(title='Horário'),yaxis=list(title='Frequência'))
    
  })  
}
shinyApp(ui,server)
