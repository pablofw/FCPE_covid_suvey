library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library("RColorBrewer")
library(DT)
library(readxl)
library(dplyr)
library(tidyverse)
library(lazyeval)
library(data.table)
library(reactlog)
options(shiny.reactlog = TRUE)
library(shinythemes)
library(shinycssloaders)
library(leaflet)
library(leaflet.providers)
library(sf)
library(raster)
library(rgdal)
library(pivottabler)
library(rsconnect)
library(leaflet.extras)
library(httr)
library(htmltools)


# ----------------------------------------------------- DONNEES ---------------------------------------------------------#


# chargement des données d'enquête
data <- read_excel("results-Enq_FCPE_dataV2.xlsx")
# chargement des données cartographiques
datavilles <- read_excel("results-Enq_FCPE_datavilles2.xlsx")


                
               
# ----------------------------------------------------- INTERFACE ONGLETS / NAV BARRE ---------------------------------------------------------#
ui <- fluidPage( theme = shinytheme("flatly"),
# création nav bar + logo    
tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:64 !important; 
                            padding-bottom:0px !important;
                            height: 64px;
                            }')),

# ----------------------------------------------------- INTERFACE INPUT ---------------------------------------------------------#
sidebarLayout(
  sidebarPanel(
    h3 ("Filtres sur la population"),
    
              checkboxGroupInput("TypeEtablissement1", "Cycle scolaire :",
                                 choices =c("Ecole maternelle" , "Ecole élémentaire" , "Collège" , "Lycée", "Lycée prof."),
                                 selected =c("Ecole maternelle" , "Ecole élémentaire" , "Collège" , "Lycée", "Lycée prof.")),
              
              checkboxGroupInput("Type_de_famille", "Caractéristiques de la famille :",
                                 choices =c("Famille couple" , "Famille monoparentale (garde alternée)" , "Famille monoparentale"),
                                 selected = unique(na.omit(unlist(data[41])))),
              
              checkboxGroupInput("Lien_avec_l_enfant_ou_les_enfants", "Parent répondant :",
                                 choices =c("Mère" , "Père" , "Autre parent·e ayant la garde"),
                                 selected = c("Mère" , "Père" , "Autre parent·e ayant la garde")),
              
              
              pickerInput(inputId = "Mon_age",label = "Âge du répondant",choices = c("20-29", "30-39", "40-49", "50-59", "59 et +"),
                          options = list(`actions-box` = TRUE,`select-All-Text` = "Sélectionner tout", `deselect-All-Text` = "Désélectionner tout", `None-selected-Text` = "Aucune sélection"),
                          multiple = TRUE,
                          selected = c("20-29", "30-39", "40-49", "50-59", "59 et +")),
              
              
              sliderInput(inputId = "Nb_d_enfants", label = "Nombre d'enfants :" , min = 1, max = 9, value = c(1,9), step = 1, round=TRUE, sep=""),
              
              pickerInput("Situation_professionnelle", "Situation professionnelle :",
                          choices =c("Sans emploi" , "Chômage technique" , "Télétravail" , "Travail à l'extérieur" ),
                          options = list(`actions-box` = TRUE,`select-All-Text` = "Sélectionner tout", `deselect-All-Text` = "Désélectionner tout", `None-selected-Text` = "Aucune sélection"), 
                          multiple = TRUE,
                          selected = c("Sans emploi" , "Chômage technique" , "Télétravail" , "Travail à l'extérieur" )),
              
              pickerInput("VilleEtablissement1","Ville :", 
                          choices = sort(unique(na.omit(unlist(data[46])))),
                          options = list(`actions-box` = TRUE,`select-All-Text` = "Sélectionner tout", `deselect-All-Text` = "Désélectionner tout", `None-selected-Text` = "Aucune sélection"),
                          multiple = TRUE, 
                          selected = sort(unique(na.omit(unlist(data[46]))))),
              
              checkboxGroupButtons(inputId = "Position", label = "Avis :", 
                                   choices = c("Pour" , "Contre" , "Mitigé"),
                                   selected = c("Pour" , "Contre" , "Mitigé")),
              
              
              helpText("Données FCPE Moselle 2020")
              
              ),
  
  
  mainPanel(
    navbarPage (title = div(img(src='iut.png', height = '32', width = '124', title="oui"),"Enquête FCPE - le déconfinement en Moselle"), theme  = shinytheme ( "flatly" ),
    
           
# ----------------------------------------------------- INTERFACE OUTPUT ---------------------------------------------------------#
#------------------------------------------------------  GRAPH STAT 
## -------------  Univariés (BARPLOT EFFECTIFS)            
                            navbarMenu ( "Table réactive" , icon  = icon ("list" ),
                                 tabPanel ( "Data Parents" , fluid  =  TRUE,
                                            mainPanel(titlePanel("Jeu de données réactif"),
                                                      tabPanel("Données actualisées", DTOutput("tbl"),width = "80%"))),
                                              tabPanel ( "Table enfants" , fluid  =  TRUE,
                                                         mainPanel(titlePanel("Jeu de données réactif"),
                                                                   (tabPanel("Data enfants", DTOutput("tbl2"),width = "80%"))))),
                                              

                                         
                             navbarMenu ( "Caract. familles" ,  icon  = icon ( "users" ),
                                          
                                          tabPanel ( "Type de famille" , fluid  =  TRUE,
                                                     tabsetPanel(
                                                     mainPanel(
                                                       titlePanel("Visualisations"),
                                                                 tabPanel("Situation familiale", withSpinner(plotOutput("Graph3",width = "100%")))))),
                                          
                                          tabPanel ( "Lien du parent répondant" , fluid  =  TRUE,
                                                     tabsetPanel(
                                                     mainPanel(
                                                       titlePanel("Visualisations"),
                                                                 tabPanel("Lien Parenté",  withSpinner(plotOutput("Graph5", width = "100%")))))),
                                                     
                                          tabPanel ( "Age" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(
                                                      tabPanel("Tranche d'âge", withSpinner(plotOutput("Graph19", width = "150%")))))),
                                          
                                          tabPanel ( "Situation professionnelle" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(
                                                      tabPanel("Situation pro", withSpinner(plotOutput("Graph4", width = "150%")))))),
                                          
                                          tabPanel ( "Nombre d'enfants" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(
                                                      tabPanel("Nombre d'enfants", withSpinner(plotOutput("Graph22", width = "150%")))))),
                                          
                                          tabPanel ( "Niveau scolaire (pop:enfants)" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(
                                                      tabPanel("Niveau Scolaire", withSpinner(plotOutput("Graph12", width = "150%"))))))),
                          
                                                   
 ## -------------  Bivariés Rép (AVIS - X)
                                          
 
                             navbarMenu ( "Répartitions des Avis" , icon  = icon ("thumbs-up" ),
                                          tabPanel ( "Type de famille" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(tabPanel("Avis - Situation familiale", withSpinner(plotOutput("Graph14", width = "150%"))
                                                                         )))),
                                          
                                          tabPanel ( "Lien du parent répondant" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(tabPanel("Avis - Lien de parenté", withSpinner(plotOutput("Graph13", width = "150%")))))),
                                          
                                          tabPanel ( "Age" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(tabPanel("Avis - Tranche d'âge", withSpinner(plotOutput("Graph20", width = "150%")))))),
                                          
                                          tabPanel ( "Situation professionnelle" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(tabPanel("Avis - Situation professionnelle", withSpinner(plotOutput("Graph8", width = "150%")))))),
                                          
                                          tabPanel ( "Nombre d'enfants" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(tabPanel("Avis - Nombre d'enfants", withSpinner(plotOutput("Graph23", width = "150%")))))),
                                          
                                          tabPanel ( "Niveau scolaire (pop:enfants)" , fluid  =  TRUE,
                                          mainPanel(titlePanel("Visualisations"),
                                                    tabsetPanel(tabPanel("Avis - Niveau scolaire", withSpinner(plotOutput("Graph7", width = "150%"))))))),
                             
                             
## -------------  Bivariés Disp (SCORE - X)


                             navbarMenu ( "Dispersion du Score" , icon  = icon ( "sort-numeric-down" ),
                                          tabPanel ( "Type de famille" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Score - Situation familiale", withSpinner(plotOutput("Graph18", width = "150%")))))),
                                          
                                          tabPanel ( "Lien du parent répondant" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(
                                                     tabPanel("Score -Liens de parenté", withSpinner(plotOutput("Graph16", width = "150%")))))),

                                          tabPanel ( "Age" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Score - Tranche d'âge", withSpinner(plotOutput("Graph21", width = "150%")))))),

                                          
                                          tabPanel ( "Situation professionnelle" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Score - Situation professionnelles", withSpinner(plotOutput("Graph17", width = "150%")))))),

                                          
                                          tabPanel ( "Nombre d'enfants" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Score - Nombre d'enfants", withSpinner(plotOutput("Graph24", width = "150%")))))),

                                          
                                          tabPanel ( "Niveau scolaire (pop:enfants)" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Score - Niveau Scolaire", withSpinner(plotOutput("Graph15", width = "150%")))))),

## ------------- Visualisation répartitions des scores moyens

                                          tabPanel ( "Répartition du score" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(
                                                                 tabPanel("Score répartition", withSpinner(plotOutput("Graph6", width = "150%"))))))
                                                          ),        



## ------------- Visualisation répartition des réponses aux questions Pour/Contre 

                             navbarMenu ( "Questions" , icon  = icon ( "question-circle" ),
                                          tabPanel ( "Questions POUR" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Questions Pour", withSpinner(plotOutput("Graph11", width = "150%")))))),
                                                                 
                                          tabPanel ( "Questions CONTRE" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Questions Contre", withSpinner(plotOutput("Graph10", width = "150%")))))),
                                                     
                                            tabPanel ( "Croisement moyennes" , fluid  =  TRUE,
                                                      mainPanel(titlePanel("Visualisations"),
                                                                tabsetPanel(tabPanel("Croisement des scores", withSpinner(plotOutput("GraphSctMP", width = "150%"))))))
                                                                                      ),


## ------------- visu avis général                 

                             navbarMenu ( "Avis Global" , icon  = icon ( "chart-bar" ),
                                          tabPanel ( "Effectifs des Avis" , fluid  =  TRUE,
                                                     mainPanel(titlePanel("Visualisations"),
                                                               tabsetPanel(tabPanel("Répartition des avis", withSpinner(plotOutput("Graph2", width = "150%"))))))
                                          ),


## ------------- map test           

                            navbarMenu ( "Map" , icon  = icon ( "globe" ),
                                         tabPanel ( "Les avis des mosellans" , fluid  =  TRUE,
                                         mainPanel(titlePanel("Cartographie"),
                                                   tabsetPanel(tabPanel("Répartition des avis", withSpinner(leafletOutput("Mappy", width = "150%"))))))),

## ------------- test stats


navbarMenu ( "Tests Khi2" , icon  = icon ( "link" ),
             tabPanel ( "Khi2" , fluid  =  TRUE,
                        mainPanel(titlePanel("études statistiques"),
                                  tabsetPanel(tabPanel("études statistiques", htmlOutput("stat"))))))  

))))                                                        


server <- function(input, output, session) {
  
  #-------------------------------------------------------- Réactivité ---------------------------------------------------------- #
  
  
  #---------------------Subsets par imbrication de tous les sélecteurs pour que seules les données filtrées apparaissent
  
  #Sélecteur Slider Nb Enfants
  #détermine les valeurs comprises entre l'intervalle modifiable entre min et max
  dataSemiActu <- reactive({
    d <- data[data$Nombre_d_enfants >= input$Nb_d_enfants[1] & data$Nombre_d_enfants <= input$Nb_d_enfants[2],]
  })

  
  #Sélecteur Cycle Scolaire 
  #Sélecteur Type de famille
  #Sélecteur Répondant  
  #Sélecteur Situation professionnelle  
  #électeur Ville
  #Sélecteur Position  
  #----------------------- Actualisation de la dataframe 
  dataActu <- reactive ({
    subset(dataSemiActu(),TypeEtablissement1 %in% input$TypeEtablissement1
           & Type_de_famille %in% input$Type_de_famille
           & Lien_avec_l_enfant_ou_les_enfants %in% input$Lien_avec_l_enfant_ou_les_enfants
           & Mon_age %in% input$Mon_age
           & Situation_professionnelle %in% input$Situation_professionnelle
           & VilleEtablissement1 %in% input$VilleEtablissement1
           & Position %in% input$Position)
  })
  

  #même opération pour les données adaptées aux enfants
  datavillesSemiActu2 <- reactive({
    d <- datavilles[datavilles$Nombre_d_enfants >= input$Nb_d_enfants[1] & datavilles$Nombre_d_enfants <= input$Nb_d_enfants[2],]
  })
  datavillesActu2 <- reactive ({
    subset(datavillesSemiActu2(),TypeEtablissement1 %in% input$TypeEtablissement1
           & Type_de_famille %in% input$Type_de_famille
           & Lien_avec_l_enfant_ou_les_enfants %in% input$Lien_avec_l_enfant_ou_les_enfants
           & Mon_age %in% input$Mon_age
           & Situation_professionnelle %in% input$Situation_professionnelle
           & VilleEtablissement1 %in% input$VilleEtablissement1
           & Position %in% input$Position)
  })
  
  #----------------------------------------------------------  GRAPH STAT  ------------------------------------------------------------ #
  
  ### Graph Avis PCM général
  output$Graph2 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    eff<-dataActu()
    eff_PCM<-table(eff$Position)
    barplot(sort(eff_PCM, decreasing = TRUE), horiz = T, xlab ="Avis des répondants", ylab="Effectifs",main = "Êtes-vous pour la reprise de l'école ?", col=c("#CB2027","#8FD175","#FAA43A"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
    }
    })
  
  
  ## -------------------------------------  Univariés
  
  ### Graph niveau scolaire
  output$Graph12 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-datavillesActu2()
    x=data$TypeEtablissement1
    x=factor(x, levels=c("Ecole maternelle", "Ecole élémentaire", "Collège","Lycée","Lycée prof."))
    eff_c<-table(x)
    barplot(eff_c, xlab ="Niveau", ylab="Effectifs",main = "Cycle scolaire des enfants", col=c("#44AA99"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
  }})    
  
  ### Graph Lien de parenté
  output$Graph5 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    eff_c<-table(data$Lien_avec_l_enfant_ou_les_enfants)
    barplot(sort(eff_c, decreasing = TRUE), xlab ="Lien", ylab="Effectifs",main = "Lien de parenté des répondants", col=c("#44AA99"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
  }})
  
  ### Graph Type des familles
  output$Graph3 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    eff_c<-table(data$Type_de_famille)
    barplot(sort(eff_c, decreasing = TRUE), xlab ="Type de familles", ylab="Effectifs",main = "Situation familiale des répondants", col=c("#44AA99"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
  }})
  
  ### Graph Situation Pro
  output$Graph4 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    eff_c<-table(data$Situation_professionnelle)
    barplot(sort(eff_c, decreasing = TRUE), xlab ="Situation professionnelle", ylab="Effectifs",main = "Situation professionnelle des répondants", col=c("#44AA99"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
  }})
  
  ### Graph effectifs tranches d'âge des répondants 
  output$Graph19 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    df <- data[order(data$Mon_age,decreasing = TRUE),]
    eff_c<-table(df$Mon_age)
    barplot(eff_c, xlab ="Âge des répondants", ylab="Effectifs",main = "Âge des répondants", col=c("#44AA99"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
  }})    
  
  ### Graph age
  output$Graph22 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    eff_c<-table(data$Nombre_d_enfants)
    barplot(sort(eff_c, decreasing = TRUE), xlab ="Nombre d'enfants", ylab="Effectifs",main = "Nombre d'enfants des répondants", col=c("#44AA99"), font.main = 4, cex.main = 2, cex.lab=1.4, cex.axis =1.2, font.axis =3)
  }})
  
  ## ------------------------------------  Bivariés - Répartition
  
  ### Graph Avis selon niveau scolaire
  output$Graph7 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-datavillesActu2()
    x=data$TypeEtablissement1
    x=factor(x, levels=c("Ecole maternelle", "Ecole élémentaire", "Collège","Lycée","Lycée prof."))
    y=data$Position
    eff_c = table(x,y)
    spineplot(eff_c, xlab= "Niveau Scolaire", ylab= "Avis", main="Répartition des avis selon le niveau scolaire",col=c("#8FD175","#FAA43A","#CB2027"))
  }})
  
  ### Graph Lien de parenté
  output$Graph13 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    x=data$Lien_avec_l_enfant_ou_les_enfants
    x=factor(x, levels=c("Mère", "Père","Autre parent·e ayant la garde"))
    y=data$Position
    eff_c = table(x,y)
    spineplot(eff_c, xlab= "Lien de parenté", ylab= "Avis", main="Répartition des avis selon le lien de parenté",col=c("#8FD175","#FAA43A","#CB2027"))
    }}) 
  
  ### Graph Type des familles
  output$Graph14 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    x=data$Type_de_famille
    x=factor(x, levels=c("Famille couple", "Famille monoparentale","Famille monoparentale (garde alternée)"))
    y=data$Position
    eff_c = table(x,y)
    spineplot(eff_c, xlab= "Situation familiale", ylab= "Avis", main="Répartition des avis selon la situation familiale",col=c("#8FD175","#FAA43A","#CB2027"))
  }})
  
  ### Graph Situation Pro
  output$Graph8 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    x=data$Situation_professionnelle
    x=factor(x, levels=c("Télétravail", "Travail à l'extérieur","Chômage technique","Sans emploi"))
    y=data$Position
    eff_c = table(x,y)
    spineplot(eff_c,xlab= "Situation", ylab= "Avis", main="Répartition des avis selon les situations pro. confinement",col=c("#8FD175","#FAA43A","#CB2027"))
  }})
  
  ### Graph Avis selon les tranches d'âge des répondants
  output$Graph20 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    eff<-dataActu()
    x=eff$Mon_age
    x=factor(x, levels=c("20-29", "30-39", "40-49","50-59","59 et +"))
    y=eff$Position
    eff = table(x,y)
    spineplot(eff, xlab= "Âge des répondants", ylab= "Avis", main="Répartition des avis selon les tranches d'âge des répondants",col=c("#8FD175","#FAA43A","#CB2027"))
  }})
  
  ### Graph Avis nb enfants
  output$Graph23 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    x=data$Nombre_d_enfants
    y=data$Position
    eff_c = table(x,y)
    spineplot(eff_c,xlab= "Nombre d'enfants", ylab= "Avis", main="Répartition des avis selon le nombre d'enfants",col=c("#8FD175","#FAA43A","#CB2027"))
  }})
  
  ## ------------------------------------  Bivariés - Dispersion 
  
  ### Graph Avis selon niveau scolaire
  output$Graph15 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-datavillesActu2()
    x=data$TypeEtablissement1
    x=factor(x, levels=c("Ecole maternelle", "Ecole élémentaire", "Collège","Lycée","Lycée prof."))
    data %>%
      filter(!is.na(TypeEtablissement1)) %>%
      ggplot() +
      aes(x = x, y = Score) +
      geom_boxplot(fill = "#26828e") +
      labs(x = "Niveaux", y = "Score", title = "Dispersion du score selon les Niveaux Scolaires") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))        
  }})
  
  ### Graph Lien de parenté
  output$Graph16 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    data %>%
      filter(!is.na(Lien_avec_l_enfant_ou_les_enfants)) %>%
      ggplot() +
      aes(x = Lien_avec_l_enfant_ou_les_enfants, y = Score) +
      geom_boxplot(fill = "#26828e") +
      labs(x = "Parents", y = "Score", title = "Dispersion du score selon les Liens de parenté") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))
  }})
  
  ### Graph Type des familles
  output$Graph17 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    data %>%
      filter(!is.na(Situation_professionnelle)) %>%
      ggplot() +
      aes(x = Situation_professionnelle, y = Score) +
      geom_boxplot(fill = "#26828e") +
      labs(x = "Situatuations professionnelles", y = "Score", title = "Dispersion du score selon les Situations Professionnelles") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))
  }})
  
  ### Graph Situation Pro
  output$Graph18 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    data %>%
      filter(!is.na(Type_de_famille)) %>%
      ggplot() +
      aes(x = Type_de_famille, y = Score) +
      geom_boxplot(fill = "#26828e") +
      labs(x = "Types de famille", y = "Score", title = "Dispersion du score selon les Types de famille") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))
  }})        
  
  ### Graph Avis selon tranches d'âge
  output$Graph21 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    data %>%
      filter(!is.na(Mon_age)) %>%
      ggplot() +
      aes(x = Mon_age, y = Score) +
      geom_boxplot(fill = "#26828e") +
      labs(x = "Tranches d'âge", y = "Score", title = "Dispersion du score selon les tranches d'âge des répondants") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))
  }})     
  
  ### Graph score nb enfants
  output$Graph24 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    data %>%
      filter(!is.na(Nombre_d_enfants)) %>%
      ggplot() +
      aes(x = Nombre_d_enfants, y = Score) +
      geom_boxplot(fill = "#26828e") +
      labs(x = "Nombre d'enfants", y = "Score", title = "Dispersion du score selon le nombre d'enfants") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))
  }}) 
  ## -------------------------------- Test corrélation nb enfant     
  
  ###Graph score moyen selon le nombre d'enfant (aucunes corrélation)
  output$Graph9 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    plot(x = data$Score, y = data$Nombre_d_enfants, xlab = "score moyen", ylab = "Nombre d'enfants")
    abline(lm(data$Nombre_d_enfants~data$Score),col="red")
  }})
  
  ## ------------------------------- Visualisation répartitions des scores moyens
  
  ### Graph histo moyenne des score 
  output$Graph6 <- renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    ggplot(data) +
      aes(x = Score) +
      geom_histogram(bins = 26L, fill = "#26828e") +
      labs(x = "Score moyen (moyPour - moyContre)", y = "Effectifs", title = "Répartition des répondants selon le score moyen", subtitle = "(contre -5 <-> +5 pour)") +
      theme_bw()+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12))
  }})
  
  
  ## ------------------------------- Visualisation répartition des réponses aux questions Pour/Contre
  
  ### Graph Questions Contre  
  output$Graph10 <-renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    C1<-as.data.frame(table(data[,31]))
    x<-names(data[,31])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C1<-bind_cols(C1,E)
    names(C1)=c("Num","nb","Quest")
    
    C2<-as.data.frame(table(data[,32]))
    x<-names(data[,32])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C2<-bind_cols(C2,E)
    names(C2)=c("Num","nb","Quest")
    
    C3<-as.data.frame(table(data[,33]))
    x<-names(data[,33])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C3<-bind_cols(C3,E)
    names(C3)=c("Num","nb","Quest")
    
    C4<-as.data.frame(table(data[,34]))
    x<-names(data[,34])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C4<-bind_cols(C4,E)
    names(C4)=c("Num","nb","Quest")
    
    C5<-as.data.frame(table(data[,35]))
    x<-names(data[,35])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C5<-bind_cols(C5,E)
    names(C5)=c("Num","nb","Quest")
    
    C6<-as.data.frame(table(data[,36]))
    x<-names(data[,36])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C6<-bind_cols(C6,E)
    names(C6)=c("Num","nb","Quest")
    
    C7<-as.data.frame(table(data[,37]))
    x<-names(data[,37])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C7<-bind_cols(C7,E)
    names(C7)=c("Num","nb","Quest")
    
    C8<-as.data.frame(table(data[,38]))
    x<-names(data[,38])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C8<-bind_cols(C8,E)
    names(C8)=c("Num","nb","Quest")
    
    plotQuestContre<-bind_rows(C1,C2,C3,C4,C5,C6,C7,C8)
    plotQuestContre<-plotQuestContre[-43,]
    plotQuestContre<-plotQuestContre[-37,]
    plotQuestContre<-plotQuestContre[-31,]
    plotQuestContre<-plotQuestContre[-25,]
    plotQuestContre<-plotQuestContre[-19,]
    plotQuestContre<-plotQuestContre[-13,]
    plotQuestContre<-plotQuestContre[-7,]
    plotQuestContre<-plotQuestContre[-1,]
    
    p<-ggplot(plotQuestContre,aes(x = Quest, y = nb))+
      geom_col(aes(fill = Num),width = 0.7)+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12)) +
      coord_flip()+
      ggtitle("Répartition des réponses aux questions CONTRE")+
      xlab("Questions")+
      ylab("Nombre de réponses")+
      scale_fill_brewer(palette="YlOrRd")
    
  
    p
  }
  })
  
  ### Graph Questions Pour
  output$Graph11 <-renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
    data<-dataActu()
    
    C1<-as.data.frame(table(data[,8]))
    x<-names(data[,8])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C1<-bind_cols(C1,E)
    names(C1)=c("Num","nb","Quest")
    
    C2<-as.data.frame(table(data[,9]))
    x<-names(data[,9])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C2<-bind_cols(C2,E)
    names(C2)=c("Num","nb","Quest")
    
    C3<-as.data.frame(table(data[,10]))
    x<-names(data[,10])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C3<-bind_cols(C3,E)
    names(C3)=c("Num","nb","Quest")
    
    C4<-as.data.frame(table(data[,16]))
    x<-names(data[,16])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C4<-bind_cols(C4,E)
    names(C4)=c("Num","nb","Quest")
    
    C5<-as.data.frame(table(data[,17]))
    x<-names(data[,17])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C5<-bind_cols(C5,E)
    names(C5)=c("Num","nb","Quest")
    
    C6<-as.data.frame(table(data[,18]))
    x<-names(data[,18])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C6<-bind_cols(C6,E)
    names(C6)=c("Num","nb","Quest")
    
    C7<-as.data.frame(table(data[,19]))
    x<-names(data[,19])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C7<-bind_cols(C7,E)
    names(C7)=c("Num","nb","Quest")
    
    C8<-as.data.frame(table(data[,22]))
    x<-names(data[,22])
    E<-as.data.frame(c(x,x,x,x,x,x))
    C8<-bind_cols(C8,E)
    names(C8)=c("Num","nb","Quest")
    
    plotQuestContre<-bind_rows(C1,C2,C3,C4,C5,C6,C7,C8)
    plotQuestContre<-plotQuestContre[-43,]
    plotQuestContre<-plotQuestContre[-37,]
    plotQuestContre<-plotQuestContre[-31,]
    plotQuestContre<-plotQuestContre[-25,]
    plotQuestContre<-plotQuestContre[-19,]
    plotQuestContre<-plotQuestContre[-13,]
    plotQuestContre<-plotQuestContre[-7,]
    plotQuestContre<-plotQuestContre[-1,]
    
    
    ggplot(plotQuestContre, aes(x = Quest, y = nb))+
      geom_col(aes(fill = Num), width = 0.7)+
      theme(plot.title = element_text(color="black", size=14, face="bold.italic"), axis.text.x = element_text(size=12, angle = 90), 
            axis.text.y.left = element_text(size = 12)) +
      coord_flip()+
      ggtitle("Répartition des réponses aux questions POUR")+
      xlab("Questions")+
      ylab("Nombre de réponses")+
      scale_fill_brewer(palette="YlOrRd")
  }})
  
  ## Graph croisement Pour Contre Moyenne 
  output$GraphSctMP <-renderPlot({
    if(is.null(input$TypeEtablissement1)){}
    else if (is.null(input$VilleEtablissement1)){}
    else if (is.null(input$Lien_avec_l_enfant_ou_les_enfants)){}  
    else if (is.null(input$Mon_age)){}    
    else if (is.null(input$Type_de_famille)){}
    else if (is.null(input$Position)){}
    else if (is.null(input$Situation_professionnelle)){}
    else{
      data<-dataActu()
      
      p<-ggplot(data) +
        aes(x = MP, y = MC) +
        geom_point(size = 2.64, colour = "#0c4c8a") +
        labs(x = "Moyenne Pour", y = "Moyenne Contre", title = "Croisement moyenne pour et moyenne contre") +
        theme_bw()
    p
  }})
  
#---------------------------------------------------------- Test statistiques ------------------------------------------------------------ # 
  'test stat dans le rapport + résultats sous les graphiques'  
  output$Graph25<-renderPlot({})
  output$Graph26<-renderPlot({})
  
  getPage<-function() {
    return(includeHTML("Analyse-Statistique.html"))
  }
  output$stats2<-renderUI({getPage()})
  
  htmlOuput <- function(Df,meta = NULL, cacheable = NA) {
    rmarkdown::render('./Analyse-Statistique.rmd',params=list(output_file = Analyse-Statistique.html))
  }
  
  output$stat<-renderUI({browseURL("./Analyse-Statistique.html")})

  #----------------------------------------------------------  CARTOGRAPHIE ------------------------------------------------------------ #
  #---------- Filtrage des données de la source villes cumulées (pour enfants et cartographie)
  
  d2 <- subset(datavilles, select = c(VilleEtablissement1, Position, Longitude, Latitude))
  d3 <- subset(d2, (!is.na(d2[,1])) & (!is.na(d2[,2])))
  d3$Longitude <-as.numeric(d3$Longitude)
  d3$Latitude <-as.numeric(d3$Latitude)
  
  #-------Tableau croisé pour extraire les données nécessaires à la cartographie
  
  pt <- PivotTable$new()
  pt$addData(d3)
  pt$addColumnDataGroups("Position", addTotal = FALSE)
  pt$addRowDataGroups("VilleEtablissement1", addTotal = FALSE, header = "Ville")
  pt$addRowDataGroups("Longitude", addTotal = FALSE, header = "Longitude")
  pt$addRowDataGroups("Latitude", addTotal = FALSE, header = "Latitude")
  pt$defineCalculation(calculationName="Position", summariseExpression="n()")
  pt$renderPivot(showRowGroupHeaders=TRUE)
  pt$evaluatePivot()
  
  #---transformation du tableau croisé obtenu en dataframe pour exploitation carto
  d4 <-pt$asDataFrame(rowGroupsAsColumns=TRUE)
  
  # Leaflet Output
  output$Mappy <- renderLeaflet({
    
    leaflet(d4) %>% addTiles(group = "enquete") %>%
      setView(lng = 6.168047, lat = 49.233079, zoom = 9) 	%>%
      addFullscreenControl()%>%
      
      
      
      # définit le cadre de la carte avec les longitudes et latitudes maxi issues du panel de communes   
      fitBounds(~min(5.881648), ~min(48.315247), ~max(7.519946), ~max(49.492669)) %>%
      
      #---Création des couches et cercles proportionnels de couleur pour les avis des répondants, une couche pour chaque groupe d'avis
      
      addCircles(lng = ~as.numeric(Longitude), lat = ~as.numeric(Latitude), weight = 1,
                 radius = ~sqrt(Pour) * 200, popup = ~paste(Ville, ":", Pour, group = "Pour"), 
                 color = "##00F5FF", fillOpacity = 0.5, group ="Pour") %>%
      addCircles(lng = ~as.numeric(Longitude), lat = ~as.numeric(Latitude), weight = 1,
                 radius = ~sqrt(Mitigé) * 200, popup = ~paste(Ville, ":", Mitigé, group = "Mitigé"),
                 color = "#FF0000", fillOpacity = 0.5, group="Mitigé") %>%
      addCircles(lng = ~as.numeric(Longitude), lat = ~as.numeric(Latitude), weight = 1,
                 radius = ~sqrt(Contre) * 200, popup = ~paste(Ville, ":", Contre, group = "Contre"),
                 color = "#7D26CD", fillOpacity = 0.5, group="Contre") %>% 
      
      
      #---Création du bouton de contrôle pour chaque couche de carte
      
      addLayersControl(baseGroups = c("enquete"),overlayGroups = c("Pour", "Mitigé", "Contre"), options = layersControlOptions(collapsed = FALSE)) %>% 
      
      # centrage de la carte sur Talange     
      setView(lng = 6.168047, lat = 49.233079, zoom = 9) 	
  })

  #----------------------------------------------------------  TABLE DATA  ------------------------------------------------------------ #
  
  #----------------------Table réactive test sur les sélecteurs imbriqués 
  
  output$tbl <- renderDT ({dataActu()},extensions = 'FixedHeader', options = list(fixedHeader = TRUE, 
                    columnDefs = list(list(targets=c(1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 ,
                                  20 , 21 , 22 , 23 , 24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 35 , 36 ,
                                  37 , 38 , 39 , 44, 47 , 48 , 49 , 51 , 52 , 53 , 54 , 55 , 56 , 57 , 58 , 59 , 60 , 61, 
                                  62 , 63 , 64 , 65 , 66 , 67 , 68 , 69 , 70 , 71 , 72 , 73),visible=FALSE, searchable=FALSE))))

  output$tbl2 <- renderDT ({datavillesActu2()},extensions = 'FixedHeader', options = list(fixedHeader = TRUE, 
                                                                                  columnDefs = list(list(targets=c(1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 ,
                                                                                                                   20 , 21 , 22 , 23 , 24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 35 , 36 ,
                                                                                                                   37 , 38 , 39 , 44, 47 , 48 , 49 ,50, 51 , 52 , 53 , 54 , 55 , 56 , 57 , 58 , 59 , 60 , 61, 
                                                                                                                   62 , 63 , 64 , 65 , 66 , 67 , 68 , 69 , 70 , 71 , 72 , 73, 75, 76),visible=FALSE,searchable=FALSE))))



}

shinyApp(ui = ui, server = server)


