# Projet lune de Miel Script
library(naivebayes)
library(e1071)
library(funModeling)
library(rio)
library(shiny)

# Importer la bdd
data = import("BDD_LM.csv") 
for(i in 1:27) { data[,i] = as.factor(data[,i]) } # Transformer en facteurs

# Modèle Naive bayes
modellnb = naive_bayes(Destination_LM ~ Sexe + Age + Dimache_Type + Accessoire_LM, data = data, laplace = 1)

# Interface
styletype <- HTML("
  body {
    font-family: 'Arial', sans-serif;
    background-color: #f7f9fc;
    color: #333;
    padding-top: 50px;
    margin: 0;
    padding: 0;
  }
  .navbar {
    background-color: #033B89;
    border: none;
    border-radius: 0;
    margin-bottom: 20px;
    padding-left: 5px;
  }
  .navbar-default .navbar-nav > li > a {
    color: #fff;
    font-size: 16px;
  }
  .navbar-default .navbar-nav > li > a:hover {
    color: #f7f9fc;
    opacity: 0.6;
  }
  .well {
    background-color: #ffffff;
    border: 1px solid #e3e3e3;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    padding: 20px;
  }
  h3 {
    font-weight: bold;
    color: #033B89;
  }
  .btn-primary {
    background-color: #033B89;
    border-color: #033B89;
    font-size: 16px;
    padding: 10px 20px;
    border-radius: 5px;
  }
  .btn-primary:hover {
    opacity: 0.6;
  }
  select, .form-control {
    border-radius: 4px;
    border: 1px solid #ddd;
    padding: 8px;
    font-size: 14px;
  }
  #pred {
    font-size: 24px;
    font-weight: bold;
    margin-top: 20px;
  }
  .fluidRow {
    margin: 0;
  }
  section {
    margin: 0;
    padding: 0;
  }
  .container-fluid, .row {
    margin: 0;
    padding: 0;
  }
")

ui <- fluidPage(
  # Inclure une feuille de style CSS pour un design moderne
  tags$head(
    tags$style(styletype)
  ),
  tags$nav(
    class = "navbar navbar-default navbar-fixed-top",
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "navbar-header",
        tags$button(
          type = "button",
          class = "navbar-toggle collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#myNavbar",
          span(class = "sr-only", "Toggle navigation"),
          span(class = "icon-bar"),
          span(class = "icon-bar"),
          span(class = "icon-bar")
        ),
        tags$a(class = "navbar-brand", href = "#", "HoneyTrip", style = "padding-left: 20px; color: white;font-weight: bold;")
      ),
      tags$div(
        class = "collapse navbar-collapse", id = "myNavbar",
        tags$ul(
          class = "nav navbar-nav",
          style = "text-align:center;",
          tags$li(tags$a(href = "#a-propos", "À propos")),
          tags$li(tags$a(href = "#generate", "Générateur")),
          tags$li(tags$a(href = "#Contacts", "Contacts"))
        )
      )
    )
  ),
  # Section Menu
  tags$section(
    id = "HoneyTrip",
    style = "
           background-image: url('https://images.unsplash.com/photo-1531789965484-b656c2777ae2?q=80&w=2070&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fA%3D%3D'); 
           background-size: cover; 
           background-position: center; 
           background-repeat: no-repeat; 
           height: 800px; 
           width: 100%; 
           margin: 0; 
           padding: 0; 
           color: white;
           padding-top: 80px;
    ", 
    tags$div(
      class = "container-fluid", 
      style = "margin: 0; padding: 0;",
      fluidRow(
        style = "margin: 0; padding: 0;padding-top: 180px;",
        column(
          width = 12,
          h3("HoneyTrip", style = "text-align: left; font-weight: bold; color: white; font-size: 240%;padding-left: 50px;"),
          p("Pour une destination de lune de miel idéale en fonction de votre personnalité.",
            style = "text-align: left; margin: 0 auto; font-size: 220%;padding-left: 50px;padding-right: 350px;")
        )
      )
    )
  ),
  # Section À propos
  tags$section(
    id = "a-propos",
    style = "background-color: #033B89;
         height: 250px; 
         width: 100%; 
         margin: 0; 
         padding: 0; 
         color: white;
         padding-top: 20px;
         padding-bottom: 15px;",
    fluidRow(
      column(
        width = 6,
        offset = 3,  # Centrer la colonne
        h3("À propos", style = "text-align: left; font-weight: bold; color: white; font-size: 240%; padding-left: 20px;"),
        p("HoneyTrip est une application interactive qui vous aide à choisir votre destination de lune de miel idéale en fonction de vos préférences personnelles.",
          style = "text-align: left; margin: 0 auto; font-size: 120%; padding-left: 20px; padding-right: 20px;"),
        p("Remplissez les informations requises et découvrez quelle destination correspond à votre style et vos envies !",
          style = "text-align: left; margin: 0 auto; font-size: 120%; padding-left: 20px; padding-right: 20px;")
      )
    )
  ),
  # Interface utilisateur
  tags$section(
    id = "generate",
    tags$style(styletype),
    style = "padding-top: 60px;padding-bottom: 60px;",
    fluidRow(
      column(
        width = 8,
        tags$div(
          class = "well",
          h3("Pour prévoir votre lune de miel parfaite, entrez vos données ici :"),
          selectInput("Sexe", "Quel est votre sexe ?", 
                      choices = list("--Sélectionner votre sexe--", "Femme", "Homme")),
          selectInput("Age", "Quel votre âge ?", 
                      choices = list("--Sélectionner votre age--", "18-24 ans", "25-34 ans", 
                                     "35-44 ans", "45-54 ans")),
          selectInput("Dimache_Type", "Quel est votre dimanche typique ?",
                      choices = list("--Sélectionner votre choix--", "Netflix and chill", "Faire une séance de sport", 
                                     "Tester un nouveau hobby", "Visite culturelle", 
                                     "Faire une séance relaxation (massage, manucure, ...)")),
          selectInput("Accessoire_LM", "Quel est l'accessoire à ne pas oublier pour votre lune de miel ?",
                      choices = list("--Sélectionner votre accessoire--", "Une huile de massage parfumée et sensuelle", 
                                     "Une tenue spéciale pour les soirées inoubliables", 
                                     "Une playlist privée pour des moments passionnés", 
                                     "Un coffret d'épices pour des surprises culinaires et autres")),
          actionButton("generate", "Générer la probabilité", class = "btn-primary btn-block")
        )
      ),
      column(
        width = 4,
        tags$div(
          class = "well",
          h3("Résultat de votre analyse :"),
          tags$p("Chargement de votre destintion..."),
          tags$h4(textOutput("pred"), class = "text-primary")
        )
      )
    )
  ),
  # Section Contact
  tags$section(
    id = "Contacts",
    style = " background-color: Black;
           height: 300px; 
           width: 100%; 
           margin: 0; 
           padding: 0; 
           color: white;
           padding-top: 60px;",
    fluidRow(
      column(
        width = 12,
        style = "text-align: center; margin: 0 auto; font-size: 100%;padding-left: 20px;padding-right: 20px;padding-top: 30px;",
        h3("Contacts", style= "color: white;"),
        p("Si vous avez des questions ou des suggestions, n'hésitez pas à nous contacter :"),
        p("Email : support@honeytrip.com"),
        p("Téléphone : +33 1 23 45 67 89")
      )
    )
  )
)

server <- function(input, output) {
  input_df <- reactive({
    validate(
      need(input$Sexe != "--Sélectionner votre sexe--", "Veuillez sélectionner un sexe."),
      need(input$Age != "--Sélectionner votre age--", "Veuillez sélectionner un âge."),
      need(input$Dimache_Type != "--Sélectionner votre choix--", "Veuillez choisir une activité du dimanche."),
      need(input$Accessoire_LM != "--Sélectionner votre accessoire--", "Veuillez sélectionner un accessoire.")
    )
    df <- data.frame(
      Age = factor(input$Age, levels = c("18-24 ans", "25-34 ans", "35-44 ans", "45-54 ans")),
      Sexe = factor(input$Sexe, levels = c("Femme", "Homme")),
      Dimache_Type = factor(input$Dimache_Type, levels = c(
        "Netflix and chill", "Faire une seance de sport", 
        "Tester un nouvel hobbie", "Visite culturelle", 
        "Faire une seance relaxation (massage, manicure, ...)"
      )),
      Accessoire_LM = factor(input$Accessoire_LM, levels = c(
        "Une huile de massage parfumee et sensuelle", 
        "Une tenue speciale pour les soirees inoubliables", 
        "Une playlist privee pour des moments passionnes", 
        "Un coffret d'epices pour des surprises culinaires et autres"
      ))
    )
    return(df)
  })
  pred <- eventReactive(input$generate, {
    tryCatch({
      df <- input_df()
      prediction <- predict(modellnb, newdata = df, type = "prob")
      return(prediction)
    }, error = function(e) {
      print(e)
      return("Erreur : Veiller remplir vos informations.")
    })
  })
  output$pred <- renderText({
    pred_result <- pred()
    if (is.character(pred_result)) {
      return("Erreur : Veiller remplir vos informations.")
    }
    top_prob <- pred_result[1, ]
    destination <- colnames(pred_result)
    max_prob_index <- which.max(top_prob)
    best_destination <- destination[max_prob_index]
    prob_value <- round(top_prob[max_prob_index], 2)
    
    paste("Votre destination de lune de miel la plus probable est : ", best_destination, " ( à", (prob_value*100), "%)")
  })
}

shinyApp(ui, server)