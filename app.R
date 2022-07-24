
model=credit_scoring_rf_model <- readRDS("C:/Users/user/Desktop/credit_scoring/Credit_scoring/credit_scoring_rf_model.rds")
library(shinydashboard)

ui <- dashboardPage(
      dashboardHeader(title = 'IBRAHIMA_DIAGNE'),
      dashboardSidebar(menuItem("Dashboard", tabName = "dashboard_Credit_scoring", icon = icon("dashboard"))),
      dashboardBody(
                    tabName='features',
                    fluidRow( (valueBoxOutput('score_prediction')),
                    box(numericInput('var1',label = 'Age du demandeur de credit',value=20,min=18))),
                    fluidRow( box(numericInput('var2',label = 'Revenu annuelle',value = 10000,min=0)),
                    box(selectInput('var3',label ='situation immobiliere',choices =c('MORTGAGE','OWN','RENT','OTHER'))),
                    fluidRow( box(numericInput('var4',label = 'Depuis quand le demandeur est en activite professionnel',value=2,min=0)),
                    box(selectInput('var5',label ='Destination du credit',choices =c('PERSONAL','EDUCATION','MEDICAL','VoiTURE','HOMEIMPROVEMENT','DEBTCONSOLIDATION'))))),
                    fluidRow(box(selectInput('var6',label ='Categorie du credit',choices=c('A','B','C','D','E','F','G'))),
                    box(numericInput('var7',label = 'Montant du credit',value=10000,min=0))),
                    fluidRow(box(numericInput('var8',label = 'taux d interet en %',value=1,min=0)),
                    box(numericInput('var9',label = 'ratio dette/revenu du demandeur de credit(valeur entre 0 et 1)',value=0.3,min=0,max=1))),
                    fluidRow(box(selectInput('var10',label ='Le demandeur de credit est il a decouvert bancaire',choices =c('Y','N'))),
                    box(numericInput('var11',label = 'Echeance du credit en cours',value=2)))
                    
                    )           
      
    
        )
    



server <- function(input, output) {
prediction<-reactive({
  predict(
    model,
     data.frame(
       'person_age'=input$var1,
       'person_income'=input$var2,
       'person_home_ownership'=input$var3,
       'person_emp_length'=input$var4,
       'loan_intent'=input$var5,
       'loan_grade'=input$var6,
       'loan_amnt'=input$var7,
       'loan_int_rate'=input$var8,
       'loan_percent_income'=input$var9,
       'cb_person_default_on_file'=input$var10,
       'cb_person_cred_hist_length'=input$var11
                             ),
                             type='raw',
                             )
 
  })
    prediction_label<-reactive({
      ifelse(prediction()=='0','Client eligible au credit','Non eligible')
    })
    prediction_prob<-reactive({predict( 
                                      model,
                                      data.frame('person_age'=input$var1,
                                                 'person_income'=input$var2,
                                                 'person_home_ownership'=input$var3,
                                                 'person_emp_length'=input$var4,
                                                 'loan_intent'=input$var5,
                                                 'loan_grade'=input$var6,
                                                 'loan_amnt'=input$var7,
                                                 'loan_int_rate'=input$var8,
                                                 'loan_percent_income'=input$var9,
                                                 'cb_person_default_on_file'=input$var10,
                                                 'cb_person_cred_hist_length'=input$var11
                                                 
                                                 
                                ),
                                      type='prob',
    )
    })
    prediction_color=reactive(
      {
        ifelse(prediction()=='0','green','red')
      }
    )
    output$score_prediction<-renderValueBox({
      valueBox(
                 value=paste(round(100*prediction_prob()$"1",0),'%'),
                 subtitle = prediction_label(),
                 color = prediction_color(),
                 icon = icon('hand-holding-usd')
      )
    })
}

    



shinyApp(ui = ui, server = server)
