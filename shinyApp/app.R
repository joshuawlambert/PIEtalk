## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(sjPlot)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Titanic Predictor"),
  dashboardSidebar(
    uiOutput(outputId = "passlist"),
    h1(),
    menuItem("Male Prediction", tabName = "male_menu_item", icon = icon("male", "fa-3x")),
    h1(),
    menuItem("Female Prediction", tabName = "female_menu_item", icon = icon("female", "fa-3x")),
    h1(),
    uiOutput(outputId = "passinfo")
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "male_menu_item",
              h2("PIE prediction for male passenger:"),
              fluidRow(
                box(width = 4,plotOutput("malemod1")),
                box(width = 4,plotOutput("malemod2")),
                box(width = 4,plotOutput("malemod3"))
              ),
              fluidRow(
                box(width=12, plotOutput("treemale"))
              )
      ),
      
      tabItem(tabName = "female_menu_item",
              h2("PIE prediction for female passenger:"),
              fluidRow(
                box(width = 4,plotOutput("femalemod1")),
                box(width = 4,plotOutput("femalemod2")),
                box(width = 4,plotOutput("femalemod3"))
              ),
              fluidRow(
                box(width=12, plotOutput("treefemale"))
              )
      )
    )
  )
)

server <- function(input, output) {
  load(file = "piedata.RData")
  passengers<-passenger
  passengers$Pclass<-as.factor(passengers$Pclass)
  passengers$Title[passengers$Title=="Dr"]<-"RareTitle"
  passengers$Title[passengers$Title=="Rev"]<-"RareTitle"
  passengers$Title<-factor(passengers$Title)
  passengers$Survived<-ifelse(passengers$Survived==1,
                              yes = "Survived",
                              no = ifelse(passengers$Survived==0, 
                                          yes = "Died",no = "NA"))
  passengers$Survived2<-c(passengers$Survived[!is.na(passengers$Survived)],
  ifelse(pie_results[,2]==1,yes = "Predicted Survived",no = ifelse(pie_results[,2]==0, 
                                          yes = "Predicted Died",no = "NA"))
  )
  
  
  male_var<-c("Age","CabinSide","Embarked","Title","SibSp")
  female_var<-c("Sex","FarePerPerson","Pclass","Fare","Title","Age")
  
  male_mod1<-glm("Survived~Age*CabinSide",family = "binomial",data = train[,c("Survived",male_var)])
  male_mod2<-glm("Survived~Title*Embarked",family = "binomial",data = train[,c("Survived",male_var)])
  male_mod3<-glm("Survived~SibSp*Title",family = "binomial",data = train[,c("Survived",male_var)])
  
  female_mod1<-glm("Survived~FarePerPerson*Sex",family = "binomial",data = train[,c("Survived",female_var)])
  female_mod2<-glm("Survived~Age*Title",family = "binomial",data = train[,c("Survived",female_var)])
  female_mod3<-glm("Survived~Fare*Pclass",family = "binomial",data = train[,c("Survived",female_var)])
  
  
  output$passlist<-renderUI({
    selectInput(inputId = "passname",label = "Select Passenger",choices = levels(passengers$Name))
  })
  output$passnameout<-renderText(expr = {
    input$passname
  })
  
  output$passinfo<-renderUI({
    i=which(passengers$Name %in% input$passname)
    nm<-paste(paste(rev(strsplit(input$passname,split = ",")[[1]]),collapse = " "),"(",passengers$DataSet[i],")",sep = "")
    h2(paste(nm," was a ",passengers[i,"Age"], " year old ", passengers[i,"Sex"],".", "Stayed in ",
             passengers[i,"CabinSide"], " side of the titanic in class ",passengers[i,"Pclass"], ". Price for fare was",  passengers[i,"Fare"], 
             " while the average fare for thier party was",
             passengers[i,"FarePerPerson"],".",collapse = "")
       ,paste(nm, " status is ",passengers$Survived2[i]),collapse = "")
      
  })

#male model plots
output$malemod1<-renderPlot({
    i=which(passengers$Name %in% input$passname)
    #male model 1 
    mod<-male_mod1
    var1<-all.vars(as.formula(mod$formula))[-1][1]
    var2<-all.vars(as.formula(mod$formula))[-1][2]
    
    p<-plot_model(mod,type = "int")
    x1<-passengers[i,var1]
    y1<-predict(mod, type = "response", newdata = passengers[i, ])
    g<-ggplot_build(p)
    p+geom_point(aes(x =x1 ,
                     y =y1),
                 colour=  unlist(unique(g$data[[1]]["fill"]))[which(levels(passengers[i,var2]) %in% passengers[i,var2])],
                 size=5, shape=9)+ggtitle(paste("Male Model 1: P(Survived):",round(y1,2),collapse = "")) 
})
  
output$malemod2<-renderPlot({
  i=which(passengers$Name %in% input$passname)
  #male model 2 
  mod<-male_mod2
  var1<-all.vars(as.formula(mod$formula))[-1][1]
  var2<-all.vars(as.formula(mod$formula))[-1][2]
  
  p<-plot_model(mod,type = "int")
  x1<-passengers[i,var1]
  y1<-predict(mod, type = "response", newdata = passengers[i, ])
  g<-ggplot_build(p)
  p+geom_point(aes(x =which(levels(x1) %in% x1) ,
                   y =y1),
               colour=  rev(unlist(unique(g$data[[1]]["fill"])))[which(levels(passengers[i,var2]) %in% passengers[i,var2])],
               size=5, shape=9)+ggtitle(paste("Male Model 2: P(Survived):",round(y1,2),collapse = "")) 
})

output$malemod3<-renderPlot({
  i=which(passengers$Name %in% input$passname)
  #male model 3 
  mod<-male_mod3
  var1<-all.vars(as.formula(mod$formula))[-1][1]
  var2<-all.vars(as.formula(mod$formula))[-1][2]
  
  p<-plot_model(mod,type = "int")
  x1<-passengers[i,var1]
  y1<-predict(mod, type = "response", newdata = passengers[i, ])
  g<-ggplot_build(p)
  p+geom_point(aes(x =x1 ,
                   y =y1),
               colour=  unlist(unique(g$data[[1]]["fill"]))[which(levels(passengers[i,var2]) %in% passengers[i,var2])],
               size=5, shape=9)+ggtitle(paste("Male Model 3: P(Survived):",round(y1,2),collapse = "")) 
})

####female model plots
output$femalemod1<-renderPlot({
  i=which(passengers$Name %in% input$passname)
  #female model 1
  mod<-female_mod1
  var1<-all.vars(as.formula(mod$formula))[-1][1]
  var2<-all.vars(as.formula(mod$formula))[-1][2]
  
  p<-plot_model(mod,type = "int")
  x1<-passengers[i,var1]
  y1<-predict(mod, type = "response", newdata = passengers[i, ])
  g<-ggplot_build(p)
  p+geom_point(aes(x =x1 ,
                   y =y1),
               colour=  unlist(unique(g$data[[1]]["fill"]))[which(levels(passengers[i,var2]) %in% passengers[i,var2])],
               size=5, shape=9)+ggtitle(paste("Female Model 1: P(Survived):",round(y1,2),collapse = "")) 
})


output$femalemod2<-renderPlot({
  i=which(passengers$Name %in% input$passname)
  #female model 2
  mod<-female_mod2
  var1<-all.vars(as.formula(mod$formula))[-1][1]
  var2<-all.vars(as.formula(mod$formula))[-1][2]
  
  p<-plot_model(mod,type = "int")
  x1<-passengers[i,var1]
  y1<-predict(mod, type = "response", newdata = passengers[i, ])
  g<-ggplot_build(p)
  p+geom_point(aes(x =x1 ,
                   y =y1),
               colour=  unlist(unique(g$data[[1]]["fill"]))[which(levels(passengers[i,var2]) %in% passengers[i,var2])],
               size=5, shape=9)+ggtitle(paste("Female Model 2: P(Survived):",round(y1,2),collapse = "")) 
})


output$femalemod3<-renderPlot({
  i=which(passengers$Name %in% input$passname)
  #female model 3
  mod<-female_mod3
  var1<-all.vars(as.formula(mod$formula))[-1][1]
  var2<-all.vars(as.formula(mod$formula))[-1][2]
  
  p<-plot_model(mod,type = "int")
  x1<-passengers[i,var1]
  y1<-predict(mod, type = "response", newdata = passengers[i, ])
  g<-ggplot_build(p)
  p+geom_point(aes(x =x1 ,
                   y =y1),
               colour=  unlist(unique(g$data[[1]]["fill"]))[which(levels(passengers[i,var2]) %in% passengers[i,var2])],
               size=5, shape=9)+ggtitle(paste("Female Model 3: P(Survived):",round(y1,2),collapse = "")) 
})

#treemale
output$treemale<-renderPlot({
  plot(tree.male)
 })
#treefemale
output$treefemale<-renderPlot({
  plot(tree.female)
})
}

shinyApp(ui, server)
