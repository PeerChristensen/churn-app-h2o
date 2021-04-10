
# ------------------------------------------------------
# PACKAGES AND DATA
# ------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(h2o)
library(yardstick)
library(DT)
library(shinycssloaders)
library(gsubfn)

h2o.init()

back_colour <- "#343E48"
line_colour <- "yellow4"
spinner_colour <- "#DCDCDC"

theme_set(theme_minimal(base_size=16) +
            theme(plot.background = element_rect(fill=back_colour,colour =back_colour),
                  panel.background =  element_rect(fill=back_colour,colour =back_colour),
                  panel.grid = element_line(size=.05,),
                  axis.title = element_blank(),
                  axis.text = element_text(colour="snow"),
                  text = element_text(colour="snow"),
                  strip.text = element_text(colour="snow",size=16),
                  plot.title = element_text(colour="snow",size=18),
                  plot.subtitle = element_text(colour="snow",size=16)))


mod  <- h2o.loadModel("GBM_grid__1_AutoML_20210409_084451_model_6")

train_hf <- h2o.uploadFile("train.csv")

test <- vroom::vroom("test.csv")  %>%
  mutate(churn = as.factor(churn)) %>%
  mutate_if(is.character,factor)

test_hf <- h2o.uploadFile("test.csv")

vars <- mod@parameters$x

# # ------------------------------------------------------
# # MODEL DETAILS
# # ------------------------------------------------------

algo <- tibble(feature = "Algorithm", value = toupper(mod@algorithm))
details <- mod@model$model_summary %>% as_tibble() %>% gather() %>% rename(feature=key)
details <- rbind(algo,details)

# # ------------------------------------------------------
# # PREDICTIONS
# # ------------------------------------------------------
# 
preds <-  h2o.predict(mod,test_hf) %>%
  as_tibble() %>%
  dplyr::mutate(case = row_number()) %>%
  mutate(churn = as.vector(test_hf$churn))

prediction_table <- read_csv("prediction_table.csv") %>%
  mutate(CustomerID = as.character(CustomerID))

customers <- as.character(1:nrow(test_hf))

# # ------------------------------------------------------
# # PERFORMANCE METRICS
# # ------------------------------------------------------

perf <- h2o.performance(mod,test_hf)

metrics_table <- perf@metrics$max_criteria_and_metric_scores %>%
  as_tibble() %>%
  slice(1:10) %>%
  select(-idx)

metrics <- as.data.frame(h2o.metric(perf)) %>%
  select(-tns,-fps,-tps,-fns)

metrics_long <- metrics %>%
  gather(metric, value, f1:tpr)

metric_names <- unique(metrics_long$metric)

auc <- round(h2o.auc(perf),2)

aucpr <- round(h2o.aucpr(perf),2)

# ------------------------------------------------------
# PERFORMANCE CURVES
# ------------------------------------------------------

#options(yardstick.event_first = F)

#roc
roc_plot <- metrics %>%
  ggplot(aes(1-specificity,tpr)) +
  geom_line(size = 1,colour=line_colour) +
  #geom_line(size = 5, alpha = .2,colour=line_colour) +
  #geom_line(size = 2.5, alpha = .4,colour=line_colour) +
  #geom_line(size = 1, alpha = .9,colour=line_colour) +
  ylim(c(0,1)) +
  geom_abline(slope = 1, intercept = 0,linetype="dashed",colour="snow")

#prroc
baseline_height <- nrow(preds[preds$churn==1,]) / nrow(preds)
prroc_plot <- metrics %>%
  ggplot(aes(recall,precision)) +
  geom_line(size = 1,colour=line_colour) +
  ylim(c(0,1)) +
  geom_hline(yintercept=baseline_height, linetype='dashed',colour="snow")

# gains
gain <- gain_curve(preds, truth = factor(churn),p1,event_level = 'second')
prop_churn <- sum(preds$churn)/nrow(preds)*100
coords <- tibble(x=c(0,prop_churn,100), y = c(0,100,100))

gains_plot <- gain %>%
  ggplot(aes(.percent_tested,.percent_found)) +
  geom_line(colour=line_colour,size=1) +
  geom_polygon(data=coords, mapping=aes(x=coords$x,y=coords$y), alpha = .2,fill = line_colour)

# lift
lift_plot <- h2o.gainsLift(mod,test_hf) %>%
  as_tibble() %>%
  ggplot(aes(cumulative_data_fraction,cumulative_lift)) +
  geom_line(colour = line_colour) +
  geom_hline(yintercept=1, linetype='dashed',colour="snow")

# ------------------------------------------------------
# UI
# ------------------------------------------------------

header <- dashboardHeader(title = "H2O Churn Model Explainer",titleWidth = "290px")

#####
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Performance",icon = icon("dashboard"),
             menuSubItem("Metrics",tabName = "metrics"),
             menuSubItem("Confusion matrices",tabName = "cm"),
             menuSubItem("Curves",tabName = "curves")),
    menuItem("Global explanations", icon = icon("globe"),
             menuSubItem("Feature Importance",tabName = "var_imp"),
             menuSubItem("Partial dependence",tabName = "pdp")),
    menuItem("Local explanations", tabName = "local_expl", icon = icon("users")),
    menuItem("Predictions", tabName = "preds", icon = icon("address-book"),selected = TRUE),
    menuItem("Model specs", tabName = "details", icon = icon("info-circle"))
  )
)

#####
body <-  dashboardBody(
  shinyDashboardThemes(theme = "grey_dark"),
  tabItems(
    tabItem(tabName = "metrics",
            fluidRow(
              box(width=9, withSpinner(plotOutput("metrics_plot", height = 500),color=spinner_colour)),
              box(width=3, title = "Metrics to plot",
                  pickerInput(
                    inputId  = "pick_metrics",
                    choices  = metric_names,
                    selected = metric_names,
                    options  = list(`actions-box` = TRUE),
                    multiple = T)
              )
            ),
            fluidRow(
              box(width=9,"Key Metrics",tableOutput("metrics_table"))
            )
    ),
    tabItem(tabName = "cm",
            fluidRow(
              box(title="By Metric",width=7,withSpinner(tableOutput("cm1"),color=spinner_colour)),
              box(width=5, title = "Choose metric",
                  selectInput('cm_metric',label="", choices = metric_names,
                              selected = "f1", selectize=TRUE))
            ),
            fluidRow(
              box(title="By Threshold",width=7,withSpinner(tableOutput("cm2"),color=spinner_colour)),
              box(width=5, title = "Choose custom threshold",
                  sliderInput("cm_custom", label = "Custom threshold",
                              min = 0,
                              max = 1,
                              value = 0.5,
                              step = 0.01))
            )),
    tabItem(tabName = "curves",
            fluidRow(
              box(title="AUC",width=6,auc),
              box(title="AUCPR",width=6,aucpr)
            ),
            fluidRow(
              box(title="ROC",width=6,plotOutput("roc")),
              box(title="PRROC",width=6,plotOutput("prroc"))
            ),
            fluidRow(
              box(title="Cumulative Gains",width=6,plotOutput("gains")),
              box(title="Cumulative Lift",width=6,plotOutput("lift"))
            )),
    tabItem(tabName = "var_imp",
            fluidRow(
              box(title="Feature Importance",width=7,
                  withSpinner(plotOutput("var_imp"),color=spinner_colour)),
              box(width=5, title = "Number of predictors",
                  sliderInput("n_preds", label = "",
                              min = 1,
                              max = length(mod@parameters$x),
                              value = 10,
                              step = 1))
            )),
    tabItem(tabName = "pdp",
            fluidRow(
              box(title="Partial Dependence",width=7,
                  withSpinner(plotOutput("pdp_plot"),color=spinner_colour)),
              box(width=5, title = "Select Predictors",
                  selectizeInput("pdp_features", label = "max 3",
                                 choices = vars,
                                 selected = NULL,
                                 multiple = TRUE,
                                 options = list(maxItems = 3)),
                  actionButton("pdp_go","Go"))
            )),
    tabItem(tabName = "local_expl",
            fluidRow(
              box(title= "Individual explanations",width=8,
                  withSpinner(plotOutput("shap_plot"),color=spinner_colour)),
              box(title = "Options",width=4,
                  selectInput("shap_cust",label="CustomerID",
                              choices = customers,
                              selected = sample(customers,1),
                              selectize = T),
                  sliderInput("shap_n_feat",label="Number of features",
                              min = 1,
                              max = 15,
                              value = 5,
                              step = 1))
            )),
    tabItem(tabName = "preds",
            fluidRow(
              box(title= "Customer Overview",width=12,
                  dataTableOutput("pred_table"))
            )),
    tabItem(tabName = "details",
            fluidRow(width=4,
                     box(title= "Model Specifications",
                         tableOutput("details"))
            ))
  )
)

ui <- dashboardPage(header,sidebar,body)

# ------------------------------------------------------
# SERVER
# ------------------------------------------------------

server <- function(input, output) {
  
  observeEvent(input$tabs, {
    
    # METRICS
    if (input$tabs=="metrics") {
      
      # METRICS TABLE
      output$metrics_table <- renderTable({
        metrics_table},
        striped = F,
        hover = T,
        width = "100%"
      )
      
      # METRICS PLOT
      output$metrics_plot <- renderPlot({
        
        metrics_long %>%
          filter(metric %in% input$pick_metrics) %>%
          ggplot(aes(x = threshold, y = value, group = metric)) +
          facet_wrap(~ metric, ncol = 4, scales = "free") +
          geom_line(colour = line_colour)}
      )}
    
    # CONFUSION MATRIX
    if (input$tabs=="cm") {
      
      output$cm1 <- renderTable({
        h2o.confusionMatrix(mod,test_hf,metric=input$cm_metric)
      })
      output$cm2 <- renderTable({
        h2o.confusionMatrix(mod,test_hf,threshold=input$cm_custom)
      })
    }
    
    # CURVES
    if (input$tabs=="curves") {
      
      output$roc   <- renderPlot({roc_plot})
      output$prroc <- renderPlot({prroc_plot})
      output$gains <- renderPlot({gains_plot})
      output$lift  <- renderPlot({lift_plot})
    }
    
    # FEATURE IMPORTANCE
    if (input$tabs=="var_imp") {
      
      output$var_imp <- renderPlot({
        
        h2o.varimp(mod) %>%
          as_tibble() %>%
          top_n(input$n_preds) %>%
          dplyr::select(variable,scaled_importance) %>%
          ggplot(aes(reorder(variable,scaled_importance),scaled_importance)) +
          geom_col(fill=line_colour) +
          coord_flip()
      })
    }
    
    #PDP
    if (input$tabs=="pdp") {
      
      output$pdp_plot <- renderPlot(ggplot())
      
      observeEvent(input$pdp_go,{
        
        observeEvent(input$pdp_features,{
          
          pdps <- h2o.partialPlot(object = mod,
                                  data   = train_hf,
                                  cols   = input$pdp_features,
                                  plot   = F)
          
          if (length(input$pdp_features) == 1) {
            
            pdp_table <- pdps %>%
              as_tibble() %>%
              mutate(upper = mean_response + stddev_response,
                     lower = mean_response - stddev_response)
            
            pdp_table$variable  <- names(pdp_table)[1]
            names(pdp_table)[1] <- "value"
            
            output$pdp_plot <- renderPlot({
              pdp_table %>%
                ggplot(aes(value,mean_response,group=1)) +
                geom_line(size=1,colour=line_colour) +
                geom_ribbon(aes(ymin=lower,ymax=upper),
                            alpha=.2,colour=NA,fill=line_colour) +
                facet_wrap(~variable,scales="free_x")
            })
          }
          
          if (length(input$pdp_features) > 1) {
            
            pdp_table <- tibble()
            
            for (i in pdps) {
              
              i <- i %>%
                as_tibble(i) %>%
                mutate(upper = mean_response + stddev_response,
                       lower = mean_response - stddev_response)
              
              i$variable <- names(i)[1]
              names(i)[1] <- "value"
              pdp_table <- rbind(pdp_table,i)
            }
            
            output$pdp_plot <- renderPlot({
              
              pdp_table %>%
                ggplot(aes(value,mean_response,group=1)) +
                geom_line(size=1,colour=line_colour) +
                geom_ribbon(aes(ymin=lower,ymax=upper),
                            alpha=.2,colour=NA,fill=line_colour) +
                facet_wrap(~variable,scales="free_x") +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank())
            })
          }
        })
      }
      )
    }
    
    # LOCAL EXPLANATIONS
    if (input$tabs == "local_expl") {
      
      observeEvent(c(input$shap_cust,input$shap_n_feat),{
        
        shap_data <- h2o.shap_explain_row_plot(mod, test_hf, 
                                               row_index = as.numeric(input$shap_cust),
                                               top_n_features = input$shap_n_feat) %>%
          ggplot_build()
        
        output$shap_plot <- renderPlot({
          
          shap_data$data[[1]] %>%
            mutate(x1 = str_extract(text,"(?<=Feature: ).+(?= \n)"),
                   x2 = str_extract(text,"(?<=Feature Value: ).+(?= \n)")) %>%
            mutate(x2= gsubfn("([0-9.]+)", ~format(round(as.numeric(x))), x2)) %>%
            mutate(x =paste(x1,"=",x2)) %>%
            mutate(y=ymin+ymax) %>%
            select(x,y) %>%
            arrange(desc(y)) %>%
            ggplot(aes(reorder(x,y),y,fill = y>=0.0)) +
            geom_col() +
            coord_flip() +
            scale_fill_manual(values= c("grey45",line_colour))
        })
      })
    }
    
    # PREDICTION TABLE
    if (input$tabs=="preds") {
      output$pred_table <- renderDataTable({
        prediction_table
      },
      filter="top",
      escape=FALSE,
      selection="multiple",
      options = list(sDom  = '<"top">lrt<"bottom">ip')
      )
    }
    
    # DETAILS
    if (input$tabs=="details") {
      output$details <- renderTable({details})
    }
  })
}

shinyApp(ui, server)
