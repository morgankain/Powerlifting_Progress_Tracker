###################################################
## Track and project gaines from a given program ##
###################################################

library(shiny)
library(ggplot2)
library(rsconnect)
library(Hmisc)
library(dplyr)
library(shinyLP)

ui <- fluidPage(

  ##  Application title
  titlePanel("Track Lifting Progress")

 , fluidRow(
        column(2,
          
      ## Lift Choice
      radioButtons("Primary_Lift", label = h3("Primary Lifts"),
      choices = list("Squat" = "Squat", "Bench" = "Bench", "Deadlift" = "Deadlift"), 
      selected = "Squat")
    
    , radioButtons("Smooth", label = h3("Add A Loess Smooth?"),
      choices = list("Yes" = "Yes", "No" = "No"), 
      selected = 'Yes')
          
    )

, column(width = 10,
      h4("For workout details click data points. If a video is linked it will show up"),
      verbatimTextOutput("click_info"))
   
, column(8,
         plotOutput(outputId = "lift_plot", click = "plot1_click") 
       , uiOutput("video_click")
    )))
  
server <- function(input, output) {
  
  ## ggplot theme
theme_set(theme_bw())
theme_update(
    axis.text.x = element_text(size = 22)
  , axis.text.y = element_text(size = 22)
  , axis.title.x = element_text(size = 22)
  , axis.title.y = element_text(size = 22)
  , legend.title = element_text(size = 14)
  , panel.grid.major = element_blank()
  , panel.grid.minor = element_blank()
  , strip.background = element_blank()
  , legend.key.size = unit(.25, "cm")
  , legend.key = element_rect(fill = "white")
  , panel.border = element_rect(colour = "black", fill = NA, size = 1)
  , strip.text.x = element_text(size = 16, colour = "black", face = "bold"))

## Load necessary data
RPE_progress <- read.csv("RPE_progress.csv")
RPE_progress <- transform(RPE_progress
  , RPE  = as.numeric(as.character(RPE))
  , Reps = as.numeric(as.character(Reps)))
RPE_chart    <- read.csv("RPE_chart.csv")

## interpolate RPE chart backwards to fill back to 3 RPE
for (i in unique(RPE_chart$Reps)) {
  temp_rpe    <- RPE_chart[RPE_chart$Reps == i, ]
  temp_extrap <- data.frame(with(temp_rpe, approxExtrap(x = RPE, y = Proportion, xout = seq(3, 6, by = 0.5))))
  names(temp_extrap) <- c("RPE", "Proportion")
  temp_extrap <- data.frame(
    RPE        = temp_extrap[, 1]
  , Reps       = rep(i, nrow(temp_extrap))
  , Proportion = temp_extrap[, 2])
  
  temp_rpe <- rbind(temp_extrap, temp_rpe)
  
  if (i == 1) {
    RPE_chart_extrap <- temp_rpe
  } else {
    RPE_chart_extrap <- rbind(RPE_chart_extrap, temp_rpe)
  }
  
}

## overright RPE with the extrapolated chart
RPE_chart <- RPE_chart_extrap

## Subset data according to user choices
lift_subset  <- reactive({
  
  ## For now just choose competition lift. Can expand later
lift_choice <- RPE_progress %>%
  filter(
    Focus == input$'Primary_Lift'
  , Type  == "Competition")

  ## Clean up the data a bit
lift_choice <- lift_choice %>% 
  left_join(., RPE_chart, by = c("RPE", "Reps"))

## and calculate projected max
lift_choice <- lift_choice %>% 
  mutate(projected_max = Weight / Proportion)

## remove "future" days
lift_choice <- lift_choice %>%
  filter(!is.na(RPE))

})

selectedData <- reactive({

    nearPoints(
      lift_subset()
    , input$plot1_click)
    
  })

output$click_info <- renderPrint({
    temp_dat <- selectedData()
    temp_dat %>% select("Date", "Week", "Exercise", "Sets", "Reps", "Weight", "RPE", "projected_max")
   
  })
  
## render the ggplot based on the spectra chosen by the user, with a background color given by the calculation
output$lift_plot <- renderPlot({
    
 ## Simple ggplot will do. Fit a GAM smooth if the user wants it
    if (input$'Smooth' == "Yes") {
      
ggplot(lift_subset(), aes(Week, projected_max)) + 
  geom_point(aes(colour = as.factor(Reps), shape = as.factor(uploaded_video)), lwd = 3.5) +
  geom_smooth(aes(group = as.factor(Exercise)), lwd = 1) + 
  xlab("Training Week") + 
  ylab("Projected Max: lbs (kg for deadlift)") +
  guides(col = guide_legend(title = "Reps"), shape = guide_legend(title = "Video?"))
      
    } else {
 
ggplot(lift_subset(), aes(Week, projected_max)) + 
  geom_point(aes(colour = as.factor(Reps), shape = as.factor(uploaded_video)), lwd = 3.5) +
  xlab("Training Week") + 
  ylab("Projected Max: lbs (kg for deadlift)") + 
  guides(col = guide_legend(title = "Reps"), shape = guide_legend(title = "Video?"))
      
    }

    })
  
output$video_click <- renderUI({
  
  video_url <- as.character(selectedData()$vid_url)

  HTML(video_url)
    
  })
  
}

shinyApp(ui = ui, server = server)
