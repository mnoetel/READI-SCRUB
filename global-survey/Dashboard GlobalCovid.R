library(plotly)
library(Hmisc)
library(shiny)
library(stringr)
library(shinyWidgets)
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(RColorBrewer)
library(readr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(haven)
library(dplyr)
library(ggrepel)
library(tidyverse)




#https://osf.io/usq5b/

data_file <- "GlobalBehaviorsPerceptions_Data_May21_2020.dta"
if(!file.exists(data_file)){
  library(osfr)
  #osf_auth()#don't need this because the repo is public
  #osf_ls_files()
  osf_node <- osf_retrieve_node("3sn2k") #GlobalBehaviors OSF
  
  osf_node_files <- osf_ls_files(osf_node)
  file_to_dl<-osf_node_files[grepl("GlobalBehaviorsPerceptions_Data_",osf_node_files$name),][1,]
  dat_file <- osf_download(file_to_dl, conflicts = "overwrite")
  dat <- read_dta(dat_file$local_path)
} else {
  dat <- read_dta(data_file)
}

#dat <- read_dta("../../../data/scrub_collab/GlobalBehaviorsPerceptions_Data_May21_2020.dta")
#https://osf.io/zt3f7/
#dat <- read_csv("../../data/scrub_collab/latest_open_science_data.csv")

dat$age_yr <- cut(dat$age_yr, breaks = c(18, 29, 39, 49, 59, 69, 79, Inf), labels = c('18-29', '30-39','40-49', '50-59','60-69','70-79','80 and over'))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How are people responding to COVID-19?"),
  setBackgroundColor("#F0F0F0"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("variable",
                  label = "What do you want to know?",
                  choices = c("Choose variable",
                              "Behaviours",
                              "Reasons for Leaving Home",
                              "Beliefs of what others should do",
                              "Perception of what others believe",
                              "Worries",
                              "Physical health",
                              "Mental health"
      ),
      multiple = FALSE,
      selected = "Choose variable"),
    
    selectInput("country",
                label = "Select a country",
                choices = c("Worldwide",sort(unique(as.character(dat$CountryofLiving)))),
                multiple = FALSE,
                selected = "Worldwide"),
    checkboxGroupInput("gender",
                       label = "What genders should be displayed?",
                       choices = c("Male","Female", "Other"), inline = T,
                       selected = c("Male","Female", "Other")),
    checkboxGroupInput("agegroup",
                       label = "What age groups should be displayed?",
                       choices = levels(dat$age_yr),
                       inline = T, width = "100%",
                       selected = levels(dat$age_yr)),
    helpText(a("Data from the International Survey of Coronavirus",br(),br(),
               href = "https://osf.io/pyhf8/", target="_blank"),
             "Project collaborators",br(),
             "Thiemo Fetzer, Marc Witte, Lukas Hensel, Jon Jachimowicz, Johannes Haushofer, Andriy Ivchenko, Stefano Caria, Elena Reutskaja, Christopher Roth, Stefano Fiorin, Margarita Gomez, Gordon Kraft-Todd, Friedrich Götz, Erez Yoeli", br(), br(),
             "Dashboard creators", br(), 
             a("Julia Schreiber",
               href = "https://www.researchgate.net/profile/Julia_Schreiber3"), ", Ran Zhang, Ben J. Smith"
    ),
    width = 3
  ),
  mainPanel(
    textOutput("title"),
    plotOutput("distPlot"))
  ))   
      server <- function(input, output){

          output$title <- renderText({
            if (input$variable=="Choose variable"){
              "Start by selecting an option from the selection box labelled 'What do you want to know?'."
            }
            else{
              title <- input$variable
              components_to_add<-c()
              if(length(input$gender)<length(c("Male","Female", "Other"))){
                components_to_add <- c(components_to_add, paste0(input$gender,collapse = " and "))
                #title <- paste0(title, " for ", paste0(input$gender,collapse = " and "), " respondents")
              }
              
              if(length(input$agegroup)<length(levels(dat$age_yr))){
                components_to_add <- c(components_to_add, paste0(input$agegroup,collapse = ", "))
                #title <- paste0(title, " (", paste0(input$agegroup,collapse = ", "), ")")
              }
              
              if(input$country!="Worldwide"){
                components_to_add <- c(components_to_add, paste0(" in ", input$country))
                #title <- paste0(title, " in ", input$country)
              }
              if(length(components_to_add)>0){
                title <- paste0(title,": respondents " , paste0(components_to_add,collapse = " "))
              }
              
              title
              
            }
            
          })
          
          output$distPlot <- renderPlot({
            if(input$country == "Worldwide"){
              plot_dat <- dat
            }  else {
              plot_dat = dplyr::filter(dat, as.character(dat$CountryofLiving) == input$country)
            }
            genders <- as.character(input$gender)
            if ("Male" %in% as.character(input$gender)){genders[genders == "Male"] <- 1}
            if ("Female" %in% as.character(input$gender)){genders[genders == "Female"] <- 2}
            if ("Other" %in% as.character(input$gender)){genders[genders == "Other"] <- 3}
            #if ("NA" %in% as.character(input$gender)){genders[genders == "NA"] <- 4}
            plot_dat <- dplyr::filter(plot_dat, gender %in% genders)
            #plot_dat <- dplyr::filter(plot_dat, as.character(plot_dat$gender) %in% as.character(input$gender))
            plot_dat <- dplyr::filter(plot_dat, as.character(plot_dat$age_yr) %in% as.character(input$agegroup))
            
            sub = as.character(nrow(plot_dat))
            
            #title of graph
            if(input$variable == "Behaviours"){
              mytitle <- "To what extent did people do following behaviors (in the past week)?"
              oth <- dplyr::select(plot_dat, contains("beh_"))
              oth <- reshape2::melt(oth, measure.variables = 1:ncol(oth), variable.names = "variable", value.name = "value")
              xaxis = "Behaviour"
              yaxis = "Extend to which behavior is applied (%)"
              labels.def <- c("Stay home", "Avoid social \n gatherings", "Keep distance", "Wash hands", "Inform others \n of sickness")
            }
            else if(input$variable == "Reasons for Leaving Home"){
              mytitle <- "Why do people leave their home?"
              oth_1 <- dplyr::select(plot_dat, contains("leavehome_reason_"))
              oth <- data.frame("xvalue" = c("Work", "Pet", "Exercise", "Grocery shopping", "Pharmacy", "Hospital", "Care", "Friends", "Tired", "Bored", "Adrenaine", "Freedom", "Other"))
              oth$percent <- round((colSums(oth_1)/nrow(oth_1))*100, digits = 2)
              xaxis = "Reasons for Leaving Home"
              yaxis = "% of agreement for each reason"
              color.n <- 13
            }
            else if(input$variable=="Beliefs of what others should do"){
              mytitle <- "What do people feel that other people should do?"
              oth_1 <- dplyr::select(plot_dat, contains("fob_"))
              oth <- data.frame("xvalue" = c("Cancel social gatherings", "Stop shaking hands", "Close unnecessary stores", "Start a curfew"))
              oth$percent <- round((colSums(oth_1)/nrow(oth_1))*100, digits = 2)
              xaxis = "Expected behaviour"
              yaxis = "Extent to which belief is hold (%)"
              color.n <- 4
            }
            else if (input$variable=="Perception of what others believe"){
              mytitle <- "What do people think how many people believe that following behaviours should be followed?"
              oth <- dplyr::select(plot_dat, contains("sob_"))
              oth <- reshape2::melt(oth, measure.variables = 1:ncol(oth), variable.names = "variable", value.names = "value")
              xaxis = "Covid-19 measurments"
              yaxis = "Number of people out of 100"
              labels.def <- c("Cancel social gatherings", "Stop shaking hands", "Close unnecessary stores", "Start a curfew")
            }
            else if(input$variable=="Worries"){
              mytitle <- "How do people feel when they think about the currrent circumstances?"
              oth <- dplyr::select(plot_dat, contains("mh_anxiety_"))
              cats <- 4
              cats.n <- 3
              labels.def <- c("Does not apply", "Somewhat does not applies","Neither applies nor does apply", "Somewhat applies", "Strongly applies")
              xaxis <- "Worries"
              yaxis <- "Percentage of people"
              alabels <- NULL
              color.order <- TRUE
            }
            else if(input$variable == "Physical health"){
              mytitle <- "How healthy are people?"
              oth <- dplyr::select(plot_dat, contains("health"))
              cats <- 4
              cats.n <- NULL
              labels.def <- c("poor", "fair", "good", "excellent")
              xaxis <- "Level of physical health"
              yaxis <- "Percentage of people"
              alabels <- NULL
              color.order <- FALSE
            } 
            else if(input$variable=="Mental health"){
              mytitle <- "How healthy are people mentally?"
              oth <- dplyr::select(plot_dat, contains("PHQ9_"))
              oth$row_Sum <- rowSums(oth)
              oth <- oth %>% mutate(oth_cat = ifelse(row_Sum %in% 4:8,1, 
                                                     ifelse(row_Sum %in% 9:13,2, 
                                                            ifelse(row_Sum %in% 14:18, 3, 
                                                                   ifelse(row_Sum %in% 19:23, 4,
                                                                          ifelse(row_Sum %in% 24:32, 5,
                                                                                 0)))))) 
              oth <-data.frame(oth$oth_cat)
              
              cats <- 5
              cats.n <- NULL
              labels.def <- c("Non - Minimal","Mild","Moderate","Moderately Severe","Severe")
              xaxis = "Level of Depression"
              yaxis = "Percentage of people"
              alabels <- "Level of Depression"
              color.order <- TRUE
            }
            
            oth_plot <- NULL
            
            if (input$variable == "Reasons for Leaving Home" || input$variable == "Beliefs of what others should do"){
              oth_plot <- oth %>% mutate(xvalue = fct_reorder(xvalue, percent)) %>%   
                ggplot() +
                geom_bar(aes(x = xvalue, y = percent, fill = xvalue), stat = "identity")+
                labs(caption = yaxis, subtitle = paste ("Number of Participants =", sub , sep = " ", collapse = NULL))+
                ylim (0, 100)+
                coord_flip()+ 
                scale_fill_manual(values = rev(colorRampPalette(brewer.pal(11, "RdYlGn"))(color.n)))+
                geom_text(data=oth, aes(x = xvalue, y = percent, label = percent),
                          position=position_stack(vjust=0.5))+
                ggtitle(mytitle) +
                ggthemes::theme_fivethirtyeight() +
                theme(
                  plot.title = element_text(size = 14, face = "bold", hjust = 1),
                  plot.subtitle = element_text(size = 14, hjust = 1),
                  plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
                  axis.text=element_text(margin = 50), legend.position = "none", plot.caption = element_text(hjust = 1)
                )
            }
            else if (input$variable == "Perception of what others believe" || input$variable == "Behaviours"){
              oth_plot <- ggplot(oth, aes(x = variable, y = value))+
                geom_boxplot(color = "black", outlier.shape = NA)+
                ggtitle(mytitle)+
                labs(caption = yaxis, subtitle = paste ("Number of Participants =", sub , sep = " ", collapse = NULL))+
                scale_x_discrete(labels = labels.def)+
                coord_flip()+
                ggtitle(mytitle) +
                ggthemes::theme_fivethirtyeight() +
                theme(
                  plot.title = element_text(size = 14, face = "bold", hjust = 1),
                  plot.subtitle = element_text(size = 14, hjust = 1),
                  plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
                  axis.text=element_text(margin = 50),  legend.position = "none")
            }
            else if (input$variable == "Worries" || input$variable == "Physical health" || input$variable == "Mental health") {
              oth_plot <- plot_likert(oth, reverse.scale = T,
                                      sort.frq = "pos.asc",
                                      values = "show",
                                      axis.labels = alabels, 
                                      legend.pos = "bottom",
                                      show.n = FALSE,
                                      catcount = cats,
                                      cat.neutral = cats.n,
                                      grid.break = 1,
                                      intercept.line.color = "#D2D2D2",
                                      geom.colors = "RdYlGn",
                                      cat.neutral.color = "grey70",
                                      reverse.colors = color.order,
                                      legend.labels = labels.def,
                                      wrap.labels = 45)+
                ggtitle(mytitle, subtitle = paste ("Number of Participants =", sub , sep = " ", collapse = NULL)) +
                ggthemes::theme_fivethirtyeight() +
                theme(
                  plot.title = element_text(size = 14, face = "bold", hjust = 1),
                  plot.subtitle = element_text(size = 14, hjust = 1),
                  plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
                  axis.text=element_text(margin = 50),
                  legend.position="bottom", legend.direction = "horizontal")
            }
            oth_plot
            
          })
          
        # }

}
          # Run the app ----
          shinyApp(ui = ui, server = server)
          