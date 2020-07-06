#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(Hmisc)
library(shiny)
library(stringr)
library(shinyWidgets)
library(sjlabelled)
library(forcats)
library(sjmisc)
library(sjPlot)
library(RColorBrewer)
library(readr)
library(reshape2)

data_file <- "latest_open_science_data.RDS"
if(!file.exists(data_file)){
  library(osfr)
  osf_auth()
  scrub <- osf_retrieve_node("zt3f7") #open science data
  #scrub <- osf_retrieve_node("q7gck") #sensitive data
  scrub_files <- osf_ls_files(scrub)
  dat <- osf_download(scrub_files[3, ], conflicts = "overwrite")
  dat <- readr::read_rds(dat$local_path)
} else {
  dat <- readr::read_rds(data_file)
}

dat$gender <- sjlabelled::replace_labels(dat$gender, labels = c("Male" = 1, "Female" = 2, "Other" = 3))
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  # Application title
  titlePanel("How are people responding to COVID-19?"),
  setBackgroundColor("#F0F0F0"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("cross_or_long",
                  label = "Do you want a snapshot or trends/comparisons?",
                  choices = c("Snapshot","Trends")),
      conditionalPanel(
        condition = "input.cross_or_long == 'Snapshot'",
        selectInput("variable",
                    label = "What do you want to know?",
                    choices = c("Choose variable",
                                "Behaviours",
                                "Worries",
                                "Confidence in authorities",
                                "Information sources",
                                "Trust in information",
                                "Perceived knowledge",
                                "Future predictions",
                                "Age distribution",
                                "Wellbeing",
                                "What is stopping people from..."#,
                                # "What do people think are symptoms?"
                    ),
                    multiple = FALSE,
                    selected = "Choose variable"),
        conditionalPanel(
          condition = "input.variable == 'What is stopping people from...'",
          selectInput("bar_fogg",
                      label = "Select a behaviour",
                      choices = c("Staying Home",
                                  "Washing Hands",
                                  "Covering Coughs",
                                  "Keeping 2m",
                                  "Avoiding Their Face",
                                  "Using COVIDsafe"),
                      multiple = FALSE,
                      selected = c("Staying home"))),
        dateRangeInput("date",
                       label = "Filter responses by date",
                       start = min(dat$startdate),
                       end = max(dat$startdate),
                       min = min(dat$startdate),
                       max = max(dat$startdate),
                       format = "d M"),
        checkboxInput("show_n",
                      label = "Show number of responses"),
        checkboxInput("show_all",
                      label = "Label each segment"),
        selectizeInput("who_dont",
                       label = "See results for people who are not...",
                       choices = c("Staying home",
                                   "Washing hands",
                                   "Covering coughs",
                                   "Keeping 2m",
                                   "Avoiding their face"),
                       multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.cross_or_long == 'Trends'",
        selectInput("var2",
                    label = "I want to see ...",
                    choices = c("Core Preventative Behaviours",
                                "Worries",
                                "Confidence",
                                "Wellbeing",
                                "Other Preventative Behaviours",
                                "Predicted Probabilities"),
                    multiple = FALSE,
                    selected = "Core Preventative Behaviours"),
        selectInput("compare",
                    label = "Compared across ...",
                    choices = c("Wave",
                                "Gender",
                                "Age",
                                "Country",
                                "Australian State"),
                    multiple = FALSE,
                    selected = "Wave")
      ),
      checkboxGroupInput("wave",
                         label = "Which wave of data do you want to see?",
                         choices = c("1","2", "3", "4"),
                         selected = c("1","2", "3", "4"),
                         inline = T, width = "100%"),
      selectInput("sh_country",
                  label = "Select a country",
                  choices = c("Worldwide",sort(names(table(dat$country))[table(dat$country)>30])),
                  multiple = FALSE,
                  selected = "Worldwide"),
      conditionalPanel(
        condition = "input.sh_country == 'Australia'",
        checkboxGroupInput("region",
                           label = "Select a state",
                           choices = na.omit(unique(as.character(dat$state_aus))),
                           selected = na.omit(unique(as.character(dat$state_aus))))),
      checkboxGroupInput("gender",
                         label = "What genders should be displayed?",
                         choices = c("Male","Female"), inline = T,
                         selected = c("Male","Female")),
      checkboxGroupInput("agegroup",
                         label = "What age groups should be displayed?",
                         choices = levels(dat$agegroup),
                         inline = T, width = "100%",
                         selected = levels(dat$agegroup)),
      helpText(a("Data from the SCRUB project",br(),
                 href = "https://www.scrubcovid19.org/", target="_blank"),
               "Project leads",br(),
               a("Alexander Saeri",
                 href = "mailto:alexander.saeri@monash.edu"),br(),
               a("Peter Slattery",
                 href = "mailto:peter.slattery@monash.edu"),br(),
               a("Michael Noetel",
                 href = "https://twitter.com/mnoetel", target="_blank"),br(),
               a("Emily Grundy",
                 href = "mailto:emilygrundy0@gmail.com"),br(),
               "For dashboard comments or requests",
               a(" email Mike",
                 href = "mailto:michael.noetel@acu.edu.au"),
               a(" or log it here",
                 href = "https://github.com/mnoetel/READI-SCRUB/issues", target ="_blank"),br(),
               "Dashboard with help from",br(),
               a("James Conigrave",
                 href = "https://twitter.com/jamesconigrave", target="_blank"),
      ),
      width = 3
    )
    
    ,
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition = "input.variable == 'Choose variable' & input.cross_or_long == 'Snapshot'",
                       HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/FtVYsD3SMpA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      conditionalPanel(condition = "input.variable != 'Choose variable' | input.cross_or_long == 'Trends'",
                       plotOutput("distPlot"), width = 9),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  gen_plot_with <- function(data, title, cats, show_n, show_all){
    date_range <- paste(format(range(as.Date.POSIXct(data$startdate)),
                               format = "%B %d"), collapse = " — ")
    data <- dplyr::select(data, -startdate)
    #data <- oth
    #each = "sum.outside"
    #show_n = F
    for(i in 1:dim(data)[2]){
      attr(data[,i], "label")[[1]] <- gsub("\\s*\\([^\\)]+\\)","",as.character(attr(data[,i], "label")[[1]]))
    }
    each <- ifelse(show_all, "show", "sum.outside")
    data_plot <- plot_likert(data, reverse.scale = T,
                             geom.colors = "RdYlBu",  #Option YlGnBu
                             sort.frq = "neg.asc",
                             values = each,
                             digits = 0,
                             #axis.labels = sapply(data,function(x){attr(x,"label")}),
                             show.n = show_n,
                             catcount = cats,
                             grid.break = 1,
                             intercept.line.color = "#D2D2D2",
                             wrap.labels = 45)
    data_plot <- data_plot +
      ggthemes::theme_fivethirtyeight() +
      scale_fill_brewer(breaks = c(1, cats),
                        labels = c(names(attr(data[,1], "labels"))[1],
                                   names(attr(data[,1], "labels"))[cats]),
                        type="qual", palette="RdYlBu") +
      ggtitle(title) +
      #scale_y_continuous(breaks = seq(from  = -100, to = 100,by = 20)) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
        axis.text=element_text(size=14, margin = 50),
        legend.text = element_text(size=14)
        #panel.grid.minor.y = element_line(colour = 'grey', linetype = 2),
      ) + geom_text(aes(label = paste(c("Data from scrubcovid19.org",
                                        date_range,
                                        paste(c("n = ", dim(data)[1]), collapse = "")),
                                      collapse = "\n"),
                        x = dim(data)[2]-.5, y = -.95),
                    vjust = "middle", hjust = "inward", colour = "darkgrey")
    
    data_plot
  }
  con_data_plot <- function(data, title){
    data <- dplyr::select(data, -startdate)
    names(data) <- sapply(data, function(x){attr(x,"label")})
    for(i in 1:dim(data)[2]){
      names(data) <- gsub("\\s*\\([^\\)]+\\)","",names(data))
    }
    data <- reshape2::melt(data, value.name = "Value")
    p <- ggplot(data, aes(x=Value, color=variable,
                          #fill=variable,
                          y = variable)) +
      geom_boxplot() +
      scale_color_brewer(palette = "RdYlBu")+ #Option YlGnBu
      #   scale_fill_brewer(palette = "RdYlBu")+ #Option YlGnBu
      ggthemes::theme_fivethirtyeight() +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30))+
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
        axis.title = element_blank(),
        axis.text = element_text(size=16),
        #axis.text.y = element_blank(),
        #legend.direction = "vertical",
        legend.position = "none",
        #legend.text = element_blank()#element_text(size=16)
        #panel.grid.minor.y = element_line(colour = 'grey', linetype = 2),
      )+
      ggtitle(title)
    p
  }
  instructions_plot <- function(){
    data <- dplyr::select(dat,beh_stayhome, othb_food)
    cats <- 5
    #data <- oth
    #each = "sum.outside"
    #show_n = F
    data_plot <- plot_likert(data, reverse.scale = T,
                             #geom.colors = "RdYlBu",  #Option YlGnBu
                             sort.frq = "neg.asc",
                             values = "sum.outside",
                             digits = 0,
                             #axis.labels = sapply(data,function(x){attr(x,"label")}),
                             show.n = F,
                             axis.labels = c("Another example item",
                                             "Use the menu on the left to choose what to display"),
                             catcount = cats,
                             grid.break = 1,
                             intercept.line.color = "#D2D2D2",
                             wrap.labels = 20)
    data_plot <- data_plot +
      ggthemes::theme_fivethirtyeight() +
      scale_fill_brewer(breaks = c(1, cats),
                        labels = c(names(attr(data[,1], "labels"))[1],
                                   names(attr(data[,1], "labels"))[cats]),
                        type="qual", palette="RdYlBu") +
      ggtitle("How to use this dashboard") +
      #scale_y_continuous(breaks = seq(from  = -100, to = 100,by = 20)) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
        axis.text = element_text(size=20, margin = 50),
        legend.text = element_text(size=20),
        #panel.grid.minor.y = element_line(colour = 'grey', linetype = 2),
      ) +
      geom_text(aes(label = "10% of people\n say Never\n Rarely or\n Sometimes", .85, y = .11,fontface = "bold", size = 20),
                vjust = "middle", hjust = "right", colour = "black", size = 6) +
      geom_text(aes(label = "90% of\n people\n say\n Always\n or Often", .85, y = -.89, fontface = "bold", size = 20),
                vjust = "middle", hjust = "right", colour = "white", size = 6) +
      geom_text(aes(label = "84% of people\nsay Never\nRarely or\nSometimes", 1.85, y = .82, fontface = "bold", size = 20),
                vjust = "middle", hjust = "left", colour = "white", size = 6)
    
    data_plot
  }
  
  filter_empties <- function(filter_me){
    #filter_me <- df
    #filter out completely empty rows (except start date)
    filter_me <- dplyr::filter(filter_me, rowSums(is.na(filter_me))<(dim(filter_me)[2]-1))
    #filter out completely empty columns
    filter_me <- dplyr::select(filter_me, as.numeric(which(colSums(is.na(filter_me))!=dim(filter_me)[1])))
    filter_me
  }
  
  plot_change <- function(plot_data, plot_title, by = "wave"){
    #plot_data <- plot_dat
    #plot_title <- "title"
    plot_data$wave <- dplyr::recode(factor(plot_data$wave),
                                    '1' = "March",
                                    '2' = "April",
                                    '3' = "May",
                                    '4' = "June")
    for(var in colnames(plot_data)[-1:-5]) {
      names(plot_data)[which(var==colnames(plot_data))] <- attr(plot_data[,deparse(as.name(var))], "label")
    }
    if(by == "australian state"){
      by  <-  "state_aus"
      plot_data <- filter(plot_data, country == "Australia")
    }
    
    groupBy <- function(df, field) {
      #df <- plot_dat
      #field <- by
      df %>% group_by_(field) %>%
        filter(n() >= 20) %>%
        #Step 3
        summarise_all(mean, na.rm = TRUE) %>%
        reshape2::melt(id.vars = field, variable.name = "Question",
                       na.rm = T)
    }
    groupSDs <- function(df, field) {
      remove_other_factors <- which((names(df)!=field)[1:5])
      df[,-remove_other_factors] %>% group_by_(field) %>%
        filter(n() >= 20)%>%
        #Step 3
        summarise_all(sd, na.rm = TRUE) %>%
        reshape2::melt(id.vars = field, variable.name = "Question",
                       na.rm = T)
    }
    
    # by <- "wave"
    # by <- "gender"
    if(by=="age"){
      by <- "agegroup"
    }
    # by <- "agegroup"
    # by <- "state_aus"
    # by <- "country"
    m <- groupBy(plot_data, by)
    m <- cbind(m, groupSDs(plot_data, by)$value)
    m <- mutate(m, q2=Question)
    only_one_wave <- table(m$Question)==1
    multiple_waves <- names(only_one_wave)[!only_one_wave]
    m <- filter(m, m$Question %in% multiple_waves)
    names(m)[1] <- "group"
    names(m)[4] <- "sd"
    m$sd <- m$sd/sqrt(length(m$sd)) #conver to standard errors of the mean
    m <- dplyr::filter(m, !is.na(group))
    m$group <- dplyr::recode(m$group,
                             'Australian Capital Territory' = "ACT",
                             'United Kingdom of Great Britain and Northern Ireland' = "UK",
                             'United States of America' = "USA")
    m$ymin <- m$value-m$sd
    m$ymax <- m$value+m$sd
    m %>%
      ggplot( aes(x=group, y=value, ymin = ymin, ymax = ymax)) +
      geom_line( data=m %>% dplyr::select(-Question), aes(group=q2), color="grey", size=0.5, alpha=0.5) +
      geom_line( aes(group=Question), color="#69b3a2", size=1.2 )+
      geom_ribbon(aes(group=Question), color=NA, fill="#69b3a2", alpha=0.1)+
      #scale_color_viridis(discrete = TRUE) +
      ggthemes::theme_fivethirtyeight() +
      theme(
        legend.position="none",
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14, angle = 45, hjust = 1),
        strip.text.x = element_text(size=14)
      ) +
      facet_wrap(~Question, dir = "v",
                 ncol = 3,
                 labeller = labeller(Question = label_wrap_gen(30))) +
      ggtitle(paste(c("Mean Comparisons of ",plot_title," Across ", str_to_title(by)), collapse = "")) #plot_title <- "testing"
  }
  
  pick_fogg <- function(df){
    ipt <- input$bar_fogg
    #ipt <- "Staying Home" #troubleshooting code
    #df <- dat
    if(ipt=="Staying Home"){
      choose_var <- "bar_fogg_stayhome_"
    } else if(ipt=="Washing Hands"){
      choose_var <- "bar_fogg_hand_"
    } else if(ipt=="Covering Coughs"){
      choose_var <- "bar_fogg_cover_"
    } else if(ipt=="Keeping 2m"){
      choose_var <- "bar_fogg_distance_"
    } else if(ipt=="Avoiding Their Face"){
      choose_var <- "bar_fogg_touch_"
    } else if(ipt=="Using COVIDsafe"){
      choose_var <- "bar_fogg_app_"
    }
    df <- dplyr::select(df, contains(choose_var), startdate)
    df
  }
  
  output$distPlot <- renderPlot({
    if(input$sh_country == "Worldwide"){
      plot_dat <- dat
      #input <- as.data.frame(NA)
      #input$sh_country <- "Australia"
      
    } else if (input$sh_country == "Australia") {
      plot_dat <- dplyr::filter(dat, as.character(dat$country) == input$sh_country)
      plot_dat <- dplyr::filter(plot_dat, as.character(plot_dat$state_aus) %in% as.character(input$region))
    } else {
      plot_dat = dplyr::filter(dat, as.character(dat$country) == input$sh_country)
    }
    attr(plot_dat$swb4,"label") <- " Overall, how anxious did you feel yesterday?"
    oth <- plot_dat
    plot_dat <- dplyr::filter(plot_dat, wave %in% as.numeric(input$wave))
    if(input$cross_or_long == 'Snapshot'){
      plot_dat <- dplyr::filter(plot_dat, as.Date.POSIXct(plot_dat$startdate) >= input$date[1])
      plot_dat <- dplyr::filter(plot_dat, as.Date.POSIXct(plot_dat$startdate) <= input$date[2])
      if(length(input$who_dont) >0){
        cols_to_keep <- c("Staying home",
                          "Washing hands",
                          "Covering coughs",
                          "Keeping 2m",
                          "Avoiding their face") %in% input$who_dont
        #plot_dat <- dat
        rows_to_keep <- as.data.frame(cbind(plot_dat$beh_stayhome <= 2,
                                            plot_dat$beh_handwash <= 2,
                                            plot_dat$beh_cover <= 2,
                                            plot_dat$beh_distance <= 2,
                                            plot_dat$beh_touch <= 2)[,cols_to_keep])
        plot_dat$naughty <- rowSums(rows_to_keep, na.rm = T)
        plot_dat <- dplyr::filter(plot_dat, plot_dat$naughty > 0)
      }
    }
    #plot_dat <- oth
    plot_dat <- dplyr::filter(plot_dat, as.character(plot_dat$gender) %in% as.character(input$gender))
    plot_dat <- dplyr::filter(plot_dat, as.character(plot_dat$agegroup) %in% as.character(input$agegroup))
    
    
    
    
    
    #title of graph
    if(input$variable == "Behaviours"){
      cats <- 5
      mytitle <- "\"In the past 7 days, which of the following\n personal actions have you taken in response to COVID-19?\""
      oth <- dplyr::select(plot_dat, startdate, starts_with("beh_"), starts_with("othb"),-contains("_pa_"), -contains("alcohol"), -contains("w3"))
      
    }
    else if(input$variable=="Worries"){
      cats <- 7
      mytitle <- "\"At the moment, how much do you worry about...?\""
      oth <- dplyr::select(plot_dat, startdate, contains("worry_"), -contains("other"))
    } else if(input$variable=="What behaviours are people aware of?"){
      cats <- 7
      mytitle <- "\"In the past 7 days, how\n AWARE were you of the need to do each of these behaviours?\""
      oth <- dplyr::select(plot_dat, startdate, contains("aware_"), -contains("factors"))
    } else if(input$variable=="What are people feeling capable of doing?"){
      cats <- 7
      mytitle <- "\"In the past 7 days, how\n EASY or DIFFICULT did you find it to do each of these behaviours?\""
      oth <- dplyr::select(plot_dat, startdate, contains("capa_"), -contains("factors"))
    } else if(input$variable=="What are people feeling supported to do?"){
      cats <- 7
      mytitle <- "\"In the past 7 days, how much were you HELPED or HINDERED by\n your social and physical environments to do each of these behaviours?\""
      oth <- dplyr::select(plot_dat, startdate, contains("opp_"), -contains("factors"))
    } else if(input$variable== "What are people motivated to do?"){
      cats <- 7
      mytitle <- "\"In the past 7 days, how much were you\n MOTIVATED to do each of these behaviours?\""
      oth <- dplyr::select(plot_dat, startdate, contains("motiv_"), -contains("factors"))
    } else if(input$variable=="Information sources"){
      cats <- 5
      mytitle <- "\"In the past 7 days, how often did you\n use information from the following groups to stay informed about COVID-19?\""
      oth <- dplyr::select(plot_dat, startdate, contains("informed"), -contains("other"))
    } else if(input$variable=="Confidence in authorities"){
      cats <- 7
      mytitle <- "\"How much confidence do you have that\n the following authorities can minimise the harm caused by COVID-19?\""
      oth <- dplyr::select(plot_dat, startdate, contains("conf_"), -contains("other"))
    } else if(input$variable=="Trust in information"){
      cats <- 5
      mytitle <- "\"How often do you take actions recommended by\n the following groups regarding COVID-19?\""
      oth <- dplyr::select(plot_dat, startdate, contains("follow_"), -contains("other"))
    } else if(input$variable=="Perceived knowledge"){
      cats <- 7
      mytitle <- "\"How would you rate your knowledge level on COVID-19?\""
      oth <- dplyr::select(plot_dat, startdate, contains("perceived_knowledge"))
    } else if(input$variable=="Age distribution"){
      cats <- 0
      mytitle <- "\"How old are you?\""
      oth <- dplyr::select(plot_dat, startdate, age)
      names(oth)[2] <- "Age"
    } else if(input$variable=="What is stopping people from..."){
      cats <- 2
      mytitle <- paste(c("What Stops People From ",input$bar_fogg, "?"), collapse = "")
      oth <- pick_fogg(plot_dat)
    } else if(input$variable=="Future predictions"){
      cats <- 0
      mytitle <- "What do people think will happen?"
      oth <- dplyr::select(plot_dat, startdate, contains("prob_"))
      attr(oth[,1:4],"label") <- c("My probability of getting COVID-19 in 12 months",
                                   "Average person's probability of getting COVID-19 in 12 months",
                                   "My chance of dying from COVID-19, if I got it",
                                   "Average person's chance of dying from COVID-19, if they got it")
    } else if(input$variable=="Wellbeing"){
      cats <- 0
      mytitle <- "Subjective wellbeing measures"
      oth <- dplyr::select(plot_dat, startdate, contains("swb"))
      attr(oth[,5],"label") <- " Overall, how anxious did you feel yesterday?"
    }
    if(input$cross_or_long == 'Trends'){
      filter_stem <- "beh_"
      if(input$var2=="Worries"){
        filter_stem <- "worry_"
      } else if(input$var2=="Confidence"){
        filter_stem <- "conf_"
      } else if(input$var2=="Wellbeing"){
        filter_stem <- "swb"
      } else if(input$var2=="Other Preventative Behaviours"){
        filter_stem <- "othb_"
      } else if (input$var2=="Predicted Probabilities"){
        filter_stem <- "prob_"
      }
    }
    
    
    # else if(input$variable=="What do people think are symptoms?"){
    #   cats <- 2
    #   mytitle <- "Which of the following can be symptoms of COVID-19?"
    #   oth <- dplyr::select(plot_dat, contains("symp_"))
    #   oth <- as.data.frame(lapply(oth, sjlabelled::as_numeric))
    #   oth <- dplyr::filter(oth, !is.na(oth$symp_fever))
    # }
    
    
    #initialise function for generating graph with data, title, and number of categories in likert scale
    
    oth <- filter_empties(oth)
    if(input$variable != "Choose variable" | input$cross_or_long == 'Trends'){
      if(dim(oth)[2]<1|dim(plot_dat)[1]<30){
        oth_plot <- ggplot(data.frame()) + geom_density() + xlim(0, 1) + ylim(0, 1) +
          scale_x_discrete(breaks = NULL)+scale_y_discrete(breaks = NULL)+
          ggthemes::theme_fivethirtyeight() +
          ggtitle("The filters you selected are too restrictive") +
          #scale_y_continuous(breaks = seq(from  = -100, to = 100,by = 20)) +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 1),
            plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
            axis.text = element_text(size=20, margin = 50),
            legend.text = element_text(size=20),
            #panel.grid.minor.y = element_line(colour = 'grey', linetype = 2),
          )
      } else {
        if(input$cross_or_long == 'Trends'){
          #plot_dat <- dat
          #filter_stem <- "beh_"
          plot_dat <- dplyr::select(plot_dat, wave, agegroup, gender, state_aus, country,
                                    starts_with(filter_stem), -contains("other"),
                                    -contains("_pa_"), -contains("alcohol"), -contains("w3"))
          oth_plot <- plot_change(plot_dat, input$var2, tolower(input$compare))
        }else{
          if(cats > 0){
            oth_plot <-   gen_plot_with(oth, mytitle, cats, input$show_n, input$show_all)#T,T)#
          } else {
            oth_plot <-   con_data_plot(oth, mytitle)
          }
        }
      }
    } else {
      oth_plot <- ggplot(data.frame()) + geom_density() + xlim(0, 1) + ylim(0, 1) +
        scale_x_discrete(breaks = NULL)+scale_y_discrete(breaks = NULL)+
        ggthemes::theme_fivethirtyeight() +
        ggtitle("Loading") +
        #scale_y_continuous(breaks = seq(from  = -100, to = 100,by = 20)) +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 1),
          plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
          axis.text = element_text(size=20, margin = 50),
          legend.text = element_text(size=20),
          #panel.grid.minor.y = element_line(colour = 'grey', linetype = 2),
        )
    }
    
    
    oth_plot
  }, height = 800)
}

# Run the application
shinyApp(ui = ui, server = server)