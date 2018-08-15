#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages




library(RCurl)
library(data.table)
library(stringr)
library(tidyr)
library(lubridate)
library(shiny)
library(shinythemes)
library(reshape2)
library(dplyr)
library(visNetwork)
library(ggplot2)
library(DT)
library(shinyjs)



# improve shiny plot quality
library(Cairo)
options(shiny.usecairo=T)





#write a function that looks at most frequent reponders


response_summary <- function(df, person, type){
  library(scales)
  #select sender 
  tmp <- df 
  tmp$indicator <- 0 
  tmp[tmp$sender == person,]$indicator <- 1 
  tmp$ind <- lag(tmp$indicator)
  
  #calculate total messages by sender 
  tmp <- tmp %>% group_by(sender) %>%
    mutate(total_messages = n()) 
  tmp
  
  #create statistics
  if (type == 1){ tmp %>% group_by(sender) %>% 
      filter(ind == 1) %>% 
      summarise(total_responses = n())
    
  } else if (type == 2){ tmp %>% group_by(sender) %>% 
      filter(ind == 1) %>% 
      mutate(total_responses_to_sender = n()) %>% 
      mutate(done = total_responses_to_sender/total_messages) %>% 
      summarise("% of all messages made in response to sender" = percent(mean(done)))} }



# Create a response matrix


response_matrix <- function(df, type){
  
  senders <- response_summary(df, df$sender[[1]], type = 1)$sender
  
  responses_total <- cbind(as.data.frame(lapply(senders, function(x)
    response_summary(df, x, type = type)[[2]]))) 
  names(responses_total) <- senders 
  row.names(responses_total) <- senders 
  responses_total
  
  
}






# Response summary (word)






#write a function that looks at most frequent reponders


response_summary_word <- function(df, person, type, word){
  library(scales)
  library(magrittr)
  #select sender 
  tmp <- df 
  tmp$indicator <- 0 
  if (length(tmp[tmp$sender == person & grepl(word, tmp$message),]$indicator) > 0){
    tmp[tmp$sender == person & grepl(word, tmp$message),]$indicator <- 1 
  } else { 
    tmp$indicator <- 0
  }
  tmp$indicator <- lag(tmp$indicator)
  tmp %<>% mutate(indicator = ifelse(is.na(indicator), 0, indicator))
  
  #calculate total messages by sender 
  tmp <- tmp %>% group_by(sender) %>%
    mutate(total_messages = n()) 
  
  
  if (type == 1){ tmp %>% group_by(sender) %>% summarise(total_responses = sum(na.omit(indicator)))
    
  } else if (type == 2){tmp %>% group_by(sender) %>%
      mutate(total_responses_to_sender = sum(na.omit(indicator))) %>%
      mutate(done = total_responses_to_sender/total_messages) %>% 
      summarise("% of all messages made in response to sender" = percent(mean(done)))} }






# Create a response matrix


response_matrix_word <- function(df, type, word){
  senders <- response_summary(df, df$sender[[1]], type = 1)$sender
  
  
  responses_total <- cbind(as.data.frame(lapply(senders, function(x)
    response_summary_word(df, x, type = type, word = word)[,2]))) 
  names(responses_total) <- senders 
  row.names(responses_total) <- senders 
  responses_total
  
  
}







# Create Network Whatsapp function




create_network_whatsapp <- function(file, word){
  
  # load chat  
  loaded_chat <- read.table(file,  
                            fill=TRUE, header=TRUE, quote="", sep="\n", encoding="UTF-8")
  names(loaded_chat) <- "string"
  
  # format the data and split into different columns
  regx <- "(\\d+/\\d+/\\d+),\\s+(\\d+:\\d+:\\d+ [A|P]M):\\s+(\\w+\\s*\\w+):\\s+(.*)"
  tmp <- loaded_chat %>%
    tidyr::extract(string, c("date", "time", "sender", "message"), regx, remove=FALSE)
  
  tmp <- filter(tmp, !is.na(sender))
  
  # create nodes list
  nodes <- data.frame(select(tmp, sender) %>% distinct()) 
  names(nodes) <- "id"
  
  # add in node colors, names 
  nodes$group <- nodes$id 
  nodes$label <- nodes$id
  
  #create number of messages sent for node size
  totals <- select(tmp, sender, message) %>%
    mutate(ind = ifelse(grepl(word, message), 1, 0)) %>%
    group_by(sender) %>%
    summarise(total_messages = sum(ind))
  
  
  nodes <- left_join(nodes, totals, by=c("id"="sender"))
  nodes$size <- pmin(pmax((nodes$total_messages *100)/sum(nodes$total_messages), 75/sum(nodes$total_messages)),50)
  nodes$title <- paste("<p style=\"color: black\">",nodes$total_messages,"</p>")
  
  
  # create response matrix
  responses <- response_matrix_word(tmp, 1, word = word)
  responses <-as.data.frame(responses)
  responses_t <- t(responses) #so that melt correctly picks up to and from
  responses_m <- melt(responses_t)
  names(responses_m) <- c("from","to", "number")
  
  
  # format as network edges
  edges <- responses_m 
  edges$to <- as.character(edges$to) 
  edges$width <- pmin((edges$number * 50)/sum(edges$number), 50)
  edges$title <- paste("<p style=\"color:black\">",edges$number,"</p>")
  edges <- edges[edges$number > 0,]
  
  # render network 
  visNetwork(nodes, edges) %>%
    visPhysics( solver ='forceAtlas2Based') %>% visNodes(font = list(color = "white"))
  
  
  
  
}








# Clean data function: 



clean_convo <- function(file){
  
  # load chat  
  loaded_chat <- read.table(file,  
                            fill=TRUE, header=TRUE, quote="", sep="\n", encoding="UTF-8")
  names(loaded_chat) <- "string"
  
  # format the data and split into different columns
  regx <- "(\\d+/\\d+/\\d+),\\s+(\\d+:\\d+:\\d+ [A|P]M):\\s+(\\w+\\s*\\w+):\\s+(.*)"
  tmp <- loaded_chat %>%
    tidyr::extract(string, c("date", "time", "sender", "message"), regx, remove=FALSE)
  
  tmp <- filter(tmp, !is.na(sender))
  tmp$message <- str_replace_all(tmp$message, "[^a-zA-Z0-9.?!-<> ]", "")
  
  tmp
}






# Word frequency functions 




library(lubridate)
library(zoo)
library(ggthemes)

# Frequency plot function

freq_plot <- function(df, word, v1, v2){
  
  df %>% mutate(date = mdy(date)) %>%
    group_by_(v1, v2) %>%
    filter(grepl(word, message)) %>%  
    summarise(count = n()) %>%
    ggplot(aes(x = date, y = count, color = sender)) + geom_line() + ylim(0,NA) + theme_bw() }




# Smoothed frequency plot function 

smoothed_frequency_plot <- function(df, word, smooth = TRUE, point = TRUE, smooth_days){
  
  tmp_data <- df %>% mutate(date = mdy(date)) %>%
    group_by(sender, date) %>%
    mutate(ind = ifelse(grepl(word, message), 1, 0)) %>% 
    summarise(count = sum(ind)) %>% 
    group_by(sender) %>% 
    mutate(smooth_count = rollmean(count, smooth_days, fill = NA))
  
  if (smooth == TRUE & point == TRUE){
    ggplot(tmp_data) + geom_point(aes(x = date, y = count, color = sender), size = 0.25) +
      geom_line(aes(x=date, y = smooth_count, color = sender), size = 0.25) + 
      ylim(0,NA) + theme_bw() + guides(colour = guide_legend(title = "Sender", override.aes = list(size=1))) +
      xlab("Date") + ylab("# of messages")
    
  } else if (smooth == TRUE & point == FALSE){
    ggplot(tmp_data) + 
      geom_line(aes(x=date, y = smooth_count, color = sender), size = 0.25) + 
      ylim(0,NA) + theme_bw() + guides(colour = guide_legend(title = "Sender", override.aes = list(size=1))) +
      xlab("Date") + ylab("# of messages")
    
  } else { 
    ggplot(tmp_data) + geom_point(aes(x = date, y = count, color = sender), size = 0.25) +
      ylim(0,NA) + theme_bw() + guides(colour = guide_legend(title = "Sender", override.aes = list(size=1))) +
      xlab("Date") + ylab("# of messages")
  }
  
}



# Share of conversation


#summary table of two variable

tw_tab <- function(df, v1, v2){
  
  tmp <- df
  
  tmp %>% group_by_(v1, v2) %>% 
    mutate(tot=n()) %>% 
    ungroup() %>% 
    group_by_(v1) %>% 
    mutate(perc = tot/n()) %>% 
    ungroup() %>% group_by_(v1, v2) %>% 
    summarise(end = mean(perc)) %>% dcast(paste(v1, '~', v2), value.var = "end")
  
}



# Smoothed share function 


smoothed_share <- function(df, smooth_days){
  
  # set up 
  tmp_data <- df
  tmp_data <- tw_tab(tmp_data, "date", "sender") %>% mutate(date = mdy(date)) %>% arrange(date)
  tmp_data[is.na(tmp_data)] <- 0
  
  # create smoothed share of conversation
  tmp_data_smoothed <- data.frame(lapply(tmp_data[,2:dim(tmp_data)[2]], function(x){rollmean(x, smooth_days, fill = NA)}))
  tmp_data_smoothed <- data.frame(Date = tmp_data[,1], tmp_data_smoothed)
  
  # produce plot
  tmp_plot_data <- tmp_data_smoothed %>% melt(id.vars = "Date")
  tmp_plot_data %>% ggplot(aes(x = Date, y = value * 100, color = variable, fill = variable)) + geom_col() +
    ylab("Share (%)") + guides(fill = guide_legend(title = "Sender"), color = "none") + theme_bw()
  
  
}





# Search function





#function that returns other columns of the data as well
text_search <- function(text, df){
  tmp <- df
  tmp_index <- grep(text, tmp$message)
  tmp[c(tmp_index),c("sender", "message")]
}


#function to search around a specific instance (message before and after)

conv_search <- function(df, line, context_lines){
  if (context_lines %% 2 !=0){"error, needs even number"} else {
    
    start <- line - context_lines/2
    end   <- line + context_lines/2  
    
    if (start >= 1 & end <= dim(df)[1]){
      df[start:end, c("sender", "message")]
    }
    else if (start < 1 & end <= dim(df)[1]){
      df[1:end, c("sender", "message")]
    }
    else if (start >= 1 & end > dim(df)[1]){
      df[start:dim(df)[1], c("sender", "message")] 
    }
    else {
      df[1:dim(df)[1], c("sender", "message")] 
    } 
    
  }
}









# Use function to write cleaner shiny app 




ui <- navbarPage("WhatsApp Analytics", useShinyjs(), theme = shinytheme("darkly"),
                 
                 
                 ## Panel 1: Network
                 tabPanel("Conversation network",
                          
                          titlePanel("Conversation network"),
                          
                          # Output        
                          fluidRow(
                            
                            column(6,
                                   # render first network
                                   visNetworkOutput("network_1") 
                                   
                                   
                            ),
                            
                            column(6,
                                   # render first network
                                   visNetworkOutput("network_2")
                                   
                            )
                          ),
                          
                          # Inputs
                          fluidRow(
                            column(4,
                                   
                                   
                                   # file input 1
                                   fileInput("file_1", "Choose file 1",
                                             accept = c(".txt") 
                                   ),
                                   
                                   # create user input word box 
                                   textInput("word_1", "Word for first network", value = ""), 
                                   p("Please input a WhatsApp text file. You can download a WhatsApp text file by following the steps laid out at the bottom of this", a("link", href='https://faq.whatsapp.com/en/iphone/20888066'), "under \"Email Conversation\".", tags$b("Note:")," this application is optimised for iPhone WhatsApp conversations. Results on Android may vary.", tags$b(tags$h4("Conversation Network: ")), "The network above is a representation of the uploaded WhatsApp conversation. The size of each node is related to the relative number of messages sent by that person. The size of the edges between each node are proportional to the number of messages sent by one person (represented by the colour of the edge) that were responded to by the person the edge connects to. You can view the exact number of messages/responses by placing your mouse over the respective node/edge. If you enter a word in the search box it will show you the network, but only for messages sent that contain that word.")
                                   ), 
                                   
                                   
                                  
                                
                             
                            column(4, offset = 4, 
                                   
                                   # file input 2
                                   fileInput("file_2", "Choose file 2",
                                             accept = c(".txt")
                                   ),
                                   
                                   # create user input word box 
                                   textInput("word_2", "Word for second network", value = "")
                            )
                          )
                 ),
                 
                 ## Panel 2: Word use statistics
                 tabPanel("Word use statistics", 
                          
                          titlePanel("Word use statistics"),
                          
                          # Set up tab panel
                          tabsetPanel(position = "left",
                                      
                                      ## Plots
                                      # Tab 1: word frequency
                                      tabPanel(  "Word frequency",         
                                                 
                                                 fluidRow(
                                                   
                                                   column(5,
                                                          
                                                          # render first plot
                                                          plotOutput("word_freq_1"),
                                                          tags$head(tags$style(HTML("
                                                                                    .shiny-plot-output{
                                                                                    max-width: 100%;
                                                                                    box-shadow: 0px 0px 3px #888;
                                                                                    }
                                                                                    ")))
                                                          ),
                                                   
                                                   column(5, offset = 2,
                                                          
                                                          # render second plot
                                                          plotOutput("word_freq_2")
                                                          
                                                   )
                                                          )
                                                          ), 
                                      
                                      # Tab 2: conversation share
                                      tabPanel("Conversation share",
                                               
                                               fluidRow(
                                                 
                                                 column(5,
                                                        
                                                        # render first plot
                                                        plotOutput("convo_share_1")
                                                        
                                                 ),
                                                 
                                                 column(5, offset = 2,
                                                        
                                                        # render second plot
                                                        plotOutput("convo_share_2")
                                                        
                                                 )
                                               )    
                                      )
                                                   ),
                          
                          ## Input sliders/text
                          fluidRow(
                            
                            column(4, offset = 0,  
                                   
                                   sliderInput("smoothing_parameter", "Number of days to smooth over", min = 1, max = 30, value = 1),
                                   checkboxInput("smooth", "Word/message frequency trend", TRUE),
                                   checkboxInput("point", "Word/message frequency points", TRUE),
                                   textInput("word_freq", "Word (leave blank to match all words)", value = "")
                                   
                            ),
                            column(4, 
                                   p(tags$b(tags$h5("Word frequency: ")),"The charts above show the number of message sent, broken down by sender and date. The options below allow you to: create a smoothed trend of the average number of messages sent over a rolling window of x days; show or hide either the individual points or the trend; and to perform this frequency analysis on a subset of messages that contain a particular word. Note, if you try and smooth over more days than people in the group have sent a message the chart will return an error.", tags$h5(tags$b("Conversation share:")), "shows the share of the conversation (by total messages) of each participant. It is also smoothed using the value in the input slider. ")
                                   ),
                            
                            column(4, offset = 0,
                                  
                                   sliderInput("smoothing_parameter_2", "Number of days to smooth over", min = 1, max = 30, value = 1),
                                   checkboxInput("smooth_2", "Word/message frequency trend", TRUE),
                                   checkboxInput("point_2", "Word/message frequency points", TRUE),
                                   textInput("word_freq_2", "Word (leave blank to match all words)", value = "") 
                                   
                                   
                            )
                            
                          )
                          
                          
                                                 ),
                 
                 ## Panel 3: Search
                 tabPanel("Search",
                          
                          titlePanel("Search"),
                          
                          sidebarLayout(
                            
                            # Inputs 
                            sidebarPanel(style='padding:10px;',
                                         p("Search for specific words or phrases using the text box below. You can refine your search by using the boxes above the \"Sender\" and \"Message\" columns. To look at the conversation around a specific message, input the line number in the box below and turn to the", em("Context"), "tab."),
                                         textInput("word_search", "Word/phrase", value = "hi"),
                                         p("Note that search is only for the", em("file 1"), "WhatsApp conversation"),
                                         numericInput("line_number", "Line number", value = 10, min = 1, step = 1),
                                         sliderInput("context_lines", "Number of context lines", min = 2, max = 50, value = 10, step = 2)
                                         
                            ),
                            
                            # Outputs
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Search results", dataTableOutput("word_search")), 
                                tabPanel("Context", dataTableOutput("word_context"))
                              )
                            )
                          )
                          
                          
                 )
                 
                 
                 
)

## Server

server <- function(input, output) {
  
  ## Create Network 1
  output$network_1 <- renderVisNetwork({
    
    # Load file, if exists
    inFile_1 <- input$file_1
    if (is.null(inFile_1))
      return(NULL)
    
    create_network_whatsapp(inFile_1$datapath, input$word_1)
    
  })
  
  ## Create Network 2
  output$network_2 <- renderVisNetwork({
    
    # Load file, if exists
    inFile_2 <- input$file_2
    if (is.null(inFile_2))
      return(NULL)
    
    create_network_whatsapp(inFile_2$datapath, input$word_2)
    
  })  
  
  ## Create Word Freq Plot 1 
  
  output$word_freq_1 <- renderPlot({
    
    # only show plot if file exists
    if(is.null(input$file_1)){
      hide("word_freq_1")}
    else{
      show("word_freq_1")}
    
    inFile_1 <- input$file_1
    
    tmp_data <- clean_convo(inFile_1$datapath)
    
    smoothed_frequency_plot(tmp_data, word = input$word_freq, 
                            smooth = input$smooth, point = input$point, 
                            smooth_days = input$smoothing_parameter) + 
      theme_hc(bgcolor = "darkunica") + 
      scale_colour_hc("darkunica") +
      ggtitle("Number of messages containing word by day") +
      theme(text=element_text(size=8))
    
  }, res=120)
  
  
  
  ## Create Word Freq Plot 2
  
  output$word_freq_2 <- renderPlot({
    
    # only show plot if file exists
    if(is.null(input$file_2)){
      hide("word_freq_2")}
    else{
      show("word_freq_2")}
    
    inFile_2 <- input$file_2
    
    tmp_data <- clean_convo(inFile_2$datapath)
    
    smoothed_frequency_plot(tmp_data, word = input$word_freq_2, 
                            smooth = input$smooth_2, point = input$point_2, 
                            smooth_days = input$smoothing_parameter_2) +   
      theme_hc(bgcolor = "darkunica") + 
      scale_colour_hc("darkunica") +
      ggtitle("Number of messages containing word by day") +
      theme(text=element_text(size=8))
    
  }, res=120)  
  
  
  
  ## Create conversation share 1 
  
  output$convo_share_1 <- renderPlot({
    
    # only show plot if file exists
    if(is.null(input$file_1)){
      hide("convo_share_1")}
    else{
      show("convo_share_1")}
    
    inFile_1 <- input$file_1
    
    tmp_data <- clean_convo(inFile_1$datapath) 
    
    smoothed_share(tmp_data, smooth_days = input$smoothing_parameter) +   
      theme_hc(bgcolor = "darkunica") + 
      scale_colour_hc("darkunica") +
      scale_fill_hc("darkunica") +
      ggtitle("Number of messages containing word by day") +
      theme(text=element_text(size=8))
    
  }, res = 120)
  
  ## Create conversation share 2
  
  output$convo_share_2  <- renderPlot({
    
    # only show plot if file exists
    if(is.null(input$file_2)){
      hide("convo_share_2")}
    else{
      show("convo_share_2")}
    
    inFile_2 <- input$file_2
    
    tmp_data <- clean_convo(inFile_2$datapath)
    
    smoothed_share(tmp_data, smooth_days = input$smoothing_parameter_2) +   
      theme_hc(bgcolor = "darkunica") + 
      scale_colour_hc("darkunica") +
      scale_fill_hc("darkunica") +
      ggtitle("Number of messages containing word by day") +
      theme(text=element_text(size=8))
    
  }, res = 120)
  
  
  ## Create search function
  
  output$word_search <- renderDataTable({
    
    # check file exists
    inFile_1 <- input$file_1
    if (is.null(inFile_1))
      return(NULL)
    
    tmp_data <- clean_convo(inFile_1$datapath) 
    
    tmp_table <- text_search(input$word_search, df = tmp_data)
    names(tmp_table) <- c("Sender", "Message")
    
    DT::datatable(tmp_table, style = 'bootstrap', 
                  filter="top", selection="multiple", 
                  escape=FALSE, options = list(sDom  = '<"top">lrt<"bottom">ip'))  
    
    
  })
  
  ## Create context function
  
  output$word_context <- renderDataTable({
    
    # check file exists
    inFile_1 <- input$file_1
    if (is.null(inFile_1))
      return(NULL)
    
    tmp_data <- clean_convo(inFile_1$datapath) 
    
    tmp_table <- conv_search(line = input$line_number, context_line = input$context_lines, df = tmp_data)
    names(tmp_table) <- c("Sender", "Message")
    
    DT::datatable(tmp_table, style = 'bootstrap', 
                  filter="top", selection="multiple", 
                  escape=FALSE, options = list(sDom  = '<"top">lrt<"bottom">ip'))  
    
    
  })
  
}

shinyApp(ui, server)






