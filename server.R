## devtools::install_github('rstudio/DT')
pkgs <-c('shiny','dplyr','ggplot2','stringr','tidyr','xlsx','plotly')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm('p','pkgs')
pdf(NULL)
source("helper.R")

shinyServer(function(input, output, session) {
  observeEvent(input$submitvote, {
    info <- posterInfo(input$posterid)
    if(info[1] != "" & info[2] != ""){
      print(info)
      if(!is.na(input$total)){
        vote <- data.frame(as.character(info[1]),
                           as.character(info[2]),
                           as.character(input$judgeid),
                           as.integer(input$total),
                           as.numeric(input$best))
        names(vote) <- fields
        if(vote$total > 12)
          vote$total <- 0
        saveData(vote)  
      }
    }
    updateTextInput(session, "posterid", value = "")
    updateNumericInput(session, "total", value = "")
    updateCheckboxInput(session, "best", value = "")
  })
  
  observeEvent(input$submitgsm, {
    info1 <- posterInfo(input$gsm1)
    info2 <- posterInfo(input$gsm2)
    info3 <- posterInfo(input$gsm3)
    df <- data.frame(input$judgeid, data.frame(info1,info2,info3) %>% t(), fix.empty.names = FALSE)
    names(df) <- c("judgeid","Category","ID")
    # print(subset(df,!is.na(df$Category) & !is.na(df$ID)))
    saveGSM(subset(df,!is.na(df$Category) & !is.na(df$ID)))
    updateTextInput(session,"gsm1",value = "")
    updateTextInput(session,"gsm2",value = "")
    updateTextInput(session,"gsm3",value = "")
  })
  
  observeEvent(input$file1, {
    if(is.null(input$file1)) {
      print("FILE IS NULL")
      return(NULL)
    }

    ext <- tools::file_ext(input$file1$name)

    if(paste(ext) != ".lsx"){
      print("THAT IS NOT AN XLSX")
      print(ext)
      return(NULL)
    }
    
    file.rename(input$file1$datapath,
                paste(input$file1$datapath, ".xlsx", sep=""))
    
    posters <- read.xlsx(paste(input$file1$datapath, ".xlsx", sep=""),sheetName = "Program")
    names(posters) <- c("ID","author","title")
    
    posters.df <- separate(posters, col = "ID", 
                            into = c("Category","ID"),
                            sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
    mutate(Category = factor(Category), ID = factor(ID))
    
    # print(posters.df)
    
    savePosterInfo(posters.df)
  })
  
  observeEvent(input$submitpepc, {
    vote1 <- posterInfo(input$pep1)
    vote2 <- posterInfo(input$pep2)
    vote3 <- posterInfo(input$pep3)
    df <- data.frame(vote1,vote2,vote3, fix.empty.names = FALSE) %>% t() %>% data.frame()
    names(df) <- c("Category","ID")
    # print(df)
    savePeoplesChoice(subset(df,!is.na(df$Category) & !is.na(df$ID)))
    updateTextInput(session,"pep1",value = "")
    updateTextInput(session,"pep2",value = "")
    updateTextInput(session,"pep3",value = "")
  })

  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })    
  
  autoInvalidate <- reactiveTimer(5000, session)
  
  output$plotG <- renderPlot({
    autoInvalidate()
    plotData("GF")
  })
  
  output$winnersG <- renderUI({
    autoInvalidate()
    winners <- getWinners("GF")
    tagList(
      h3(paste("3rd place goes to: ", winners[3,]$author)),
      h3(paste("For their poster titled: ", winners[3,]$title)),
      
      h3(paste("2nd place goes to: ", winners[2,]$author)),
      h3(paste("For their poster titled: ", winners[2,]$title)),
      
      h2(paste("1st place goes to: ", winners[1,]$author)),
      h2(paste("For their poster titled: ", winners[1,]$title))
    )
  })
  
  output$plotU <- renderPlot({
    autoInvalidate()
    plotData("UF")
  })
  
  output$winnersU <- renderUI({
    autoInvalidate()
    winners <- getWinners("UF")
    tagList(
      h3(paste("3rd place goes to: ", winners[3,]$author)),
      h3(paste("For their poster titled: ", winners[3,]$title)),

      h3(paste("2nd place goes to: ", winners[2,]$author)),
      h3(paste("For their poster titled: ", winners[2,]$title)),

      h2(paste("1st place goes to: ", winners[1,]$author)),
      h2(paste("For their poster titled: ", winners[1,]$title))
    )
  })
  
  output$plotS <- renderPlot({ 
    autoInvalidate()
    plotData("S")
  })
  
  output$winnersS <- renderUI({
    autoInvalidate()
    winners <- getWinners("S")
    tagList(
      h3(paste("3rd place goes to: ", winners[3,]$author)),
      h3(paste("For their poster titled: ", winners[3,]$title)),
      
      h3(paste("2nd place goes to: ", winners[2,]$author)),
      h3(paste("For their poster titled: ", winners[2,]$title)),
      
      h2(paste("1st place goes to: ", winners[1,]$author)),
      h2(paste("For their poster titled: ", winners[1,]$title))
    )
  })
  
  output$plotGSM <- renderPlot({
    autoInvalidate()
    plotData("GSM")
  })
  
  output$plotPEPC <- renderPlotly({
    autoInvalidate()
    plotlyData("PEOPLESCHOICE")
  })
  
  output$winersPep <- renderUI({
    autoInvalidate()
    winners <- getWinners("PEOPLESCHOICE")
    tagList(
      fluidRow(
        column(12,
          column(4,
                 h4(paste("Graduate/Faculty Peoples Choice 1st place, with a total of", winners[1,]$score,"votes")),
                 h4(paste("goes to: ", winners[1,]$author)),
                 h4(paste("For their poster titled: ", winners[1,]$title))
          ),
          column(4,
                 h4(paste("Undergraduate/Faculty Peoples Choice 1st place, with a total of", winners[2,]$score,"votes")),
                 h4(paste("goes to: ", winners[2,]$author)),
                 h4(paste("For their poster titled: ", winners[2,]$title))
          ),
          column(4,
                 h4(paste("Student Class Project Posters 1st pace with a total of", winners[3,]$score,"votes")),
                 h4(paste("goes to: ", winners[3,]$author)),
                 h4(paste("For their poster titled: ", winners[3,]$title))
          )
        )
      )
            
      
    )
  })
  
  output$winnersGSM <- renderUI({
    autoInvalidate()
    winners <- getWinners("GSM")
    tagList(
      h2(paste("1st place, with a total of", winners[1,]$score,"votes")),
      h2(paste("goes to: ", winners[1,]$author)),
      h2(paste("For their poster titled: ", winners[1,]$title)),
      
      h3(paste("2nd place, with a total of", winners[2,]$score,"votes")),
      h3(paste("goes to: ", winners[2,]$author)),
      h3(paste("For their poster titled: ", winners[2,]$title)),
      
      h3(paste("3rd place, with a total of", winners[3,]$score,"votes")),
      h3(paste("goes to: ", winners[3,]$author)),
      h3(paste("For their poster titled: ", winners[3,]$title))
    )
  })
  
  output$judgeResponses <- downloadHandler(
    filename = function() { paste('responses','.csv', sep='') },
    content = function(file) {
      write.csv(responses, file, row.names = FALSE)
    }
  )
  
  output$GSMVotes <- downloadHandler(
    filename = function() { paste('GSM','.csv', sep='') },
    content = function(file) {
      write.csv(GSM, file, row.names = FALSE)
    }
  )
  
  output$peoplesChoice <- downloadHandler(
    filename = function() { paste('peoplesChoice','.csv', sep='') },
    content = function(file) {
      write.csv(peoplesCho, file, row.names = FALSE)
    }
  )
  
})
