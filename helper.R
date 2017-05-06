fields <- c("Category","ID","JudgeID","total","best")

files <- list.files("~/CNSApp")
csv.pattern <- ".+(\\.csv)$"
csv.files <- grep(csv.pattern,files,value = TRUE)

if(length(csv.files) != 0){
  posters.csv <- grep("posterinfo",csv.files, value = TRUE)
  poster.in <- read.csv(posters.csv)
  poster.in$ID <- factor(poster.in$ID)
  posters.df <<- poster.in
}


posterInfo <- function(poster){
  poster <- str_replace_all(poster, "[ ]", "")
  category <- toupper(str_extract_all(poster,"[a-zA-Z]+"))
  if(category == "G")
    category = "GF"
  if(category == "U")
    category = "UF"
  posterno <- str_extract(poster,"[0-9]+")
  if(category == "CHARACTER(0)")
    category <- NA
  return(c(category,posterno))
}

saveData <- function(data) {
  data <- as.data.frame(data)
  if (exists("responses")) {
    ## Validate if judge exists ?
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

saveGSM <- function(votes){
  if(exists("GSM")){
    GSM <<- rbind(GSM,votes)
  } else {
    GSM <<- votes
  }
}

savePeoplesChoice <- function(votes) {
  if(exists("PEOPLESCHOICE")){
    PEOPLESCHOICE <<- rbind(PEOPLESCHOICE,votes)
  } else {
    PEOPLESCHOICE <<- votes
  }
}

savePosterInfo <- function(posters){
  files <- list.files("~/CNSApp")
  # files
  csv.pattern <- ".+(\\.csv)$"
  csv.files <- grep(csv.pattern,files,value = TRUE)
  # csv.files
  if(length(csv.fies) != 0){
    posters.df <<- posters
    write.csv(posters.df, "~/CNSApp/posterinfo.csv", row.names = FALSE)
  }
  else{
    poster.in <- read.csv(csv.files)
    posters.df <<- posters
    if(all.equal(poster.in,posters.df)){
      write.csv(posters.df, "~/CNSApp/posterinfo.csv", row.names = FALSE)
    }
    else{
      write.csv(posters.df, "~/CNSApp/posterinfo.csv", row.names = FALSE)
    }  
  }
}

getpalette <- function
(input){
  if(input == "GF")
    return("Reds")
  if(input == "UF")
    return("Greens")
  if(input == "S")
    return("Blues")
  if(input == "GSM")
    return("Oranges")
  return("Set3")
}

plotData <- function(category){
  if(category == "GF" | category == "S" | category == "UF"){
    if(exists("responses")){
      if(length(which(responses[,"Category"] == category)) == 0) {
        return(NULL)
      }
      return(responses %>% 
               filter(Category == category) %>% 
               group_by(ID) %>% mutate(score = sum(total,best)) %>%
               arrange(desc(score)) %>% head(n=5) %>%  
               ggplot(aes(x = reorder(paste(Category,ID,sep=""),desc(score)), y = score, fill = ID)) +
               scale_fill_brewer(palette = paste(getpalette(category))) +
               geom_bar(stat="identity") +
               geom_text(aes(label = score), vjust = 0) + 
               theme(legend.position = "none", 
                     axis.ticks = element_blank(), 
                     panel.background = element_blank()) +
               labs(x = "Poster ID", y = "Total Score")
             )
    }  
  }
  if(category == "GSM"){
    if(exists("GSM")){
      return(GSM %>% 
               group_by("ID" = paste(Category,ID,sep="")) %>% 
               count(sort = TRUE) %>% 
               data.frame() %>% head(n = 5) %>%
               
               ggplot(aes(x = reorder(ID,desc(n)), y = n, fill = ID)) + 
               scale_fill_brewer(palette = paste(getpalette("GSM"))) +
               geom_bar(stat="identity") + 
               geom_text(aes(label = n), vjust = 0) +
               theme(legend.position = "none",
                     axis.ticks = element_blank(),
                     panel.background = element_blank()) +
               labs(x = "Poster ID", y = "Total Votes"))
    }   
  }
  if(category == "PEOPLESCHOICE"){
    if(exists("PEOPLESCHOICE")){
      return(PEOPLESCHOICE %>% 
               group_by(Category, "ID" = paste(Category,ID,sep="")) %>% 
               tally() %>% 
               top_n(n=2) %>%
               
               ggplot(aes(x = reorder(ID,desc(n)), y = n, 
                          fill = 
                            c("1","2","3")[as.numeric(Category)])) +
               scale_fill_manual(values = c("red","blue","green")) +
               geom_bar(stat="identity") + 
               geom_text(aes(label = n), vjust = 0) +
               theme(legend.position = "none",
                     axis.ticks = element_blank(),
                     panel.background = element_blank()) +
               labs(x = "Poster ID", y = "Total Votes")
             )
    }
  }
}

plotlyData <- function(category){
  if(category == "PEOPLESCHOICE"){
    if(exists("PEOPLESCHOICE") & exists("posters.df")){
      
      suppressWarnings(
        suppressMessages(
          df <- PEOPLESCHOICE %>% 
        group_by(Category, "NAME" = paste(Category,ID,sep="")) %>% 
        tally() %>% 
        top_n(n=2) %>% data.frame() %>% ungroup() %>%
        separate(col = "NAME", into = c("Original C","ID"),
                 sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
        left_join(posters.df)
        )
      )
      
      gg <- df %>% ggplot(aes(x = reorder(paste(Category,ID,sep=""),desc(n)), 
                              y = n,
                              fill = Category,
                              fill.title = title,
                              fill.author = author)) +
               geom_bar(stat="identity") + 
               scale_fill_brewer(palette = paste(getpalette(category))) +
               geom_text(aes(label = n), vjust = 0) +
               theme(legend.position = "none",
                     axis.ticks = element_blank(),
                     panel.background = element_blank()) +
               labs(x = "Poster ID", y = "Total Votes")
      ggplotly(gg, tooltip=c("author","title"))
      
    }
  }
}

getWinners <- function(category){
  if(exists("posters.df")){
    if(category == "GF" | category == "S" | category == "UF"){
      if(exists("responses")){
        suppressWarnings(
          suppressMessages(
            winners <- responses %>% 
              filter(Category == category) %>% 
              left_join(posters.df) %>% rowwise() %>%
              mutate(score = sum(total,best)) %>% 
              arrange(desc(score)) %>% 
              data.frame()
          )
        )
        to.return <- data.frame("author" = rep("NA",3),"title" = rep("NA",3),  "score" = rep(NA,3), stringsAsFactors = FALSE)
        num.posters <- nrow(winners)
        if(num.posters >= 1) {
          to.return[1,]$author <- as.character(winners[1,]$author)
          to.return[1,]$title <- as.character(winners[1,]$title)
          to.return[1,]$score <- as.integer(winners[1,]$score)
        }
        if(num.posters >= 2){
          to.return[2,]$author <- as.character(winners[2,]$author)
          to.return[2,]$title <- as.character(winners[2,]$title)
          to.return[2,]$score <- as.integer(winners[2,]$score)
        }
        if(num.posters >= 3){
          to.return[3,]$author <- as.character(winners[3,]$author)
          to.return[3,]$title <- as.character(winners[3,]$title)
          to.return[3,]$score <- as.integer(winners[3,]$score)
        }
        return(to.return)
      }
    }
    if(category == "PEOPLESCHOICE"){
      if(exists("PEOPLESCHOICE") & exists("posters.df")){
        suppressWarnings(
          suppressMessages(
            winners <- PEOPLESCHOICE %>% 
              group_by(Category, "NAME" = paste(Category,ID,sep="")) %>% 
              tally() %>% 
              slice(which.max(n)) %>% data.frame() %>% ungroup() %>%
              separate(col = "NAME", into = c("Original C","ID"),
                       sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
              left_join(posters.df)
          )
        )
        to.return <- data.frame("author" = rep("NA",3),"title" = rep("NA",3),  "score" = rep(NA,3), stringsAsFactors = FALSE)
        num.posters <- nrow(winners)
        if(num.posters >= 1) {
          to.return[1,]$author <- as.character(winners[1,]$author)
          to.return[1,]$title <- as.character(winners[1,]$title)
          to.return[1,]$score <- as.integer(winners[1,]$n)
        }
        if(num.posters >= 2){
          to.return[2,]$author <- as.character(winners[2,]$author)
          to.return[2,]$title <- as.character(winners[2,]$title)
          to.return[2,]$score <- as.integer(winners[2,]$n)
        }
        if(num.posters >= 3){
          to.return[3,]$author <- as.character(winners[3,]$author)
          to.return[3,]$title <- as.character(winners[3,]$title)
          to.return[3,]$score <- as.integer(winners[3,]$n)
        }
        return(subset(to.return,!is.na(to.return$author) & !is.na(to.return$author) & !is.na(to.return$score)))
      }
    }
    if(category == "GSM"){
      if(exists("GSM")){
        suppressWarnings(
          suppressMessages(
            winners <- GSM %>% 
              group_by("ID" = paste(Category,ID,sep="")) %>%
              count(sort = TRUE) %>% 
              data.frame() %>% head(n = 3) %>% 
              separate(col = "ID", into = c("Category","ID"), 
                       sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
              left_join(posters.df)
          )
        )
        to.return <- data.frame("author" = rep("NA",3),"title" = rep("NA",3),  "score" = rep(NA,3), stringsAsFactors = FALSE)
        num.posters <- nrow(winners)
        if(num.posters >= 1) {
          to.return[1,]$author <- as.character(winners[1,]$author)
          to.return[1,]$title <- as.character(winners[1,]$title)
          to.return[1,]$score <- as.integer(winners[1,]$n)
        }
        if(num.posters >= 2){
          to.return[2,]$author <- as.character(winners[2,]$author)
          to.return[2,]$title <- as.character(winners[2,]$title)
          to.return[2,]$score <- as.integer(winners[2,]$n)
        }
        if(num.posters >= 3){
          to.return[3,]$author <- as.character(winners[3,]$author)
          to.return[3,]$title <- as.character(winners[3,]$title)
          to.return[3,]$score <- as.integer(winners[3,]$n)
        }
        return(subset(to.return,!is.na(to.return$author) & !is.na(to.return$author) & !is.na(to.return$score)))
      }
    }
  }
}
