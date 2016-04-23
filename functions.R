# http://stackoverflow.com/questions/32136304/conditional-calculation-of-mean
# function calculates the mean only if there are 10 or more respondents to each individual question
f1 <- function(x) if(sum(!is.na(x))>9) mean(as.numeric(x), na.rm=TRUE) else NA_real_

#function calculates number of respondents (not NA)
f2 <- function(x) sum(!is.na(x))

table_course <- function(dataset){
  temp <- as.data.frame(table(dataset$A.5.When.did.you.start.EM.Course._Response))
  names(temp) <- c("Year when respondent started the EMJMD", "Number of respondents to CQSS 2015")
  return(temp)
}
  

questionprint <- function(x, dataset = overall){
  ### function for printing out the likert plot about each individual section of a survey.
  
  ### x = name of the question to be printed
  ### dataset = which dataset should be used to extract data from. Default = overall.

  z <- question_prepare(x, dataset)
  question <- z[[1]]
  name_of_the_question <- z[[2]]
  
  ### checking to see if question has more than 1 dimension with 10 or more respondents to proceed. 
  ### Otherwise it doesn't make sense to calculate Cronbach's alpha and plot
  if (!is.null(dim(question)[2])){
    if (dim(question)[2] > 1) {
      
      ### calculating Cronbach's alpha. If there is an error it won't print out anything
      #try(printing_alpha(x, question))
      
      wrap_function <- wrap_format(85) #wrap-function to print question correctly
      name_of_the_question <- wrap_function(name_of_the_question)
      
      p <- plot_question(question, name_of_the_question)
      return(p)
    }
  }
}

questionprint_grouping <- function(x, dataset = overall, grouping, grouping_levels){
  ### x = name of the question to be printed
  ### dataset = which dataset should be used to extract data from. Default = overall.
  ### grouping = grouping variable to be used to display the results. MUST be the same length as the dataset itself.
  ### grouping_levels = grouping levels to be used to display. Function automatically deletes rows with levels not present in grouping_levels variable.
  
  ### returns nothing, prints the plot directly to the window.
  z <- question_prepare(x, dataset)
  question <- z[[1]]
  name_of_the_question <- z[[2]]
  
  grouping_variable <- factor(grouping, levels = grouping_levels)
  question <- question[complete.cases(grouping_variable), ]
  grouping_variable <- grouping_variable[complete.cases(grouping_variable)]
  questionl <- likert(question, grouping = grouping_variable)
  
  plot(questionl) +
    ggtitle(name_of_the_question)
}

question_prepare <- function(x, dataset){
  # function to prepare a dataset for future use
  # x = name of the question
  # dataset = dataset that needs to be transformed 

  # returns a list with 2 elements:
  # First element is a transformed dataset. All column names are readable. All columns are transformed into respective levels.
  #   Each column has at least 10 answers in them.
  # Second element is the name of the question that will be used as a title for the plot.
  likert_levels <- c("Very unsatisfied", "Somewhat unsatisfied", "Somewhat satisfied", "Very satisfied")
  agree_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")
  
  question <- dataset[, substr(names(dataset), 1, nchar(x)) == x]
  colnames(question) <- gsub("\\.", " ", colnames(question)) #making names of questions readable
  name_of_the_question <- extract_name(x, dataset)
  colnames(question) <- gsub("(.*?)_(.*)", "\\2", colnames(question)) #leaving just the dimension name
  
  
  levels <- likert_levels # default is likert_levels
  if (x == "L.6" || x == "L.5" || x == "L.4")
    levels <- agree_levels # using agree levels only for relevant questions
  
  ### making sure that levels go in order they were meant to go
  for(i in seq_along(question)) {
    ### this step will also reduce all other answers to NA's
    question[,i] <- factor(question[,i], levels = levels)
  }
  
  ### checking to see to have 10 or more answers in each column, otherwise delete it
  question <- question[, colSums(!is.na(question)) >= 10]
  output <- list(question, name_of_the_question)
  return(output)
}

extract_name <- function(x, dataset){
  #extracts name of the question and returns it
  
  ### x = string containing the identifier of the question (e.g. "B.1.3")
  question <- dataset[, substr(names(dataset), 1, nchar(x)) == x]
  name_of_the_question <- gsub("(.*)_(.*)", "\\1", colnames(question)[1]) #storing the name of the section for title of the plot
  name_of_the_question <- substring(name_of_the_question, nchar(x)+1)
  name_of_the_question <- gsub("\\.", " ", name_of_the_question)
  
  return(name_of_the_question)
}

printing_alpha <- function(x, question_dataset, saved = TRUE){
  ### Function takes in name of the quetions, dataset containing information about the question and prints out information about Cronbach alpha
  ### either to a specified folder "Question_statistics" or directly to a prompt. Later can be used to generate reports.
  
  ### x = name of the question
  ### question_dataset = dataset containing information only about given question
  ### saved = logical flag indicating whether to just save the tables in a folder, or print them
  if (sum(complete.cases(question_dataset)) > 0){ # calculating Cronbach's alpha only for cases where complete cases exist
    question_alpha <- psych::alpha(data.matrix(question_dataset))
    question_alpha_head <- xtable(question_alpha$total, caption = sprintf("Statistics for %s question", x), digits = 2)
    question_alpha_drop <- xtable(question_alpha$alpha.drop, caption = sprintf("What if each individual dimension is dropped for %s question", x), digits = 2)
    question_alpha_stats <- xtable(question_alpha$item.stats, caption = sprintf("Summary statistics for %s question", x), digits = 2)
    if(saved){
      print.xtable(question_alpha_head, type = "html", file = sprintf("./Question_statistics/%s_alpha.html", x))
      print.xtable(question_alpha_drop, type = "html", file = sprintf("./Question_statistics/%s_drop.html", x))
      print.xtable(question_alpha_stats, type = "html", file = sprintf("./Question_statistics/%s_stats.html", x))
    } else {
      xtable(question_alpha$item.stats, caption = sprintf("Summary statistics for %s question", x), digits = 2, type = "html")
    }
  }
}


comparative_df <- function(x, course_dataset, dataset){
  ### function to create a dataframe to compare course with all other courses on a given question. Takes in a question and returns a data frame
  ### with summary statistics for both a course and overall for all courses.
  
  ### x = name of the question.
  overall_dataset <- question_prepare(x, dataset)[[1]]
  question_dataset <- question_prepare(x, course_dataset)[[1]]
  
  
  # checking to see if there is any data for that question
  if(dim(question_dataset)[2] == 0)
    return (as.data.frame("Not enough data were collected on this question"))

  #calculating means for a specific course
  means_question <- question_dataset %>% 
    summarise_each(funs(f1, f2)) %>%
    gather(variable, value) %>%
    separate(variable, c("var", "stat"), sep = "\\_f") %>%
    spread(var, value)%>%
    t() %>%
    as.data.frame()
  

  #cleaning the means_question dataset to use it in a table
  names(means_question) <- c("Mean", "Respondents")
  means_question <- means_question[2:nrow(means_question),]
  means_question$Mean <- as.numeric(as.character(means_question$Mean))
  means_question$Respondents <- as.numeric(as.character(means_question$Respondents))
  
  #calculating means for the entire dataset
  means_overall <- overall_dataset %>%
    summarise_each(funs(f1))%>%
    t()%>%
    as.data.frame()
  names(means_overall) <- c("Mean for all courses")
  
  #merging the 
  df <- merge(means_question, means_overall, by = 0, all = TRUE)
  rownames(df) <- df$Row.names
  df$Row.names <- NULL

####################################################################################################
  ### normally function between those two lines will be a separate function means_prepare, but because of NSE in dplyr I don't know how to make it work 
  ### inside of Shiny. So that is something we need to think about.
  #calculating means for each course and each question to create a quantile variable
  overall_dataset$course <- dataset$A.2.name.of.Erasmus.Mundus.master.course.
  
  means_overall_each <- overall_dataset %>%
    group_by(course) %>%
    summarise_each(funs(f1))
  
  
  #storing the names of the courses for future use
  rownames_store <- means_overall_each$course
  means_overall_each$course <- NULL
  
  #logical vector to find out rows with all NA's
  vector <- !!rowSums(!is.na(means_overall_each)) 
  
  means_overall_each <- means_overall_each[vector,] #deleting the rows with all NA's
  rownames(means_overall_each) <- rownames_store[vector] #writing down the names of the courses
  colnames(means_overall_each) <- gsub("\\.", " ", colnames(means_overall_each)) #making names of questions readable
  colnames(means_overall_each) <- gsub("(.*?)_(.*)", "\\2", colnames(means_overall_each)) #leaving just the dimension name
####################################################################################################
  
  ### adding information about quartiles
  quantiles <- as.data.frame(t(apply(X = means_overall_each, FUN = function(x) quantile(x, na.rm = TRUE), MARGIN = 2)))
  quartile_info <- merge(quantiles, means_question, by = 0, all = TRUE)
  rownames(quartile_info) <- quartile_info$Row.names
  quartile_info$Row.names <- NULL
  quartile_info$Mean <- quartile_info$Mean + 0.0001 #just to make sure that lowest means are also recognized by cut
  quartile_info$quartile <- apply(quartile_info, 1, function(x) cut(x[6], x[1:5], include.lowest = TRUE, labels = FALSE))
  quartile_info <- quartile_info["quartile"]
  
  clean_quantiles <- quantiles #copying quantiles to a different data frame to create ranges
  clean_quantiles$first <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[1], x[2])) #creating ranges
  clean_quantiles$second <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[2] + 0.01, x[3])) #+0.01 to create non-overlapping range.
  clean_quantiles$third <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[3] + 0.01, x[4]))
  clean_quantiles$fourth <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[4] + 0.01, x[5]))
  clean_quantiles <- clean_quantiles[c("first", "second", "third", "fourth")] #deleting unnecessary columns

  df <- merge(df, clean_quantiles, by = 0, all = TRUE)
  rownames(df) <- df$Row.names
  df$Row.names <- NULL
  
  df <- merge(df, quartile_info, by = 0, all = TRUE)

  names(df) <- c("Dimension", "Course mean", "n", "EM mean", "0% - 25%", "25% - 50%", "50% - 75%", "75% - 100%", "quartile")
  
  df <- df[complete.cases(df),] #deleting all rows with NA in the mean
  
  #rounding mean and EM mean to 2 decimal places
  df[,2] <- round(df[,2], 2) 
  df[,4] <- round(df[,4], 2)
  
  df$quartile <- NULL
  return(df[,c(1,3,2,4:ncol(df))])
}

means_prepare <- function(x, dataset){
  ### calculates means for the given question. Returns prepared means matrix.
  
  ### x = name of the question.
  ### dataset = overall dataset with data for all courses
  
  overall_dataset <- question_prepare(x, dataset)[[1]]

  #calculating means for each course and each question to create a quantile variable
  overall_dataset$course <- dataset$A.2.name.of.Erasmus.Mundus.master.course.

  means <- overall_dataset %>%
    group_by(course) %>%
    summarise_each(funs(f1))
  
  
  #storing the names of the courses for future use
  rownames_store <- means$course
  means$course <- NULL
  
  #logical vector to find out rows with all NA's
  vector <- !!rowSums(!is.na(means)) 
  
  means <- means[vector,] #deleting the rows with all NA's
  rownames(means) <- rownames_store[vector] #writing down the names of the courses
  colnames(means) <- gsub("\\.", " ", colnames(means)) #making names of questions readable
  colnames(means) <- gsub("(.*?)_(.*)", "\\2", colnames(means)) #leaving just the dimension name
#   wrap_function_x <- wrap_format(35)
#   colnames(means) <- wrap_function_x(colnames(means))
#   wrap_function_y <- wrap_format(50)
#   rownames(means) <- wrap_function_y(rownames(means))
  
  return(list(means, vector))
}

heatmap_printing <- function(means, vector, scaled = TRUE, saved = TRUE){
  ### function to print out a heatmap for any given question. Used in conjuction with means_prepare.
  
  ### means = data frame containing means for any given question. Comes from means_prepare function.
  ### vector = vector that is used to calculate the height and width of the resulting plot.
  ### scaled = logical flag to indicate whether heatmap should be scaled or not. Default is TRUE.
  ### saved = logical flag indicating whether to save or print the graph directly. Default is TRUE.
  
  # different colors depending on scaled/not scaled
  if (scaled){
    mycolors <- brewer.pal(length(c(-Inf,-2:2,Inf)), "BrBG")
  } else {
    mycolors <- brewer.pal(length(seq(1,4, by = 0.5)), "BrBG")
  }
  # scaling the matrix or not
  if (scaled){
    means_matrix <- scale(data.matrix(means))
  } else {
    means_matrix <- data.matrix(means)
  }
  # melted matrix is easier to use with ggplot
  means_matrix_melted <- melt(means_matrix)
  
  
  ### different ways to cut values depending if scaled/not scaled
  if (scaled) {
    means_matrix_melted$value1 <- cut(means_matrix_melted$value, c(-Inf,-2:2,Inf), right = FALSE) 
  } else {
    means_matrix_melted$value1 <- cut(means_matrix_melted$value, seq(1,4, by = 0.5), right = FALSE)
  }
  
  #plotting the heatmap
  p <- ggplot(means_matrix_melted, aes(x = X2, y = X1)) + 
    ggtitle(as.character(questions[i])) +
    geom_tile(aes(fill = value1)) + 
    scale_fill_manual(name = levels(means_matrix_melted$value1), 
                      values = mycolors,
                      na.value = "black", # default color for NA values
                      drop = FALSE) + # not dropping levels with 0 values in them
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),          
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
  
  if (saved){
    if(scaled){
    ggsave(filename = sprintf("./Heatmaps_scaled/%s.png", questions[i]), plot = p, units = "mm", width = 250, height = (70 + sum(vector)*4))
    } else { 
      ggsave(filename = sprintf("./Heatmaps/%s.png", questions[i]), plot = p, units = "mm", width = 250, height = (70 + sum(vector)*4))
    }
  } else {print(p)}
}

report_question <- function(question, course_dataset){
  ## Function to print out the question in the individual report.
  
  ## question = string, containing the question to be printed.
  ## course_dataset = course dataset to produce graphs and tables.
  
  switch(question,
         B.1.1 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.1.3 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.2.1 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.2.2 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         C.1 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.2.a = {
           first_heading <- '##Internship experience.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.3.a = {
           first_heading <- '##Field experience.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.4 = {
           first_heading <- '##First supervisor.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.5 = {
           first_heading <- '##Second supervisor.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.6 = {
           first_heading <- '##Personal development.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.1.1 = {
           first_heading <- '###First university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.1.3 = {
           first_heading <- '###First university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.2.1 = {
           first_heading <- '###First university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.2.2 = {
           first_heading <- '###First university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.3.1 = {
           first_heading <- '###First university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.4.1 = {
           first_heading <- '###First university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.1.1 = {
           first_heading <- '###Second university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.1.3 = {
           first_heading <- '###Second university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.2.1 = {
           first_heading <- '###Second university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.2.2 = {
           first_heading <- '###Second university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.3.1 = {
           first_heading <- '###Second university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.4.1 = {
           first_heading <- '###Second university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.1.1 = {
           first_heading <- '###Third university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.1.3 = {
           first_heading <- '###Third university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.2.1 = {
           first_heading <- '###Third university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.2.2 = {
           first_heading <- '###Third university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.3.1 = {
           first_heading <- '###Third university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.4.1 = {
           first_heading <- '###Third university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.1.1 = {
           first_heading <- '###Fourth university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.1.3 = {
           first_heading <- '###Fourth university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.2.1 = {
           first_heading <- '###Fourth university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.2.2 = {
           first_heading <- '###Fourth university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.3.1 = {
           first_heading <- '###Fourth university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.4.1 = {
           first_heading <- '###Fourth university.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         }
  )
  
  #logical flag to see if there is any data on a given question.
  #It will evaluate to TRUE if there is any error or will return the value otherwise.
  try_flag <- tryCatch(comparative_df(question, course_dataset), error = function(err) return(TRUE)) 
  
  if(!is.logical(try_flag)){ #checking if try_flag is logical. If it is, then do nothing. Otherwise print out the information about the question.
    temp <- sprintf("\n%s%s%s\n\n", first_heading, "Question:", question)
    cat(temp)
    #cat(intro_text)
    cat("\n")
    
    #prtinting out the question
    questionprint(question, dataset = course_dataset, save = FALSE)
    
    #cat(graph_text)
    cat("\n")
    
    not_print <- c("N", "O", "P", "Q") #not printing comparative tables for questions on specific university
    
    if (!(substr(question, 1, 1) %in% not_print)) {  
      #preparing and printing table
      df <- comparative_df(question, course_dataset)
      z <- xtable(df, caption = sprintf("Summary statistics"), digits = c(0,0,2,2,2,2,2,2), type = "html")
      align(z) <- "|p{5cm}|cc|c|cccc|"
      print(z, NA.string = "NA", sanitize.text.function = function(x) x)
    }  
    
    cat("\n")
    #cat(table_text)
  }
}

means_printing <- function(x){
  ### function to print out a boxplot for any given question. Used in conjuction with means_prepare.
  
  # getting the means table for a given question
  means <- means_prepare(x)[[1]]
  # melting the means table to use it in plotting
  melted_means <- as.data.frame(melt(as.matrix(means)))

  #getting name of the question
  name_of_the_question <- extract_name(x)
  #wrap statement to have new lines after a given character
  wrap_function_title <- wrap_format(80)
  name_of_the_question <- wrap_function_title(name_of_the_question)
  #same for dimension names
  wrap_function_label <- wrap_format(40)

  #plotting
  p <- ggplot(melted_means, aes(y = value, x = X2)) + 
    ggtitle(name_of_the_question) +
    geom_boxplot() + 
    coord_flip() + 
    ylab("Distribution of means") +
    #http://stackoverflow.com/questions/21878974/auto-wrapping-of-labels-via-labeller-label-wrap-in-ggplot2
    scale_x_discrete(labels = function(x) wrap_function_label(x))+
    theme(axis.title.y=element_blank())

  #saving the resulting graph
  ggsave(filename = sprintf("./Mean_plots/%s.png", x), plot = p, units = "mm", width = 250, height = (70 + length(levels(factor(melted_means$X2)))*10))
}

figure_height <- function(question, course_dataset){
  # function to determine the height of the figure for a given question. Returns height if there is something to return, returns 0 
  # if comparative_df returns error. Function is needed since otherwise it would be impossible to determine the height and make sure
  # that script runs properly when there is no data to print out
  question_dataset <- question_prepare(question, course_dataset)[[1]]
  
  #calculating means for a specific course
  if(ncol(question_dataset) > 1) {  
    means_question <- question_dataset %>% 
      summarise_each(funs(f1, n())) %>%
      gather(variable, value) %>%
      separate(variable, c("var", "stat"), sep = "\\_") %>%
      spread(var, value)%>%
      t() %>%
      as.data.frame()
    
    means_question <- means_question[-1,]
    
    means_question <- means_question[complete.cases(means_question),]
    
    if(nrow(means_question) > 1){ #checking if try_flag is logical. If it is, then do nothing. Otherwise print out the information about the question.
       return (50+50*nrow(means_question))
    } 
  }
  else return (1) #return 1 height in case there is nothing to print
}
  
plot_question <- function(question, name_of_the_question){
  #function to create a graph of a question
  #question - dataset containing prepared question data
  #name_of_the_question - string, containing the name of the question to be used in title
  
  questionl <- likert(question) #creating likert-type variable for plotting
  ### printing out the file
  p <- plot(questionl, 
            plot.percents = TRUE, # displaying percents for each answer
            plot.percent.low = FALSE,  # displaying cummulative percents for negative answers
            plot.percent.high = FALSE, # displaying cummulative percents for positive answers
            centered = FALSE, # stretcthing the bar from left to right
            wrap = 40, # wrap statement for dimension names
            legend.position = "top",
            legend = NULL,
            group.order = sort(names(question))) + 
    ggtitle(name_of_the_question) + # title of the question
    theme(text = element_text(size = 14, family = "Open Sans"), # setting the text size of the plot
          plot.margin = unit(c(0.2, 0.8, 0.3, 0), "lines"), # decreasing white space around the plot
          legend.margin = unit(0, "lines"), # deleting space around legend
          legend.key.size = unit(0.5, "lines"), # decreasing size of legend elements
          legend.background = element_rect(colour = "gray", fill = NA, size = 0.1), # adding a frame around the legend
          axis.title.x=element_blank(), #deleting x-label 
          plot.title = element_text(size = 14)) + #size of the text in the title
    geom_hline(yintercept=seq(25, 75, by=25), linetype = "dashed", size = 0.2) + # adding dashed lines at 25, 50, 75% to make it more clear
    coord_fixed() +
    coord_flip(ylim = c(4,96)) #reducing white space left to 0 and right to 100
  return(p)
}