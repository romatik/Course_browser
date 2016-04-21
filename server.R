dataset <- read.csv("./data/course_browser_anonymized.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
dataset$X <- NULL
dataset$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL
dataset$L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience_2 <- NULL

questions <- c('B.1.1', 'B.1.3', 'B.2.1', 'B.2.2', 'C.1', #overall program satisfaction
               "L.4", "L.5", "L.6", 'L.3.a', 'L.2.a') #internship/field experience
likert_levels <- c("Very unsatisfied", "Somewhat unsatisfied", "Somewhat satisfied", "Very satisfied")
agree_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")
load("./data/example.dat")

shinyServer(function(input, output, session) {

  ### values change only after user presses "Submit" button
  datasetInput <- eventReactive(input$go, {
    dataset[dataset$A.2.name.of.Erasmus.Mundus.master.course. == input$course,] 
  })

  erasmusInput <- eventReactive(input$go,{
    dataset
  })
  
  questionInput <- eventReactive(input$go, {
    input$checkGroup
  })
  
  courseInput <- eventReactive(input$go, { 
    paste(input$course)
  })
  
  output$students <- renderDataTable(table_course(datasetInput()), 
                                     options = list(dom = 't', searching = FALSE,
                                                    columnDefs = list(list(width = "190px", targets = "_all")),
                                                    align = "center"
                                                    )
                                     )
  
  ### output values to be sent to user after the button "Submit" was pressed
  output$course_name <- renderText({courseInput()})
  
  output$example <- renderDataTable(z, options = list(dom = 't', autoWidth = TRUE, searching = FALSE,
                                                      columnDefs = list(list(width = "190px", targets = c(0)))))
  
  output$course_plots <- renderUI({
    plot_output_list <- 
      lapply(questionInput(),
             function(i){
               tags$div(class = "group-output",
                textOutput(paste0("question", i), container = h3),
                plotOutput(paste0("plot", i), height = "100%"),
                br(),
                dataTableOutput(paste0("tablename", i)),
                hr())
             })
    do.call(tagList, plot_output_list)
  })
  
  
  observeEvent(input$go, {
    for (i in input$checkGroup) {
      local({
        local_i <- i
        output[[paste0("question", local_i)]] <-
          renderText({
            switch(local_i,
                   B.1.1 = {text <- "Question 1: Info and support before the start"},
                   B.1.3 = {text <- "Question 2: Introduction process"},
                   B.2.1 = {text <- "Question 3: Helpfulness of units/people"},
                   B.2.2 = {text <- "Question 4: Support on issues"},
                   C.1 =   {text <- "Question 5: Feedback mechanisms"},
                   L.4 =   {text <- "Question 6: First supervisor"},
                   L.5 =   {text <- "Question 7: Second supervisor"},
                   L.6 =   {text <- "Question 8: Personal development"},
                   L.3.a = {text <- "Question 9: Field experience"},
                   L.2.a = {text <- "Question 10: Internship experience"}
            )
            paste0(text)
          })
        
        output[[paste0("plot", local_i)]] <- 
          renderPlot({
            questionprint(local_i, datasetInput())
          }, height = figure_height(local_i, datasetInput()))
        
        output[[paste0("tablename", local_i)]] <-
          renderDataTable(comparative_df(local_i, datasetInput(), dataset), 
                          options = list(dom = 't', autoWidth = TRUE, searching = FALSE,
                                         columnDefs = list(list(width = "190px", targets = c(0)))))
      })
    }
  })
  
  #http://stackoverflow.com/questions/34315485/linking-to-a-tab-or-panel-of-a-shiny-app
  observeEvent(input$link_to_programs, {
    newvalue <- "programs"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_acknowledgments, {
    newvalue <- "acknowledgments"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_cqss, {
    newvalue <- "cqss"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_cqab, {
    newvalue <- "cqab"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_ema, {
    newvalue <- "ema"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_faq, {
    newvalue <- "faq"
    updateTabsetPanel(session, "panels", newvalue)
  })

})