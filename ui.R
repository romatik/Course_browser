library(dplyr)
library(plyr)
library(likert)
library(scales)
library(reshape)
library(grid)
library(RColorBrewer)
library(tidyr)
library(classInt)
library(RColorBrewer)
library(stringr)
library(shiny)

source("functions.R")
dataset <- read.csv("./data/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
dataset$X <- NULL
dataset$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL

likert_levels <- c("Very unsatisfied", "Somewhat unsatisfied", "Somewhat satisfied", "Very satisfied")
agree_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")

### questions that need to be printed out
questions <- c('B.1.1', 'B.1.3', 'B.2.1', 'B.2.2', 'C.1', #overall program satisfaction
               "L.4", "L.5", "L.6", 'L.3.a', 'L.2.a', #internship/field experience
               "N.1.1", "N.1.3", "N.2.1", "N.2.2", "N.3.1", "N.4.1", #satisfaction in first university
               "O.1.1", "O.1.3", "O.2.1", "O.2.2", "O.3.1", "O.4.1", #satisfaction in second university
               "P.1.1", "P.1.3", "P.2.1", "P.2.2", "P.3.1", "P.4.1", #satisfaction in third university
               "Q.1.1", "Q.1.3", "Q.2.1", "Q.2.2", "Q.3.1", "Q.4.1") #satisfaction in fourth university

### finding out courses with 10 or more respondents in the dataset
tenormore <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  dplyr::summarise(respondents = n()) %>%
  filter(respondents >= 10)
colnames(tenormore) <- c("Course", "Respondents")

shinyUI(fluidPage(

  # Application title
  titlePanel("Course Browser"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("course", "Course:", 
                  choices=unique(as.character(tenormore$Course))),
      hr(),
      checkboxGroupInput("checkGroup", label = h3("Questions"), 
                         c("Question 1: Info and support before the start" = "B.1.1",
                           "Question 2: Introduction process" = "B.1.3",
                           "Question 3: Helpfulness of units/people" = "B.2.1",
                           "Question 4: Support on issues" = "B.2.2",
                           "Question 5: Feedback mechanisms" = "C.1",
                           "Question 6: First supervisor" = "L.4",
                           "Question 7: Second supervisor" = "L.5",
                           "Question 8: Personal development" = "L.6",
                           "Question 9: Field experience" = "L.3.a",
                           "Question 10: Internship experience" = "L.2.a"),
                         selected = questions[1]),
      actionButton("go", "Submit")
    ),
    
    mainPanel(h2(textOutput("course_name"), align = "center"),
              hr(),
              uiOutput("course_plots"),
              h6(textOutput("created_by"))
    )
  )
))