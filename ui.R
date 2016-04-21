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
dataset <- read.csv("./data/course_browser_anonymized.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
dataset$X <- NULL
dataset$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL
dataset$L.2.a.Rate.the.following.statements.about.internship._Overall.quality.of.the.internship_2 <- NULL
dataset$L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience_2 <- NULL

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

shinyUI(
  navbarPage(title = "Course Browser", id = "panels",
             footer = h6("Created by Mikhail Balyasin, head of Quantitative team of CQAB of EMA |", 
                         a(" github", href = "https://github.com/romatik/Course_browser"), 
                         align = "center"),
             header = tags$style(HTML("
                                      @import url(https://fonts.googleapis.com/css?family=Open+Sans);
                                      
                                      h3 {
                                      font-size: 28px;
                                      }

                                      /* latin */
                                      body {
                                      font-family: 'Open Sans';
                                      font-style: normal;
                                      font-weight: 400;
                                      font-size: 16px;
                                      unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215, U+E0FF, U+EFFD, U+F000;
                                      }
                        
                                      /* changing appearance of submit button */
                                      #go{background-color:rgb(0,78,134);
                                          color:white;
                                          vertical-align: middle; 
                                          height: 50px; 
                                          width: 100px; 
                                          font-size: 20px;
                                      }
                                        
                                      /* tables */
                                      TD {font-size:12px}
                                      TH {font-size:12px}

                                      /* removing underlining for all links on hover */
                                      a:focus, a:hover{
                                          text-decoration: none;
                                      }
                                      ")),
             
  tabPanel("Home", value = "home",
           fluidRow(
             column(1),
             column(10,
                    h3("Welcome"),
                    p(img(src = "EMA_large.png", align = "right", width = "30%"),
                      "This web-site hosts an online version of the data that", 
                      actionLink("link_to_cqab", "Course Quality Advisory Board (CQAB)"),
                      "of the",
                      actionLink("link_to_ema", "Erasmus Mundus Student and Alumni Association (EMA)"),
                      "collected in 2015 through Course Quality of Students Services (CQSS) survey. It's main goal is to give
                      access to everyone in a most transparent and convenient way. Course browser, that you can find at the tab ",
                      actionLink("link_to_programs", "CQSS 2015"),
                      ", gives access to information about 78 Erasmus Mundus programs that received 10 or more responses from
                      students of their respective programs. We hope that you find that information useful and enlightening."),
                    p("CQAB is always eager to receive further feedback from stakeholders on how future CQSS 
                      products and their contents may be improved in order to maximize their usefulness. 
                      We understand that courses themselves are best suited to address quality concerns, 
                      and we strongly suggest that present information, with its limitations, is triangulated 
                      with other available data for each course."),
                    p("If you want to learn more about people behind this report you can visit tab ", 
                      actionLink("link_to_acknowledgments", "\"Acknowledgments\".")),
                    p("To learn more about CQSS and what kind of data we've collected click ", 
                      actionLink("link_to_cqss", "here"), 
                      " or click on \"About... -> CQSS\" at the top menu",
                      ". If you have questions about the tool, you can visit FAQ ",
                      actionLink("link_to_faq", "here.")),
                    p("If you still have questions about any information presented at this page, you can always
                      contact us at cqab.chair@em-a.eu. We are looking forward to your feedback!"),
                    fluidRow(
                      column(12,
                             selectInput("course", label = h3("1. Choose course:"), 
                                         choices=unique(as.character(tenormore$Course)), 
                                         width = "54%")
                      )
                    ),
                    fluidRow(
                      column(12,
                             checkboxGroupInput("checkGroup", label = h3("2. Choose questions:"), 
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
                                                selected = c("B.1.1", "B.1.3", "B.2.1", "B.2.2", "C.1", "L.4", "L.5", "L.6", "L.3.a", "L.2.a"),
                                                width = "100%")
                      )
                    ),
                    fluidRow(
                      column(6, align = "center", offset = 3,
                             actionButton("go", "Submit", align = "center")
                             )
                    ),
                    fluidRow(h2(textOutput("course_name"), align = "center"),
                             hr(),
                             br(),
                             dataTableOutput("students"),
                             hr(),
                             uiOutput("course_plots"))
                    ),
             column(1)
           )
  ),
  
  
  tabPanel("FAQ", value = "faq",
           fluidRow(
             column(1),
             column(10, 
                    includeHTML("faq.Rmd")),
             column(1)
           )
  ),
  
  navbarMenu("About...",
             tabPanel("CQSS", value = "cqss",
                      fluidRow(
                        column(1),
                        column(10, 
                               includeMarkdown("about_cqss_before.Rmd"),
                               dataTableOutput("example"),
                               includeMarkdown("about_cqss_after_table.Rmd")
                               ),
                        column(1)
                      )
             ),
             tabPanel("CQAB", value = "cqab",
                      fluidRow(
                        column(1),
                        column(10, 
                               includeMarkdown("about_cqab.Rmd")),
                        column(1)
                      )
             ),
             tabPanel("EMA", value = "ema",
                       fluidRow(
                         column(1),
                         column(10, 
                                includeMarkdown("about_ema.Rmd")),
                         column(1)
                       )
             )
  ),
  tabPanel("Acknowledgments", value = "acknowledgments",
           fluidRow(
             column(1),
             column(10, 
                    includeMarkdown("acknowledgments.Rmd")),
             column(1)
           )
  )
))