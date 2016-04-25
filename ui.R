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

questions <- c('B.1.1', 'B.1.3', 'B.2.1', 'B.2.2', 'C.1', #overall program satisfaction
               "L.4", "L.5", "L.6", 'L.3.a', 'L.2.a', #internship/field experience
               "N.1.1", "N.1.3", "N.2.1", "N.2.2", "N.3.1", "N.4.1", #satisfaction in first university
               "O.1.1", "O.1.3", "O.2.1", "O.2.2", "O.3.1", "O.4.1", #satisfaction in second university
               "P.1.1", "P.1.3", "P.2.1", "P.2.2", "P.3.1", "P.4.1", #satisfaction in third university
               "Q.1.1", "Q.1.3", "Q.2.1", "Q.2.2", "Q.3.1", "Q.4.1") #satisfaction in fourth university

tenormore <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  dplyr::summarise(respondents = n()) %>%
  filter(respondents >= 10)
colnames(tenormore) <- c("Course", "Respondents")

shinyUI(
  navbarPage(title = "Course Browser", id = "panels",
             footer = h6("Created by ", a(href = "https://ru.linkedin.com/in/mikhailbalyasin", target = "_blank",
                                          onclick="ga('send', 'event', 'click', 'link', 'linkedin', 1)",
                                          "Mikhail Balyasin"), "head of Quantitative team of CQAB of EMA |", 
                         a(" github", href = "https://github.com/romatik/Course_browser", target = "_blank",
                           onclick="ga('send', 'event', 'click', 'link', 'github', 1)"), 
                         align = "center", id = "footer"),
             header = HTML('
                            <link rel="stylesheet" type="text/css" href="style.css">
                            <script type="text/javascript" src="busy.js"></script>
                            <script type="text/javascript" src="ga.js"></script>
                           '),
             
  tabPanel("Home", value = "home",
           fluidRow(
             column(1),
             column(10,
                    h3("Welcome to Course Browser"),
                    p(actionLink("link_to_ema", img(src = "EMA_large.png", align = "right", width = "30%")),
                      "an online tool that represents the data that the ", 
                      actionLink("link_to_cqab", "Course Quality Advisory Board (CQAB)"),
                      "of the",
                      actionLink("link_to_ema1", "Erasmus Mundus Student and Alumni Association (EMA)"),
                      "collected through the Course Quality Students Services (CQSS) survey. In 2015, 
                      CQAB collected data from students and alumni of more than 150 Erasmus Mundus Joint Master Degree 
                      (EMJMD) programs in order to ask them about their experiences with their Erasmus Mundus 
                      masters."),
                    p("The main goal of Course Browser is to give you access to information about 78* EMJMDs 
                      in a most transparent and convenient way. We hope that you find this information useful 
                      and relevant."),
                    p(class = "footnote", "* Course Browser gives information on 78 Erasmus Mundus programs with 10 or more responses
                      students of their respective programs. You can find out 
                      an explanation about what that information means", actionLink("link_to_cqss", "here")),
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
                                                  "Question 2: Introduction process during orientation" = "B.1.3",
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
                    fluidRow(
                      column(6, align = "center", ofset = 3, 
                                 div(class = "busy",  
                                 p("Please wait..."), 
                                 img(src="https://upload.wikimedia.org/wikipedia/commons/4/42/Loading.gif"))
                    )),
                    fluidRow(h2(textOutput("course_name"), align = "center"),
                             hr(),
                             p(class = "footnote", textOutput("disclaimer")),
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
                    p(actionLink("link_to_home", "Back to Home"))),
             column(1)
           ),
           
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
                               p(actionLink("link_to_home1", "Back to Home"))),
                        column(1)
                      ),
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
                               p(actionLink("link_to_home2", "Back to Home"))),
                        column(1)
                      ),
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
                               p(actionLink("link_to_home3", "Back to Home"))),
                        column(1)
                      ),
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
                    p(actionLink("link_to_home4", "Back to Home"))),
             column(1)
           ),
           fluidRow(
             column(1),
             column(10, 
                    includeMarkdown("acknowledgments.Rmd")),
             column(1)
           )
  ),
  tabPanel("Contact us", value = "contact",
           fluidRow(
             column(1),
             column(10,
                    p(actionLink("link_to_home5", "Back to Home"))),
             column(1)
           ),
           fluidRow(
             column(1),
             column(10, 
                    h3("Contact information"),
                    p("We are always happy to hear your feedback. If you have questions about anything you found on this page, first we recommend
                      checking out the FAQ, but if your issue is not answered there, feel free to send us an e-mail at ",
                      a(href="mailto:cqab.char@em-a.eu", "cqab.char@em-a.eu",
                        onclick="ga('send', 'event', 'click', 'link', 'cqab.chair', 1)"), "."),
                    a(href = "http://www.em-a.eu/en/about-ema/advisory-boards/course-quality.html", 
                      onclick="ga('send', 'event', 'click', 'link', 'cqab', 1)",
                      img(src = "CQAB_large.png", style="margin: 0px 20px", width = "50%"),
                      target = "_blank")
                    ),
             column(1)
           )
  )
))