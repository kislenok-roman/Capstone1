
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# additional control
inputTextArea <- function(inputId, value = "", placeholder = "", nrows = 5, ncols = 10) {
    tagList(
        singleton(tags$head(tags$script(src = "js/textarea.js"))),
        tags$textarea(id = inputId,
                      class = "form-control",
                      placeholder = "Input text here",
                      rows = nrows,
                      cols = ncols,
                      as.character(value))
    )
}

shinyUI(
    bootstrapPage(
        tags$head(tags$script(src = "js/app.js")),
        tags$head(tags$link(href = "css/bootstrap.min.css", rel = "stylesheet")),
        tags$head(tags$link(href = "css/main.css", rel = "stylesheet")),
        tags$nav(
            div(
                div(
                    a("Type helper application", class = "navbar-brand"),
                    class="navbar-header"
                ),
                div(
                    tags$ul(
                        tags$li(
                            a("About", href = "http://rpubs.com/kislenok01/capstoneDSS", target = "_blank")
                        ),
                        class="nav navbar-nav navbar-right"
                    ),
                    class = "collapse navbar-collapse"
                ), class = "container-fluid"
            ), class="navbar navbar-default", role="navigation"
        ),
        div(
            div(
                div(
                    inputTextArea("mainTextInput", placeholder = "Start typing..."),
                    class = "col-lg-12"
                ), class = "row"
            ),
            div(
                div(htmlOutput("predictedWords"), class = "col-lg-12 predict-place"),
                class = "row"),
            div(
                div(span("Application gives you words ordered by probability (left has most probability)", class = "help-block"),
                    class = "col-lg-12"),
                class = "row"),
            div(
                div(htmlOutput("guessedTextType"), class = "col-lg-12"),
                class = "row"),
            div(
                div(h4("Options"), class = "col-lg-12"),
                class = "row options"),
            div(
                div(radioButtons("amountTypeRadio", "What kind of text do you input?",
                                 list("Guess text type" = "guess",
                                      "Unite all text sources" = "amount",
                                      "Twitter" = "t.amount",
                                      "Blogs" = "b.amount",
                                      "News" = "n.amount")),
                    span("On some test 'correct' text type can substantialy improve word prediction", class = "help-block"),
                    span("You can alter text type and get different prediction.", class = "help-block"),
                    span("Application can try guess input text type by estimating sentence probability", class = "help-block"),
                    class = "col-lg-4"),
                div(sliderInput("maxPredictSizeSlider", "Predict no more than, words: ",
                                min = 3, max = 20, value = 5, step = 1),
                    span("Restrict amount of word to predict", class = "help-block"),
                    span("It can speed up (lower values) or slow down (high values) prediction", class = "help-block"),
                    class = "col-lg-4"),
                div(
                    checkboxInput("caseCheck", "Output in most common letter case?", value = TRUE),
                    span("Use statistics to predict whether word should be written in lower, UPPER or Mixed case", class = "help-block"),
                    class = "col-lg-4"),
                class = "row"
            ),
            div(
                div(htmlOutput("techStat"), class = "col-lg-12"),
                class = "row footer"
            ), class = "container-fluid"
        ), title = "Type helper", theme = "css/bootstrap.min.css"
    )
)

