# install.packages("readxl")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("shiny")
# install.packages("shinyjs")
# install.packages("lubridate")
library(base)
library(utils)
library(readxl)
library(stringr)
library(dplyr)
library(shiny)
library(shinyjs)
library(lubridate)


source("words.R")
source("functions.R")
source("js functions.R")








# UI
ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/f175d6d133.js"),
  tags$head(tags$script(HTML(jscode)),
            tags$style(HTML("
                            .container-fluid {
                            max-width: 1000px
                            }

                            #used {
                            display: grid;
                            grid-template-columns: repeat(13, 50px);
                            gap: 5px;
                            justify-content: center;
                            padding: 15px;
                            }

                            #result {
                            display: grid;
                            grid-template-columns: repeat(5, 50px);
                            gap: 5px;
                            justify-content: center;
                            padding: 15px;
                            }

                            #attemptstext {
                            font-size: 20px;
                            }

                            .keyboard{
                            display: flex;
                            flex-direction: column;
                            align-items: center;
                            }
                            
                            .my-action {
                            padding: 5px;
                            }

                            .my-action .key{
                            margin: -5px;
                            border: 0;
                            display: inline-block;
                            font-family: 'nyt-franklin';
                            font-weight: bold;
                            font-size: 20px;
                            width: 45px;
                            height: 45px;
                            place-content: center;
                            text-align: center;
                            background-color: #d3d6da;
                            padding: 10px 0;
                            color: black;
                            border-radius: 5px;
                            }
                            
                            .my-action .delete{
                            margin: -5px;
                            border: 0;
                            display: inline-block;
                            font-family: 'nyt-franklin';
                            font-weight: bold;
                            font-size: 20px;
                            width: 45px;
                            height: 45px;
                            place-content: center;
                            text-align: center;
                            background-color: #d3d6da;
                            padding: 10px 0;
                            color: black;
                            border-radius: 5px;
                            }

                            .row{
                            margin-bottom: .3rem;
                            }

                            .keyboard-letters{
                            border: 1px solid white;
                            width: 50px;
                            height: 50px;
                            display: grid;
                            place-content: center;
                            border-radius: 5px;
                            color: black;
                            font-size: 20px;
                            background-color: white;
                            }

                            .letter-design {
                            border: 1px solid blanchedalmond;
                            width: 50px;
                            height: 50px;
                            display: grid;
                            place-content: center;
                            border-raidus: 5px;
                            color: white;
                            font-size: 20px;
                            }

                            .start {
                            background-color: #d3d6da;
                            }

                            .in-word {
                            background-color: #c8b458;
                            }

                            .my-action .in-word1 {
                            margin: -5px;
                            border: 0;
                            font-size: 20px;
                            width: 45px;
                            height: 45px;
                            display: inline-block;
                            place-content: center;
                            text-align: center;
                            background-color: #c8b458;
                            padding: 10px 0;
                            color: white;
                            border-radius: 5px;
                            }

                            .not-in-word {
                            background-color: #787c7e;
                            }

                            .my-action .not-in-word1 {
                            margin: -5px;
                            border: 0;
                            font-size: 20px;
                            width: 45px;
                            height: 45px;
                            display: inline-block;
                            place-content: center;
                            text-align: center;
                            background-color: #787c7e;
                            padding: 10px 0;
                            color: white;
                            border-radius: 5px;
                            }

                            .correct {
                            background-color: #6aa964;
                            }

                            .my-action .correct1 {
                            margin: -5px;
                            border: 0;
                            font-size: 20px;
                            width: 45px;
                            height: 45px;
                            display: inline-block;
                            place-content: center;
                            text-align: center;
                            background-color: #6aa964;
                            padding: 10px 0;
                            color: white;
                            border-radius: 5px;
                            }

                            .instructions {
                            font-size: 16px;
                            margin-top: 1px;
                            margin-left: 1px;
                            position: absolute;
                            top: 0;
                            right: 0;
                            }

                            .input-wrapper {
                            display: flex;
                            align-items: flex-end;
                            justify-content: center;
                            padding-bottom: 10px;
                            padding-bottom: 50px;
                            }

                            .input-wrapper > div {
                            margin-bottom:  0;
                            }

                            .input-wrapper > button {
                            margin-left: 10px;
                            }

                            "))),
  useShinyjs(),
  div(
    actionButton("instructions", "", tags$i(class = "fa-regular fa-circle-question", style = "font-size:2.5rem;")),
    class = "instructions"
  ),
  selectInput(inputId = 'attempts', label = 'Փորձերի Քանակ', c(6, 7, 8, 9, 10)),
  align = "center", textOutput("attemptstext"),
  div(
    tagAppendAttributes(
      textInput("guess", "", placeholder = "Գրել Այստեղ"),
      `data-proxy-click` = "go"),
    actionButton("go", "",tags$i(class = "fa-solid fa-right-to-bracket", style = "font-size: 1.4rem;")),
    class = "input-wrapper"),
  p(strong("Կարևոր՝"), "«ու» տառը գրելիս օգտագործել միայն «ւ» մասնիկը՝ ձագւկ, ջւթակ։"),
  uiOutput("result"),
  tags$div(
    class = "keyboard",
    tags$div(
      class = "row",
      actionButton("l1", div(class = "key", "է", "data-value" = "է"), class = "my-action"),
      actionButton("l2", div(class = "key", "թ", "data-value" = "թ"), class = "my-action"),
      actionButton("l3", div(class = "key", "փ", "data-value" = "փ"), class = "my-action"),
      actionButton("l4", div(class = "key", "ձ", "data-value" = "ձ"), class = "my-action"),
      actionButton("l5", div(class = "key", "ջ", "data-value" = "ջ"), class = "my-action"),
      actionButton("l6", div(class = "key", "ւ", "data-value" = "ւ"), class = "my-action"),
      actionButton("l7", div(class = "key", "և", "data-value" = "և"), class = "my-action"),
      actionButton("l8", div(class = "key", "ր", "data-value" = "ր"), class = "my-action"),
      actionButton("l9", div(class = "key", "չ", "data-value" = "չ"), class = "my-action"),
      actionButton("l10", div(class = "key", "ճ", "data-value" = "ճ"), class = "my-action"),
      actionButton("l11", div(class = "key", "ժ", "data-value" = "ժ"), class = "my-action"),
    ),
    tags$div(
      class = "row",
      actionButton("l12", div(class = "key", "ք", "data-value" = "ք"), class = "my-action"),
      actionButton("l13", div(class = "key", "ո", "data-value" = "ո"), class = "my-action"),
      actionButton("l14", div(class = "key", "ե", "data-value" = "ե"), class = "my-action"),
      actionButton("l15", div(class = "key", "ռ", "data-value" = "ռ"), class = "my-action"),
      actionButton("l16", div(class = "key", "տ", "data-value" = "տ"), class = "my-action"),
      actionButton("l17", div(class = "key", "ը", "data-value" = "ը"), class = "my-action"),
      actionButton("l18", div(class = "key", "ւ", "data-value" = "ւ"), class = "my-action"),
      actionButton("l19", div(class = "key", "ի", "data-value" = "ի"), class = "my-action"),
      actionButton("l20", div(class = "key", "օ", "data-value" = "օ"), class = "my-action"),
      actionButton("l21", div(class = "key", "պ", "data-value" = "պ"), class = "my-action"),
      actionButton("l22", div(class = "key", "խ", "data-value" = "խ"), class = "my-action"),
      actionButton("l23", div(class = "key", "ծ", "data-value" = "ծ"), class = "my-action"),
      actionButton("l24", div(class = "key", "շ", "data-value" = "շ"), class = "my-action")
    ),
    tags$div(
      class = "row",
      actionButton("l25", div(class = "key", "ա", "data-value" = "ա"), class = "my-action"),
      actionButton("l26", div(class = "key", "ս", "data-value" = "ս"), class = "my-action"),
      actionButton("l27", div(class = "key", "դ", "data-value" = "դ"), class = "my-action"),
      actionButton("l28", div(class = "key", "ֆ", "data-value" = "ֆ"), class = "my-action"),
      actionButton("l29", div(class = "key", "գ", "data-value" = "գ"), class = "my-action"),
      actionButton("l30", div(class = "key", "հ", "data-value" = "հ"), class = "my-action"),
      actionButton("l31", div(class = "key", "յ", "data-value" = "յ"), class = "my-action"),
      actionButton("l32", div(class = "key", "կ", "data-value" = "կ"), class = "my-action"),
      actionButton("l33", div(class = "key", "լ", "data-value" = "լ"), class = "my-action")
    ),
    tags$div(
      class = "row",
      actionButton("l34", div(class = "key", "զ", "data-value" = "զ"), class = "my-action"),
      actionButton("l35", div(class = "key", "ղ", "data-value" = "ղ"), class = "my-action"),
      actionButton("l36", div(class = "key", "ց", "data-value" = "ց"), class = "my-action"),
      actionButton("l37", div(class = "key", "վ", "data-value" = "վ"), class = "my-action"),
      actionButton("l38", div(class = "key", "բ", "data-value" = "բ"), class = "my-action"),
      actionButton("l39", div(class = "key", "ն", "data-value" = "ն"), class = "my-action"),
      actionButton("l40", div(class = "key", "մ", "data-value" = "մ"), class = "my-action"),
      actionButton("l41", div(class = "delete", tags$i(class = "fas fa-delete-left")), class = "my-action")
    )
  ),
  tags$script(HTML(listener)),
  tags$script(HTML(delete_b)),
  # column(12, align = "center",p("Ստորև օգտագործված տառերն են:")),
  uiOutput("used")
)


Sys.setenv(TZ='Asia/Yerevan')
set.seed(as.integer(Sys.Date()))
target <- sample(words_common_5, 1)




# Server
server <- function(input, output, session) {
  
  # To clean the text input after submitting each word
  observeEvent(input$go, {
    updateTextInput(session, "guess", value = "")
  })
  
  # Not to allow to have more than 5 characters in the text input
  observe({
    shinyjs::runjs("$('#guess').attr('maxlength', 5)")
  })
  
  # Create reactiveVal to store all the guesses
  all_guesses <- reactiveVal(character())
  
  # observeEvent(input$go, {
  #   shinyjs::disable("go")
  #   debounce(10000, function() {
  #     shinyjs::enable("go"))
  # })
  # })
  
  output$result <- renderUI({
    if (!tolower(input$guess) %in% words_all_5) {
      req(FALSE, cancelOutput = TRUE)
    }
    
    # Set time to show when the next game will be
    tomorrow <- Sys.Date() + 1 +
      hours(0) +
      minutes(0) +
      seconds(1)
    today <- Sys.time()
    difference <- difftime(format(tomorrow, "%Y-%m-%d %H:%M:%S"),
                           format(today, "%Y-%m-%d %H:%M:%S"))
    timedifference <- round(difference[[1]], 2)
    hours <- floor(timedifference)
    minutes <- round((timedifference - floor(timedifference))*60, 0)
    
    all_guesses_new <- c(all_guesses(), tolower(input$guess))
    all_guesses(all_guesses_new)
    
    processing <- function(guess) {
      result <- check_words(target, tolower(guess))
      format_result(result)
    }
    
    out_str <- lapply(all_guesses(), processing)
    
    victory <- vapply(all_guesses(), function(guess) {
      check_words(target, guess)$correct
    }, logical(1))
    
    
    output$attemptstext <- renderText({
      attempts_left <- if(length(out_str) < as.numeric(input$attempts)) {
        as.numeric(input$attempts) - length(out_str)
      } else {
        0
      }
      paste("Ձեզ մնացել է ", attempts_left, "փորձ")
      
    })
    
    # Show when game is lost (include time of next game)
    if (length(out_str) >= as.numeric(input$attempts)) {
      disable("go") &
        disable("guess") &
        disable("attempts") &
        showModal(modalDialog(
          title = p(strong("Ցավում ենք, բայց այս անգամ չստացվեց :(")),
          paste0("Ճիշտ բառը «", target, "» -ն էր։ Հաջորդ խաղը կլինի ", hours, " ժամ ", minutes, " րոպեից։"),
          easyClose = TRUE,
          footer = NULL
        ))
    }
    
    final <- if (length(out_str) > as.numeric(input$attempts)) {
      out_str[1:as.numeric(input$attempts)]
    } else {
      out_str
    }
    
    # Show when game is won (include time of next game)
    if (TRUE %in% victory & length(final) < as.numeric(input$attempts)) {
      disable("go") &
        disable("guess") &
        disable("attempts") &
        showModal(modalDialog(
          title = p(strong("Հաղթանակ!")),
          paste0("Շնորհավորում ենք, դուք հաղթեցիք։ Ճիշտ բառը «", target, "» -ն էր։ Հաջորդ խաղը կլինի ", hours, " ժամ ", minutes, " րոպեից։"),
          easyClose = TRUE,
          footer = NULL
        ))
    }
    
    
    final
    
  }) %>% bindEvent(input$go)
  
  # Add section for game instructions
  observeEvent(input$instructions, {
    showModal(modalDialog(
      title = p(strong("Խաղի Կանոնները")),
      p("Խաղի նպատակն է գուշակել 5 տառից բաղկացած բառը։ Օրինակ՝ դպրոց, սեղան, մատիտ, շեփոր։"),
      p("Բառը գուշակելիս ", strong("չպետք"),"է օգտագործել հատուկ անուններ։ Օրինակ՝ Արման, Շուշան, Մալթա։"),
      p("«ու» տառը գրելիս օգտագործել միայն «ւ» մասնիկը։ Օրինակ՝ մանուկ - մանւկ, ջութակ - ջւթակ։"),
      p(strong(span("Կանաչը", style = "color:#6aa964")), "նշանակում է, որ տառը ճիշտ է։"),
      p(strong(span("Դեղինը", style = "color:#c8b458")), "նշանակում է, որ տառը գոյություն ունի բառի մեջ, բայց սխալ դիրքում է։"),
      p(strong(span("Մոխրագույնը", style = "color:#787c7e")), "նշանակում է, որ տառը սխալ է։"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Show the used letters
  letter_matches <- list()
  let_val1 <- reactiveVal()
  
  
  
  
  output$used <- renderUI({
    if (!tolower(input$guess) %in% words_all_5) {
      req(FALSE, cancelOutput = TRUE)
    }
    
    result_1 <- check_words(target, tolower(input$guess))
    
    mapply(result_1$letters, result_1$result, SIMPLIFY = FALSE, USE.NAMES = FALSE,
           FUN = function(letter, match) {
             prev_match <- letter_matches[[letter]]
             if (is.null(prev_match)) {
               letter_matches[[letter]] <<- match
             } else {
               if (match == "correct" && prev_match %in% c("not-in-word", "in-word")) {
                 letter_matches[[letter]] <<- match
               } else if (match == "in-word" && prev_match == "not-in-word") {
                 letter_matches[[letter]] <<- match
               }
             }
           }
    )
    
    removed_letters <- list(
      letters = names(letter_matches),
      result = unlist(unname(letter_matches)))
    
    let_val1(removed_letters)
    # Delete the row below if we do not want to see the removed letters below the keyboard
    # If deleted, activate retutn button below or update RenderUI
    return("")
    # format_result(removed_letters)
    
  }) %>% bindEvent(input$go)
  
  
  # Update keyboard after submit button
  observeEvent(input$go, {
    let <- let_val1()
    
    letters_list <- c("է", "թ", "փ", "ձ", "ջ", "ւ", "և", "ր", "չ", "ճ", "ժ", "ք", "ո", "ե", "ռ", "տ", "ը", "ւ", "ի", "օ", "պ", "խ", "ծ", "շ", "ա", "ս", "դ", "ֆ", "գ", "հ", "յ", "կ", "լ", "զ", "ղ", "ց", "վ", "բ", "ն", "մ")
    buttons_list <- c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9", "l10", "l11", "l12", "l13", "l14", "l15", "l16", "l17", "l18", "l19", "l20", "l21", "l22", "l23", "l24", "l25", "l26", "l27", "l28", "l29", "l30", "l31", "l32", "l33", "l34", "l35", "l36", "l37", "l38", "l39", "l40")
    
    for (i in seq_along(letters_list)) {
      letter <- letters_list[i]
      button <- buttons_list[i]
      
      if (letter %in% let$letters) {
        updateActionButton(session, button, label = paste(
          toString(div(class = paste(let$result[which(let$letters == letter)],1, sep = ""), letter,  "data-value" = letter)),
          toString(tags$script(HTML(listener))), sep = ""))
      } else {
        updateActionButton(session, button, label = paste(
          toString(div(class = "key", letter,  "data-value" = letter)), 
          toString(tags$script(HTML(listener))), sep = ""))
      }
    }
  })
  
  
}



# Run the app
shinyApp(ui = ui, server = server)
