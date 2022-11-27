start_word_prediction <- readRDS("start_word_prediction.rds")
freq2_with_stop <- readRDS("freq2_with_stop.RDS")
freq3_with_stop <- readRDS("freq3_ultra.RDS")
freq4_with_stop <- readRDS("freq4_ultra.RDS")


#### Helper functions
show_prediction <- function(vector, n = 1) {
    if(n == 1)
        return(vector[1])
    else if(n == 2)
        return(vector[2])
    else if(n== 3)
        return(vector[3])
}


match_predict2 <- function(user_input, ngrams) {
    
    ############# Ngrams = 4    
    if(ngrams > 3) {  # Handle cases with longer than 3 words
        #1 exact match
        user_input_limit3 <- paste(user_input[length(user_input)-2], user_input[length(user_input)-1], user_input[length(user_input)])
        data_tokens <- freq4_with_stop %>% filter(variable == user_input_limit3)
        if(nrow(data_tokens) >= 1) {
            #already in order:  setorder(data_tokens, -percent)
            #print("freq4_with_stop")
            return(data_tokens$outcome[1:3])
        }
        #3 backoff to 3 grams 
        return(match_predict2(user_input, ngrams - 1))
    }
    
    ############# Ngrams = 3
    
    if(ngrams == 3) {
        user_input_limited <- paste(user_input[length(user_input)-1], user_input[length(user_input)])
        data_tokens <- freq3_with_stop %>% filter(variable == user_input_limited)
        if(nrow(data_tokens) >= 1) {
            #already in order:  setorder(data_tokens, -percent)
            #print("freq3_with_stop")
            return(data_tokens$outcome[1:3])
        }
        #Backoff
        return(match_predict2(user_input, ngrams - 1))
    }
    
    ############# Ngram = 2
    
    if(ngrams < 3) {
        user_input_limited <- user_input[length(user_input)]
        data_tokens <- freq2_with_stop %>% filter(variable == user_input_limited)
        #if(nrow(data_tokens) >= 1) {
        #already in order setorder(data_tokens, -percent)
        #print("freq2_with_stop")
        return(data_tokens$outcome[1:3])
        #}
        #Backoff
        #return(match_predict(user_input, ngrams - 1))
    }
    
    ############# Ngram = 1  (Eliminated for speed considerations)
    
    return(NA)
}



clean_input <- function(input) {
    if(input == "" | is.na(input))
        return("")
    input <- tolower(input)
    input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case=F, perl=T) #remove ordinal numbers
    input <- gsub("[.\\-!]", " ", input, ignore.case=F, perl=T) #remove punctuation
    input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case=F, perl=T) #remove punctuation, leaving '
    input <- gsub("^\\s+|\\s+$", "", input) #trim leading and trailing whitespace
    input <- stripWhitespace(input)
    if(input == "" | is.na(input))
        return("")
    input <- unlist(strsplit(input, " "))
    
    return(input)
}

main2 <- function(input, word = 0) {
    
    #print(input)    #for debugging
    input <- clean_input(input)
    
    if(input[1] == "") {
        output <- start_word_prediction
    }
    
    else if(length(input) == 1) {
        output <- match_predict2(input, ngrams = 2)  #exact scenaro match to predict 2nd word
    }
    
    else if(length(input) == 2) {
        output <- match_predict2(input, ngrams = 3)
    }
    
    else if(length(input) > 2) {
        output <- match_predict2(input, ngrams = 4)
    }
    
    
    if (word == 0)
        return(output)
    else if (word == 1)
        return(output[1])
    else if (word == 2)
        return(output[2])
    else if (word == 3)
        return(output[3])
}




shinyServer(function(input, output) {
    
    
    # Control for reactive data
    ans1 <- reactive({
        main2(input$user_input, 1)
    })
    ans2 <- reactive({
        main2(input$user_input, 2)
    })
    ans3 <- reactive({
        main2(input$user_input, 3)
    })
    
    
    # Output Control
    
    output$guess1 <- ans1
    output$guess2 <- ans2
    output$guess3 <- ans3
    
})

#})
