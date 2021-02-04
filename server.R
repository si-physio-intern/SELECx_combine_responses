### Combine response from SELECx

library(shiny)
library(shinyFeedback)
library(tidyverse)
library(vroom)
library(tools)
library(glue)
library(readxl)
library(openxlsx)

# Functions : Combine response ---------------------------------------------------------------

read_multi <- function(file_name,file_path){
    
    df <- tibble(ext = tools::file_ext(file_name), path = file_path)
    
    read <- function(ext,path){  
        
        switch(ext,
               csv = read_csv(path),
               xls = readxl::read_excel(path),
               xlsx = readxl::read_excel(path),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
               
        )
    }
    
    data_ls <- df %>% pmap(read)
    
    names(data_ls) <- str_remove(file_name,"\\.[^\\.]+$") #remove .xxx (content after last dot)
    
    data_ls
    
}

is.valid_list <- function(list, valid_regex){
    
    is.valid_df <- function(df, valid_regex){
        
        map_lgl(valid_regex , ~ str_detect(names(df), .x) %>% any() ) %>% all()
        
    }
    
    list %>% map( ~is.valid_df(.x , valid_regex)  )
}

clean_list <- function(list.raw){
    
    list.raw %>% 
        map(~select(.x,-Institution,-Department,-`Started on`,-`Time taken`,-Completed,
                    -starts_with("Grade"))) %>% 
        map(~rename(.x,Email="Email address")) %>% 
        map(~unite(.x ,"First name","Surname",sep = " ",col = "Name")) %>% 
        map(~filter(.x,!is.na(Email))) %>% 
        map(~mutate(.x, ID = str_extract(Email,"[:digit:]+") %>% as.character(), .keep = "unused")) %>% 
        map(~relocate(.x, Name,ID))
    
    
}

rename_list <- function(list){
    
    rename_list_df <- function(list,no,quiz_name){
        
        list %>%
            pluck(no) %>% 
            rename_with(.cols = starts_with("Response"), ~glue("{quiz_name}", ": ", "{.x}")) %>% 
            rename_with(.cols = "State", ~glue("{quiz_name}", ": ", "{.x}"))
        
    }
    
    name_list <- names(list)
    
    map2(.x = name_list, .y = seq_along(list), 
         ~rename_list_df(list, no = .y, quiz_name = .x) ) %>% set_names(name_list)
}

join_list <- function(list.renamed){
    
    joined_df <- list.renamed %>% reduce(full_join, by = c("ID","Name"))
    
    has.num_col  <- joined_df %>% map(is.numeric) %>% any()
    
    if(has.num_col == T){
        
        joined_df %>% 
            mutate(Total = rowSums(across(where(is.numeric)), na.rm = T ))
        
    }else{
        
        joined_df
    }
    
}

join_id <- function(ids, df, ... ){
    
    clean_id <- ids %>% 
        map_df(as.character) %>% 
        mutate(ID = str_extract(ID,"[:digit:]+")) %>% 
        select(ID,Name, ... )
    
    full_join(clean_id, df, by = "ID") %>% 
        rename(Name_from_ID = "Name.x", 
               Name_from_SELECx = "Name.y") %>% 
        relocate(... , .after = Name_from_SELECx) %>% 
        arrange(ID)
    
}


# Function : scoring ------------------------------------------------------


count_cols <- function(list,regex){
    
    list %>% 
        map_depth(2, names) %>% 
        map(names) %>% 
        map(~ sum(str_detect(.x,regex) )) %>% flatten_dbl()
    
}

get_ans_list <- function(list.cleaned, suffix_vec){
    
    list.ans <-  list.cleaned %>% 
        map(~na_if(.x, "-")) %>% ## Detect dash as NA 
        map(~rowwise(.x)) %>% 
        map(~mutate(.x, Answer = sum(!is.na((c_across(starts_with("Response")))))) ) %>% 
        map(~group_by(.x, Name,ID)) %>% 
        map(~summarise(.x, Answer = max(Answer)))
    
    rename_list_at <- function(list,var, suffix_vec ){
        
        rename_2 <- function(list,no,var, suffix_vec){
            
            list %>%
                pluck(no) %>% 
                rename_with( .cols = {{var}}, 
                             ~ glue("Count: ","{names(list)[no]}","_Max=","{suffix_vec[no]}")  )
            
        }
        
        
        seq_along(list) %>% 
            map(~rename_2(list, no = .x, var = {{var}}, suffix_vec = suffix_vec)) %>% 
            set_names(names(list))
    }
    
    list.ans %>% rename_list_at(Answer, suffix_vec)
    
}

# Server ------------------------------------------------------------------



server <- function(input, output, session) {
    
    # Upload files Quiz ---------------------------------------------------------------
    
    list.pre <- reactive({
        
        req(input$file)
        read_multi(file_name = input$file$name, file_path = input$file$datapath)
        
    })
    
    proper_list <- reactive({
        
        valid_regex <- c("First name","Surname","Email address","State","Response")
        
        list.pre() %>% is.valid_list(valid_regex) %>% all()
        
    })
    
    list.raw <- reactive({
        
        shinyFeedback::feedbackWarning("file", !proper_list(), "Incorrect file specification")
        
        req(proper_list())
        
        list.pre()
        
    })
    
    #output$raw <- renderPrint({ proper_list() })
    
    
    # Upload files IDs ---------------------------------------------------------------
    
    ids <- reactive({
        
        req(input$file_id) # Require - code wait until file uploaded
        
        ext <- tools::file_ext(input$file_id$name)
        switch(ext,
               csv = vroom::vroom(input$file_id$datapath, delim = ","),
               xls = readxl::read_excel(input$file_id$datapath),
               xlsx = readxl::read_excel(input$file_id$datapath),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
        )
    })
    
    proper_id <- reactive({  
        
        all(c("ID","Name") %in% colnames( ids() ) && all(str_detect(ids() %>% pull(ID),"[:digit:]+"))) 
        
    })
    
    
    observeEvent(input$file_id,
                 shinyFeedback::feedbackWarning(
                     "file_id", 
                     !proper_id(),
                     "Incorrect ID file specification"
                 )  
    )
    
    
    # Clean -------------------------------------------------------------------
    
    list.cleaned <- reactive({
        
        list.raw() %>% clean_list()
        
    })
    
    
    # Rename ------------------------------------------------------------------
    
    
    list.renamed <- reactive({
        
        list.cleaned() %>% rename_list()
        
    })
    
    # Join list to DF ---------------------------------------------------------------
    
    df.joined <- reactive({
        
        list.renamed() %>% join_list()
        
    })
    
    # Join DF to ids ---------------------------------------------------------------
    
    id_cols <- reactive({ 
        
        req(proper_id())
        ids() %>% select(-ID,-Name) %>% colnames() 
        
    })
    
    output$select <- renderUI({
        
        if(input$add_cols == T){
            selectInput("cols","Choose column",choices = id_cols(), multiple = TRUE)
        }
        
    })
    
    
    df.joined_ids <- reactive({
        
        if(( !isTruthy(input$file_id)) || ( !proper_id()) ){  
            
            df.joined()
            
        }else if(input$add_cols == T){
            
            join_id(ids = ids() ,df = df.joined(), input$cols)
            
        }else{
            join_id(ids = ids() ,df = df.joined())}
        
        
    })  
    
    
    # Count responses  ---------------------------------------------------------
    
    col_resp_num <- reactive({
        
        list.cleaned()  %>% count_cols(regex = "Response")
        
    })
    
    ans.df  <- reactive({
        
        list.cleaned() %>% 
            get_ans_list(suffix_vec = col_resp_num()) %>% 
            join_list()
        
    })
    
    ans.df_id <- reactive({
        
        if(( !isTruthy(input$file_id)) || ( !proper_id()) ){  
            
            ans.df()
            
        }else if(input$add_cols == T){
            
            join_id(ids = ids() ,df = ans.df(), input$cols)
            
        }else{
            join_id(ids = ids() ,df = ans.df())}
        
        
    })
    
    
    # Missing names ,  no answer ----------------------------------------------
    
    check_na <- reactive({
        
        ans.df_id() %>% 
            filter_at(vars(-ID), any_vars( is.na(.)) )
        
    })  
    
    
    # Display table ---------------------------------------------------------------
    
    output$table <- renderDataTable( 
        
        df.joined_ids(), options = list(lengthMenu = c(5,10,20,50), pageLength = 5 )
        
    )
    
    output$table_2 <- renderDataTable( 
        
        ans.df_id(), options = list(lengthMenu = c(5,10,20,50), pageLength = 5 )
        
    )
    
    output$missing <- renderDataTable(
        
        check_na(), options = list(lengthMenu = c(5,10,20,50), pageLength = 5 )
        
    )
    
    # Download ---------------------------------------------------------------
    
    output$download <- downloadHandler(
        
        filename = function() {
            paste0("Combined_response",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx(df.joined_ids(), file)
        }
    )
    
    
    output$download_2 <- downloadHandler(
        
        filename = function() {
            paste0("Count_response",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx(ans.df_id(), file)
        }
    )
    
    
}

