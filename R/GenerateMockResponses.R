# Generating Mock submissions

library(readxl)
library(uuid)

#' Generate Mock Response
#'
#' This is a function to generate a mock response based on
#' the survey file alone
#'
#' @param survey_path The path to the xls survey file
#'
#' @return
#' @export
#'
#' @examples
generate_mock_response <- function(survey_path){
    survey <- readxl::read_excel(survey_path, sheet = "survey")

    choices <- readxl::read_excel(survey_path, sheet = "choices")

    submission_xml <- ""
    xml_level <- 1
    group_at_level <- c()

    repeat_rows <- identify_repeat_locations(survey)
    repeat_start_rows <- sapply(repeat_rows,function(x)x[1])



    # Still need to do decimal
    # integer
    # deviceid
    # calculate
    # start (start time auto)
    # end (end time auto)
    # geopoint
    # integer
    # image
    # text
    #
    # repeat
    #
    #
    for (survey_row in 1:nrow(survey)){

        if (survey_row %in% unlist(repeat_rows)==F)
        {
            response <- generate_random_row(submission_xml,survey,choices,survey_row,xml_level, group_at_level)
            submission_xml <-response$submission_xml
            xml_level <-response$xml_level
            group_at_level <- response$group_at_level
        }

        # if (survey_row %in% unlist(repeat_rows)==T)
        # {
        #     submission_xml <- paste0(submission_xml,"\n repeats")
        #
        # }

        # Dealing with loops and repeats --------------------------------
        if (survey_row%in%repeat_start_rows){
            loop_xml <- adding_looped_data(survey, choices,repeat_rows[[which(repeat_start_rows%in%survey_row)]])
            loop_xml<- print_chunk_with_tab_spacing(loop_xml,xml_level)
            submission_xml<-paste0(submission_xml,loop_xml)
        }

    }


    submission_xml <- add_headers_and_footers(submission_xml, survey_path )

    return(submission_xml)
}


#' Adding Looped Data
#'
#' Some of the survey questions in ODK central are asked in a
#' repeat format. This function allows us to carry out these
#' repeats for a randome number of loops (capped at 5)
#'
#' @param survey The survey table as a tibble
#' @param choices The choices table as a tibble
#' @param rows The rows in the survey table which correspond
#' to the looped questions
#'
#' @return
#' @export
#'
#' @examples
adding_looped_data <- function(survey, choices,rows){

    repeat_loop_xml <- "\n"
    repeat_name <- survey$name[rows[1]]
    number_of_loops <- sample(c(1:5),1)


    repeat_loop_xml <- paste0(repeat_loop_xml,"<",repeat_name,"_count>",number_of_loops,"</",repeat_name,"_count>")

    for (i in 1:number_of_loops){
        repeat_loop_xml <- paste0(repeat_loop_xml,"\n<",repeat_name,">")


        loop_name <- survey$name[rows[2]]
        repeat_loop_xml <- paste0(repeat_loop_xml,"\n    <",loop_name,">",i,"</",loop_name,">")

        repeat_group_row_start <- rows[3]
        repeat_group_row_end <- which(survey$name==survey$name[rows[3]]& survey$type=="end group")

        xml_level <- 2
        group_at_level <- c("","")
        for (repeat_row in c(repeat_group_row_start:repeat_group_row_end))
        {
            response <- generate_random_row(repeat_loop_xml,
                                            survey,choices,
                                            repeat_row,
                                            xml_level=xml_level,
                                            group_at_level=group_at_level)
            repeat_loop_xml <- paste0(response$submission_xml)
            xml_level <-response$xml_level
            group_at_level <- response$group_at_level
            cat(repeat_loop_xml)
        }

        repeat_loop_xml <- paste0(repeat_loop_xml,"\n</",repeat_name,">")

    }

    return(repeat_loop_xml)
}


#' Print Chunk with Tab Spacing
#'
#' When a xml chunk is created, say for a looped
#' set of questions, we can add the required
#' number of tabs at the beginning of each line
#'
#' @param xml_string The string which needs extra tabs
#' at the beginning
#' @param xml_level The number of tabs to be added at
#' the beginning of all lines
#'
#' @return
#' @export
#'
#' @examples
print_chunk_with_tab_spacing <- function(xml_string, xml_level){
    spaces <- generate_tab_spaces(xml_level)

    xml_string <- gsub("\n",paste0("\n",spaces),xml_string)

    return(xml_string)
}


#' Identify Repeat Locations
#'
#' Find the rows in the survey file
#' where questions are in repeat loops
#'
#' @param survey The survey sheet of the
#' excel file as a tibble
#'
#' @return
#' @export
#'
#' @examples
identify_repeat_locations <- function(survey){

    repeat_starts <- which(survey$type=="begin repeat")
    repeat_ends <- which(survey$type=="end repeat")

    if (length(repeat_starts)!=length(repeat_ends)){
        stop("There are not the same number of repeat starts and ends. Must be a problem")
    }

    repeat_columns <- sapply(c(1:length(repeat_starts)), function(x){
        return(c(repeat_starts[x]:repeat_ends[x]))
    })

    return(repeat_columns)

}

#' Generate a Random Row
#'
#' For each row in the survey file generate
#' the xml row needed for submission.
#'
#' @param submission_xml The previous xml which must be
#' appended
#' @param survey The survey sheet to read
#' @param choices The choices to read
#' @param survey_row The index (which row) of the
#' survey sheet which we must examine
#' @param xml_level The level at which we are looking
#' i.e, how many tabs to put at the start of this line
#' @param group_at_level A dictionary telling us which
#' "names" are associate with level of indentation, making
#' it easier to close the groups when the survey groups
#' when they have ended.
#'
#' @return
#' @export
#'
#' @examples
generate_random_row <- function(submission_xml,survey,choices,survey_row, xml_level, group_at_level){


    spaces <- generate_tab_spaces(xml_level)


    # Dealing with the group outlines ---------------------------------
    if(survey[[survey_row,"type"]]=="begin group"){
        xml_level <- xml_level+1


        group <- survey[survey_row,"name"]
        group_at_level[xml_level] <- group
        submission_xml <- paste0(submission_xml,"\n",spaces,"<",group,">")
    }

    if(survey[[survey_row,"type"]]=="end group"){
        #group <- survey[survey_row,"name"]

        spaces <- generate_tab_spaces(xml_level-1)

        group <- group_at_level[xml_level]

        submission_xml <- paste0(submission_xml,"\n",spaces,"</",group,">")
        xml_level <- xml_level-1
    }


    # Multiple choice ------------------------------------------------
    question_type <- survey[[survey_row,"type"]]
    if (unlist(strsplit(question_type," "))[1]=="select_multiple"){
        name <- survey[[survey_row,"name"]]
        row_for_choices_sheet <- choices$list_name==unlist(strsplit(question_type," "))[2]

        question_options <- choices$name[row_for_choices_sheet]
        selection <- select_multiple(question_options)
        selection <- paste0(selection,collapse = " ")

        submission_xml <- paste0(submission_xml,"\n",spaces,"<",name,">",selection,"</",name,">")
    }

    if (unlist(strsplit(question_type," "))[1]=="select_one"){
        name <- survey[[survey_row,"name"]]
        row_for_choices_sheet <- choices$list_name==unlist(strsplit(question_type," "))[2]

        question_options <- choices$name[row_for_choices_sheet]
        selection <- sample(question_options,1)

        submission_xml <- paste0(submission_xml,"\n",spaces,"<",name,">",selection,"</",name,">")
    }





    #--- Dealing with easy types
    if(survey[[survey_row,"type"]]=="integer"){
        random_int <- sample(c(1:1000),1)
        name <- survey[[survey_row,"name"]]
        submission_xml <- paste0(submission_xml,"\n",spaces,"<",name,">",random_int,"</",name,">")
    }

    if(survey[[survey_row,"type"]]=="text"){
        random_number_of_characters <- sample(c(1:100),1)
        alphabet <- c(" ","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",".","!")
        words <- paste0(sample(alphabet,random_number_of_characters, replace = T),collapse="")

        name <- survey[[survey_row,"name"]]
        submission_xml <- paste0(submission_xml,"\n",spaces,"<",name,">",words,"</",name,">")
    }

    if(survey[[survey_row,"type"]]=="decimal"){
        random_decimal <- runif(1,min=0,max=500)
        name <- survey[[survey_row,"name"]]
        submission_xml <- paste0(submission_xml,"\n",spaces,"<",name,">",random_decimal,"</",name,">")
    }

    return(list(submission_xml=submission_xml,xml_level=xml_level, group_at_level=group_at_level))
}



#' Generate Tab Spaces
#'
#' Make the necessary number of
#' spaces based on the level of the xml form
#'
#' @param form_level The level (amount of indentation)
#' we need for a particular line in the xml response
#'
#' @return
#' @export
#'
#' @examples
generate_tab_spaces <- function(form_level){
    if (form_level==1){
        spaces <- ""
    }
    if (form_level>1){
        spaces <- rep("    ", form_level-1)
        spaces <- paste0(spaces, collapse = "")
    }

    return(spaces)
}

#' Select Multiple
#'
#' A function for selecting multiple choices
#' from a list at random
#'
#' @param list_to_sample The list of objects which need to be
#' sampled
#'
#' @return
#' @export
#'
#' @examples
select_multiple <- function(list_to_sample){
    number_to_choose_from <- sample(c(1:length(list_to_sample)), 1)
    samples <- sample(list_to_sample, number_to_choose_from)
    return(samples)
}


#' Add Headers and Footers
#'
#' Submissions to ODK central have headers
#' and footers so the forms can be managed and metadata included.
#' This function adds the necessary headers and footer based on the survey file
#'
#' @param xml_string The string which needs headers and
#' footers added
#' @param survey_path The path to the survey file where we
#' obtain the metadata information
#'
#' @return
#' @export
#'
#' @examples
add_headers_and_footers <- function(xml_string,survey_path){
    metadata <- readxl::read_excel(survey_path, sheet="settings")



    id <- metadata$form_id[1]
    version <- metadata$version[1]
    header_1 <- '<?xml version="1.0" encoding="UTF-8"?>'
    data_header <- paste0('<data version="',version,'" id="',id,'" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:jr="http://openrosa.org/javarosa" xmlns:h="http://www.w3.org/1999/xhtml" xmlns:odk="http://www.opendatakit.org/xforms" xmlns:orx="http://openrosa.org/xforms" xmlns:ev="http://www.w3.org/2001/xml-events">')

    instance_id <- uuid::UUIDgenerate()

    random_number_of_characters <- sample(c(1:100),1)
    alphabet <- c(" ","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",".","!")
    instance_name <- paste0(sample(alphabet,random_number_of_characters, replace = T),collapse="")

    metadata_footer <- paste0('    <meta>\n        <instanceID>uuid:',instance_id,'</instanceID>\n        <instanceName>',instance_name,'</instanceName>\n    </meta>')
    footer <- '</data>'

    xml_string <- print_chunk_with_tab_spacing(xml_string,2)

    xml_string <- paste(header_1,data_header,xml_string,metadata_footer,footer,sep="\n")
    return(xml_string)
}
