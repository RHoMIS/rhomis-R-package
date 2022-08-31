
#' Load RHoMIS Central
#'
#' Load Raw RHoMIS data from ODK central and
#' convert the column names into a shortened, standardised
#' version.
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK-central server you are using.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param central_email The email of the ODK-central account you are using.
#' ONLY RELEVANT IF "dataSource" WAS "central"
#' @param central_password The password of the ODK-central account you are using.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param project_name The name of the ODK-central project you are processing.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param form_name The name of the ODK-central form you are processing.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param central_test_case This flag is used for running a test-sample dataset from ODK the inst/sample_central_project/ folder
#' @param database The name of the database you would like to save results to
#' @param isDraft Whether or not the ODK form you are working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#' @return
#' @export
#'
#' @examples

load_rhomis_central <- function(
                                central_url=NULL,
                                central_email=NULL,
                                central_password=NULL,
                                project_name=NULL,
                                form_name=NULL,
                                database=NULL,
                                isDraft=NULL,
                                central_test_case=FALSE
                                ){


    # Checking if the right arguments are supplied to obtain data from ODK central
    items_to_test <- list("central_email",
                          "central_password",
                          "project_name",
                          "form_name",
                          "database",
                          "isDraft")
    null_variables <-sapply(items_to_test, function(x) is.null(get(x)))
    if(any(null_variables)){
        error_message <- paste(items_to_test[null_variables], collapse="\n")
        stop(paste0('You specified the data was coming from a ODK central. You need to define: \n',error_message))
    }

    if ( !(central_test_case) ){

        # Getting project and formID
        projectID <- get_project_id_from_name( project_name, central_url, central_email, central_password )

        # Finding form information from the API
        formID <- get_xml_form_id_from_name( form_name, projectID, central_url, central_email, central_password )

    } else {

        # Set empty strings for testing purposes
        projectID <- ""
        formID <- ""
    }

    rhomis_data <- get_submission_data(
        central_url,
        central_email,
        central_password,
        projectID,
        formID,
        form_name,
        isDraft )


    # Cleaning the column names
    colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data))
    # There are some extra central columns
    # which are problematic, these need to be removed
    rhomis_data <- rhomis_data %>%
        remove_extra_central_columns()

    rhomis_data <- make_id_columns(
        data = rhomis_data,
        country_column = pkg.env$identification_column_list$country,
        unique_id_col = "key",
        hh_id_col = NULL,
        id_type = "string",
        proj_id = project_name,
        form_id = form_name)

    # Convert the IDs to lower case
    rhomis_data<- convert_all_columns_to_lower_case(rhomis_data)

    # An extra step to ensure all -999 values are set to NA,
    # This wasn't always happening with ODK central datasets.
    rhomis_data <- sapply(rhomis_data, function(x){
        x[as.numeric(x)==-999]<-NA
        x
    }, simplify = F) %>% tibble::as_tibble()

    return(rhomis_data)

}



#' Get Email Token
#'
#' A function used to get the email token required to connext to ODK central
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url which points to an ODK Central sever
#' @param central_email The email linked to your ODK central account
#' @param central_password The password used for your central account
#'
#' @export
#'
#' @return This function will return a token. This token is useful for
#' @examples
get_email_token <- function(central_url, central_email, central_password) {
  data_for_request <- list(email = central_email, password = central_password)
  # Passing a handle with the request allows for
  # the use of cookies. Facilitating multiple requests.
  h <- httr::handle(central_url)
  central_response <- httr::POST(
    url = paste0(central_url, "/v1/sessions"),
    body = data_for_request,
    encode = "json",
    httr::add_headers("Content-Type" = "application/json"),
    handle = h
  )

  central_response_content <- httr::content(central_response)
  token <- central_response_content$token

  if (is.null(token)) {
    stop("Unable to return token.\n
         Please check the credentials you provided are
         correct by going to your URL and logging in.")
  }

  return(token)
}



#' Get Users
#'
#' Get a table of users from the ODK central database
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#'
#' @return A tibble of all of the users in the central database
#' @export
#'
#' @examples
get_users <- function(central_url, central_email, central_password) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(central_url, "/v1/users"),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_users <- httr::content(central_response)

  central_users <- central_results_to_df(central_users)
  return(central_users)
}


#' Get Project ID from name
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param project_name The name of the project from which you require the ID
#' @param central_url The url which points to an ODK Central sever
#' @param central_email The email linked to your ODK central account
#' @param central_password The password used for your central account
#'
#' @return
#' @export
#'
#' @examples
get_project_id_from_name <- function(project_name,
                                     central_url,
                                     central_email,
                                     central_password) {
  projects <- get_projects(
    central_url,
    central_email,
    central_password
  )
  projectID <- projects$id[projects$name == project_name]

  return(projectID)
}


#' Get Form ID from Name
#'
#' Sometimes it is necessary to get the form
#' ID from the project name, in particular
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param form_name The name of the form for which you would like the ID
#' @param central_url The url which points to an ODK Central sever
#' @param central_email The email linked to your ODK central account
#' @param central_password The password used for your central account
#' @param projectID The ID for the project you are searching for
#'
#' @return
#' @export
#'
#' @examples
get_xml_form_id_from_name <- function(form_name,
                                      projectID,
                                      central_url,
                                      central_email,
                                      central_password) {
  forms <- get_forms(
    central_url,
    central_email,
    central_password,
    projectID
  )
  formID <- forms$xmlFormId[forms$name == form_name]
  return(formID)
}

#' Get Projects
#'
#' Get a table of all of the projects on the ODK central server
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#'
#' @return A table of all of the projects and their details
#' @export
#'
#' @examples
#' central_url <- "https://central.rhomis.cgiar.org"
#' central_email <- "my_email"
#' central_password <- "my_password"
#' # get_projects(central_url,central_email,central_password)
get_projects <- function(central_url, central_email, central_password) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(central_url, "/v1/projects"),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_projects <- httr::content(central_response)
  central_projects <- central_results_to_df(central_projects)
  return(central_projects)
}


#' Delete Project
#'
#' Delete a project from ODK central based on its ID
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are hoping to delete
delete_project <- function(central_url,
                           central_email,
                           central_password,
                           projectID) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::DELETE(
    url = paste0(central_url, "/v1/projects/", projectID),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_projects <- httr::content(central_response)
  print(central_projects)
}

create_project <- function(central_url,
                           central_email,
                           central_password,
                           project_name) {
  central_url <- "https://central.rhomis.cgiar.org"
  central_email <- "leomgorman@outlook.com"
  central_password <- "mang0.T.118"
  email_token <- get_email_token(central_url, central_email, central_password)

  project_name <- "project_made_by_r"

  data_for_request <- list(name = project_name)


  central_response <- httr::POST(
    url = paste0(central_url, "/v1/projects/"),
    encode = "json",
    body = data_for_request,
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_projects <- httr::content(central_response)
  print(central_projects)
}

#' Get forms
#'
#' Get the forms for a specific ODK central project
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the
#' "get_projects" function
#'
#' @return A table of the forms available for a specific project
#' @export
#'
#' @examples
get_forms <- function(central_url, central_email, central_password, projectID) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(central_url, "/v1/projects/", projectID, "/forms"),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_forms <- httr::content(central_response)
  central_forms <- central_results_to_df(central_forms)
  return(central_forms)
}


#' Get XLS Form
#'
#' Allows the user to get an ODK central xls form
#' in order to extract any meta-data
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at. To get a
#' list of project, see the
#' @param formID The XML form ID from a specific project
#' @param form_version The version of the form you are examining. For now
#' we presume you are looking for the first version of the form
#' @param isDraft Stating whether or not the form is a draft
#'
#' @return
#' @export
#'
#' @examples
get_xls_form <- function(central_url, central_email, central_password, projectID, formID,  isDraft = T) {
  if (isDraft) {
    url <- paste0(central_url, "/v1/projects/", projectID, "/forms/", formID, "/draft.xlsx")
  } else {
    url <- paste0(central_url, "/v1/projects/", projectID, "/forms/", formID, ".xlsx")
  }

  file_destination <- tempfile(fileext = ".xls")
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = url,
    encode = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token)),
    httr::write_disk(file_destination, overwrite = TRUE)
  )

  response <- httr::content(central_response)



  xls_form <- list()
  xls_form$survey <- readxl::read_xlsx(file_destination, sheet = "survey")
  xls_form$choices <- readxl::read_xlsx(file_destination, sheet = "choices")
  xls_form$settings <- readxl::read_xlsx(file_destination, sheet = "settings")
  unlink(file_destination)

  return(xls_form)
}


#' Get xls survey file
#'
#' A function for getting the xls survey from odk central,
#' writing it to a file, and returning the file path
#'
#' Rpackage file: utils-odk-central.R

#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the
#' "get_projects" function
#' @param formID The XML form ID from a specific project
#' @param form_version The version of the form you are examining. For now
#' we presume you are looking for the first version of the form
#' @param file_destination Allow the user to specify the destination of
#' the xls file they are working with.
#' include the file name and the extension. Must make sure the directory exists
#'
#' @return
#' @export
#'
#' @examples
get_xls_survey_file <- function(central_url,
                                central_email,
                                central_password,
                                projectID,
                                formID,
                                file_destination = NULL,
                                form_version = 1) {
  if (is.null(file_destination)) {
    file_destination <- tempfile(fileext = ".xls")
  }
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(
      central_url,
      "/v1/projects/",
      projectID,
      "/forms/",
      formID,
      "/versions/",
      form_version,
      ".xlsx"
    ),
    encode = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token)),
    httr::write_disk(file_destination, overwrite = TRUE)
  )

  xls_form <- readxl::read_xlsx(file_destination, sheet = "survey")

  return(xls_form)
}

#' Extract Form Metadata
#'
#' Extract the metadata for a RHoMIS project
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the
#' "get_projects" function
#' @param formID The XML form ID from a specific project
#' @param form_version The version of the form you are examining. For now
#' we presume you are looking for the first version of the form
#' @param isDraft Whether or not the form was a draft
#'
#' @return
#' @export
#'
#' @examples
extract_form_metadata <- function(central_url, central_email, central_password, projectID, formID, form_version = 1, isDraft) {
  xls_form <- get_xls_form(central_url, central_email, central_password, projectID, formID, form_version, isDraft)

  metadata <- xls_form[c("metadata_variable", "metadata_value")]
  metadata <- metadata[!is.na(metadata["metadata_variable"]) & !is.na(metadata["metadata_value"]), ]

  return(metadata)
}


#' Get Submissions List
#'
#' A function for seeing which submissions have been made for
#' a specific form, from a specific project
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at. To get a list of project, see the
#' "get_projects" function
#' @param formID The ID of the form containing the submissions you are looking at.
#' To get the list of forms see the "get_forms" function.
#'
#' @return
#' @export
#'
#' @examples
get_submissions_list <- function(central_url,
                                 central_email,
                                 central_password,
                                 projectID,
                                 formID) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(
      central_url,
      "/v1/projects/",
      projectID,
      "/forms/",
      formID,
      "/submissions"
    ),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_submissions <- httr::content(central_response)
  central_submissions <- central_results_to_df(central_submissions)
  return(central_submissions)
}

#' Submission All
#'
#' Get a list of the submissions for a specific form
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the "get_projects" function
#' @param formID The ID of the form containing the submissions
#' you are looking at.
#' To get the list of forms see the "get_forms" function.
#'
#' @return
#' @export
#'
#' @examples
submissions_all <- function(central_url,
                            central_email,
                            central_password,
                            projectID,
                            formID) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(
      central_url,
      "/v1/projects/",
      projectID,
      "/forms/",
      formID,
      "/submissions"
    ),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_submissions <- httr::content(central_response)
  central_submissions <- central_results_to_df(central_submissions)

  return(central_submissions)
}

#' Get Submission XML
#'
#' Get the XML of an ODK submission
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the
#' "get_projects" function
#' @param formID The ID of the form containing the submissions
#' you are looking at.
#' To get the list of forms see the "get_forms" function.
#' @param submissionID The instance ID of the specific submission we want in XML
#'
#' @return
#' @export
#'
#' @examples
get_submission_xml <- function(central_url,
                               central_email,
                               central_password,
                               projectID,
                               formID,
                               submissionID) {
  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::GET(
    url = paste0(
      central_url, "/v1/projects/",
      projectID,
      "/forms/",
      formID,
      "/submissions/",
      submissionID, ".xml"
    ),
    encode = "json",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_submission <- httr::content(central_response)
  submission_xml <- paste0(central_submission, collapse = "\n")
  return(submission_xml)
}


#' Submit XML Data
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param xml_string The XML string for the data submission you
#' would like to make
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the
#' "get_projects" function
#' @param formID The ID of the form containing the submissions you are looking at.
#' To get the list of forms see the "get_forms" function.
#' @param isDraft Whether the form you are submitting to is a draft or not
#'
#' @return
#' @export
#'
#' @examples
submit_xml_data <- function(xml_string, central_url, central_email, central_password, projectID, formID, isDraft) {
  deviceID <- uuid::UUIDgenerate()
  instanceID <- uuid::UUIDgenerate()

  unknownID <- gsub(".*<instanceID>uuid:", "", xml_string)
  unknownID <- gsub("</instanceID>.*", "", unknownID)


  xml_string <- gsub(unknownID, instanceID, xml_string)

  if (isDraft) {
    url <- paste0(central_url, "/v1/projects/", projectID, "/forms/", formID, "/draft/submissions?deviceID=", deviceID)
  } else {
    url <- paste0(central_url, "/v1/projects/", projectID, "/forms/", formID, "/submissions?deviceID=", deviceID)
  }

  email_token <- get_email_token(central_url, central_email, central_password)
  central_response <- httr::POST(
    url = url,
    body = xml_string,
    encode = "raw",
    httr::add_headers("Authorization" = paste0("Bearer ", email_token))
  )
  central_submission <- httr::content(central_response)
  submission_xml <- paste0(central_submission, collapse = "\n")
  return(submission_xml)
}


#' Get Submission Data
#'
#' A function for retrieving all of the ODK data as a zip file
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at.
#' To get a list of project, see the
#' "get_projects" function
#' @param formID The ID of the form containing the
#' submissions you are looking at.
#' @param form_name The name of the form you are downloading
#' To get the list of forms see the "get_forms" function.
#' @param isDraft Whether or not the form is a draft or whether it is finalized
#' @param file_destination The location to store temporary files
#'
#' @return
#' @export
#'
#' @examples


get_submission_data <- function(central_url, central_email, central_password, projectID, formID, form_name, isDraft, file_destination=NULL){

    # Test case:
    # (If running a test case then the formID and projectID will be empty strings)
    if (formID=="" & projectID==""){
        form_name <- "dec-demo"
        file_destination <- tempfile(fileext=".zip")
        central_response <- download.file(url = central_url, destfile = file_destination)

    # Fetching actual submission data (not test case)
    } else {

        email_token <- get_email_token(central_url,central_email,central_password)
        if (is.null(file_destination))
        {
            file_destination <- tempfile(fileext=".zip")
        }

        if (isDraft) {
            url <- paste0(central_url, "/v1/projects/", projectID, "/forms/", formID, "/draft/submissions.csv.zip?attachments=false")
        } else {
            url <- paste0(central_url, "/v1/projects/", projectID, "/forms/", formID, "/submissions.csv.zip?attachments=false")
        }


        central_response <- httr::GET(url = url,
                                      encode = "json",
                                      httr::add_headers("Authorization" = paste0("Bearer ",email_token)),
                                      httr::write_disk(file_destination, overwrite = TRUE))
    }

    # check whether the zip file from ODK has been successfully written to disk
    if ( !(file.exists(file_destination)) ){
        stop("Error: cannot find temporary zip archive ",file_destination)
    }

    # unzip
    files_names <- unzip(file_destination)

    files <-sapply(files_names, function(x){
        readr::read_csv(x, col_types = readr::cols(), na = c("n/a", "-999", "-99", "NA"), locale = readr::locale(encoding = "latin1"))
    }, simplify=F)




    # read in the core data file list
    main_df_file_name <- paste0("./",form_name, ".csv")
    main_data_set <- files[[main_df_file_name]]

    remaining_files <- files[names(files)!=main_df_file_name]

    # loop over the repeat columns to download the data files individually
    # these files can be formatted in a non-standard way, hence they are downloaded separately and then combined with the core data
    for (remaining_file in remaining_files){



        # read in repeat files

        # reformat the loop column names from the ODK files to match with rhomis dataset syntax and join with core data
        main_data_set <- central_loops_to_rhomis_loops(main_data_set, remaining_file)

    }
    for (file_to_delete in files_names){
        unlink(file_to_delete)

    }


    # clean up column names in core, but no need to pass repeat column names here
    # (as they are reformated in central_loops_to_rhomis and require a different treatment when retrieved from ODK in this way)
    colnames(main_data_set) <- clean_column_names(colnames(main_data_set))

    # Removing duplicate "deviceid" column
    if (sum(colnames(main_data_set) == "deviceid") > 1) {
        column_to_keep <- which(colnames(main_data_set) == "deviceid" & colSums(is.na(main_data_set)) == 0)
        column_to_remove <- which(colnames(main_data_set) == "deviceid" & colSums(is.na(main_data_set)) > 0)

        main_data_set <- main_data_set[-column_to_remove]
    }

    unlink(file_destination)

  return(main_data_set)
}


#----------------------------------------------------------
#' Convert the nested list of central results into a tibble
#'
#' ODK central returns context in the form of a nested list. This converts
#' this nested list into a more accessible tibble format
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param central_results The nested list returned from a central query
#'
#' @return A table of central results
#' @export
#'
#' @examples
#' central_results <- list(
#'   list("id" = 1, "name" = "name1", "email" = "email1"),
#'   list("id" = 2, "name" = "name2", "email" = "email2"),
#'   list("id" = 3, "name" = "name3", "email" = "email3")
#' )
#' central_results_to_df(list(
#'   list("id" = 1, "name" = "name1", "email" = "email1"),
#'   list("id" = 2, "name" = "name2", "email" = "email2"),
#'   list("id" = 3, "name" = "name3", "email" = "email3")
#' ))
central_results_to_df <- function(central_results) {
  # replace nulls with NA

  # Identifying all the null values in the nested list
  # structure returned by the request
  subsets <- sapply(1:length(central_results), function(x) {
    sapply(central_results[[x]], is.null, simplify = F)
  }, simplify = F)

  # Going through each value of the nested loop, replacing the
  # Nulls with the NAs for each subset
  central_results <- sapply(1:length(central_results), function(x) {
    central_results[[x]][unlist(subsets[[x]])] <- NA
    return(central_results[[x]])
  }, simplify = F)

  # Identifying all of the necessary column headers
  column_headers <- unique(names(unlist(central_results)))
  all_tibbles <- sapply(central_results, function(x) {
    widen_individual_result(x)
  },
  simplify = F
  )


  final_df <- dplyr::bind_rows(all_tibbles)
  return(final_df)
}

#' Widen Individual Central Results
#'
#' Widening a single central result into a simple tibble
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param individual_central_item The individual list item subsetted
#' from a central query
#' @param column_headers The desired headers for the new table
#'
#' @return
#' @export
#'
#' @examples
#' central_results <- list(
#'   list("id" = 1, "name" = "name1", "email" = "email1"),
#'   list("id" = 2, "name" = "name2", "email" = "email2"),
#'   list("id" = 3, "name" = "name3", "email" = "email3")
#' )
#' column_headers <- unique(names(unlist(central_results)))
#' individual_central_result <- central_results[2]
#' widen_individual_result(individual_central_result, column_headers)
widen_individual_result <- function(individual_central_item, column_headers) {
  item_to_tibble <- stack(unlist(individual_central_item)[column_headers]) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = "ind", values_from = "values") %>%
    dplyr::mutate_all(as.character)

  return(item_to_tibble)
}


#' Remove Extra Central Columns
#'
#' Data downloaded from ODK central has extra columns
#' with no additional information. These extra columns
#' follow a particular pattern. This function removes these
#' columns based on this pattern
#'
#' Rpackage file: utils-odk-central.R
#'
#' @param data ODK central data with the excess columns
#'
#' @return
#' @export
#'
#' @examples
remove_extra_central_columns <- function(data) {
  extra_columns <- grepl("no[[:digit:]]+_+[[:digit:]]", colnames(data))
  data <- data[extra_columns == F]

  return(data)
}
