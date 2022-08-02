#LOAD-----
library(tidyverse)

library(blastula)
library(glue)
library(here)
library(jsonlite)
library(paws)
library(quarto)
library(remotes)
library(ddh)

#remotes::install_github("matthewhirschey/ddh")

# load_ddh_data(app_data_dir = here::here("data"),
#               privateMode = TRUE)

# download_ddh_data(app_data_dir = here::here("data"),
#                   test = TRUE,
#                   overwrite = FALSE)

# download_ddh_data(app_data_dir = here::here("data"),
#                   object_name = "achilles_lower",
#                   test = TRUE,
#                   overwrite = TRUE)


#' Function to download all DDH .Rds files
#'
#' @param app_data_dir Data directory path.
#' @param object_name Optional object name to get a single file; default gets all files in bucket
#' @param test Boolean that will download test data instead of full data
#' @param overwrite Boolean that deletes the app_data_dir thereby removing files, before remaking dir and downloading all files
#' @param privateMode Boolean indicating if private data is required.
#'
#' @export
download_ddh_data <- function(app_data_dir,
                              object_name = NULL,
                              test = FALSE,
                              overwrite = FALSE,
                              privateMode = TRUE){
  #delete dir to overwrite
  if(overwrite == TRUE) {
    path <- stringr::str_glue("{app_data_dir}/{object_name}.Rds")
    path %>% purrr::walk(unlink, recursive = TRUE) #handles 1, many, or NULL file_name
  }
  #make dir
  if(!dir.exists(app_data_dir)){
    dir.create(app_data_dir)
  }
  #get_data function
  get_aws_data <- function(bucket_id, 
                           object_name){
    bucket_name <- Sys.getenv(bucket_id)
    s3 <- paws::s3()
    data_objects <- 
      s3$list_objects(Bucket = bucket_name) %>% 
      purrr::pluck("Contents")
    
    print(glue::glue('{length(data_objects)} objects in the {bucket_name} bucket'))
    
    #filter list for single object
    if(!is.null(object_name)){
      file_name <- glue::glue("{object_name}.Rds")
      data_objects <- 
        data_objects %>% #take full list
        keep(purrr::map_lgl(.x = 1:length(data_objects), ~ data_objects[[.x]][["Key"]] %in% file_name)) #pass map_lgl to keep to filter names to keep
      print(glue::glue('filtered to keep only {length(data_objects)}'))
    }
    
    for (i in 1:length(data_objects)) {
      #check for no objects
      if(length(data_objects) == 0){ 
        print(glue::glue("file downloaded: {file_name}"))
      } else {
        file_name <- data_objects[[i]][["Key"]]
        #check if files exists
        if(file.exists(glue::glue("{app_data_dir}/{file_name}"))){
          print(glue::glue("file already exists: {file_name}"))
        } else {
          #if not, then download
          s3$download_file(Bucket = bucket_name,
                           Key = as.character(file_name),
                           Filename = as.character(glue::glue("{app_data_dir}/{file_name}")))
          print(glue::glue("file downloaded: {file_name}"))
        }
      }
    }
  }
  #get test data
  if(test == TRUE){
    get_aws_data(object_name, 
                 bucket_id = "AWS_DATA_BUCKET_ID_TEST")
    return(print("test data download complete"))
  }
  #get data
  if(privateMode == FALSE){   #get public data only
    
    get_aws_data(object_name, 
                 bucket_id = "AWS_DATA_BUCKET_ID_TEST")
    return(print("public data download complete"))
  } else { #get both
    get_aws_data(object_name, 
                 bucket_id = "AWS_DATA_BUCKET_ID")
    get_aws_data(object_name, 
                 bucket_id = "AWS_DATA_PRIVATE_BUCKET_ID")
    return(print("private data download complete"))
  }
}


#' Function to Load All DDH Data Including .Rds Files and Colors
#'
#' @param app_data_dir Data directory path.
#' @param file_name Optional file name to get a single file; default loads all files
#' @param privateMode Boolean indicating if private data is required.
#'
#' @export
load_ddh_data <- function(app_data_dir,
                          object_name = NULL) {
  
  # Load .RDS files
  load_ddh_rds(app_data_dir, 
               object_name)
  message("loaded Rds files")
  if(!is.null(object_name)){ #stop here
    return(message("done"))
  }
  
  #load DB cons
  load_ddh_db()
  message("loaded db connections")
  
  # Load colors
  load_ddh_colors()
  message("loaded colors")
  
  message("finished loaing")
}

#' Function to load all DDH .RDS files
#'
#' @param app_data_dir Data directory path.
#' @param object_name Optional object name to load a single file; default loads all files
#'
#' @export
load_ddh_rds <- function(app_data_dir, 
                         object_name = NULL) {
  if(is.null(object_name)){
    all_objects <- list.files(app_data_dir) %>% 
      purrr::map_chr(stringr::str_remove, pattern = "\\.Rds")
  } else {
    all_objects <- object_name
  }
  
  #file loader constructor
  load_rds_object <- function(single_object){
    file_name <- glue::glue("{single_object}.Rds")
    assign(single_object, readRDS(here::here(app_data_dir, file_name)), 
           envir = .GlobalEnv)
    message(glue::glue("loaded {single_object}"))
  }
  
  #walk through to load all files
  all_objects %>% purrr::walk(load_rds_object)
  
  #print done
  message("finished loading")
}

#' Function to load all DDH db connections
#'
#' @param object_name Optional object name to load a single db connection; default loads all connections
#'
#' @export
load_ddh_db <- function(object_name = NULL) {
  #fun for db connection
  #load_db_connection <- function(db_name){}
  
  #manually (?) list all dbs to connect to
  
  #filter list for those in file_name
  #db_cons <- 
  # if(is.null(file_name)){
  #   db_cons <-  #manually (?) list all dbs to connect to
  # } else {
  #   db_cons <- file_name
  # }
  
  #db_cons %>% purrr::walk(load_db_connection)
  
  #placeholder for db connections
  message("placeholder for db connections")
}

#MESSAGES-----
# if needed for testing, send_report_message() is located in ddh package

get_message <- function(){
  #https://github.com/paws-r/paws/blob/main/examples/sqs.R
  sqs <- paws::sqs()
  sqs$receive_message(
    QueueUrl = Sys.getenv("AWS_SQS_SERVICE_URL")
  )
  
}

delete_message <- function(receipt_handle){
  sqs <- paws::sqs()
  sqs$delete_message(
    QueueUrl = Sys.getenv("AWS_SQS_SERVICE_URL"),
    ReceiptHandle = receipt_handle
  )
  message(glue::glue('Message {str_sub(receipt_handle, 1, 6)} deleted'))
}

#REPORTS-----
render_report <- function(input = list(), 
                          private, 
                          report_dir = NULL){ #removed output_file
  if(is.null(report_dir)){
    #render everything in quarto dir
    report_base_dir = here::here("quarto")
    # create a temporary directory
    temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
    dir.create(temp_dir)
  } else {
    temp_dir <- report_dir
  }
  #make a pretty folder for figures
  dir.create(glue::glue('{temp_dir}/figures')) 
  #make tempdir our working directory
  owd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(owd))
  
  # copy the qmd file into our temporary(current) directory
  qmd_files_from <- list.files(path = report_base_dir, pattern = "\\.qmd", full.names = TRUE)
  qmd_files_to <- list.files(path = report_base_dir, pattern = "\\.qmd", full.names = FALSE)
  file.copy(qmd_files_from, qmd_files_to, overwrite = TRUE) #setting wd makes this happen
  
  # set the filename of the qmd file we will use for rendering
  report_template <- glue::glue("report_{input$type}.qmd") #"report_gene_dummy.qmd"
  private_var <- if_else(private == TRUE, "true", "false")
  
  #good file names
  good_file_name <- input$content
  if (input$subtype == "gene_list" | input$subtype == "compound_list" | input$subtype == "cell_list") {
    good_file_name <- paste0("custom_", paste0(input$content, collapse=""))
  }
  
  output_html_filename <- glue::glue("{good_file_name}_REPORT.html")
  output_zip_filename <- glue::glue("{good_file_name}.zip")
  
  quarto::quarto_render(input = here::here(temp_dir, report_template),
                        execute_params = list(type = input$type, 
                                              subtype = input$subtype,
                                              query = input$query,
                                              content = input$content, 
                                              private = private_var),
                        cache_refresh = TRUE, 
                        output_file = here::here(temp_dir, output_html_filename))
  
  #get report files dir
  report_image_dir <- glue::glue("report_{input$type}_files")
  
  #copy figures into better sub-dir
  image_files_from <- list.files(path = here::here(temp_dir, report_image_dir), pattern = "\\.png", recursive = TRUE, full.names = TRUE)
  image_files_to <- 
    image_files_from %>% 
    purrr::map_chr(~ stringr::str_replace(string = .x, pattern = glue::glue("{report_image_dir}/figure-html"), replacement = "figures")) %>% 
    purrr::map_chr(~ stringr::str_replace(string = .x, pattern = "1", replacement = good_file_name))
  file.copy(image_files_from, image_files_to, overwrite = TRUE)
  unlink(here::here(temp_dir, report_image_dir), recursive = TRUE) #remove so list.files works below
  
  #match pngs
  images <- list.files(path = here::here(temp_dir),
                       pattern = "\\.png",
                       recursive = TRUE) #
  
  # to keep the quarto sub-directory structure we will zip from within /quarto
  # append filenames relative to /quarto directorybasename
  zip_filenames <- c(output_html_filename)
  zip_filenames <- append(zip_filenames, images) #glue::glue('{report_image_dir}/{images})')
  print(zip_filenames)
  
  #zip
  final_zip_path <- glue::glue("{temp_dir}/{output_zip_filename}")
  zip(zipfile = final_zip_path, 
      files = zip_filenames)
  
  return(final_zip_path)
}

#render_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = TRUE)
#render_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = FALSE)

make_report <- function(input = list(), 
                        private, 
                        report_dir = NULL, 
                        overwrite = FALSE){
  
  good_file_name <- input$content
  if (input$subtype == "gene_list" | input$subtype == "compound_list" | input$subtype == "cell_list") {
    good_file_name <- paste0("custom_", paste0(input$content, collapse=""))
  }
  report_filename <- glue::glue('{good_file_name}.zip') #"ADCK1.zip"
  
  #checks to see if report exists by trying to update report_file with path
  s3 <- paws::s3()
  report_file <- 
    tryCatch({
      s3$head_object(Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
                     Key = report_filename) %>% 
        purrr::pluck("ContentType")}, #"application/zip"
      error = function(e) {
        NULL #set report file to NULL
      })
  
  #overwrite
  if(overwrite == TRUE){report_file <- NULL} #
  
  if(!is.null(report_file)){ #if report exists on S3
    if(is.null(report_dir)){
      #make temp dir
      report_base_dir = here::here("quarto")
      temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
      dir.create(temp_dir)
    } else {
      temp_dir <- report_dir
    }
    
    #download file into temp dir
    report_path <- glue::glue("{temp_dir}/{report_filename}")
    s3$download_file(Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
                     Key = report_filename, 
                     Filename = report_path)
    message("grabbed old report: way to be efficient")
    return(report_path)
    
  } else if(is.null(report_file)) { #if report zip doesn't exist
    
    #make report and return path
    report_path <- render_report(input = input, 
                                 private = private)
    
    #Upload file to s3 so others can use it (i.e. we don't have to make it again)
    s3$put_object(
      Body = as.character(report_path),
      Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
      Key = as.character(report_filename) #need as.character because of glue::glue
    )
    message("uploaded fresh report: nice work")
    return(report_path)
  } else {
    print("something went wrong")
  }
}

# make_report(input = list(type = "gene", subtype = "gene", content = "ROCK1"), private = TRUE)
# make_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = TRUE, overwrite = TRUE)


send_email <- function(first_name,
                       email_address,
                       input = list(), 
                       private){
  
  #make temp dir
  report_base_dir = here::here("quarto")
  temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
  dir.create(temp_dir)
  
  report_file <- make_report(input = input, 
                             private = private, 
                             report_dir = temp_dir)
  
  #make pretty https://pkgs.rstudio.com/blastula/reference/index.html#section-email-sending-through-smtp
  email_body <- 
    compose_email( #https://github.com/rstudio/blastula/issues/226
      header = add_image(file = here::here("ddh YouTube banner.png"), width = "100%"),
      body = render_email(input = here::here("email_template.Rmd"), 
                          envir = parent.frame(),
                          render_options = list(params = 
                                                  list(name = first_name, 
                                                       type = input$type,
                                                       subtype = input$subtype,
                                                       query = input$query,
                                                       content = input$content,
                                                       private = private)
                          ))$html_html,
      footer = blocks(
        #block_text("Thanks for reading! Find us here:"),
        block_social_links(
          social_link(
            service = "website",
            link = dplyr::if_else(private == TRUE, "https://www.datadrivenhypothesis.com", "https://www.datadrivenhypothesis.org"),
            variant = "bw"
          ),
          social_link(
            service = "twitter",
            link = "https://www.twitter.com/dd_hypothesis",
            variant = "color"
          ),
          social_link(
            service = "youtube",
            link = "https://www.youtube.com/channel/UCstAS6IpgYgZ9Cvcag_YlCQ",
            variant = "color"
          ),
          social_link(
            service = "github",
            link = "https://github.com/matthewhirschey",
            variant = "color"
          )
        )
      )
    )
  
  #attach report
  email_body <- add_attachment(email = email_body,
                               file = report_file)
  
  # email_body #preview it
  email_body %>% 
    smtp_send(
      from = "matthew@hirschey.org",
      to = email_address,
      subject = "DDH report",
      credentials = creds_envvar(
        provider = "gmail",
        user = Sys.getenv("SMTP_USERNAME")
        #pass_envvar = Sys.getenv("SMTP_PASSWORD"), #use default
      )
    )
  #remove tempdirs
  on.exit(unlink(temp_dir, recursive = TRUE))
}

# send_email(first_name = "Matthew",
#            email_address = "matthew@hirschey.org",
#            input = list(type = "gene", subtype = "gene", query = "ROCK2", content = "ROCK2"),
#            private = TRUE)

get_sqs_report <- function(){
  s3 <- paws::s3()
  sqs_list <- 
    s3$list_objects(Bucket = Sys.getenv("AWS_SQS_BUCKET_ID")) %>% 
    purrr::pluck("Contents")
  
  query_table <- tibble()
  
  for (i in seq_along(sqs_list)) {
    id <- sqs_list[[i]][["Key"]]
    
    object <- 
      s3$get_object(
        Bucket = Sys.getenv("AWS_SQS_BUCKET_ID"), 
        Key = id
      ) 
    
    timestamp <- 
      object %>% 
      purrr::pluck("LastModified")
    
    csv_object <- 
      object %>% 
      purrr::pluck("Body") %>% 
      rawToChar() %>% 
      readr::read_csv(., show_col_types = FALSE) %>% 
      dplyr::bind_cols(timestamp = timestamp) %>% 
      dplyr::select(timestamp, everything())
    
    query_table <-
      query_table %>% 
      dplyr::bind_rows(csv_object)
    
    message(glue::glue("retreived query {i}"))
  }
  
  message("finished retreiving queries")
  return(query_table)
  
}
# query_table <- get_sqs_report()
