#load libraries-----
library(tidyverse)

library(blastula)
library(glue)
library(here)
library(jsonlite)
library(paws)
library(quarto)

#messages-----
# if needed for testing, send_report_message() is located in ddh package

get_message <- function(){
  #https://github.com/paws-r/paws/blob/main/examples/sqs.R
  sqs <- paws::sqs()
  sqs$receive_message(
    QueueUrl = "https://sqs.us-east-1.amazonaws.com/344055253315/emailer"
  )
  
}

delete_message <- function(receipt_handle){
  sqs <- paws::sqs()
  sqs$delete_message(
    QueueUrl = "https://sqs.us-east-1.amazonaws.com/344055253315/emailer",
    ReceiptHandle = receipt_handle
  )
  message(glue::glue('Message {str_sub(receipt_handle, 1, 6)} deleted'))
}

#REPORTS-----
render_report <- function(input = list(), 
                          private){ #removed output_file
  #render everything in quarto dir
  report_base_dir = here::here("quarto")
  
  # create a temporary directory and make it our working directory
  temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
  dir.create(temp_dir)
  dir.create(glue::glue('{temp_dir}/figures')) #make a pretty folder for figures
  owd <- setwd(temp_dir)
  on.exit(setwd(owd))
  #delete later #on.exit(unlink(temp_dir, recursive = TRUE))
  
  # copy the qmd file into our temporary(current) directory
  qmd_files_from <- list.files(path = report_base_dir, pattern = "\\.qmd", full.names = TRUE)
  qmd_files_to <- list.files(path = report_base_dir, pattern = "\\.qmd", full.names = FALSE)
  file.copy(qmd_files_from, qmd_files_to, overwrite = TRUE) #setting wd makes this happen
  
  # set the filename of the qmd file we will use for rendering
  report_template <- "report_gene.qmd" #glue::glue("report_{input$type}.qmd")
  private_var <- if_else(private == TRUE, "true", "false")
  
  #good file names
  good_file_name <- input$content
  if (input$subtype == "gene_list" | input$subtype == "compound_list" | input$subtype == "cell_list") {
    good_file_name <- paste0("custom_", paste0(input$content, collapse="_"))
  }
  
  output_html_filename <- glue::glue("{good_file_name}_REPORT.html")
  output_zip_filename <- glue::glue("{good_file_name}.zip")
  
  quarto::quarto_render(input = here::here(temp_dir, report_template),
                        execute_params = list(type = input$type, 
                                              subtype = input$subtype,
                                              content = input$content, 
                                              private = private_var),
                        cache_refresh = TRUE, 
                        output_file = here::here(temp_dir, output_html_filename))
  
  #get report files dir
  report_image_dir <- glue::glue("report_{input$type}_files")
  
  #copy figures into better sub-dir
  image_files_from <- list.files(path = here::here(temp_dir, report_image_dir), pattern = "\\.png", recursive = TRUE, full.names = TRUE)
  image_files_to <- image_files_from %>% map_chr(~ stringr::str_replace(string = .x, pattern = glue::glue("{report_image_dir}/figure-html"), replacement = "figures"))
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

#render_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = TRUE) #output_file = "temp.zip"


make_report <- function(input = list(), 
                        private){
  #set report file to NULL
  report_file <- NULL
  good_file_name <- input$content
  if (input$subtype == "gene_list" | input$subtype == "compound_list" | input$subtype == "cell_list") {
    good_file_name <- paste0("custom_", paste0(input$content, collapse="_"))
  }
  report_filename <- glue::glue('{good_file_name}.zip') #"ADCK1.zip"
  
  #try to update file with path
  s3 <- paws::s3()
  report_file <- 
    tryCatch({
      s3$head_object(Bucket = "ddh-reports",
                     Key = report_filename) %>% 
        purrr::pluck("ContentType")}, #"application/zip"
      error = function(e) {
        NULL
      })
  
  if(!is.null(report_file)){ #if report exists on S3
    
    #make temp dir
    report_base_dir = here::here("quarto")
    temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
    dir.create(temp_dir)
    #delete later #on.exit(unlink(temp_dir, recursive = TRUE))
    
    #download file into temp dir
    report_path <- glue::glue("{temp_dir}/{report_filename}")
    s3$download_file(Bucket = "ddh-reports",
                     Key = report_filename, 
                     Filename = report_path)
    return(report_path)
    
  } else if(is.null(report_file)) { #if report zip doesn't exist
    
    #make report and return path
    report_path <- render_report(input = input, 
                                 private = private)
    
    #Upload file to s3 so others can use it (i.e. we don't have to make it again)
    s3$put_object(
      Body = as.character(report_path),
      Bucket = "ddh-reports",
      Key = as.character(glue::glue('{input$content}.zip'))
    )
    return(report_path)
  } else {
    print("something went wrong")
  }
}

# make_report(input = list(type = "gene", subtype = "gene", content = "ADCK1"), private = TRUE)
# make_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = TRUE)


send_email <- function(first_name,
                       email_address,
                       input = list(),
                       private){
  report_file <- make_report(input = input, 
                             private = private)
  
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
  #remove old tempdirs
  temp_dirs <- list.files(here::here("quarto"), pattern = "tmpdir")
  on.exit(unlink(temp_dirs, recursive = TRUE))
}

# send_email(first_name = "Matthew",
#            email_address = "matthew@hirschey.org",
#            input = list(type = "gene", subtype = "gene", query = "ROCK1", content = "ROCK1"),
#            private = TRUE)

#get sqs report-----
get_sqs_report <- function(){
  s3 <- paws::s3()
  sqs_list <- 
    s3$list_objects(Bucket = "ddh-sqs-logs") %>% 
    purrr::pluck("Contents")
  
  query_table <- tibble()
  
  for (i in seq_along(sqs_list)) {
    id <- sqs_list[[i]][["Key"]]
    
    csv_object <- 
      s3$get_object(
        Bucket = "ddh-sqs-logs", 
        Key = id
      ) %>% 
      purrr::pluck("Body") %>% 
      rawToChar() %>% 
      readr::read_csv(., show_col_types = FALSE)
    
    query_table <-
      query_table %>% 
      dplyr::bind_rows(csv_object)
    
    message(glue::glue("retreived query {i}"))
  }
  
  message("finished retreiving queries")
  return(query_table)
  
}
# query_table <- get_sqs_report()
