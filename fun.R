#LOAD-----
library(tidyverse)

library(blastula)
library(glue)
library(here)
library(jsonlite)
library(paws)
library(quarto)
library(remotes)

#remotes::install_github("matthewhirschey/ddh")

# load_ddh_data(aws_download = TRUE,
#               app_data_dir = here::here("data"),
#               privateMode = TRUE)

#' Function to Load All DDH Data Including .RDS Files and Colors
#'
#' @param aws_download Boolean to indicate if data should be downloaded from AWS S3 bucket.
#' @param overwrite Boolean that deletes the app_data_dir thereby removing files, before remaking dir and downloading all files
#' @param app_data_dir Data directory path.
#' @param privateMode Boolean indicating if private data is required.
#'
#' @export
load_ddh_data <- function(aws_download = FALSE, 
                          overwrite = FALSE,
                          app_data_dir = NULL,
                          privateMode = TRUE) {
  if(aws_download == TRUE){ 
    #delete dir to overwrite
    if(overwrite == TRUE) {
      unlink(app_data_dir, recursive = TRUE)
    }
    #make dir
    if(!dir.exists(app_data_dir)){
      dir.create(app_data_dir)
    }
    #get_data function
    get_aws_data <- function(bucket_id){
      s3 <- paws::s3()
      bucket_name <- Sys.getenv(bucket_id)
      data_objects <- 
        s3$list_objects(Bucket = bucket_name) %>% 
        purrr::pluck("Contents")
      
      print(glue::glue('{length(data_objects)} objects in the {bucket_name} bucket'))
      
      for (i in 1:length(data_objects)) {
        file_name <- data_objects[[i]][["Key"]]
        #check if files exists
        if(file.exists(glue::glue("{app_data_dir}/{file_name}"))){
          print(glue::glue("file already exists: {file_name}"))
        } else {
          #if not, thne download
          s3$download_file(Bucket = bucket_name,
                           Key = file_name,
                           Filename = glue::glue("{app_data_dir}/{file_name}"))
          print(glue::glue("file downloaded: {file_name}"))
        }
      }
    }
    #get data
    get_aws_data(bucket_id = "AWS_DATA_BUCKET_ID")
    
    #get private data
    if(privateMode == TRUE){ get_aws_data(bucket_id = "AWS_DATA_PRIVATE_BUCKET_ID")}
  }
  # Load .RDS files
  load_ddh_rds(app_data_dir = app_data_dir)
  
  # Load colors
  load_ddh_colors()
  
}

#' Function to load all DDH .RDS files
#'
#' @param app_data_dir Data directory path.
#'
#' @export
load_ddh_rds <- function(app_data_dir = NULL) {
  
  data_files <- list.files(app_data_dir)
  
  #file loader constructor
  load_rds_object <- function(file_name){
    object_name <- stringr::str_remove(file_name, pattern = "\\.Rds")
    assign(object_name, readRDS(here::here(app_data_dir, file_name)), 
           envir = .GlobalEnv)
    print(glue::glue("loaded {object_name}"))
  }
  
  #walk through to load all files
  data_files %>% purrr::walk(load_rds_object)
  
  #print done
  print("loaded them all")
}

#' Function to load DDH colors
#'
#' @export
load_ddh_colors <- function() {
  ## MAIN COLORS -----------------------------------------------------------------
  ##2EC09C  ## cyan
  ##BE34EF  ## violet
  ##E06B12  ## orange
  ##004AAB  ## blue
  ##F0CE44  ## yellow
  ##1785A4  ## blend cyan + blue
  
  load_colors <- function() {
    ## Color sets  for genes
    color_set_gene <- generate_colors("#2EC09C")  ## cyan
    #CC is the hex alpha conversion for 80%, so the next line adds it; used in graph
    color_set_gene_alpha <- purrr::map_chr(color_set_gene, ~ glue::glue_collapse(c(.x, "CC"), sep = ""))
    
    ## Palette function for genes
    color_pal_gene <- grDevices::colorRampPalette(color_set_gene)
    
    ## Color sets for proteins
    color_set_protein <- generate_colors("#004AAB")  ## blue
    color_set_protein_alpha <- purrr::map_chr(color_set_protein, ~ glue::glue_collapse(c(.x, "CC"), sep = ""))
    
    ## Palette function for proteins
    color_pal_protein <- grDevices::colorRampPalette(color_set_protein)
    
    ## Color sets for proteins
    color_set_geneprotein <- generate_colors("#1785A4")  ## blue
    color_set_geneprotein_alpha <- purrr::map_chr(color_set_geneprotein, ~ glue::glue_collapse(c(.x, "CC"), sep = ""))
    
    ## Palette function for proteins
    color_pal_geneprotein <- grDevices::colorRampPalette(color_set_geneprotein)
    
    ## Color sets for cells
    color_set_cell <- generate_colors("#BE34EF")  ## violet
    color_set_cell_alpha <- purrr::map_chr(color_set_cell, ~ glue::glue_collapse(c(.x, "CC"), sep = ""))
    
    ## Palette function for cells
    color_pal_cell <- grDevices::colorRampPalette(color_set_cell)
    
    ## Color sets for compounds
    color_set_compound <- generate_colors("#E06B12")  ## orange
    color_set_compound_alpha <- purrr::map_chr(color_set_compound, ~ glue::glue_collapse(c(.x, "CC"), sep = ""))
    
    ## Palette function for compounds
    color_pal_compound <- grDevices::colorRampPalette(color_set_compound)
    
    return(list(color_set_gene=color_set_gene,
                color_set_gene_alpha=color_set_gene_alpha,
                color_pal_gene=color_pal_gene,
                color_set_protein=color_set_protein,
                color_set_protein_alpha=color_set_protein_alpha,
                color_pal_protein=color_pal_protein,
                color_set_geneprotein=color_set_geneprotein,
                color_set_geneprotein_alpha=color_set_geneprotein_alpha,
                color_pal_geneprotein=color_pal_geneprotein,
                color_set_cell=color_set_cell,
                color_set_cell_alpha=color_set_cell_alpha,
                color_pal_cell=color_pal_cell,
                color_set_compound=color_set_compound,
                color_set_compound_alpha=color_set_compound_alpha,
                color_pal_compound=color_pal_compound)
    )
  }
  
  ddh_colors <- load_colors()
  list2env(ddh_colors, .GlobalEnv)
  
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
  report_template <- "report_gene.qmd" #glue::glue("report_{input$type}.qmd")
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

make_report <- function(input = list(), 
                        private, 
                        report_dir = NULL){
  #set report file to NULL
  report_file <- NULL
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
        NULL
      })
  
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
  #remove old tempdirs
  #temp_dirs <- list.files(here::here("quarto"), pattern = "tmpdir", full.names = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
}

# send_email(first_name = "Matthew",
#            email_address = "matthew@hirschey.org",
#            input = list(type = "gene", subtype = "gene", query = "ROCK1", content = "ROCK1"),
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
