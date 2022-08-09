#CREATE METHODS.HTML DOCUMENT

#generate methods via quarto
quarto::quarto_render(input = here::here("quarto"), #expecting a dir to render
                      #output_file = "methods.html", 
                      output_format = "html", #output dir is set in _quarto.yml
                      cache_refresh = TRUE)

#make methods dir in www
if(!dir.exists(here::here("code", "www", "methods"))) {dir.create(here::here("code", "www", "methods"), recursive = TRUE)}

#move methods from output to www (cannot do this in render call) ## look into this.
file.rename(from = here::here("code", "quarto", "methods"), 
            to = here::here("code", "www", "methods"))

#zip

#move to S3 as www/methods.zip




