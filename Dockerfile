# start from the rocker/tidyverse:latest image, not a base r-ver:3.5.0 image
FROM rocker/tidyverse:4

# install quarto


# install packages
RUN R -e "install.packages(c('blastula', 'glue', 'here', 'jsonlite', 'paws', 'quarto'))"

# copy everything from the current directory into the container
COPY / /

# when the container starts, start the main.R script
CMD Rscript main.R
