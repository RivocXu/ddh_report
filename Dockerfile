# start from the rocker/tidyverse:latest image, not a base r-ver:3.5.0 image
FROM rocker/tidyverse:4

#install libglpk for igraph deps, and libxt for x11
RUN apt-get update && apt-get install -y libxt-dev libglpk-dev && rm -rf /var/lib/apt/lists/*

# install packages
RUN R -e "install.packages(c('blastula', 'corrr', 'ggrepel', 'glue', 'gt', 'here', 'htmltools', 'jsonlite', 'patchwork', 'paws', 'quarto'))"
RUN R -e "devtools::install_github('matthewhirschey/ddh', force = TRUE)" 

# copy everything from the current directory into the container
COPY ./ ./src

#set working directory
WORKDIR /src

# when the container starts, start the main.R script
CMD Rscript main.R
