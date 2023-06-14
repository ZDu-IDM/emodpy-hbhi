FROM rocker/shiny:4.0.5

RUN apt-get update
RUN apt-get install -y libcurl4-gnutls-dev \
   libssl-dev \
   libxml2-dev \
   libsodium-dev \
   libgdal-dev \
   units \
   libudunits2-dev \
   nano

RUN R -q -e "install.packages('remotes')"
RUN R -q -e "install.packages('sodium')"

RUN R -q -e "remotes::install_github('paulc91/shinyauthr')" && \
    R -q -e "install.packages('shinyjs')" && \
    R -q -e "install.packages('ggpubr')" && \
    R -q -e "install.packages('raster')" && \
    R -q -e "install.packages('rgdal')" && \
    R -q -e "install.packages('rjson')" && \
    R -q -e "install.packages('shinythemes')" && \
    R -q -e "install.packages('tidyverse')" && \
    R -q -e "install.packages('dplyr')" && \
    R -q -e "install.packages('here')" && \
    R -q -e "install.packages('sf')" && \
    R -q -e "install.packages('ggplot2')" && \
    R -q -e "install.packages('reshape2')"
    
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD . /srv/shiny-server/shiny
#ADD app.R /srv/shiny-server/shiny

# run app
#CMD ["/usr/bin/shiny-server"]