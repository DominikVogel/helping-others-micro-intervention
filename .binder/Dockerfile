FROM rocker/binder:3.5.3
LABEL maintainer='Dominik Vogel'
USER root
RUN apt-get install texlive-lang-english 
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}
RUN wget https://github.com/DominikVogel/helping-others-micro-intervention/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-04-01/')); devtools::install_deps()"
