FROM rocker/binder:3.5.3
LABEL maintainer='Dominik Vogel'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}
RUN apt-get install texlive-full && wget https://github.com/DominikVogel/helping-others-micro-intervention/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-04-01/')); devtools::install_deps()"
