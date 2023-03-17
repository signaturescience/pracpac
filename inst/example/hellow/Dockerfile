FROM rocker/r-ver:latest

## copy the renv.lock into the image
COPY renv.lock /renv.lock

## install renv
RUN Rscript -e 'install.packages(c("renv"))'

## set the renv path var to the renv lib
ENV RENV_PATHS_LIBRARY renv/library

## restore packages from renv.lock
RUN Rscript -e 'renv::restore(lockfile = "/renv.lock", repos = NULL)'

## copy in built R package
COPY hellow_0.1.0.tar.gz /hellow_0.1.0.tar.gz

## run script to install built R package from source
RUN Rscript -e 'install.packages("/hellow_0.1.0.tar.gz", type="source", repos=NULL)'

## COPY in the pre processing R script and run shell script
COPY assets/pre.R /pre.R
COPY assets/run.sh /run.sh

## add the translate-shell tool and dependencies
RUN apt-get update && apt-get install -y bsdmainutils translate-shell

## enter at run shell script
## allows for parameters passed to container at runtime
ENTRYPOINT ["bash", "/run.sh"]
