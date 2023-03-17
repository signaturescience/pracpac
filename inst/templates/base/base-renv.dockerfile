FROM {base_image}

## copy the renv.lock into the image
COPY renv.lock /renv.lock

## install renv
RUN Rscript -e 'install.packages(c("renv"))'

## set the renv path var to the renv lib
ENV RENV_PATHS_LIBRARY renv/library

## restore packages from renv.lock
RUN Rscript -e 'renv::restore(lockfile="/renv.lock", repos={repos})'

## copy in built R package
COPY {pkgname}_{pkgver}.tar.gz /{pkgname}_{pkgver}.tar.gz

## run script to install built R package from source
RUN Rscript -e 'install.packages("/{pkgname}_{pkgver}.tar.gz", type="source", repos=NULL)'

