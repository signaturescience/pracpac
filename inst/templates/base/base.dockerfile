FROM {base_image}

COPY {pkgname}_{pkgver}.tar.gz /{pkgname}_{pkgver}.tar.gz

# Domain-specific stuff here
# RUN apt update && apt install -y yourpackages

RUN Rscript -e 'install.packages("BiocManager")'
RUN Rscript -e 'BiocManager::install(c({pkgs}), update=FALSE, ask=FALSE)'
RUN Rscript -e 'install.packages("/{pkgname}_{pkgver}.tar.gz", type="source", repos=NULL)'
