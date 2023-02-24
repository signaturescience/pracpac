# This doesn't work, because build is happening in docker/ as parent dir
# COPY inst/shiny/app.R /srv/shiny-server

# This doesn't work either (security?)
# COPY ../inst/shiny/app.R /srv/shiny-server

# Hack for now - copy inst/shiny/app.R to docker pre-build
COPY app.R /srv/shiny-server
CMD ["/usr/bin/shiny-server"]
