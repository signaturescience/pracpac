# Placeholder for demonstration purposes
# Shows how to copy your app.R file from host to Docker image
# Note the following uses the files created during templating and copied to assets/
COPY assets/app.R /srv/shiny-server

CMD ["/usr/bin/shiny-server"]
