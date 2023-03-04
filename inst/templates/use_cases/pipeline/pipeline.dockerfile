# Placeholder for demonstration purposes
# Edit this Dockerfile for your specific needs
# Note that it may be necessary to install domain-specific tools

# Shows how to copy pipeline scripts from assets
# Note the following uses the files created during templating and copied to assets/
COPY assets/pre.R /pre.R
COPY assets/post.R /post.R
COPY assets/run.sh /run.sh

# Example runs a bash script wrapper
# Calls to run pre/post processing in R
CMD ["bash", "/run.sh"]
