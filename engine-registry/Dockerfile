FROM ubuntu:20.04

WORKDIR /application

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl

# Add built exectutable binary
ADD engine-registry-bin ./scripts/docker-run.sh /application/

# Create folder for configs & customization
RUN mkdir /application/engine-registry

# Add templates
ADD templates /application/engine-registry/templates

# Add configs
ADD config/application.yml /application/engine-registry/config/application.yml
ADD config/build-info.yml /application/engine-registry/config/build-info.yml
ADD config/localization.json /application/engine-registry/config/localization.json

# Run the run script (preparations and engine-wizard-bin)
CMD ["bash", "./docker-run.sh"]
