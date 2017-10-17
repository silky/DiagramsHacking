# Prepare the image with:
#   docker build -t diagrams-hacking .
FROM tensorflow/tensorflow:1.0.0
MAINTAINER Noon

RUN apt-get update

RUN apt-get install -y \
        # Required by snappy-frames dependency.
        libsnappy-dev \
        # Avoids /usr/bin/ld: cannot find -ltinfo
        libncurses5-dev \
        # Makes stack viable in the container
        libgmp-dev \
        # Required for locales configuration.
        locales \
        # low-discrepency sequences
        libgsl0ldbl \
        libgsl0-dev \
        gsl-bin \
        # Diagrams-stuff
        libcairo-dev \
        libglib2.0-dev \
        libpango1.0-dev \
        libcairo2-dev \
        # So we can clone 3rd-party repos
        git


# Our MNIST demo program outputs Unicode characters.
RUN dpkg-reconfigure locales && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8

# Installs protoc and the libraries.
RUN \
    curl -O -L https://github.com/google/protobuf/releases/download/v3.2.0/protoc-3.2.0-linux-x86_64.zip && \
    unzip -d /usr/local protoc-3.2.0-linux-x86_64.zip bin/protoc && \
    chmod 755 /usr/local/bin/protoc && \
    curl -O https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.0.0.tar.gz && \
    tar zxf libtensorflow-cpu-linux-x86_64-1.0.0.tar.gz -C /usr/local && \
    ldconfig

ENV LANG en_US.UTF-8
