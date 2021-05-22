FROM ubuntu:20.04

RUN apt-get update

RUN apt-get -qq install -y curl
RUN apt-get -qq install -y unzip
RUN apt-get -qq install -y zip

RUN curl -s https://get.sdkman.io | bash
RUN bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && \
    yes | sdk install java 16-open && \
    yes | sdk install sbt && \
    rm -rf $HOME/.sdkman/archives/* && \
    rm -rf $HOME/.sdkman/tmp/*"

WORKDIR /project
