FROM ubuntu:20.04

RUN apt-get update

RUN apt-get -qq install -y curl
RUN apt-get -qq install -y unzip
RUN apt-get -qq install -y zip

RUN apt-get install -qq default-jdk

RUN apt-get -qq install -y gnupg2
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update
RUN apt-get install -y sbt

WORKDIR /app
RUN git clone https://github.com/wwagner4/gaia
WORKDIR /app/gaia
RUN git submodule init
RUN git submodule update
WORKDIR /app/gaia/viz
RUN sbt publishLocal

WORKDIR /tmp
RUN curl -sL "http://entelijan.net/gaiadata.zip" | jar xvf /dev/stdin
RUN mkdir -p $HOME/gaia
RUN mkdir -p $HOME/gaia/data
RUN mkdir -p $HOME/gaia/data/basic

RUN mv /tmp/gaia/*.gz $HOME/gaia/data/basic

RUN apt-get install -qq view3dscene
RUN apt-get install -y libgtkglext1
RUN apt-get install -y x11vnc xvfb
RUN apt-get install -y ffmpeg

ENV GAIA_IN_DOCKER=YES

# WORKDIR /app/gaia
WORKDIR /project
