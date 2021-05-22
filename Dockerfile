FROM ubuntu:20.04

ENV GAIA_IN_DOCKER=YES

RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y unzip
RUN apt-get install -y zip
RUN apt-get install -y gnupg2

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update

RUN apt-get install -y view3dscene
RUN apt-get install -y libgtkglext1
RUN apt-get install -y x11vnc xvfb
RUN apt-get install -y ffmpeg
RUN apt-get install -y default-jdk
RUN apt-get install -y sbt

RUN mkdir -p /work/gaia/data/basic
ENV GAIA_WORK_BASE=/work

WORKDIR /tmp
RUN curl -sL "http://entelijan.net/gaiadata.zip" | jar xvf /dev/stdin
RUN mv /tmp/gaia/*.gz /work/gaia/data/basic

WORKDIR /app
RUN git clone https://github.com/wwagner4/gaia
WORKDIR /app/gaia
RUN git submodule init
RUN git submodule update
WORKDIR /app/gaia/viz
RUN sbt publishLocal



RUN chmod 777 /work/gaia

# WORKDIR /app/gaia
WORKDIR /project
