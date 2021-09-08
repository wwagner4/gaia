FROM debian:unstable-20210511-slim

ENV GAIA_IN_DOCKER=YES
ENV DEBIAN_FRONTEND=noninteractive

RUN mkdir -p /usr/share/man/man1

RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y unzip
RUN apt-get install -y zip
RUN apt-get install -y gnupg2

RUN apt-get install -y view3dscene
RUN apt-get install -y libgtkglext1
RUN apt-get install -y x11vnc xvfb
RUN apt-get install -y ffmpeg
RUN apt-get install -y default-jre
RUN apt-get install -y default-jdk
RUN apt-get install -y gnuplot-data
RUN apt-get install -y gnuplot-mode
RUN apt-get install -y chromium
RUN apt-get install -y git
RUN apt-get install -y vim

WORKDIR /tmp
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v1.5.3/sbt-1.5.3.zip" | jar xvf /dev/stdin
RUN mv /tmp/sbt /usr/local
RUN chmod 755 /usr/local/sbt/bin/sbt
RUN ln -s /usr/local/sbt/bin/sbt /usr/local/bin/

ENV HOME=/home
ENV JAVA_OPTS="-Duser.home=/home"
ENV SBT_OPTS="-Xms2G -Xmx4G -Xss4M --supershell=false"

WORKDIR /home
