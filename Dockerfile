FROM debian:unstable-20210511-slim

ARG USER_ID
ARG GROUP_ID

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
RUN apt-get install -y sbt
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

RUN groupadd -g ${GROUP_ID} ugaia
RUN useradd -l -u ${USER_ID} -g ugaia ugaia
RUN install -d -m 0755 -o ugaia -g ugaia /home/ugaia
RUN chown --changes --silent --no-dereference --recursive --from=33:33 ${USER_ID}:${GROUP_ID} /home/ugaia
USER ugaia

RUN mkdir -p /home/ugaia/work/gaia/data/basic
ENV GAIA_WORK_BASE=/home/ugaia/work

WORKDIR /tmp
RUN curl -sL "http://entelijan.net/gaia-data-basic.zip" | jar xvf /dev/stdin
RUN mkdir -p /home/ugaia/work/gaia/data/basic
RUN mv /tmp/basic/*.gz /home/ugaia/work/gaia/data/basic

RUN mkdir -p /home/ugaia/app
WORKDIR /home/ugaia/app
# force no cache an change
ADD https://api.github.com/repos/wwagner4/gaia/git/refs/heads/master version.json
RUN git clone https://github.com/wwagner4/gaia
WORKDIR /home/ugaia/app/gaia
RUN git submodule init
RUN git submodule update
WORKDIR /home/ugaia/app/gaia/viz
RUN sbt publishLocal

WORKDIR /home/ugaia/app/gaia
RUN sbt test

WORKDIR /home/ugaia
# WORKDIR /app/gaia
# WORKDIR /project
