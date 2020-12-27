FROM hseeberger/scala-sbt:15.0.1_1.4.4_2.13.4

RUN apt-get update
RUN apt-get install -y view3dscene
RUN apt-get install -y ffmpeg

RUN mkdir /opt/prj

WORKDIR /opt/prj

RUN git clone https://github.com/wwagner4/gaia.git

RUN cd gaia

WORKDIR /opt/prj/gaia

RUN sbt compile
