# Getting started gaia-visual
Currently gaia-visual is only avaylable as source code.

## Prerequisites
In order to compile and run it you must have a java development kit
[jdk](https://www.google.com/search?channel=fs&client=ubuntu&q=install+jdk)
and the build tool [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) installed on you computer.

To download gaia-visual you need [git](https://git-scm.com/downloads) to be installed
on your computer

For generating x3d models you need to have [view3dimage](https://castle-engine.io/view3dscene.php)

For generating videos you have to have [ffmpeg](https://www.ffmpeg.org/download.html)

## Download and provide data
The actual dataset can be downloaded from 
```
https://drive.google.com/file/d/1YvV2LIO_ml4_bokMKW3JLAHYJxBTNp4n/view?usp=sharing
```
or using wget
```
wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1YvV2LIO_ml4_bokMKW3JLAHYJxBTNp4n' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1YvV2LIO_ml4_bokMKW3JLAHYJxBTNp4n" -O gaia1.zip && rm -rf /tmp/cookies.txt
```
or use entelijan
```
wget http://entelijan.net/gaia-data-basic.zip 
```

Download unzip and copy the 'basic' directory to ~/work/gaia/data

## Download, compile and run

change to a directory of your choise &lt;yourdirectory>

there execute:
```shell
git clone https://github.com/wwagner4/gaia
```

change to the newly created directory 'gaia'

as gaia-visual uses the library 'viz' for generating
diagrams you must install this library
on your computer.
'viz' is delivered as a git submodule with gaia-visual.

execute the following steps in order to compile and publish 'viz'
```shell
git submodule init
git submodule update
```
change to the newly created subdirectory 'viz'

there execute
```shell
sbt publishLocal
```
'viz' should be properly installed on your computer now.

in order to compile and run 'gaia-visual' change to &lt;yourdirectory>/gaia

there execute
```shell
sbt run
```

follow the instructions in stdout to run any of the available gaia features

## Docker
All these prerequisites are also defined in a docker file

build
```shell
docker build -t gaia .
```
### For development
```
docker run -it  \
-v <your home dir>:/home \
-u $(id -u):$(id -g)  gaia bash
```

#### work
```
docker run -v /home/itsv.org.sv-services.at/31100428:/home -u $(id -u):$(id -g) -it gaia bash
```

#### bob
```
docker run -v /home/wwagner4/:/home -u 1000:1000 -it gaia bash
```

#### wallace
```
docker run -v /home/wwagner4/:/home -u 1000:1000 -it gaia bash
```

### Multirun: 

Use file 'mrun' in project root
```
docker run --name gaiamrun --rm \
-v /home/wwagner5:/home \
-d -u $(id -u):$(id -g) \
gaia bash /home/prj/gaia/mrun
```

File might look like
```shell
cd project && sbt "\
;test\
;run tryout\
;run tryout\
;run tryout\
;test\
"
```
