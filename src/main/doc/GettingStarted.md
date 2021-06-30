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

Download unzip and copy the *.gz files to ~/gaia/data/basic

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
docker build --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) -t gaia .
```
### For development
```shell
docker run -it  \
-v <your project dir>/gaia:/home/ugaia/project \
-v <your work dir>/gaia/out:/home/ugaia/work/gaia/out \
-u $(id -u):$(id -g)  gaia bash

Make sure '<your work dir>/gaia/out' exists. Otherwise it will be created as 'root'

```
e.g.
```shell
# wallace
docker run -it  -v /home/wwagner4/prj/gaia:/home/ugaia/project \
-v /home/wwagner4/work/gaia/out:/home/ugaia/work/gaia/out \
-u $(id -u):$(id -g) gaia bash
```
```shell
# ben
docker run -it  -v /home/wwagner4/prj/gaia:/home/ugaia/project \
-v /data/work/gaia/out:/home/ugaia/work/gaia/out \
-u $(id -u):$(id -g) gaia bash
```
```shell
# work
docker run -it  -v /home/itsv.org.sv-services.at/31100428/prj/gaia:/home/ugaia/project \
-v /home/itsv.org.sv-services.at/31100428/work/gaia/out:/home/ugaia/work/gaia/out \
-u $(id -u):$(id -g) gaia bash
```

Multirun: 
Use file 'mrun' in project root
```shell
docker run --name gaiamrun --rm \
-v /home/itsv.org.sv-services.at/31100428/prj/gaia:/home/ugaia/project \
-v /home/itsv.org.sv-services.at/31100428/work/gaia/out:/home/ugaia/work/gaia/out -d -u $(id -u):$(id -g) \
gaia bash project/mrun
```
```shell
# ben
docker run --name gaiamrun --rm \
-v /home/wwagner4/prj/gaia:/home/ugaia/project \
-v /data/work/gaia/out:/home/ugaia/work/gaia/out -d -u $(id -u):$(id -g) \
gaia bash project/mrun
```

```shell
# wallace
docker run --name gaiamrun --rm \
-v /home/wwagner4/prj/gaia:/home/ugaia/project \
-v /home/wwagner4/work/gaia/out:/home/ugaia/work/gaia/out -d -u $(id -u):$(id -g) \
gaia bash project/mrun
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




* cd /home/ugaia/project
* sbt

### For just running
```shell
docker run -it  \
-v <your project dir>/gaia:/home/ugaia/project \
-v <your work dir>:/home/ugaia/work \
-u $(id -u):$(id -g)  gaia bash
```
* cd /home/ugaia/app/gaia
* sbt
