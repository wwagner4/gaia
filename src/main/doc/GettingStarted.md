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

