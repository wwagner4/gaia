What is needed on the page

- Describe in general what gaia-visual is good for
- Describe in detail what the (stable) parts of gaia-visual can be used for
-- x3d model
-- x3d animated model
-- video preview
-- video
- todos
- x3d info (separate page)
- getting started (separate page)
-- current state
-- getting the source code
-- compiling gaia-visual and submodule viz
-- run


<general description>
# gaia-visual

gaia-visual is a collection of tools to visualize the data delivered by the
[gaia space observatory|https://en.wikipedia.org/wiki/Gaia_(spacecraft)]

currently gaia-visual provides the following features:

## create a x3d model
## create an animated x3d model
## create video sniplets
## create preview video sniplets
## create still images

# TODOs
....

link to getting started GettingStarted.md

Currently gaia-visual is only avaylable as source code.

## prerequisites
In order to compile and run it you must have a java development kit
(jdk|https://www.google.com/search?channel=fs&client=ubuntu&q=install+jdk)
and the build tool (sbt|https://www.scala-sbt.org/1.x/docs/Setup.html) installed on you computer.

To download gaia-visual you need (git|https://git-scm.com/downloads) to be installed
on your computer

For generating x3d models you need to have (view3dimage|https://castle-engine.io/view3dscene.php)

For generating videos you have to have 'ffmpeg|https://www.ffmpeg.org/download.html'


## download, compile and run

change to a directory of your choise <yourdirectory>

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

in order to compile and run 'gaia-visual' change to <yourdirectory>/gaia

there execute
```shell
sbt run
```

follow the instructions in stdout to run any of the available gaia features



link to x3d resources X3dResources.md
