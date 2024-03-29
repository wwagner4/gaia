# gaia-visual
gaia-visual is a collection of tools to visualize the data delivered by the
[gaia space observatory](https://en.wikipedia.org/wiki/Gaia_(spacecraft))


## Features
Currently, gaia-visual provides the following features:

### Subset of the gaia 
The data are taken from the [data release 2](https://www.cosmos.esa.int/web/gaia/data-release-2) and prepared
for easy access in scala. Currently position and movement information of stars are used.
### Create a x3d model
From the data [x3d](https://en.wikipedia.org/wiki/X3D) xml files can be created
### Create an animated x3d model
From the data [x3d](https://en.wikipedia.org/wiki/X3D) xml files with animation can be created
### Create video snippets
From the x3d animated models high resolution videos can be created. They might be used for
creating videos. for exampes see [my video channel TODO](https://www.youtube.com/channel/UC4FUfmBs-A-m3iCHTP3tHVw)
### Create still images
From the x3d animated models high resolution images can be created

## TODO
- Normalisation. Branch norm. 
  - Analyse the distribution in sectors based on dens1
    - Refactor Util files. E.g. introduce a Io or File Util
  - Make a dataset with number of stars are about equal per sector based on the analys
  - Make one or more datasets not including the region around the sun
  - Make some images using that dataset
    - Arund the sun
    - Around the galactic center
    - Somewhere else in the galaxy
- Add new features to camera create (when needed in the subsequent points).
    - other ranges than 0 - 360, e.g. longer 0 - 400, shorter 0, 180


## [Getting started](src/main/doc/GettingStarted.md)

## [X3d resources](src/main/doc/X3dResources.md)