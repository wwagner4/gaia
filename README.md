# Visualisation for the data of the gaia space observatory

The project ties to find some visualisation for the data retrieved by the 
gaia [space observatory](https://en.wikipedia.org/wiki/Gaia_(spacecraft))

Results can be seen at [entelijan](http://entelijan.net/wolfi-hp/gaia/)

Sources of data and data descriptions
* [All data can be found here](https://www.cosmos.esa.int/web/gaia/data)
* [Most important data is described here](https://gea.esac.esa.int/archive/documentation/GDR2/Gaia_archive/chap_datamodel/sec_dm_main_tables/ssec_dm_gaia_source.html)
* [Source csv files can be found here](http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/)

## Gaia

* ra: 0, 360
* dec: -90 +90
* parallax: -51 + 304 Why negative ?

* Distance ranges from 0.0032 to 2100000 kpc
* 1149 of 7183262 parallaxes are negative. This is 0.99 %
* Diameter of our galaxy 30 kpc
* Distance of the sun from the galactic center 8.6 kpc
* From 7112113 stars 37598 (0.53 %) are further away than 30 kpc 

```
Stars in shell of thikness 0.1kpc:
1 kpc - 167836 stars
2 kpc - 124447 stars
3 kpc - 76738 stars
4 kpc - 37556 stars
5 kpc - 21477 stars
6 kpc - 13602 stars
7 kpc - 9373 stars
8 kpc - 6573 stars
9 kpc - 4690 stars
10 kpc - 3588 stars
11 kpc - 2625 stars
12 kpc - 2116 stars
13 kpc - 1630 stars
14 kpc - 1371 stars
```

Names of the csv file and their index
```
   0 - solution_id
   1 - designation
   2 - source_id
   3 - random_index
   4 - ref_epoch
   5 - ra
   6 - ra_error
   7 - dec
   8 - dec_error
   9 - parallax
  10 - parallax_error
  11 - parallax_over_error
  12 - pmra
  13 - pmra_error
  14 - pmdec
  15 - pmdec_error
  16 - ra_dec_corr
  17 - ra_parallax_corr
  18 - ra_pmra_corr
  19 - ra_pmdec_corr
  20 - dec_parallax_corr
  21 - dec_pmra_corr
  22 - dec_pmdec_corr
  23 - parallax_pmra_corr
  24 - parallax_pmdec_corr
  25 - pmra_pmdec_corr
  26 - astrometric_n_obs_al
  27 - astrometric_n_obs_ac
  28 - astrometric_n_good_obs_al
  29 - astrometric_n_bad_obs_al
  30 - astrometric_gof_al
  31 - astrometric_chi2_al
  32 - astrometric_excess_noise
  33 - astrometric_excess_noise_sig
  34 - astrometric_params_solved
  35 - astrometric_primary_flag
  36 - astrometric_weight_al
  37 - astrometric_pseudo_colour
  38 - astrometric_pseudo_colour_error
  39 - mean_varpi_factor_al
  40 - astrometric_matched_observations
  41 - visibility_periods_used
  42 - astrometric_sigma5d_max
  43 - frame_rotator_object_type
  44 - matched_observations
  45 - duplicated_source
  46 - phot_g_n_obs
  47 - phot_g_mean_flux
  48 - phot_g_mean_flux_error
  49 - phot_g_mean_flux_over_error
  50 - phot_g_mean_mag
  51 - phot_bp_n_obs
  52 - phot_bp_mean_flux
  53 - phot_bp_mean_flux_error
  54 - phot_bp_mean_flux_over_error
  55 - phot_bp_mean_mag
  56 - phot_rp_n_obs
  57 - phot_rp_mean_flux
  58 - phot_rp_mean_flux_error
  59 - phot_rp_mean_flux_over_error
  60 - phot_rp_mean_mag
  61 - phot_bp_rp_excess_factor
  62 - phot_proc_mode
  63 - bp_rp
  64 - bp_g
  65 - g_rp
  66 - radial_velocity
  67 - radial_velocity_error
  68 - rv_nb_transits
  69 - rv_template_teff
  70 - rv_template_logg
  71 - rv_template_fe_h
  72 - phot_variable_flag
  73 - l
  74 - b
  75 - ecl_lon
  76 - ecl_lat
  77 - priam_flags
  78 - teff_val
  79 - teff_percentile_lower
  80 - teff_percentile_upper
  81 - a_g_val
  82 - a_g_percentile_lower
  83 - a_g_percentile_upper
  84 - e_bp_min_rp_val
  85 - e_bp_min_rp_percentile_lower
  86 - e_bp_min_rp_percentile_upper
  87 - flame_flags
  88 - radius_val
  89 - radius_percentile_lower
  90 - radius_percentile_upper
  91 - lum_val
  92 - lum_percentile_lower
  93 - lum_percentile_upper
```

## Fileinfos.
There are 61234 Files on.  
Every file contains about 10000 lines.

### Space Motion
In order to display the direction stars are moving relative to the sun. space velocity
has to be calculated from their proper motion.
This results for every star in a vector of three components (x, y, z) representing
the velosity of that star relative to the sun in km/s.
See: Imageutil.properMotionToSpaceMotion

References: 
- http://www.astronomy.ohio-state.edu/~pogge/Ast162/Unit1/motions.html Somehow incomplete
- https://en.wikipedia.org/wiki/Proper_motion No calc for radial velosity
- https://web.njit.edu/~gary/321/Lecture18.html

In order to display the motion of stars relative to the galactic center the motion
of the sun relative to the galactic center must be known.
References:
- https://aasnova.org/2016/12/14/determining-our-motion-through-the-galaxy/ Calculates the revolution velocity
of the sun to 239+-9 km/s 
- https://en.wikipedia.org/wiki/Galactic_year Describes the time the sun needs to travel around the galactic center. 230 km/s
- https://www.eso.org/public/news/eso0411/

TODO: Make a visualisation of the sun in galactic coordinates. Then calculat x, y, z components of the velocity  

### TODO
- Extract the movement of the sun from the movement of stars
- vid Generate video sniplets in one directory for AUTOMOVE and CAM
- vid Generate 'references' and 'closing content' and still images in one module and place them next to the video sniplets
- vid Start automove using Util.callAllCommands

### Docker
```
# build the image
docker build -t gaia .

# run the image 
docker run -d --rm -v /root/gaia:/root/gaia/data/out  --name gaia gaia sbt "run dt"

# see the logs
docker run -d --rm -v /root/gaia:/root/gaia/data/out  --name gaia gaia sbt "run dt"
```
### X3d
Overview of geometric nodes
```
        +- X3DGeometryNode -+- Arc2D
        |                   +- ArcClose2D
        |                   +- Box
        |                   +- Circle2D
        |                   +- Cone
        |                   +- Cylinder
        |                   +- Disk2D
        |                   +- ElevationGrid
        |                   +- Extrusion
        |                   +- GeoElevationGrid
        |                   +- IndexedLineSet
        |                   +- LineSet
        |                   +- PointSet
        |                   +- Polyline2D
        |                   +- Polypoint2D
        |                   +- Rectangle2D
        |                   +- Sphere
        |                   +- Text
        |                   +- TriangleSet2D 
        |                   |
        |                   +- X3DComposedGeometryNode -+- IndexedFaceSet
        |                   |                           +- IndexedTriangleFanSet
        |                   |                           +- IndexedTriangleSet
        |                   |                           +- IndexedTriangleStripSet
        |                   |                           +- IndexedQuadSet
        |                   |                           +- QuadSet
        |                   |                           +- TriangleFanSet
        |                   |                           +- TriangleSet
        |                   |                           +- TriangleStripSet
        |                   |
        |                   +- X3DParametricGeometryNode -+- NurbsCurve
        |                                                 +- NurbsSweptSurface
        |                                                 +- NurbsSwungSurface
        |                                                 |
        |                                                 +- X3DNurbsSurfaceGeometryNode -+- NurbsPatchSurface
        |                                                                                 +- NurbsTrimmedSurface
```

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).

### Data
* http://entelijan.net/gaiadata.zip

Download and put the content to ~/gaia/data/basic


### Music
* https://freemusicarchive.org/
* https://freemusicarchive.org/music/Daniel_Birch
* https://freemusicarchive.org/music/Xylo-Ziko Polyrhythms
* https://freemusicarchive.org/music/Michael_Bruckner

### TODOs
- add new features to camera create (when needed in the subsequent points). 
  - moving center
  - other ranges than 0 - 360, e.g longer 0 - 400, shorter 0, 180     
- create camera configs in parallel
