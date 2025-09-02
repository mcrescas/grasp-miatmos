# CHANGELOG

This file explains main changes between different GRASP versions.
To know how to update your settings files from a version to run in different one,
please, check the file [UPGRADE.md](UPGRADE.md)


## v1.1.3 (2023-02-08) 

- If given particle size parameter provided in settings is less than minimum kernel node 
  then kernel value for minimum kernel size parameter is used to calculate particle 
  single scattering characteristics. 
- Improvements in the code for error estimation. 
- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability
  

## v1.1.2 (2022-06-03) 

- Option to provide time difference threshold value for multi pixel smoothness constrains: retrieval.constraints.multi_pixel_smoothness_T_difference_threshold
- Added the possibility to calculate rigorous dynamic error estimates of the rertieved characteristics and some derived products.
- Added a possibility to add bias to the systematic component of error:	inversion.noises.noise[].bias_equation
- Added a possibility to add bias to simulated measurements: inversion.noises.noise[].bias_measurements_synthetic
- Look-uptables of the refractive index of the chemical components now can be installed 
  with grasp-manager as a kernel.
- Added an option retrieval.constraints.multi_pixel_smoothness_T_difference_threshold 
  to provide time difference threshold value (in seconds) for multi pixel smoothness constrains 
  if time differences are not constant in segment measurement data. If the value = 0., 
  the threashold is not used. 
- Added a possibility to add random noise to input or simulated measurements (sdata)
  and modified option to add random noise to measurements used as fitted ones. `inversion.noises.add_random_noise` with valid values `disable`, `measurement_fitting`, `sdata`
  Valid values: disable              - do not add random noise s
                measurement_fitting  - add random noise to vector of measurements
                sdata                - add random noise to sdata measurements
- Added an option to use Transport model tracer average        approach to calculate measurements: retrieval.forward_model.phase_matrix.use_transport_model=true and   
- Added an option to use Transport model column average tracer approach to calculate measurements: retrieval.forward_model.phase_matrix.transport_model.vertical_profile=tracer_average
- Added the option retrieval.inversion.noises.add_random_noise to simplify the way to generating synthetic data with noise.  
  Valid values: disable (standard_deviation_synthetic is ignored), enable (standard_deviation_synthetic is added to the measurements but it is not added to the fitting),
  sdata (standard_deviation_synthetic is added to the fitting so one can get an sdata file with noise added). If retrieval.mode=inversion, only 'disable' option is valid

## v1.1.1 (2021-09-09)

- Option to calculate particlate matter is disabled, will providee error message asking to switch it to false.
- Added an option to choose linear or spline kernels interpolation to be used for particle size parameter. 
- Added integrated over scattering angle range first element of scattering matrix as a measurement 
  type p11_intd_cut_off_N with maximum radius given by settings options. 
- Added a possibility to invert differences of p11_intd_cut_off_N measurements for N>2. 
  p11_integrated_cutoff_N measurements are calculated as p11_integrated_cutoff_N=p11_integrated_cutoff_N-sum(p11_integrated_cutoff_1:p11_integrated_cutoff_(N-1)).
- Added a new "threshold" aerosol vertical profile type. 
  Function simulates aerosol as a uniform layer, below vertical_parameter_height with almost 0 aerosol concentration above it. 
  It can be selected as one of the options in retrieval.forward-model.radiative_transfer.aerosol_profile_vertical_type: threshold alongside with "exponential" and "gaussian".
  - Added first implementation of gas absorption and hyperspectral channels.
  - Added chemical components refactor.
  - Added the thermal infrared spectral range to the Radiative transfer. New settings related to the configuration of this new feature.
  - New vertical discretization in the forward model for the Radiative Transfer. This change slighlty modifies the output of the forward model and some results of the inversion.

## v1.0.1 (2021-02-19)

- Updated kernels for nonspherical particles because of changes for ratio=0.4823 
- Added a possibility to retrieve spectral complex refractive index at reduced number of wavelengths and 
  interpolate/extrapolate it using Angstrom exponent approach for the rest of wavelengths provided 
  in retrieval. 
- Added a possibility to retrieve surface parameters at reduced number of wavelengths and 
  interpolate/extrapolate them using Angstrom exponent approach for the rest of wavelengths provided 
  in retrieval. 
- Added a possibility to calculate new surface product - isotropic BiHemispherical Reflectance (bhr_iso)
- Bug fixed! The bug affected retrievals with different types of characteristics 
  for real and imag refractive index in one run. 
    For example: 
    1. real spectral
       imag const 
    2. real   const 
       imag spectral
  Everything works as expected now.
- Added integrated over scattering angle range first element of scattering matrix as a measurement type p11_intd. 
- Added retrieval.debug.error_estimates_using_Levenberg-Marquardt (T/F) to calculate error 
  estimates with/without Levenberg-Marquardt type constraints inclusion
- Added new settings option retrieval.forward_model.radiative_transfer.number_of_geometrical_levels - 
  number of levels for discret vertical profile.
- Added new settings options:
  * retrieval.convergence.minimum_value_of_Levenberg-Marquardt_term - 
    Levenberg-Marquardt term minimum value if Levenberg-Marquardt constraint inclusion is applied.
  * retrieval.convergence.minimum_value_of_residual_coefficient - 
    minimum value of applied residual coefficient.
  * retrieval.convergence.maximum_value_of_residual_coefficient - 
    maximum value of applied residual coefficient.

## v1.0.0 (2020-05-23)

- Added new functions "^expand(X;Y)" and "^repeat(X;N)"
- New feature in the settings: It is possible to define variables (^variable: value)
- Improvements in GRASP-Manager: It checks if indentation of the grasp-manager.yml file is correct

- Added options to use:
  * smoothness estimates
  * covariance matrix diagonal (weights) for smoothness

- Added new measurement types to be provided in sdata
  * P11_rel_ang = 27 (scattering matrix elements P11 devided by P11 at given in settings scattering angle) 
  * P12_rel = 28 (scattering matrix elements -P12 devided by P11),
  * I_rel_sum = 45 (radiances devided by sum of valid radiancemeasurements) 
  * P_rel = 46 (relative linear polarization) 


- Fixed severe bug, that was causing lidar data not to be fitted correctly in the case when
  distance was significantly different from altitude (for e.g. high altitude sites or inclined lidars)
- Fixed minor bug that was causing lidar data to be filled with zeros if HOBS and HGR where not 
  corresponding neither to groundbased or spaceorne oservations
- Moved module `python` directly to the GRASP repository, so it is now automatically included
  with every GRASP compilation.


## v0.8.4 (2020-05-21)

- Fixed bugs in MPI engine. Only parallel processing was affected.



## v0.8.3 (2019-08-09)

- Fixed important bug in the output (Many fields where 0 or inconsistent)
- Generic constants set has been changed to reduce memory usage by default
- Many minor improvements which makes the product much more mature:
    - Documentation reviewed
    - Cleaned code, refactor and better commented
    - GRASP-Cloud integration:
        - Internal adaptation for python interface
        - Auto-deploy 
- Renormalization of single scattering matrix:
    - elements of scattering matrix are divided by integral of f11 and 
      then multiplied by scattering coefficient

## v0.8.2 (2019-04-09)

- Added option to retrieve aspect ratio distribution of spheroidal particles.
- Added the functions all() and repeat(N) for the constraints definition.
- Minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved
- Major improvements for the speed of RT calculations introduced
- Experimental feature of radiative transfer kernels generation and usage added  (one aerosol mode only)

## v0.8.1 (2018-06-13)

- Removed an option to use time-space groups of the same parameters in order to reduce Jacobian size. Removed all routines related to this option.  
- Removed classic main program and all related to that routines. 
- Added option to compress CSV output files (only if zlib function is available at compilation time). To compress files the user has to set output.tile.function_settings.csv.compression or output.segment.function_settings.csv.compression option to true.
- Added support to superlu5. CMake automatically detects which interface has to been used, if version 4.x or 5.x
- New feature: added measurement types for observing aerosol vertical structures:
    - meas_type_RL = 32 - RAMAN lidar (could be both Stokes and anti-Stokes shifted)
    - meas_type_DP = 35 - volume depolarization ratio from lidar
    - meas_type_VEXT = 36 - vertical extinction profile
(could be retrieved from RAMAN observaitons, airborne AOD flight or radiosonde measurements)
    - meas_type_VBS = 39 - vertical backscatter profile
(could be retrieved from RAMAN observaitons or radiosonde measurements)
- New feature: added support for the inclined lidar sounding
- new conventions to provide lidar measurements for GRASP (check documentation of SData format):
    - instead of altitude, range should be provided, also elastic and RAMAN
lidar signals should be normalized to integral of the signal over range
    - vertical profile of volume depolarisation ratio should be provided in persentage
i.e. accepted values of the signal are in the range [1.0E-6 ,..., 100],
no normalisation is expected for this type of observations
    - if lidar system is inclined, sounding zenith angle (0 means vertical installation)
should be provided in the field corresponding to SZA at the wavelenths of lidar sounding
    - altitude above sea level (in meters) of the lidar and/or sunphotometer should be provided
in the field corresponsing to the pixel elevation, note that in that case field H_OBS should
be equal or greated than this value
    - molecular depolarisation ratio specific for the used lidar system should be provided in the
field gas_par at the position corresponding to the measurement of vertical depolarisation profile
- Different pixels can now have different altitude sets for retrieved aerosol vertical profile,
NOTE: their number should be the same, and no inter-pixel constraints for this parameter
will make sense in such case, so it shoud be set to 0.
This makes difference only for multi-temporal retrievals using lidars and radiosondes,
since their altitude sampling can differ from each other
- Removed lagecy code to run retrieval algorithm as stand alone app. 
- Minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved

## v0.8.0 (2018-02-12)

- Released publicly the dependencies module. It helps to install GRASP dependencies.
- Added the keyword 'stderr' to streams. Now the user can redirect to stderr streams from GRASP.
- Acceleration of RT with optimised integration over optical thickness in RT’s equations.
- The parameter retrieval.radiative_transfer.number_of_layers is optimized and can be defined using 2 numbers,
minimum and maximum. To backward compatibility it can still be defined with a single number, in this case
the number will be used as minimun an the value of the constant KNT will be used as maximum. Also, to avoid errors,
a validator to check that those numbers are lower than KNT has been added. 
- New feature: Added possibility to include aerosol absorption optical depth as a measurement to be inverted.
- New feature: Added setting to print the output of the retrieval after each iteration
- Added alpha version of a python interface (GRASP can be executed from Python using an external module)
- Minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved
- Use `-` in command line arguments to read from STDIN


## v0.7.4 (2017-05-15)

- Adding a setting to dump initial guess information: input.imagedat.dump
- Important bug was fixed that mainly affect to retrieval with pixels with different number of wavelengths.
- Introduced module report stop: It allows to don't stop entire processing if single segment is wrong.
- Added two extra wildcards to streams: segment_first_date, segment_last_date and iinversion
- Added a new default transformer: segment_imagedat to set up different initial guess for each pixel in a single segment retrieval
- Minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved


## v0.7.3 (2017-01-20)

- Models approach is integrated and can be loaded
- Refactor of SSA output
- Old bug fixed: Sometimes appear a pixels with all values set to 0 in the output
- Minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved


## v0.7.2 (2016-11-11)

- Settings module allow to set default values for arrays with different values. This change affects to the settings interface of extension modules.
- Moved loops in inversion routine to their own module for better abstraction and easier parallelization.
- Minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved


## v0.7.1 (2016-07-28)

- Refactoring for vertical profile part; discret vertical profile for given number of altitudes is passes as input to radiative transfer routine. 
- Possibility to run 2 aerosol mode retrieval with precalculated lognormal bin size distribution model
- Particle component volume fractions linear mixture model of refractive index contains: 
  carbon, dust, iron and water components for 1 aerosol mode retrieval;  
  black carbon, brown carbon, quartz and water (fine mode) and quartz, iron and water (coarse mode) for 2 aerosol mode retrieval.
- Other minor improvements, bugs fixed, code more cleaned, documentation improved and code stability improved


## v0.7.0 (2016-05-24)

- Added license agreement
- Added official examples
- Minor improvements and bugs fixed


## v0.6.8

- Code has been cleaned removing many external code (extensions, modules, kernels...)
- grasp-manager can manage modules, kernels and constants sets.
- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability


## v0.6.7

- An important bug introduced in version v0.6.6 has been fixed. It affects to retrieval over ocean surfaces.
- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability


## v0.6.6

- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability


## v0.6.5

- Minor improvements and bugs fixed


## v0.6.4

- Added new wildcards to grasp output streams
- Many new actions added to grasp-manager script
- Code has been speeded up in not-mixed surface (pure land or pure ocean)
- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability


## v0.6.3

- Parameter **retrieval.constraints.characteristic[1].mode[1].single_pixel.a_priori_estimates.difference_order** has been removed and
  **retrieval.constraints.characteristic[].mode[].single_pixel.a_priori_estimates.lagrange_multiplier** accept an array instead of single value.
  It allows to define a different constraint to each parameter in the initial guess.  
- New option was added allowing to use in current segment retrieval retrieved parameters of pixels-neighbors from preceding retrievals.
  In order to use this option additional settings for edges have to be provided in settings file providing the number 
  of pixels-edges for longitude (X), latitude (Y) and time (T) coordinates to be used in current segment inversion (by default 0 is set). 
- Particulate matter (air quality) product added
- Aerosol typing added (aerosol type indicies: 
    0 – Complex mixture, 
    1 – Background Aerosol
    2 – Water/Maritime
    3 — Urban Polluted
    4 – Mixed aerosol
    5 – Urban Clean
    6 – Smoke Smoldering
    7 – Smoke flaming
    8 – Mineral dust)
- Option to select aerosol vertical profile type between normal and exponential added

```yml
retrieval:
    edges_size:
        x: 2
        y: 2
        t: 2
```

- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability


## v0.6.2

- Added a parameter in settings files: **retrieval.product_configuration.wavelength_indices_for_ndvi**: Indices of wavelengths which will be used to calculate NDVI if it is calculated
- Added "valgrind" constants set
- Minor improvements, bugs fixed, code more cleaned, improved documentation and improved code stability


## v0.6.1

- Added more actions to grasp-manager script
- Minor improvements, bugs fixed, code more cleaned, and improved code stability

## v0.6.0

