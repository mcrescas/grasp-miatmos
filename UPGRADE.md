# UPGRADE

This file explains how to update your settings files from a version to run in different one.
If you want to know the changes in code (bug-fixed, new features, ...) please, check 
the file [CHANGELOG.md](CHANGELOG.md)


# v1.1.3 (2023-02-08)

nothing to do to update to this version from v1.1.2

## v1.1.2 (2022-06-03) 

- Removed retrieval.inversion.measurement_fitting.polarization=relative_phase_matrix_meas ; Same behaviour
  can be obtained using measurement_type=28 (p12_rel) and it is confusing to have both alternatives
- Removed input.sdata.dump_original. Instead one can use:
  - input.debug.sdata_dump_after_driver (debug option)
  - input.debug.sdata_dump_after_transformer (debug option)
  - input.sdata.dump

- Removed retrieval.debug.simulated_sdata_file. Instead, it is recommended to use output.sdata.dump 

The following two characteristics have to be present in settings with characteristic.retrieved: false.

``` 
    constraints:
        characteristic[]: 
            type: level_height
            retrieved: false

        characteristic[]: 
            type: level_relative_humidity
            retrieved: false

```


## v1.1.1 (2021-09-09)

- Option to calculate particlate matter is disabled, will providee error message asking to switch it to false.
```
  retrieval:
    products:
      aerosol: 
        particulate_matter: false
```
` 
- Option to choose kernels interpolation type for particle size parameter (linear/spline).
```
  retrieval:
    forward_model:
        phase_matrix: 
            kernels_interpolation_for_size_parameter: linear 
```
`  

## v1.0.1 (2021-02-19)

- Options to be used for integrated p11 measurements with PM cut-off.
```
  retrieval:
      inversion:
          measurement_fitting:
              integrated_p11_measurements_cut_off: [1.0, 2.5, 4.0, 10.0]
              integrated_p11_measurements_cut_off_diff: false

```
`  

- The valid values of retrieval.forward_model.radiative_transfer.simulating_derivatives.order_of_scattering
  has been renamed. Before it was single_scattering and derivatives. Now it is
  single_scattering and multiple_scattering. If you are using derivatives you
  should replace it by multiple_scattering

- retrieval.products.surface section has been reorganized. Before it was simply
  retrieval.products.surface=True|False but now it is:
  ```
  retrieval:
      products:
          surface:
              main_characteristics: True # Equivalent to the old surface
              isotropic_bihemispherical_reflectance: False
  ```

  To update old settings retrieval.products.surface has to be renamed as
  retrieval.products.surface.main_characteristics

` 

## v1.0.0 (2020-05-23)

- removed functions "all" and "repeat" -> see changelog, the have been replaced
  by "expand" and "repeat".
- Major change in the settings. 
    - retrieval.convergence.stop_before performing_retrieval has been removed. Now
      it is retrieval.mode and its valid values are 'inversion' or 'forward'
    - Created new section retrieval.inversion which contains the sections 
      convergence, regime_of_multipixel_constraints and noises
    - Created new section forward_model which contains phase_matrix and radiative_transfer
    - The section product_configuration has been included under products.configuration
    - retrieval.inversion.regime_of_multipixel_constraints.inversion_regime has been renamed to retrieval.inversion.regime
    - retrieval.inversion.regime_of_multipixel_constraints group has been renamed to retrieval.inversion.multipixel_constraints

- Values of smoothness estimates and their weights can be provided in settings 
  yml
  retrieval
      constraints:
          characteristic[]: 
              mode[]: 
                  single_pixel:
                      estimates: [ ]
                      weights: [ ]

- For error estimate computations both Fisher matrix and vector of corrections for 
  retrieved parameters are used with Levenberg–Marquardt type constraints inclusion 
  if they are applied in inversion.
- Removed option retrieval.regime_of_measurement_fitting.radiance instead of it  
  added a new measurement type - relative radiance (can be provided in sdata):
- Added new measurement types (can be provided in sdata): 
  * P11_rel_ang = 27 (scattering matrix elements P11 devided by P11 at given in settings scattering angle) 
  * P12_rel = 28 (scattering matrix elements -P12 devided by P11),
  * I_rel_sum = 45 (radiances devided by sum of valid radiancemeasurements) 
  * P_rel = 46 (relative linear polarization) 
- Remove `modules: python` from grasp-manager.yml if it was specified there and remove the directory 
  `src/python` if it is present. The `python` module is now directly included with grasp. 
  

## v0.8.4 (2020-05-21)

nothing to do to update to this version from v0.8.3


## v0.8.3 (2019-08-09)

nothing to do to update to this version from v0.8.2


## v0.8.2 (2019-04-09)

- global.resources has been renamed to resources_path
- The parameter retrieval.phase_matrix.use_models has been removed. A new characteristic
type has been added "aerosol_model_concentration". To use models now that characteristic has
to be used instead of "size_distribution_precalculated_lognormal".
- The presence of any type of complex refractive index and sphericity parameters are no longer needed if 
"aerosol_model_concentration" is defined in the settings file. 
- Experimental option "look_up table" renamed to "rt_kernes"
- Renamed 2 of 5 possible values of retrieval.regime_of_measurement_fitting.polarization:
    * absolute_polarization_components/abs_pol 
    * relative_polarization_components/rel_pol
    * polarized_reflectance/pol_ref
    * linear_polarization/lin_pol -> degree_of_polarization/deg_pol
    * relative_linear_polarization/rel_lin_pol -> degree_of_polarization_meas/deg_pol_meas


## v0.8.1 (2018-06-13)

- The executable sdata_dump has been renamed as grasp_sdata_dump
- Some parameters have been removed from the settings file: 

```yml
retrieval:
    regime_of_multipixel_constraints:
        time_space_groups 
        time-scale
        x-scale
        y-scale
```

## v0.8.0 (2018-02-12)

- ascii output functions (for segment and tile) has been renamed to CSV and now these
new functions include more options. Old ascii functions still can be used installing them 
as an extension using the grasp-manager
- Due to last changes in radiative transfer results could differ slightly. The behaviour 
of the parameter retrieval.radiative_transfer.number_of_layers has been changed. Before
it was the value to be used now it is minimum and maximum. For compatibility, if only a value
is given it is used as minimum while the maximum used will be (by default) the value of the 
constant KNT minus 1 (KNT-1). In order to do not feel changes just because constants used
at compilation time we highly recommend to set explicitly both values. 

- The debug settings retrieval.debug.iteration_information has been improved. Now
the segment output functions can be use to print the results after each iteration.

To adapt all settings files to new feature, first you have to remove the old parameter:

```yml
retrieval:
    debug:
        iteration_information: false
```

And add the function(s) you want to use in output section, for example: 

```yml
output:
    iteration:
        function: ascii 
        stream: screen
```

By default the output function used is "none" so nothing will be printed


- All options related to cloud retrieval has been removed. This retrieval was an
experiment that it was not finish and these settings never should be arrived to an
official release. Specifically these settings can be removed safely:

```yml
retrieval
    products:    
        clouds:
            chemistry: false                          
            lidar: false                              
            optical_properties: false                 
            phase_matrix: false                       
            refractive_index: false                  
            theoretical_bimodal_extinction: false     
            theoretical_bimodal_parameters: false     
        error_estimation:
            clouds:
                lidar: false             
                optical_properties: false
          
```

## v0.7.4 (2017-05-15)

nothing to do to update to this version from v0.7.3


## v0.7.3 (2017-01-20)

nothing to do to update to this version from v0.7.2


## v0.7.2 (2016-11-11)

nothing to do to update to this version from v0.7.1 for users. Internally, the interface
of settings module has changed so new extensions have to have it into account.


## v0.7.1 (2016-07-28)

nothing to do to update to this version from v0.7.0


## v0.7.0 (2016-05-24)

nothing to do to update to this version from v0.6.8


## v0.6.8

- A characteristic type has been renamed: **size_distribution_bimodal** now is **size_distribution_lognormal**
- Kernels now are not available by default and the user has to install them via grasp-manager. The name of kernels has changed and it will affect to **kernels_folder** parameter
- The retrieval.kernel section has been reorganized

Before:
```yml
retrieval:
    kernel:
        phase_matrix_package:
        size_binning_method_for_triangle_bins: logarithm
            elements_of_phase_matrix: 4
            path_to_kernels: "AERONET_n22k16_83/"     
        radius:
            mode[1]:
                min: 0.05
                max: 15.0
```

After:
```yml
retrieval:
    phase_matrix:
        size_binning_method_for_triangle_bins: logarithm
        number_of_elements: 4
        kernels_folder: "aeronet_n22k16_83"     
        radius:
            mode[1]:
                min: 0.05
                max: 15.0
```

- Some input settings related with polder inversion has been moved to polder driver. If you don't use them you can remove them. If you work with polder driver you'll need to update following section:

Before:
```yml
input:
    gas_correction:
        enable: true
        path_no2: <valid path>
        path_o3: <valid path>
    land_ocean_filter: none
```

After:
```yml
input: 
    driver_settings:
        polder:
            gas_correction:
                enable: true
                path_no2: <valid path>
                path_o3: <valid path>
            land_ocean_filter: none
```


## v0.6.7

nothing to do to update to this version from v0.6.6


## v0.6.6

- **retrieval.debug.silent** is renamed by **retrieval.debug.verbose** and the value is reverse comparing to before value. True is print. Before it was false.
- **retrieval.debug.retrieval_iteration_information** is renamed by **retrieval.debug.iteration_information** 


## v0.6.5

nothing to do to update to this version from v0.6.4

## v0.6.4

### Some characteristic names has been renamed

Some of the possible values of retrieval.constraints.characteristic[].type has been change:
- **complex_refractive_index_chemistry** is renamed by **particle_component_fractions_chemical_mixture**
- **sphericity_fraction** is renamed by **sphere_fraction**
- **sphericity_distribution** is renamed by **aspect_ratio_distribution**

## v0.6.3

### single_pixel.a_priori_estimates have changed to an array.

The parameter **retrieval.constraints.characteristic[].mode[].single_pixel.a_priori_estimates.difference_order** has been removed
and **retrieval.constraints.characteristic[].mode[].single_pixel.a_priori_estimates.lagrange_multiplier** have change to an array of values.
Before, same value was applied to the characteristic but now it is possible to modify it for each parameter. By defualt these values are 0 so 
possibly you need just to remove these options:

```yml
a_priori_estimates:
    difference_order:   0
    lagrange_multiplier: 0.0
```

after each characteristic type. If you had not defined the value like 0 you have to add an array with as many elements as the characteristic
type with same value that you had before.

### Default value of retrieval.kernel.phase_matrix_package.size_binning_method_for_triangle_bins has changed

Before, the default value of size_binning_method_for_triangle_bins was absolute. If you have a 
settings file which you are using implicity this value, now you'll need to write it explicitly
because by default 'logarithm' will be used.

## v0.6.2

### New setting wavelength_indices_for_ndvi

A new setting has been added:

retrieval.product_configuration.wavelength_indices_for_ndvi: Indices of wavelengths which will be used to calculate NDVI if it is calculated

Before, this value was hardcoded to work with POLDER data. The settings files to
process polder data have been updated with:

```yml
retrieval:
    product_configuration:
        wavelength_indices_for_ndvi: [4, 5]
```


### The measurements 11 (TOD) and 12 (AOD) have been renamed

To clarify the code some constants have been renamed. TAU and EXT now are AOD and TOD. 
This change affects settings file in the description of noises. Specifically the parameter 
retrieval.noises.noise[].measurement_type[].type can be set to "aod" and "tod" instead 
of "tau" and "ext". So now retrieval.noises.noise[].measurement_type[].type can
be: tod, aod, p11, p12, p22, p33, p34, p44, ls, dp, rl, i, q or u .


## v0.6.1

### General block in settings has been refactored

The old general block has been reorganized completely. Here the list of changes that you have to do
in order to adapt a settings file from v0.6.0:

- **retrieval.general.path_to_internal_files** moved to **retrieval.general.path_to_internal_files**
- **retrieval.general.stop_before_performing_retrieval** moved to **retrieval.convergence.stop_before_performing_retrieval** 
- **retrieval.general.minimization_convention** moved to **retrieval.convergence.minimization_convention**
- **retrieval.general.maximum_iterations_of_Levenberg-Marquardt** moved to **retrieval.convergence.maximum_iterations_of_Levenberg-Marquardt** 
- **retrieval.general.threshold_for_stopping** moved to **retrieval.convergence.threshold_for_stopping**  
- **retrieval.general.threshold_for_stopping_Q_iterations** moved to **retrieval.convergence.threshold_for_stopping_Q_iterations** 
- **retrieval.general.scale_for_finite_difference** moved to **retrieval.convergence.scale_for_finite_difference** 
- **retrieval.general.normal_system_solver** moved to **retrieval.convergence.normal_system_solver** 
- **retrieval.general.shift_for_applying_logarithm_to_negative_values** moved to **retrieval.convergence.shift_for_applying_logarithm_to_negative_values**  
- **retrieval.general.maximum_iterations_for_stopping** moved to **retrieval.convergence.maximum_iterations_for_stopping** 
- **retrieval.general.regime_of_measurement_fitting** moved to **retrieval.regime_of_measurement_fitting.polarization** 
- **retrieval.general.regime_of_radiance_measurement_fitting** moved to **retrieval.regime_of_measurement_fitting.radiance** 
- **retrieval.general.regime_of_multipixel_constraints.inversion_regime** moved to **retrieval.regime_of_multipixel_constraints.inversion_regime** 
- **retrieval.general.regime_of_multipixel_constraints.time_space_groups** moved to **retrieval.regime_of_multipixel_constraints.time_space_groups**
- **retrieval.general.regime_of_multipixel_constraints.time-scale** moved to **retrieval.regime_of_multipixel_constraints.time-scale** 
- **retrieval.general.regime_of_multipixel_constraints.x-scale** moved to **retrieval.regime_of_multipixel_constraints.x-scale**
- **retrieval.general.regime_of_multipixel_constraints.y-scale** moved to **retrieval.regime_of_multipixel_constraints.y-scale**
- **retrieval.general.wavelength_indices_for_angstrom** moved to **retrieval.product_configuration.wavelength_indices_for_angstrom** 
- **retrieval.general.binning_method** moved to **retrieval.kernel.phase_matrix_package.size_binning_method_for_triangle_bins** 


Following example shows an update procedure:

Original code (v0.6.0)

```yml
retrieval:    
    general:
        minimization_convention: logarithm
        stop_before_performing_retrieval: false
        binning_method: logarithm
        maximum_iterations_of_Levenberg-Marquardt: 15
        threshold_for_stopping: 1.0e-5
        path_to_internal_files: "../../src/retrieval/internal_files/"
        regime_of_measurement_fitting: linear_polarization
        normal_system_solver: sparse_matrix_solver
        threshold_for_stopping_Q_iterations: 1e-12
        scale_for_finite_difference: 1.0e-5  
        wavelength_indices_for_angstrom: [4, 5]
        regime_of_multipixel_constraints: 
            inversion_regime: single_pixel 

```
Code after update:

```yml
retrieval:    
    general:
        path_to_internal_files: "../../src/retrieval/internal_files/"
        
    convergence:
        stop_before_performing_retrieval: false
        minimization_convention: logarithm
        maximum_iterations_of_Levenberg-Marquardt: 15
        threshold_for_stopping: 1.0e-5
        threshold_for_stopping_Q_iterations: 1e-12
        scale_for_finite_difference: 1.0e-5  
        normal_system_solver: sparse_matrix_solver
                
    regime_of_measurement_fitting:
        polarization: linear_polarization                
        
    product_configuration:
        wavelength_indices_for_angstrom: [4, 5]
    
    regime_of_multipixel_constraints: 
        inversion_regime: single_pixel 

    kernel:
        phase_matrix_package:
            size_binning_method_for_triangle_bins: logarithm
```


## v0.6.0
