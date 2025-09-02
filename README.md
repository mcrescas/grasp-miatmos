
# GRASP 

GRASP is a highly accurate aerosol retrieval algorithm that processes properties 
of aerosol- and land-surface-reflectance in cloud free environments. It infers 
nearly 50 aerosol and surface parameters including particle size distribution, 
the spectral index of refraction, the degree of sphericity and absorption. The 
algorithm is designed for the enhanced characterization of aerosol properties 
from spectral, multiangular polarimetric remote sensing observations. GRASP works 
under different conditions, including bright surfaces like deserts, where the 
reflectance overwhelms the signal of aerosols. GRASP is highly versatile and allows
 input from a wide variety of satellite and surface measurements.

For more information about GRASP, please see the official project webpage: http://www.grasp-open.com/

For user documentation, please go to: http://www.grasp-open.com/doc

For technical documentation, please go to: http://www.grasp-open.com/tech-doc

The code is released under GRASP Open Source License V1.0 
Copyright 2016 CNRS & Universite de Lille. All rights reserved.
See LICENSE file for more information. 
For more information on commercial use or interest in a specific research project, please contact office@grasp-sas.com


# How to build

There are many different ways to get the code compiled. GRASP uses cmake 
(http://www.cmake.org/) to compile the code, but it is a wrapper realised by a classical make
 script which simplifies the procedure.  Before you continue, please check that your 
environment fulfils all requirements. For more information, please check the "Build dependencies" section.

## Using make wrapper

```sh
make # build the project using the default build settings
sudo make install # install grasp
grasp # test the command
```


## Advanced mode: Using directly cmake

```sh
mkdir ~/grasp/build
cd ~/grasp/build
cmake .. -DCMAKE_BUILD_TYPE=Release -DADDITIONAL_DEPENDENCIES_PATH=/usr/local/grasp-deps -DCONSTANTS_SET=generic
make -j12
sudo make install
grasp # test the command
```


## Building GPGPU retrieval

GPGPU module is now an external extension. Use grasp manager to install it and then
to run the GPGPU version of GRASP, the following CMake flags are required:
```sh
-DENABLE_MODULES=gpgpu -DENABLE_GPGPU=ON
```


## Build dependencies

The following instructions will guide the user to install the dependencies required to compile
the core of GRASP code. The core can be extended using extensions, and those extensions 
may require some additional dependencies. Please check the instructions for the extensions
you want to install, in case you wish to have more specific information about its requirements.

### Ubuntu:

Required:

```sh
sudo apt-get install build-essential cmake git gfortran libyaml-dev libglib2.0-dev libcunit1-dev 
```

SuperLu (solver) is also required. GRASP is compatible with version 4, 5 and 6 of SuperLu. The following command will install the primary version of your system.

```sh
sudo apt-get install libsuperlu-dev
```

The user has to take into account that there are optional dependencies, according to the installed extensions.

### CentOS

```sh
yum install CUnit-devel SuperLU-devel blas-devel cmake gcc libyaml-devel zlib-devel
```

### MacOS:

The following steps are based on Homebrew package manager. First, the Homebrew has to be installed in your system (https://brew.sh/index_es.html). To install it, you can type:

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Once Homebrew is available, you can install the dependencies with the following command:

```sh
brew install gcc cmake libyaml glib cunit pkg-config
```
Superlu (solver) dependency is also needed. An old version of superlu (superlu4) is installed
diferently than the latest one (superlu5). The newest version can be installed by:

```sh
brew install superlu
```

Just, to keep the compatiblity with older systems, in case the previous command
did not work, the user can try: (the old way)

```sh
brew install homebrew/science/superlu43
brew link --force homebrew/science/superlu43 
```


### Windows:

Windows systems are only supported thanks to Windows Subsystem for Linux (WSL): https://msdn.microsoft.com/es-es/commandline/wsl/install_guide
This feature is only available by Windows 10 or newer. Once you have installed WSL, you can follow the instructions of Ubuntu systems.

There are other alternatives such as virtual machines or toolbox as CygWin. The installation using these tools is more complex and requires
some extra knowledge.


# GPGPU version

The GPGPU code is currently at the state of the v0.3.1 tag.

For Windows users we recommend using the [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10)
