# KNOWN BUGS


## v0.8.2

- Bug in the output: Many fields where 0 or inconsistent. It cannot be identified by aerosol products but all surface products (and maybe others) affected. Fixed in v0.8.3
- radiative transfer kernels work with only one aerosol mode, generation for two either overwrites everything in one file, either messes up with the first component of the first mode

