# Meson overlay for energy-fortran, which upstream only builds with fpm.
project(
  'fortran_energy',
  'fortran',
  version: '0.1.0',
  license: 'MIT',
  meson_version: '>=1.3.2',
)

fortran_energy_lib = static_library(
  'fortran_energy',
  'src/fortran_energy.f90',
)

fortran_energy_dep = declare_dependency(
  link_with: fortran_energy_lib,
)
