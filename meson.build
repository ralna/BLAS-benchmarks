project(
  'green_computing_benchmark',
  'fortran',
  'c',
  version: '0.1.0',
  default_options: ['warning_level=3'],
  meson_version: '>=1.3.2',
)
###################
# Setup           #
###################

src = []
deps = []
pp_args = ['-cpp']

use_cuda = get_option('cuda')
use_hip = get_option('hip')

fc = meson.get_compiler('fortran')

###################
# Define sources  #
###################

subdir('src')

tomlf_dep = dependency(
  'toml-f',
  version: '>=0.2.0',
  fallback: ['toml-f', 'tomlf_dep'],
  default_options: ['default_library=static'],
)

deps += tomlf_dep

if use_cuda
  subdir('src/gpu/cuda')

  cuda_dep = dependency('cuda', version: '>=11.0', modules: ['cublas', 'cudart'])
  deps += cuda_dep
  pp_args += ['-DCUDA']
endif

if use_hip
  subdir('src/gpu/hip')

  hipfort_dep = dependency('hipfort', modules: ['hipfort::hipfort-amdgcn'])
  deps += hipfort_dep

  hipblas_dep = dependency('hipblas')
  deps += hipblas_dep
  pp_args += ['-DHIP']
endif

###################
# Build library   #
###################

flexiblas_dep = dependency('flexiblas')
deps += flexiblas_dep

executable(
  'benchmark_blas',
  src,
  fortran_args: pp_args,
  dependencies: deps,
  link_language: 'fortran',
  install: true,
)