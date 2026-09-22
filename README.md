# Green Computing Benchmarks

This repository contains a tool for benchmarking different implementations of the BLAS and LAPACK libraries.

## Installation

This tool has the following dependencies:
- [meson](https://mesonbuild.com/index.html), version 1.3.2 or newer.
- [ninja](https://ninja-build.org/)
- A Fortran compiler
    - The build has been tested with `gfortran 11.4.0` and `ifx 2025.3.0`. The project does not currently build with `flang`. 
- [FlexiBLAS](https://www.mpi-magdeburg.mpg.de/projects/flexiblas)
    - All development and testing was done with version `3.4.5`.
- [CMake](https://cmake.org/) (for finding dependencies)

To install the tool:

- Clone this repository
- Navigate to the green-computing-benchmarks directory
- Run the following commands:

```
meson setup build --optimization=3
meson compile -C build
```

To specify the compiler you want to use, replace the first command with: 
```
FC=compiler meson setup build --optimization=3
```
Where `compiler` is replaced with your chosen compiler executable.

### Building for GPUs

The tool has support for running on GPUs using CUDA and HIP.

#### CUDA
To enable CUDA benchmarks, use the meson option ``-Dcuda=true``. For example:
```
meson setup build -Dcuda=true --optimization=3
```
You must have an Nvidia GPU and the [CUDA and CUBLAS](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/) libraries installed.

#### HIP

To enable HIP benchmarks, use the meson option ``-Dhip=true``. For example:

```
meson setup build -Dhip=true --optimization=3
```

You must have [HIPFort](https://github.com/ROCm/hipfort) (note: this must be on your `CMAKE_PREFIX_PATH`, e.g `CMAKE_PREFIX_PATH=/opt/hipfort/lib/fortran/f95/cmake/`), [HIP](https://rocm.docs.amd.com/projects/HIP/en/docs-5.6.0/how_to_guides/install.html) and [HIPBLAS](https://rocm.docs.amd.com/projects/hipBLAS/en/develop/install/Linux_Install_Guide.html) installed. Currently this tool only supports HIP on AMD GPUs.

## Running

You can specify the benchmarks you wish to run using a [toml](https://toml.io/en/) configuration file. To add a function to benchmark, use the syntax:

```
[[benchmarks]]
name = "NAME"
m-sizes = [10, 20, 30]
n-sizes = [10, 50, 30]
k-sizes = [10, 50, 30]
```

`example.toml` is included at the top level of this repository to demonstrate an example config.

Where "NAME" corresponds to the function you wish to benchmark. The list of available functions is below; unknown names will be ignored. `m-` `n-` and `k-sizes` correspond to the sizes of the matrices and/or vectors the functions will be benchmarked with. 
Benchmarks will be run on the products of these arrays, for example the config above will run on 27 different sized problems. The problems are randomly generated.

To run the benchmarks, run `build/benchmark_blas config.toml`, where `config.toml` is replaced with the path to your own configuration file.
A separate csv file will be created for each routine in the directory you run the tool from. Alternatively, you can run the `run_blas_benchmarks.sh` script, which will run your chosen benchmarks with all BLAS implementations available to FLEXIBLAS and collect all results files in a timestamped directory. We recommend that this script is not used for GPU runs, as changing the BLAS  implementation will have no effect but multiple runs will still be performed.
The same is true of the `NAIVE` benchmark option.

To manually change the BLAS backend benchmarks are run with, use:

```
FLEXIBLAS="YOUR_BLAS" ./build/benchmark_blas config.toml
```
Available BLAS backends are shown with `flexiblas list`.

## Visualising results

A `plot_results.py` script is included to turn the generated CSV files into
heatmaps, to compare the performance of different BLAS
implementations across problem sizes. Each heatmap has the implementations down
the rows and the problem sizes across the columns, with colour encoding the
measured GFLOP/s.

Install its single dependency and run it:

```
pip install -r requirements.txt
python plot_results.py results_<datetime>/
```

With no arguments it discovers `results_*.csv` in the current directory and one
level below; you can also pass specific files or directories. A `.png` is written
next to each CSV.

## Available functions

### Level 1 (vector operations):
- DAXPY
    - Double precision $\alpha x + y$
    - Required options:
        - n-sizes (array)
- DASUM
    - Double precision sum of the absolute values of a vector
    - Required options:
        - n-sizes (array)

### Level 2 (matrix-vector operations):
- DGEMV
    - Double precision $\alpha A x + \beta y$
    - Required options:
        - m-sizes (array)
        - n-sizes (array)

### Level 3 (matrix-matrix operations):
- DGEMM
    - Double precision $\alpha A B + \beta C$
    - Required options:
        - m-sizes (array)
        - n-sizes (array)
        - k-sizes (array)

- CUBLAS_DGEMM (CUDA only)
    - Double precision $\alpha A B + \beta C$ on Nvidia GPU
    - Required options:
        - m-sizes (array)
        - n-sizes (array)
        - k-sizes (array)

- HIPBLAS_DGEMM (HIP only)
    - Double precision $\alpha A B + \beta C$ on AMD GPU
    - Required options:
        - m-sizes (array)
        - n-sizes (array)
        - k-sizes (array)

- DSYRK
    - Double precision symmetric rank-k update $\alpha A A^T + \beta C$
    - Required options:
        - n-sizes (array)
        - k-sizes (array)

- DSY2RK
    - Double precision symmetric rank-2k update $\alpha A B^T + \alpha B A^T+ \beta C$
    - Required options:
        - n-sizes (array)
        - k-sizes (array)

- NAIVE
    - Naive non-BLAS matrix multiply $\alpha A B + \beta C$
    - Required options:
        - m-sizes (array)
        - n-sizes (array)
        - k-sizes (array)

### LAPACK Linear solvers

- DGESV
    - Double precision solution to $Ax = B$
    - Required options:
        - n-sizes (array)