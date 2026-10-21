module benchmark_config
   use iso_fortran_env, only: int64, real64
   use misc_benchmarks, only: DummyBenchmark, NaiveMatmulBenchmark
   use blas_l3_benchmarks, only: DGEMMBenchmark, DSYRKBenchmark, DSYR2KBenchmark
   use blas_l2_benchmarks, only: DGEMVBenchmark
   use blas_l1_benchmarks, only: DAXPYBenchmark, DASUMBenchmark

   use lapack_linsolve_benchmarks, only: DGESVBenchmark
   use benchmark_types, only: Benchmark, BenchmarkContainer
   use tomlf, only : toml_table, toml_array, toml_parse, toml_error, get_value, len

#ifdef CUDA
   use cublas_benchmarks,  only: CUBLASDGEMMBenchmark
#endif

#ifdef HIP
   use hipblas_benchmarks,  only: HIPBLASDGEMMBenchmark
#endif

   implicit none (external)
   public read_config
   private

contains

   subroutine read_config(config_file, benchmarks)
      !! Read in a TOML config file, initialise the benchmarks that it
      !! specifies and store them in the supplied array

      character(len=64), intent(in) :: config_file
      !! Name of TOML config file
      class(BenchmarkContainer), intent(out), allocatable :: benchmarks(:)
      !! Array of benchmark types to be filled

      type(toml_table), allocatable :: config_table
      !! TOML table of all benchmarks
      type(toml_array), pointer :: toml_arr
      !! Array of benchmark config tables
      type(toml_error), allocatable :: error
      !! Error type for parsing config file
      type(toml_table), pointer :: benchmark_table
      !! Table of an individual benchmark's config

      logical :: file_exists
      !! Does supplied config exist?
      integer :: in_unit
      !! Unit to read config file from
      integer :: i
      !! Loop counter
      integer :: num_repeats
      !! Number of timed calls per problem size

      !Read file
      inquire(file=config_file, exist=file_exists)

      if ( .not.(file_exists) ) then
         print *, "Supplied config file ", trim(config_file), " does not exist"
         stop 1
      end if
      open(file=config_file, newunit=in_unit)

      config_table = toml_table()

      call toml_parse(config_table, in_unit, error)
      close(in_unit)
      if (allocated(error)) then
         print '(a)', "Error: "//error%message
         stop 1
      end if

      call get_value(config_table, "num_repeats", num_repeats, 100)

      if ( num_repeats <= 0 ) then
         print *, "num_repeats must be positive"
         stop 1
      end if

      ! Loop over list of benchmark tables
      call get_value(config_table, "benchmarks", toml_arr)
      allocate(benchmarks(len(toml_arr)))

      do i = 1, size(benchmarks)
         call get_value(toml_arr, i, benchmark_table)
         ! Set up benchmark object according to config
         call init_benchmark(benchmark_table, benchmarks(i))
         benchmarks(i)%b%num_repeats = num_repeats
      end do
   end subroutine read_config

   subroutine init_benchmark(benchmark_table, benchmark)
      !! Initialise a benchmark type according to the given TOML config

      type(toml_table), pointer, intent(inout) :: benchmark_table
      !! TOML table containing benchmark parameters
      character(len=:), allocatable :: name
      !! Name of the benchmark
      class(BenchmarkContainer) :: benchmark
      !! Benchmark type which we will initialise to the type of
      !! the specified routine

      !Get name
      call get_value(benchmark_table, "name", name)

      !Create benchmark of correct derived type
      call select_benchmark(name, benchmark)

      select type(b => benchmark%b)
       class is (DummyBenchmark)
         print *, "WARNING: Failed to create benchmark for ", name, ": Invalid name"
         return
      end select

      !Intialize size arrays
      call benchmark%b%init(benchmark_table)
   end subroutine init_benchmark

   subroutine select_benchmark(name, benchmark)
      !! Match given name to derived type for a routine
      !! and allocate space for that type

      character(len=:), allocatable, intent(in) :: name
      !! Benchmark name
      class(BenchmarkContainer), intent(inout) :: benchmark
      !! Generic container type that will be allocated as a
      !! derived type specific to a routine

      select case (name)
       case ("DGEMM")
         allocate(DGEMMBenchmark::benchmark%b)
         benchmark%b = DGEMMBenchmark(name="DGEMM")

       case ("DGEMV")
         allocate(DGEMVBenchmark::benchmark%b)
         benchmark%b = DGEMVBenchmark(name="DGEMV")

       case ("DAXPY")
         allocate(DAXPYBenchmark::benchmark%b)
         benchmark%b = DAXPYBenchmark(name="DAXPY")

       case ("DASUM")
         allocate(DASUMBenchmark::benchmark%b)
         benchmark%b = DASUMBenchmark(name="DASUM")

       case ("DSYRK")
         allocate(DSYRKBenchmark::benchmark%b)
         benchmark%b = DSYRKBenchmark(name="DSYRK")

       case ("DSYR2K")
         allocate(DSYR2KBenchmark::benchmark%b)
         benchmark%b = DSYR2KBenchmark(name="DSYR2K")

       case ("DGESV")
         allocate(DGESVBenchmark::benchmark%b)
         benchmark%b = DGESVBenchmark(name="DGESV")

       case ("NAIVE")
         allocate(NaiveMatmulBenchmark::benchmark%b)
         benchmark%b = NaiveMatmulBenchmark(name="Naive")

#ifdef CUDA
       case ("CU_DGEMM")
         allocate(CUBLASDGEMMBenchmark::benchmark%b)
         benchmark%b = CUBLASDGEMMBenchmark(name="CUBLAS_DGEMM")
#endif
#ifdef HIP
        case ("HIP_DGEMM")
         allocate(HIPBLASDGEMMBenchmark::benchmark%b)
         benchmark%b = HIPBLASDGEMMBenchmark(name="HIPBLAS_DGEMM")
#endif
       case default
         allocate(DummyBenchmark::benchmark%b)
         benchmark%b = DummyBenchmark(name="failed")
      end select

      return
   end subroutine select_benchmark
end module benchmark_config
