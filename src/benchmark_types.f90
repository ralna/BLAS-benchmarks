module benchmark_types
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table, toml_array, get_value
   use fortran_energy_mod, only: energy_interval, energy_interval_t
   implicit none (external)
   public :: write_result_header, write_result

   type, public :: BenchmarkResult
      real(real64) :: gflops = 0
      !! Best GFLOP/s over all iterations
      real(real64) :: watts = 0
      !! Mean power over the whole timed loop (W)
      real(real64) :: gflops_per_joule = 0
      !! Energy efficiency over the whole timed loop (GFLOP/J)
   end type BenchmarkResult

   type, public, abstract :: Benchmark
      integer(int64) :: num_flops = 0.
      character(len=32) :: name

      integer, dimension(:), allocatable :: m_sizes
      integer, dimension(:), allocatable :: n_sizes
      integer, dimension(:), allocatable :: k_sizes

   contains
      procedure(run), deferred :: run
      procedure(call_benchmark), deferred :: call_benchmark
      procedure(init), deferred :: init
      procedure :: open_results_file
      procedure :: get_filename
      procedure :: time_benchmark
   end type Benchmark

   type, public :: BenchmarkContainer
      class(Benchmark), allocatable :: b
   end type BenchmarkContainer

   abstract interface
      subroutine run(self, blas_name)
         import
         implicit none (external)
         class(Benchmark), intent(inout) :: self
         character(len=16), intent(in) :: blas_name
      end subroutine run

      subroutine call_benchmark(self)
         import
         implicit none (external)
         class(Benchmark), intent(inout) :: self
      end subroutine call_benchmark

      subroutine init(self, benchmark_table)
         import
         implicit none (external)
         class(Benchmark), intent(inout) :: self
         type(toml_table), pointer, intent(in) :: benchmark_table

      end subroutine init
   end interface

contains
   character(len=64) function get_filename(self) result(filename)
      class(Benchmark), intent(inout) :: self

      filename = "results_"//trim(self%name)//".csv"
      return
   end function get_filename

   function time_benchmark(self, n_iters) result(res)
      !! Run the benchmark n_iters times. Timing is per iteration (best
      !! GFLOP/s is kept); energy is measured once around the whole loop,
      !! as a single call is usually shorter than the sensor update interval.
      class(Benchmark), intent(inout) :: self
      integer, intent(in) :: n_iters

      type(BenchmarkResult) :: res

      integer(int64) :: start_count, end_count
      integer(int64) :: count_rate, count_max

      real(real64) :: elapsed_time

      real(real64) :: max_flops, flops

      type(energy_interval_t) :: iv

      integer :: i

      ! Discard energy consumed since the previous measurement
      call energy_interval(iv)

      max_flops = 0
      do i = 1, n_iters
         call system_clock(start_count, count_rate, count_max)
         call self%call_benchmark()
         call system_clock(end_count, count_rate, count_max)

         !Time (s)
         elapsed_time = real(end_count - start_count) / real(count_rate)

         !FLOPS/s
         flops = real(self%num_flops) / (elapsed_time)

         max_flops = max(max_flops, flops)

      end do

      call energy_interval(iv)

      res%gflops = max_flops / (1000.0**3)

      if (iv%j > 0) then
         res%watts = iv%w_mean
         res%gflops_per_joule = real(self%num_flops, real64) * n_iters / iv%j / (1000.0**3)
      end if
      return
   end function time_benchmark

   subroutine open_results_file(self, iunit, file_exists)
      class(Benchmark), intent(inout) :: self
      integer, intent(out) :: iunit

      character(len = 64) :: filename
      logical, intent(out) :: file_exists

      filename = self%get_filename()

      inquire(file=filename, exist=file_exists)
      open(newunit=iunit, file=filename, position="append")
   end subroutine open_results_file


   subroutine read_array(table, arr_name, benchmark_name,  array)
      !! Read array in toml table called "name" into the provided array

      type(toml_table), pointer, intent(in) :: table
      !! toml table containing benchmark config

      character(len=*), intent(in) :: arr_name
      !! Name of array to read in

      character(len=*), intent(in) :: benchmark_name
      !! Name of benchmark config we are reading
      !! Needed for error message

      integer, allocatable, intent(out) ::  array(:)
      !! Array to populate

      type(toml_array), pointer :: toml_arr
      !! TOML array to hold size array information

      call get_value(table, arr_name, toml_arr)
      call get_value(toml_arr, array)

      if ( size(array) <= 0 ) then
         print *, "Missing ",  arr_name, " in ", trim(benchmark_name), " config"
         stop 1
      end if

   end subroutine read_array

   subroutine write_result_header(iunit)
      integer, intent(in) :: iunit
      !! Unit to write to

      write(iunit, "(A)") "implementation,m,n,k,gflops,watts,gflops_per_joule"
   end subroutine write_result_header

   subroutine write_result(iunit, implementation, res, m, n, k)
      !! Write a single measurement as one CSV line:
      !! implementation,m,n,k,gflops,watts,gflops_per_joule
      !! Dimensions (m, n, k) not applicable to a routine are omitted and
      !! written as empty fields. Energy fields are empty when no energy
      !! was measured (no sensor, or the run was too short to register).
      integer, intent(in) :: iunit
      !! Unit to write to

      character(len=*), intent(in) :: implementation
      !! Name of the BLAS implementation used

      type(BenchmarkResult), intent(in) :: res
      !! Measured performance and energy

      integer(int64), intent(in), optional :: m, n, k
      !! Problem dimensions

      character(len=32) :: ms, ns, ks, gs, ws, es
      !! String buffers for the numeric fields

      ms = ""
      ns = ""
      ks = ""
      if (present(m)) write(ms, "(I0)") m
      if (present(n)) write(ns, "(I0)") n
      if (present(k)) write(ks, "(I0)") k
      write(gs, "(F0.7)") res%gflops

      ws = ""
      es = ""
      if (res%gflops_per_joule > 0) then
         write(ws, "(F0.7)") res%watts
         write(es, "(F0.7)") res%gflops_per_joule
      end if

      write(iunit, "(A)") trim(implementation)//","// &
         trim(ms)//","//trim(ns)//","//trim(ks)//","//trim(adjustl(gs))//","// &
         trim(adjustl(ws))//","//trim(adjustl(es))
   end subroutine write_result
end module benchmark_types
