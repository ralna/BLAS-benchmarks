module benchmark_types
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table, toml_array, get_value
   implicit none (external)
   public :: write_result_header, write_result
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

   function time_benchmark(self, n_iters) result(max_flops)
      class(Benchmark), intent(inout) :: self
      integer, intent(in) :: n_iters

      integer(int64) :: start_count, end_count
      integer(int64) :: count_rate, count_max

      real(real64) :: elapsed_time

      real(real64) :: max_flops, flops

      integer :: i

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

      max_flops = max_flops / (1000.0**3)
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

      write(iunit, "(A)") "implementation,m,n,k,gflops"
   end subroutine write_result_header

   subroutine write_result(iunit, implementation, gflops, m, n, k)
      !! Write a single measurement as one CSV line:
      !! implementation,m,n,k,gflops
      !! Dimensions (m, n, k) not applicable to a routine are omitted and
      !! written as empty fields.
      integer, intent(in) :: iunit
      !! Unit to write to

      character(len=*), intent(in) :: implementation
      !! Name of the BLAS implementation used

      real(real64), intent(in) :: gflops
      !! Measured performance in GFLOP/s

      integer(int64), intent(in), optional :: m, n, k
      !! Problem dimensions

      character(len=32) :: ms, ns, ks, gs
      !! String buffers for the numeric fields

      ms = ""
      ns = ""
      ks = ""
      if (present(m)) write(ms, "(I0)") m
      if (present(n)) write(ns, "(I0)") n
      if (present(k)) write(ks, "(I0)") k
      write(gs, "(F0.7)") gflops

      write(iunit, "(A)") trim(implementation)//","// &
         trim(ms)//","//trim(ns)//","//trim(ks)//","//trim(adjustl(gs))
   end subroutine write_result
end module benchmark_types
