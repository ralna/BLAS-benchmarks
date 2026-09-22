module blas_l1_benchmarks
   use benchmark_types, only: Benchmark, write_result_header, write_result, read_array
   use l1_interfaces, only: dasum, daxpy
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: DAXPYBenchmark
      double precision :: alpha = 1.

      integer(int64) :: n = 1

      double precision, dimension(:), allocatable :: x
      double precision, dimension(:), allocatable :: y

   contains
      procedure :: call_benchmark => call_daxpy
      procedure :: run => run_daxpy
      procedure :: init => init_daxpy
   end type DAXPYBenchmark

   type, public, extends(Benchmark) :: DASUMBenchmark
      double precision :: alpha = 1.

      integer(int64) :: n = 1

      double precision, dimension(:), allocatable :: x

   contains
      procedure :: call_benchmark => call_dasum
      procedure :: run => run_dasum
      procedure :: init => init_dasum
   end type DASUMBenchmark

contains
   !!!!!!!!!
   ! DAXPY !
   !!!!!!!!!

   subroutine init_daxpy(self, benchmark_table)
      class(DAXPYBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
   end subroutine init_daxpy

   subroutine call_daxpy(self)
      class(DAXPYBenchmark), intent(inout) :: self
      self%num_flops = self%n
      call daxpy(self%n, self%alpha, self%x, 1, self%y, 1)
   end subroutine call_daxpy

   subroutine run_daxpy(self, blas_name)
      class(DAXPYBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%n_sizes)
         self%n = self%n_sizes(i)

         self%num_flops = self%n

         allocate(self%x(self%n))
         allocate(self%y(self%n))

         call random_number(self%x)
         call random_number(self%y)

         avg_gflops = self%time_benchmark(100)

         call write_result(iunit, trim(blas_name), &
            avg_gflops, n=self%n)

         deallocate(self%x)
         deallocate(self%y)
      end do

      close(iunit)
   end subroutine run_daxpy

   !!!!!!!!!
   ! DASUM !
   !!!!!!!!!

   subroutine init_dasum(self, benchmark_table)
      class(DASUMBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
   end subroutine init_dasum

   subroutine call_dasum(self)
      class(DASUMBenchmark), intent(inout) :: self
      self%num_flops = self%n
      call dasum(self%n, self%x, 1)
   end subroutine call_dasum

   subroutine run_dasum(self, blas_name)
      class(DASUMBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%n_sizes)
         self%n = self%n_sizes(i)

         self%num_flops = self%n

         allocate(self%x(self%n))

         call random_number(self%x)

         avg_gflops = self%time_benchmark(100)

         call write_result(iunit, trim(blas_name), &
            avg_gflops, n=self%n)

         deallocate(self%x)
      end do

      close(iunit)
   end subroutine run_dasum


end module blas_l1_benchmarks
