module blas_l2_benchmarks
   use benchmark_types, only: Benchmark, write_result_header, write_result, read_array
   use l2_interfaces, only: dgemv, sgemv
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: DGEMVBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0

      integer(int64) :: m = 1
      integer(int64) :: n = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:), allocatable :: x
      double precision, dimension(:), allocatable :: y
   contains
      procedure :: call_benchmark => call_dgemv
      procedure :: run => run_dgemv
      procedure :: init => init_dgemv
   end type DGEMVBenchmark

contains

   !!!!!!!!!
   ! DGEMV !
   !!!!!!!!!

   subroutine init_dgemv(self, benchmark_table)
      class(DGEMVBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "m-sizes", self%name, self%m_sizes)
      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
   end subroutine init_dgemv

   subroutine call_dgemv(self)
      class(DGEMVBenchmark), intent(inout) :: self

      call dgemv(&
         "N",&
         self%m,&
         self%n,&
         self%alpha,&
         self%A,&
         self%m,&
         self%x,&
         1,&
         self%beta,&
         self%y,&
         1&
         )
   end subroutine call_dgemv

   subroutine run_dgemv(self, blas_name)
      class(DGEMVBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, j, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%m_sizes)
         do j = 1, size(self%n_sizes)
            self%m = self%m_sizes(i)
            self%n = self%n_sizes(j)

            self%num_flops = 2 * self%m * self%n

            allocate(self%A(self%m , self%n))
            allocate(self%x(self%n))
            allocate(self%y(self%m))

            call random_number(self%A)
            call random_number(self%x)
            call random_number(self%y)

            avg_gflops = self%time_benchmark(100)

            call write_result(iunit, trim(blas_name), &
               avg_gflops, m=self%m, n=self%n)

            deallocate(self%A)
            deallocate(self%x)
            deallocate(self%y)
         end do
      end do

   close(iunit)
end subroutine run_dgemv

end module blas_l2_benchmarks
