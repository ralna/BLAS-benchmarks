module lapack_linsolve_benchmarks
   use benchmark_types, only: Benchmark, read_array, write_result_header, write_result
   use lapack_interfaces, only: dgesv
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: DGESVBenchmark
      integer(int64) :: n = 1
      integer(int64) :: nrhs = 1

      integer :: info = 0

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:,:), allocatable :: B
      double precision, dimension(:), allocatable :: ipiv

   contains
      procedure :: run => run_dgesv
      procedure :: call_benchmark => call_dgesv
      procedure :: init => init_dgesv
   end type DGESVBenchmark

contains

   !!!!!!!!!
   ! DGESV !
   !!!!!!!!!
   subroutine init_dgesv(self, benchmark_table)
      class(DGESVBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)

   end subroutine init_dgesv

   subroutine run_dgesv(self, blas_name)
      class(DGESVBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, iunit
      real(real64) :: avg_gflops

      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%n_sizes)
         self%n = self%n_sizes(i)

         ! From https://www.netlib.org/lapack/lug/node71.html#standardflopcount
         self%num_flops = 0.67 * self%n ** 3.0

         allocate(self%A(self%n , self%n))
         allocate(self%B(self%n , self%nrhs))
         allocate(self%ipiv(self%n))

         call random_number(self%A)
         call random_number(self%B)

         avg_gflops = self%time_benchmark(100)

         call write_result(iunit, trim(blas_name), &
            avg_gflops, n=self%n)

         deallocate(self%A)
         deallocate(self%B)
         deallocate(self%ipiv)
      end do

      close(iunit)
   end subroutine run_dgesv

   subroutine call_dgesv(self)
      class(DGESVBenchmark), intent(inout) :: self
      call dgesv(&
         self%n,&
         self%nrhs,&
         self%A,&
         self%n,&
         self%ipiv,&
         self%B,&
         self%n,&
         self%info&
         )

   end subroutine call_dgesv
end module lapack_linsolve_benchmarks
