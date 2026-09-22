module blas_l3_benchmarks
   use benchmark_types, only: Benchmark, read_array, write_result_header, write_result
   use l3_interfaces, only: dgemm, dsyrk, dsyr2k
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: DGEMMBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0

      integer(int64) :: m = 1
      integer(int64) :: n = 1
      integer(int64) :: k = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:,:), allocatable :: B
      double precision, dimension(:,:), allocatable :: C
   contains
      procedure :: run => run_dgemm
      procedure :: call_benchmark => call_dgemm
      procedure :: init => init_dgemm
   end type DGEMMBenchmark

   type, public, extends(Benchmark) :: DSYRKBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0

      integer(int64) :: n = 1
      integer(int64) :: k = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:,:), allocatable :: C
   contains
      procedure :: run => run_dsyrk
      procedure :: call_benchmark => call_dsyrk
      procedure :: init => init_dsyrk
   end type DSYRKBenchmark

   type, public, extends(Benchmark) :: DSYR2KBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0

      integer(int64) :: n = 1
      integer(int64) :: k = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:,:), allocatable :: B
      double precision, dimension(:,:), allocatable :: C
   contains
      procedure :: run => run_dsyr2k
      procedure :: call_benchmark => call_dsyr2k
      procedure :: init => init_dsyr2k
   end type DSYR2KBenchmark
contains

   !!!!!!!!!
   ! DGEMM !
   !!!!!!!!!
   subroutine init_dgemm(self, benchmark_table)
      class(DGEMMBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "m-sizes", self%name,  self%m_sizes)
      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)

   end subroutine init_dgemm

   subroutine run_dgemm(self, blas_name)
      class(DGEMMBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, j, k, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%m_sizes)
         do j = 1, size(self%n_sizes)
            do k = 1, size(self%k_sizes)
               self%m = self%m_sizes(i)
               self%n = self%n_sizes(j)
               self%k = self%k_sizes(k)

               self%num_flops = 2 * self%m * self%n * self%k

               allocate(self%A(self%m , self%k))
               allocate(self%B(self%k , self%n))
               allocate(self%C(self%m , self%n))

               call random_number(self%A)
               call random_number(self%B)
               call random_number(self%C)

               avg_gflops = self%time_benchmark(100)

               call write_result(iunit, trim(blas_name), &
                  avg_gflops, m=self%m, n=self%n, k=self%k)

               deallocate(self%A)
               deallocate(self%B)
               deallocate(self%C)
            end do
         end do
      end do

      close(iunit)
   end subroutine run_dgemm

   subroutine call_dgemm(self)
      class(DGEMMBenchmark), intent(inout) :: self
      call dgemm(&
         "N",&
         "N",&
         self%m,&
         self%n,&
         self%k,&
         self%alpha,&
         self%A,&
         self%m,&
         self%B,&
         self%k,&
         self%beta,&
         self%C,&
         self%m&
         )
   end subroutine call_dgemm

   !!!!!!!!!
   ! DSYRK !
   !!!!!!!!!
   subroutine init_dsyrk(self, benchmark_table)
      class(DSYRKBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "n-sizes", self%name,  self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)

   end subroutine init_dsyrk

   subroutine run_dsyrk(self, blas_name)
      class(DSYRKBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, j, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%n_sizes)
         do j = 1, size(self%k_sizes)
            self%n = self%n_sizes(i)
            self%k = self%k_sizes(j)

            self%num_flops = self%n * self%n * self%k

            allocate(self%A(self%n , self%k))
            allocate(self%C(self%n , self%n))

            call random_number(self%A)
            call random_number(self%C)

            avg_gflops = self%time_benchmark(100)

            call write_result(iunit, trim(blas_name), &
               avg_gflops, n=self%n, k=self%k)

            deallocate(self%A)
            deallocate(self%C)
         end do
      end do

      close(iunit)
   end subroutine run_dsyrk

   subroutine call_dsyrk(self)
      class(DSYRKBenchmark), intent(inout) :: self
      call dsyrk(&
         "U",&
         "N",&
         self%n,&
         self%k,&
         self%alpha,&
         self%A,&
         self%n,&
         self%beta,&
         self%C,&
         self%n&
         )
   end subroutine call_dsyrk

   !!!!!!!!!!
   ! DSYR2K !
   !!!!!!!!!!

   subroutine init_dsyr2k(self, benchmark_table)
      class(DSYR2KBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "n-sizes", self%name,  self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)

   end subroutine init_dsyr2k

   subroutine run_dsyr2k(self, blas_name)
      class(DSYR2KBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, j, iunit
      real(real64) :: avg_gflops
      logical :: file_exists

      call self%open_results_file(iunit, file_exists)

      if (.not. file_exists) call write_result_header(iunit)

      do i = 1, size(self%n_sizes)
         do j = 1, size(self%k_sizes)
            self%n = self%n_sizes(i)
            self%k = self%k_sizes(j)

            self%num_flops = self%n * self%n * self%k

            allocate(self%A(self%n , self%k))
            allocate(self%B(self%n , self%k))
            allocate(self%C(self%n , self%n))

            call random_number(self%A)
            call random_number(self%B)
            call random_number(self%C)

            avg_gflops = self%time_benchmark(100)

            call write_result(iunit, trim(blas_name), &
               avg_gflops, n=self%n, k=self%k)

            deallocate(self%A)
            deallocate(self%B)
            deallocate(self%C)
         end do
      end do

      close(iunit)
   end subroutine run_dsyr2k

   subroutine call_dsyr2k(self)
      class(DSYR2KBenchmark), intent(inout) :: self
      call dsyr2k(&
         "U",&
         "N",&
         self%n,&
         self%k,&
         self%alpha,&
         self%A,&
         self%n,&
         self%B,&
         self%n,&
         self%beta,&
         self%C,&
         self%n&
         )
   end subroutine call_dsyr2k
end module blas_l3_benchmarks
