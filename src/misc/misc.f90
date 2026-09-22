module misc_benchmarks
   use benchmark_types, only: Benchmark, write_result_header, write_result, read_array
   use iso_fortran_env, only: int64, real64
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: NaiveMatmulBenchmark
      double precision :: alpha = 1.0
      double precision :: beta = 1.0

      integer(int64) :: m = 1
      integer(int64) :: n = 1
      integer(int64) :: k = 1

      double precision, dimension(:,:), allocatable :: A
      double precision, dimension(:,:), allocatable :: B
      double precision, dimension(:,:), allocatable :: C
   contains
      procedure :: call_benchmark => call_naive_matmul
      procedure :: run => run_naive_matmul
      procedure :: init => init_naive_matmul
   end type NaiveMatmulBenchmark

   type, public, extends(Benchmark) :: DummyBenchmark
   contains
      procedure :: run => dummy_run
      procedure :: call_benchmark => dummy_call
      procedure :: init => dummy_init
   end type DummyBenchmark

contains
   !!!!!!!!!
   ! NAIVE !
   !!!!!!!!!
   subroutine init_naive_matmul(self, benchmark_table)
      class(NaiveMatmulBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "m-sizes", self%name, self%m_sizes)
      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)

   end subroutine init_naive_matmul

   subroutine run_naive_matmul(self, blas_name)
      class(NaiveMatmulBenchmark), intent(inout) :: self
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
   end subroutine run_naive_matmul

   subroutine call_naive_matmul(self)
      class(NaiveMatmulBenchmark), intent(inout) :: self
      self%num_flops = 2 * self%m * self%n * self%k
      call naive_matmul(&
         self%m,&
         self%n,&
         self%k,&
         self%alpha,&
         self%beta,&
         self%A,&
         self%B,&
         self%C&
         )
   end subroutine call_naive_matmul

   subroutine naive_matmul(m, n, k, alpha, beta, A, B, C)
      integer(int64), intent(in) :: m, n, k
      double precision, intent(in) :: alpha, beta
      double precision, dimension(:,:), intent(in) ::  A, B
      double precision, dimension(:,:), intent(inout) ::  C

      integer(int64) x, y, z

      do x = 1, m
         do y = 1, k
            do z = 1, n
               C(x, z) = beta * C(x, z) + alpha * A(x, y) * B(y, z)
            end do
         end do
      end do

   end subroutine naive_matmul

   subroutine dummy_run(self, blas_name)
      class(DummyBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name
   end subroutine dummy_run

   subroutine dummy_call(self)
      class(DummyBenchmark), intent(inout) :: self
   end subroutine dummy_call

   subroutine dummy_init(self, benchmark_table)
      class(DummyBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table
   end subroutine dummy_init
end module misc_benchmarks
