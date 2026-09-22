module cublas_benchmarks
   use benchmark_types, only: Benchmark, read_array, write_result_header, write_result
   use iso_fortran_env, only: int64, real64, int32
   use iso_c_binding, only: c_ptr, c_null_ptr, c_size_t, c_int, c_long, c_double
   use cuda_interfaces
   use tomlf, only: toml_table
   implicit none (external)
   private

   type, public, extends(Benchmark) :: CUBLASDGEMMBenchmark
      integer(c_long) :: m = 1
      integer(c_long) :: n = 1
      integer(c_long) :: k = 1

      real(c_double) :: alpha = 1.0
      real(c_double) :: beta = 1.0

      ! Host arrays
      real(c_double), dimension(:,:), pointer :: A => null()
      real(c_double), dimension(:,:), pointer :: B => null()
      real(c_double), dimension(:,:), pointer :: C => null()

      ! Device arrays
      type(c_ptr) :: A_d = c_null_ptr
      type(c_ptr) :: B_d = c_null_ptr
      type(c_ptr) :: C_d = c_null_ptr

      ! cuBLAS handle
      type(c_ptr) :: handle = c_null_ptr

   contains
      procedure :: run => run_cublas_dgemm
      procedure :: call_benchmark => call_cublas_dgemm
      procedure :: init => init_cublas_dgemm
   end type CUBLASDGEMMBenchmark

contains
   subroutine cuda_err_check(stat, flag)
      integer, intent(in) :: stat
      integer, intent(out) :: flag

      if ( stat /= 0 ) then
         print *, "CUDA failed with error: ", stat
         flag = 1
      end if
   end subroutine cuda_err_check

   !!!!!!!!!!!!!!!!
   ! CUBLAS DGEMM !
   !!!!!!!!!!!!!!!!
   subroutine init_cublas_dgemm(self, benchmark_table)
      class(CUBLASDGEMMBenchmark), intent(inout) :: self
      type(toml_table), pointer, intent(in) :: benchmark_table

      call read_array(benchmark_table, "m-sizes", self%name, self%m_sizes)
      call read_array(benchmark_table, "n-sizes", self%name, self%n_sizes)
      call read_array(benchmark_table, "k-sizes", self%name, self%k_sizes)
   end subroutine init_cublas_dgemm

   subroutine run_cublas_dgemm(self, blas_name)
      class(CUBLASDGEMMBenchmark), intent(inout) :: self
      character(len=16), intent(in) :: blas_name

      integer :: i, j, k, iunit
      real(real64) :: avg_gflops
      logical :: file_exists
      integer :: flag = 0

      ! init CUBLAS handle
      self%handle = c_null_ptr
      call cuda_err_check(cublasCreate(self%handle), flag)

      if ( flag == 1 ) then
         print *, "Failed to create CUBLAS handle"
         return
      end if

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

               ! Allocate device memory
               call cuda_err_check(cudaMalloc(self%A_d,&
                  int(self%m*self%k, c_size_t)*c_sizeof(self%A(1,1))),&
                  flag)
               call cuda_err_check(cudaMalloc(self%B_d,&
                  int(self%k*self%n, c_size_t)*c_sizeof(self%B(1,1))),&
                  flag)
               call cuda_err_check(cudaMalloc(self%C_d,&
                  int(self%m*self%n, c_size_t)*c_sizeof(self%C(1,1))),&
                  flag)

               ! Copy to device
               call cuda_err_check(cudaMemcpy(self%A_d,&
                  c_loc(self%A(1,1)), int(self%m*self%k, c_size_t)*c_sizeof(self%A(1,1)), cudaMemcpyHostToDevice),&
                  flag)

               call cuda_err_check(cudaMemcpy(self%B_d,&
                  c_loc(self%B(1,1)), int(self%k*self%n, c_size_t)*c_sizeof(self%B(1,1)), cudaMemcpyHostToDevice),&
                  flag)

               call cuda_err_check(cudaMemcpy(self%C_d,&
                  c_loc(self%C(1,1)), int(self%m*self%n, c_size_t)*c_sizeof(self%C(1,1)), cudaMemcpyHostToDevice),&
                  flag)

               if ( flag == 1 ) then
                  print *, "Failed to run CUBLAS DGEMM"
                  avg_gflops = -1
               else
                  avg_gflops = self%time_benchmark(100)
               end if

               call cuda_err_check(cudaFree(self%A_d), flag)
               call cuda_err_check(cudaFree(self%B_d), flag)
               call cuda_err_check(cudaFree(self%C_d), flag)

               call write_result(iunit, "CUDA", &
                  avg_gflops, m=self%m, n=self%n, k=self%k)

               deallocate(self%A)
               deallocate(self%B)
               deallocate(self%C)

               if (flag == 1) then
                  return
               end if
            end do
         end do
      end do

      close(iunit)
   end subroutine run_cublas_dgemm

   subroutine call_cublas_dgemm(self)
      class(CUBLASDGEMMBenchmark), intent(inout) :: self
      integer :: flag

      real(c_double), target :: alpha_t, beta_t

      alpha_t = self%alpha
      beta_t = self%beta

      call cuda_err_check(cublasDgemm(&
         self%handle,&
         CUBLAS_OP_N,&
         CUBLAS_OP_N,&
         self%m,&
         self%n,&
         self%k,&
         c_loc(alpha_t),&
         self%A_d,&
         self%m,&
         self%B_d,&
         self%k,&
         c_loc(beta_t),&
         self%C_d,&
         self%m&
         ), flag)

      if ( flag == 1 ) then
         print *, "Failed to run CUBLAS DGEMM"
      end if

      call cuda_err_check(cudaDeviceSynchronize(), flag)
   end subroutine call_cublas_dgemm
end module cublas_benchmarks
