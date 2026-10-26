
module cuda_interfaces
   use, intrinsic :: iso_c_binding
   implicit none (external)

   ! cuBLAS/CUDA constants
   integer(c_int), parameter :: CUBLAS_OP_N = 0
   integer(c_int), parameter :: CUBLAS_OP_T = 1
   integer(c_int), parameter :: CUBLAS_OP_C = 2

   integer(c_int), parameter :: cudaMemcpyHostToDevice = 1
   integer(c_int), parameter :: cudaMemcpyDeviceToHost = 2

   interface
      function cublasCreate(handle) bind(C, name="cublasCreate_v2")
         import :: c_ptr, c_int
         type(c_ptr) :: handle
         integer(c_int) :: cublasCreate
      end function

      function cublasDestroy(handle) bind(C, name="cublasDestroy_v2")
         import :: c_ptr, c_int
         type(c_ptr), value :: handle
         integer(c_int) :: cublasDestroy
      end function

      function cublasDgemm(handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind(C, name="cublasDgemm_v2")
         import :: c_ptr, c_int
         type(c_ptr), value :: handle
         integer(c_int), value :: transa, transb
         integer(c_int), value :: m, n, k
         type(c_ptr), value :: alpha
         type(c_ptr), value :: A
         integer(c_int), value :: lda
         type(c_ptr), value :: B
         integer(c_int), value :: ldb
         type(c_ptr), value :: beta
         type(c_ptr), value :: C
         integer(c_int), value :: ldc
         integer(c_int) :: cublasDgemm
      end function


      function cudaMemcpy(dst, src, count, kind) bind(C, name="cudaMemcpy")
         import :: c_ptr, c_int, c_size_t
         type(c_ptr), value :: dst
         type(c_ptr), value :: src
         integer(c_size_t), value :: count
         integer(c_int),     value :: kind
         integer(c_int) :: cudaMemcpy
      end function cudaMemcpy

      function cudaFree(devPtr) bind(C, name="cudaFree")
         import :: c_ptr, c_int
         type(c_ptr), value :: devPtr
         integer(c_int) :: cudaFree
      end function cudaFree

      function cudaMalloc(devPtr, size) bind(C, name="cudaMalloc")
         import :: c_ptr, c_size_t, c_int
         type(c_ptr) :: devPtr
         integer(c_size_t), value :: size
         integer(c_int) :: cudaMalloc
      end function cudaMalloc

      function cudaDeviceSynchronize() bind(C, name="cudaDeviceSynchronize")
         import :: c_int
         integer(c_int) :: cudaDeviceSynchronize
      end function
   end interface
end module cuda_interfaces
