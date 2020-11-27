# import numpy as np
# cimport numpy as np

# DTYPE = np.int8
# ctypedef np.int8_t DTYPE_t

# def fft2_phase(np.ndarray[DTYPE_t, ndim=1] vec):
#     cdef int N = len(vec)
#     cdef int idx
#     cdef DTYPE_t v

#     for idx in range(1, N):
#         v = vec[idx-1] + vec[idx]
#         vec[idx] = v % 10

from cpython cimport array
import array

def fft2_phase(array.array vec):
    cdef int N = len(vec)
    cdef int idx
    cdef int v

    for idx in range(1, N):
        v = vec[idx-1] + vec[idx]
        vec[idx] = v % 10
