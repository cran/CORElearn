#
# support for OpenMP multithreading on different platforms
# 
ifeq ($(CXX),g++)
  # GNU compilers, also MinGW
  OMP_FLAG = -fopenmp
else
  ifeq ($(CXX),CC)
    # Oracle's Sun Studio
    OMP_FLAG = -xopenmp
  else
     ifeq ($(CXX),cl)
       #Microsoft MSVC compiler
       OMP_FLAG = /openmp
     else
        # provide option for your compiler here, or leave empty for no OpenMP support
        OMP_FLAG = 
     endif
  endif
endif
all:
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) $(OMP_FLAG) -c *.cpp
	$(SHLIB_CXXLD) $(SHLIB_CXXLDFLAGS) $(OMP_FLAG) -o CORElearn$(SHLIB_EXT) *.o
clean:
	rm -f *.o

