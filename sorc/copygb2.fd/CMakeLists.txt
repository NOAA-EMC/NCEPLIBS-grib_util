cmake_minimum_required(VERSION 2.6)
set(EXENAME copygb2 )
file(GLOB EXE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f ${CMAKE_CURRENT_SOURCE_DIR}/*.f90 ${CMAKE_CURRENT_SOURCE_DIR}/*.F)
#file(GLOB EXE_C_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
if(IntelComp)
    message("setting intel flags")
    set(CMAKE_Fortran_8_FLAGS "-g -O3 -r8 -convert big_endian -auto -fpp ${OpenMP_Fortran_FLAGS} " CACHE INTERNAL "" )
elseif(GNUComp)
    message("setting gnu flags")
    set(CMAKE_Fortran_8_FLAGS "-g -O3 -fdefault-real-8 -fconvert=big-endian -cpp ${OpenMP_Fortran_FLAGS} " CACHE INTERNAL "" )
else()
    message("unknown compiler!")
    exit()
endif()


#set_source_files_properties( ${EXE_C_SRC} PROPERTIES COMPILE_FLAGS ${CMAKE_C_FLAGS} )
#add_definitions( ${CMAKE_Fortran_8_FLAGS} )
add_executable(${EXENAME} ${EXE_SRC} ${EXE_C_SRC} )
set_target_properties( ${EXENAME} PROPERTIES COMPILE_FLAGS ${CMAKE_Fortran_8_FLAGS} )
set_target_properties( ${EXENAME} PROPERTIES LINK_FLAGS ${CMAKE_Fortran_8_FLAGS} )
include_directories( ${G2_INCd} ${SP_INCd} ${IP_INCd} )
set(CMAKE_INCLUDE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_8")
set_target_properties(${EXENAME} PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_8")
target_include_directories(${EXENAME} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/include_8")
if(IntelComp)
    target_link_libraries(${EXENAME} ${G2_LIBd} ${PNG_LIBRARIES} ${JASPER_LIBRARIES} ${BACIO_LIB4} ${W3NCO_LIBd} ${IP_LIBd} ${SP_LIBd} ${OpenMP_Fortran_FLAGS} ${LAPACK_LINKER_FLAGS} -static-intel)
elseif(GNUComp)
    target_link_libraries(${EXENAME} ${G2_LIBd} ${PNG_LIBRARIES} ${JASPER_LIBRARIES} ${BACIO_LIB4} ${W3NCO_LIBd} ${IP_LIBd} ${SP_LIBd} ${OpenMP_Fortran_FLAGS} ${LAPACK_LINKER_FLAGS} -static-libgfortran)
else()
    message("unknown compiler!")
    exit()
endif()

file(MAKE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})

install(TARGETS ${EXENAME}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib) 
install(DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/ DESTINATION include_8 )

