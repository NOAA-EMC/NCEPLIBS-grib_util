# This is the CMake file for building the docs directory of grib_util
# utility cnvgrib.
#
# Ed Hartnett 12/13/21

# Create doxyfile.
set(abs_top_srcdir "${CMAKE_SOURCE_DIR}")
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

# Build documentation with target all.
message(STATUS, "DOXYGEN_EXECUTABLE = ${DOXYGEN_EXECUTABLE}")
add_custom_target(cnvgrib_doc ALL
  ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating API Documentation with Doxygen" VERBATIM)

# Ensure top-level docs have been generated.
add_dependencies(cnvgrib_doc doc)


