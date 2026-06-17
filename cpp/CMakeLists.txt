# SPDX-License-Identifier: LGPL-3.0-only
cmake_minimum_required(VERSION 3.10)
project(dcf_wire_cpp CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Header-only wire codec.
add_library(dcf_wire INTERFACE)
target_include_directories(dcf_wire INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

enable_testing()
add_executable(certify tests/certify.cpp)
target_link_libraries(certify PRIVATE dcf_wire)
add_test(NAME certify
         COMMAND certify ${CMAKE_CURRENT_SOURCE_DIR}/../Documentation/golden_vectors.json)
