# SPDX-License-Identifier: LGPL-3.0-only
cmake_minimum_required(VERSION 3.13)
project(dcf_wire_cpp CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(DCF_CPP_GRPC "Build the dcfcpp gRPC node (needs gRPC + protobuf)" ON)

# Header-only wire codec (frame.hpp + superpack.hpp).
add_library(dcf_wire INTERFACE)
target_include_directories(dcf_wire INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

enable_testing()
add_executable(certify tests/certify.cpp)
target_link_libraries(certify PRIVATE dcf_wire)
add_test(NAME certify
         COMMAND certify ${CMAKE_CURRENT_SOURCE_DIR}/../Documentation/golden_vectors.json)

add_executable(certify_superpack tests/certify_superpack.cpp)
target_link_libraries(certify_superpack PRIVATE dcf_wire)
add_test(NAME certify_superpack COMMAND certify_superpack
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/..)

# ── Supercharged gRPC node ───────────────────────────────────────────────────
if(DCF_CPP_GRPC)
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    find_package(Protobuf CONFIG REQUIRED)
    find_package(gRPC CONFIG REQUIRED)

    add_library(dcf_proto STATIC proto/dcf.proto)
    target_link_libraries(dcf_proto PUBLIC protobuf::libprotobuf gRPC::grpc++)
    target_include_directories(dcf_proto PUBLIC "${CMAKE_CURRENT_BINARY_DIR}")

    protobuf_generate(TARGET dcf_proto LANGUAGE cpp
        IMPORT_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/proto
        PROTOC_OUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
    protobuf_generate(TARGET dcf_proto LANGUAGE grpc
        GENERATE_EXTENSIONS .grpc.pb.h .grpc.pb.cc
        PLUGIN "protoc-gen-grpc=$<TARGET_FILE:gRPC::grpc_cpp_plugin>"
        IMPORT_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/proto
        PROTOC_OUT_DIR ${CMAKE_CURRENT_BINARY_DIR})

    add_executable(dcfcpp src/dcf_node.cpp)
    target_include_directories(dcfcpp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
    target_link_libraries(dcfcpp PRIVATE
        dcf_proto dcf_wire protobuf::libprotobuf
        gRPC::grpc++ gRPC::grpc++_reflection)

    include(GNUInstallDirs)
    install(TARGETS dcfcpp RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
