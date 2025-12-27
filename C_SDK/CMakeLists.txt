cmake_minimum_required(VERSION 3.14)

project(DCF 
    VERSION 5.2.0
    DESCRIPTION "Distributed Communication Framework - Battle Tested"
    LANGUAGES C
)

# ============================================================================
# Options
# ============================================================================

option(DCF_BUILD_SHARED "Build shared library" ON)
option(DCF_BUILD_STATIC "Build static library" ON)
option(DCF_BUILD_TESTS "Build unit tests" ON)
option(DCF_BUILD_BENCHMARKS "Build benchmarks" OFF)
option(DCF_BUILD_EXAMPLES "Build examples" ON)
option(DCF_ENABLE_SANITIZERS "Enable address/thread sanitizers" OFF)
option(DCF_ENABLE_COVERAGE "Enable code coverage" OFF)
option(DCF_ENABLE_LTO "Enable link-time optimization" OFF)

# ============================================================================
# Compiler Configuration
# ============================================================================

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type" FORCE)
endif()

# Compiler warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    set(DCF_WARNINGS
        -Wall
        -Wextra
        -Wpedantic
        -Werror=return-type
        -Werror=implicit-function-declaration
        -Wformat=2
        -Wformat-security
        -Wnull-dereference
        -Wstack-protector
        -Wstrict-overflow=2
        -Warray-bounds=2
        -Wshift-overflow=2
        -Wcast-qual
        -Wconversion
        -Wsign-conversion
        -Wdouble-promotion
        -Wfloat-equal
        -Wundef
        -Wshadow
        -Wpointer-arith
        -Wunused-macros
        -Wmissing-declarations
        -Wmissing-prototypes
        -Wstrict-prototypes
        -Wredundant-decls
        -Wswitch-default
        -Wswitch-enum
    )
    
    if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
        list(APPEND DCF_WARNINGS
            -Wlogical-op
            -Wduplicated-cond
            -Wduplicated-branches
            -Wrestrict
            -Wnull-dereference
        )
    endif()
    
    if(CMAKE_C_COMPILER_ID STREQUAL "Clang")
        list(APPEND DCF_WARNINGS
            -Weverything
            -Wno-padded
            -Wno-covered-switch-default
            -Wno-declaration-after-statement
            -Wno-disabled-macro-expansion
            -Wno-used-but-marked-unused
        )
    endif()
elseif(MSVC)
    set(DCF_WARNINGS /W4 /WX /permissive-)
endif()

# Hardening flags
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    set(DCF_HARDENING
        -fstack-protector-strong
        -D_FORTIFY_SOURCE=2
        -fPIE
    )
    if(NOT CMAKE_SYSTEM_NAME STREQUAL "Darwin")
        list(APPEND DCF_HARDENING -fstack-clash-protection)
    endif()
endif()

# Sanitizers
if(DCF_ENABLE_SANITIZERS)
    set(DCF_SANITIZER_FLAGS -fsanitize=address,undefined -fno-omit-frame-pointer)
endif()

# Coverage
if(DCF_ENABLE_COVERAGE)
    set(DCF_COVERAGE_FLAGS --coverage -fprofile-arcs -ftest-coverage)
endif()

# LTO
if(DCF_ENABLE_LTO)
    include(CheckIPOSupported)
    check_ipo_supported(RESULT ipo_supported)
    if(ipo_supported)
        set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
    endif()
endif()

# ============================================================================
# Dependencies
# ============================================================================

find_package(Threads REQUIRED)

# Check for optional dependencies
include(CheckSymbolExists)
include(CheckIncludeFile)

check_include_file("execinfo.h" HAVE_EXECINFO_H)
check_include_file("sys/eventfd.h" HAVE_EVENTFD_H)
check_symbol_exists(getrandom "sys/random.h" HAVE_GETRANDOM)
check_symbol_exists(arc4random "stdlib.h" HAVE_ARC4RANDOM)

# ============================================================================
# Library Sources
# ============================================================================

set(DCF_HEADERS
    include/dcf_platform.h
    include/dcf_types.h
    include/dcf_error.h
    include/dcf_ringbuf.h
    include/dcf_connpool.h
)

set(DCF_SOURCES
    src/dcf_platform.c
    src/dcf_error.c
    src/dcf_ringbuf.c
    src/dcf_connpool.c
)

# ============================================================================
# Library Targets
# ============================================================================

# Common compile definitions
set(DCF_COMPILE_DEFS
    $<$<BOOL:${HAVE_EXECINFO_H}>:HAVE_EXECINFO_H>
    $<$<BOOL:${HAVE_EVENTFD_H}>:HAVE_EVENTFD_H>
    $<$<BOOL:${HAVE_GETRANDOM}>:HAVE_GETRANDOM>
    $<$<BOOL:${HAVE_ARC4RANDOM}>:HAVE_ARC4RANDOM>
)

# Static library
if(DCF_BUILD_STATIC)
    add_library(dcf_static STATIC ${DCF_SOURCES})
    target_include_directories(dcf_static 
        PUBLIC 
            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
            $<INSTALL_INTERFACE:include>
    )
    target_compile_options(dcf_static PRIVATE 
        ${DCF_WARNINGS} 
        ${DCF_HARDENING}
        ${DCF_SANITIZER_FLAGS}
        ${DCF_COVERAGE_FLAGS}
    )
    target_compile_definitions(dcf_static PRIVATE ${DCF_COMPILE_DEFS})
    target_link_libraries(dcf_static PUBLIC Threads::Threads)
    
    if(UNIX AND NOT APPLE)
        target_link_libraries(dcf_static PUBLIC dl)
    endif()
    
    set_target_properties(dcf_static PROPERTIES
        OUTPUT_NAME dcf
        POSITION_INDEPENDENT_CODE ON
        C_VISIBILITY_PRESET hidden
    )
    
    if(DCF_ENABLE_SANITIZERS)
        target_link_options(dcf_static PUBLIC ${DCF_SANITIZER_FLAGS})
    endif()
    if(DCF_ENABLE_COVERAGE)
        target_link_options(dcf_static PUBLIC ${DCF_COVERAGE_FLAGS})
    endif()
endif()

# Shared library
if(DCF_BUILD_SHARED)
    add_library(dcf_shared SHARED ${DCF_SOURCES})
    target_include_directories(dcf_shared 
        PUBLIC 
            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
            $<INSTALL_INTERFACE:include>
    )
    target_compile_options(dcf_shared PRIVATE 
        ${DCF_WARNINGS} 
        ${DCF_HARDENING}
        ${DCF_SANITIZER_FLAGS}
        ${DCF_COVERAGE_FLAGS}
    )
    target_compile_definitions(dcf_shared 
        PRIVATE DCF_BUILD_SHARED ${DCF_COMPILE_DEFS}
    )
    target_link_libraries(dcf_shared PUBLIC Threads::Threads)
    
    if(UNIX AND NOT APPLE)
        target_link_libraries(dcf_shared PUBLIC dl)
    endif()
    
    set_target_properties(dcf_shared PROPERTIES
        OUTPUT_NAME dcf
        VERSION ${PROJECT_VERSION}
        SOVERSION ${PROJECT_VERSION_MAJOR}
        POSITION_INDEPENDENT_CODE ON
        C_VISIBILITY_PRESET hidden
        VISIBILITY_INLINES_HIDDEN ON
    )
    
    if(DCF_ENABLE_SANITIZERS)
        target_link_options(dcf_shared PUBLIC ${DCF_SANITIZER_FLAGS})
    endif()
    if(DCF_ENABLE_COVERAGE)
        target_link_options(dcf_shared PUBLIC ${DCF_COVERAGE_FLAGS})
    endif()
endif()

# Alias for convenience
if(DCF_BUILD_STATIC)
    add_library(dcf::static ALIAS dcf_static)
endif()
if(DCF_BUILD_SHARED)
    add_library(dcf::shared ALIAS dcf_shared)
endif()

# Default library target
if(DCF_BUILD_STATIC)
    add_library(dcf::dcf ALIAS dcf_static)
elseif(DCF_BUILD_SHARED)
    add_library(dcf::dcf ALIAS dcf_shared)
endif()

# ============================================================================
# Tests
# ============================================================================

if(DCF_BUILD_TESTS)
    enable_testing()
    
    add_executable(dcf_tests tests/unit/test_suite.c)
    target_link_libraries(dcf_tests PRIVATE dcf::dcf)
    target_compile_options(dcf_tests PRIVATE ${DCF_WARNINGS})
    
    if(DCF_ENABLE_SANITIZERS)
        target_compile_options(dcf_tests PRIVATE ${DCF_SANITIZER_FLAGS})
        target_link_options(dcf_tests PRIVATE ${DCF_SANITIZER_FLAGS})
    endif()
    
    if(DCF_ENABLE_COVERAGE)
        target_compile_options(dcf_tests PRIVATE ${DCF_COVERAGE_FLAGS})
        target_link_options(dcf_tests PRIVATE ${DCF_COVERAGE_FLAGS})
    endif()
    
    add_test(NAME unit_tests COMMAND dcf_tests)
    set_tests_properties(unit_tests PROPERTIES
        TIMEOUT 60
        ENVIRONMENT "ASAN_OPTIONS=detect_leaks=1"
    )
endif()

# ============================================================================
# Examples
# ============================================================================

if(DCF_BUILD_EXAMPLES)
    add_executable(example_basic examples/basic_example.c)
    target_link_libraries(example_basic PRIVATE dcf::dcf)
endif()

# ============================================================================
# Installation
# ============================================================================

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Install headers
install(FILES ${DCF_HEADERS}
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/dcf
)

# Install libraries
if(DCF_BUILD_STATIC)
    install(TARGETS dcf_static
        EXPORT DCFTargets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
endif()

if(DCF_BUILD_SHARED)
    install(TARGETS dcf_shared
        EXPORT DCFTargets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
endif()

# Install CMake config
install(EXPORT DCFTargets
    FILE DCFTargets.cmake
    NAMESPACE dcf::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/DCF
)

configure_package_config_file(
    cmake/DCFConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/DCFConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/DCF
)

write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/DCFConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/DCFConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/DCFConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/DCF
)

# Install pkg-config file
configure_file(cmake/dcf.pc.in ${CMAKE_CURRENT_BINARY_DIR}/dcf.pc @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/dcf.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)

# ============================================================================
# Package
# ============================================================================

set(CPACK_PACKAGE_NAME "dcf")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${PROJECT_DESCRIPTION})
set(CPACK_PACKAGE_VENDOR "DCF Project")
set(CPACK_PACKAGE_CONTACT "dcf@example.com")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")

if(UNIX AND NOT APPLE)
    set(CPACK_GENERATOR "TGZ;DEB;RPM")
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6, libpthread-stubs0-dev")
    set(CPACK_RPM_PACKAGE_REQUIRES "glibc")
elseif(APPLE)
    set(CPACK_GENERATOR "TGZ")
elseif(WIN32)
    set(CPACK_GENERATOR "ZIP;NSIS")
endif()

include(CPack)

# ============================================================================
# Summary
# ============================================================================

message(STATUS "")
message(STATUS "DCF Configuration Summary")
message(STATUS "-------------------------")
message(STATUS "  Version:          ${PROJECT_VERSION}")
message(STATUS "  Build type:       ${CMAKE_BUILD_TYPE}")
message(STATUS "  C Compiler:       ${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION}")
message(STATUS "  Install prefix:   ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
message(STATUS "  Build shared:     ${DCF_BUILD_SHARED}")
message(STATUS "  Build static:     ${DCF_BUILD_STATIC}")
message(STATUS "  Build tests:      ${DCF_BUILD_TESTS}")
message(STATUS "  Build examples:   ${DCF_BUILD_EXAMPLES}")
message(STATUS "  Sanitizers:       ${DCF_ENABLE_SANITIZERS}")
message(STATUS "  Coverage:         ${DCF_ENABLE_COVERAGE}")
message(STATUS "  LTO:              ${DCF_ENABLE_LTO}")
message(STATUS "")
