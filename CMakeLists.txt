cmake_minimum_required(VERSION 3.26)
project(simple_ast_parser)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_CXX_STANDARD 20)

add_executable(simple_ast_parser
        main.cpp
        KaleidoscopeJIT.h
        ast/BaseNode.h
        ast/NumberNode.h
        ast/NumberNode.cpp
        ast/VariableAccessNode.h
        ast/VariableAccessNode.cpp
        ast/BinOpNode.h
        ast/BinOpNode.cpp
        ir/IRCodegen.cpp
        ir/IRCodegen.h
        ast/FunctionNode.h
        ast/FunctionNode.cpp
        ast/ProtoFunctionStatement.h
        ast/ProtoFunctionStatement.cpp
        ast/VariableDefinitionStatement.h
        ast/VariableDefinitionStatement.cpp
        ast/CallFunctionNode.h
        ast/CallFunctionNode.cpp
        ast/IfStatement.h
        ast/IfStatement.cpp
        ast/ForLoopNode.h
        ast/ForLoopNode.cpp
        ast/UnaryOpNode.h
        ast/UnaryOpNode.cpp
        Lexer.cpp
        Lexer.h
        Parser.h
        Parser.cpp
        NodePrinter.h
        NodePrinter.cpp)

target_include_directories(simple_ast_parser PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_options(simple_ast_parser PRIVATE
        -Wall
        -Wpedantic
#        -Wextra
        -Werror=unused-result
        -Werror=return-type
)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs
        #        Analysis
        Core
        #        ExecutionEngine
        #        InstCombine
        #        Object
        OrcJIT
        #        Passes
        #        RuntimeDyld
        #        ScalarOpts
        #        Support
        native
)

# Link against LLVM libraries
target_link_libraries(simple_ast_parser ${llvm_libs})