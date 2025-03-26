cmake_minimum_required(VERSION 3.26)
project(simple_ast_parser)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_subdirectory(tests)

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
        ast/StringNode.h
        ast/StringNode.cpp
        ast/IdentNode.h
        ast/IdentNode.cpp
        ast/BinOpNode.h
        ast/BinOpNode.cpp
        ir/LLVMCodegen.cpp
        ir/LLVMCodegen.h
        ast/FunctionNode.h
        ast/FunctionNode.cpp
        ast/ProtoFunctionStatement.h
        ast/ProtoFunctionStatement.cpp
        ast/AssignmentNode.h
        ast/AssignmentNode.cpp
        ast/FunctionCallNode.h
        ast/FunctionCallNode.cpp
        ast/IfStatement.h
        ast/IfStatement.cpp
        ast/UnaryOpNode.h
        ast/UnaryOpNode.cpp
        Lexer.cpp
        Lexer.h
        Parser.h
        Parser.cpp
        NodePrinter.h
        NodePrinter.cpp
        Util.h
        ast/BooleanNode.cpp
        ast/BooleanNode.h
        ast/LoopCondNode.cpp
        ast/LoopCondNode.h
        ast/BlockNode.cpp
        ast/BlockNode.h
        ast/DeclarationNode.cpp
        ast/DeclarationNode.h
        ast/TypeNode.cpp
        ast/TypeNode.h
        ir/SymbolTable.cpp
        ir/SymbolTable.h
        ast/ReturnNode.cpp
        ast/ReturnNode.h
        ast/TernaryOperatorNode.cpp
        ast/TernaryOperatorNode.h
        ast/CondBranch.h
        ir/IRType.cpp
        ir/IRType.h
        ir/IntIRType.cpp
        ir/IntIRType.h
        ir/DoubleIRType.cpp
        ir/DoubleIRType.h
        ir/TypeManager.cpp
        ir/TypeManager.h
        ir/TypeFactory.h
        ir/ModuleContext.h
        ir/NumericIRType.h
        ir/VoidIRType.h
        ir/BooleanIRType.cpp
        ir/BooleanIRType.h
        ir/StrIRType.cpp
        ir/StrIRType.h
        ir/ByteIRType.cpp
        ir/ByteIRType.h
        ir/CharIRType.cpp
        ir/CharIRType.h
        ir/VoidIRType.cpp
        ir/NumericIRType.cpp)

target_include_directories(simple_ast_parser PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_options(simple_ast_parser PRIVATE
        -Wall
        -Wpedantic
#        -Wextra
        -Werror=unused-result
        -Werror=return-type
        -Wstrict-aliasing
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