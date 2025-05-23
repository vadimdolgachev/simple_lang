cmake_minimum_required(VERSION 3.26)
project(simple_lang)

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

set(CMAKE_CXX_STANDARD 23)

add_executable(simple_lang
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
        type/Type.cpp
        type/Type.h
        SymbolTable.cpp
        SymbolTable.h
        ast/ReturnNode.cpp
        ast/ReturnNode.h
        ast/TernaryOperatorNode.cpp
        ast/TernaryOperatorNode.h
        ast/CondBranch.h
        ir/IRType.cpp
        ir/IRType.h
        ir/ArrayIRType.h
        ir/ArrayIRType.cpp
        ir/IntIRType.cpp
        ir/IntIRType.h
        ir/DoubleIRType.cpp
        ir/DoubleIRType.h
        ir/IRTypeFactory.h
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
        ir/NumericIRType.cpp
        ast/MemberAccessNode.cpp
        ast/MemberAccessNode.h
        ast/CommentNode.cpp
        ast/CommentNode.h
        ast/MethodCallNode.cpp
        ast/MethodCallNode.h
        ast/FieldAccessNode.cpp
        ast/FieldAccessNode.h
        ir/IRTypeFactory.cpp
        ast/ModuleNode.cpp
        ast/ModuleNode.h
        CompilerFronted.cpp
        CompilerFronted.h
        SemanticAnalyzer.cpp
        SemanticAnalyzer.h
        ast/TypeCastNode.cpp
        ast/TypeCastNode.h
        type/NumericType.cpp
        type/NumericType.h
        type/VoidType.cpp
        type/VoidType.h
        type/FunctionType.cpp
        type/FunctionType.h
        type/BoolType.cpp
        type/BoolType.h
        type/PointerType.cpp
        type/PointerType.h
        type/StrType.cpp
        type/StrType.h
        type/TypeFactory.cpp
        type/TypeFactory.h
        type/CharType.cpp
        type/CharType.h
        type/ByteType.cpp
        type/ByteType.h
        BuiltinSymbols.cpp
        BuiltinSymbols.h
        ast/ArrayNode.cpp
        ast/ArrayNode.h
        type/ArrayType.cpp
        type/ArrayType.h
        ast/IndexAccessNode.cpp
        ast/IndexAccessNode.h
)


target_compile_options(simple_lang PRIVATE
    $<$<CONFIG:Debug>:
        -fsanitize=address
        -fno-omit-frame-pointer
    >
)

target_link_options(simple_lang PRIVATE
    $<$<CONFIG:Debug>:
        -fsanitize=address
    >
)

target_include_directories(simple_lang PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_options(simple_lang PRIVATE
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
target_link_libraries(simple_lang ${llvm_libs})