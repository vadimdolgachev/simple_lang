//
// Created by vadim on 25.03.25.
//

#include "StrIRType.h"

#include "ast/StringNode.h"
#include "../type/Type.h"
#include "ast/ProtoFunctionStatement.h"
#include "type/TypeFactory.h"

StrIRType::StrIRType(const bool isPointer):
    IRType(isPointer) {
    if (!isPointer) {
        throw std::invalid_argument("String type must be a pointer");
    }
    methods.insert(std::make_unique<ProtoFunctionStatement>("len",
                                                            TypeFactory::makePrimitiveType(TypeKind::Integer),
                                                            std::vector<std::unique_ptr<DeclarationNode>>{}));
}

bool StrIRType::isOperationSupported(const TokenType op, const IRType *rhs) const {
    if (!dynamic_cast<const StrIRType *>(rhs)) {
        return false;
    }

    return op == TokenType::Equal || op == TokenType::NotEqual;
}

llvm::Value *StrIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                       const TokenType op,
                                       llvm::Value *lhs,
                                       llvm::Value *rhs,
                                       const std::string &name) const {
    llvm::Module *module = builder.GetInsertBlock()->getModule();

    switch (op) {
        case TokenType::Equal:
        case TokenType::NotEqual: {
            auto *const strcmpFunc = getOrDeclareStrcmp(module);
            auto *const cmpResult = builder.CreateCall(strcmpFunc,
                                                       {lhs, rhs},
                                                       "strcmp.result");
            auto *const zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
            auto *const result = builder.CreateICmp(
                    op == TokenType::Equal ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::ICMP_NE,
                    cmpResult,
                    zero,
                    name);
            return result;
        }
        default:
            throw std::invalid_argument("Unsupported string operation");
    }
}

bool StrIRType::isUnaryOperationSupported(TokenType op) const {
    return false;
}

llvm::Value *StrIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                      TokenType op,
                                      llvm::Value *operand,
                                      llvm::Value *storage,
                                      const std::string &name) const {
    throw std::invalid_argument("Unsupported unary operation");
}

llvm::Type *StrIRType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
}

llvm::Value *StrIRType::createValue(const BaseNode *node,
                                    llvm::IRBuilder<> & /*builder*/,
                                    llvm::Module &module) {
    const auto *strNode = dynamic_cast<const StringNode *>(node);
    auto *gv = new llvm::GlobalVariable(module,
                                        getLLVMType(module.getContext()),
                                        true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        llvm::ConstantDataArray::getString(module.getContext(), strNode->str),
                                        ".str");
    return llvm::ConstantExpr::getBitCast(gv, getLLVMType(module.getContext()));
}

llvm::Value *StrIRType::createMethodCall(llvm::IRBuilder<> &builder,
                                         const std::string &method,
                                         llvm::Value *object,
                                         const std::vector<llvm::Value *> &args,
                                         const std::string &name) const {
    if (method == "len") {
        if (!args.empty()) {
            throw std::invalid_argument("len() does not take arguments");
        }
        auto *const strlenFunc = getOrDeclareStrlen(builder.GetInsertBlock()->getModule());
        return builder.CreateCall(strlenFunc, {object}, name);
    }
    return IRType::createMethodCall(builder, method, object, args, name);
}

const IRType::MethodLists &StrIRType::methodList() const {
    return methods;
}

llvm::Function *StrIRType::getOrDeclareStrcmp(llvm::Module *module) const {
    auto &context = module->getContext();
    auto *const funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context),
                                                   {getLLVMType(context), getLLVMType(context)},
                                                   false);
    return llvm::cast<llvm::Function>(
            module->getOrInsertFunction("strcmp", funcType).getCallee()
            );
}

llvm::Function *StrIRType::getOrDeclareStrlen(llvm::Module *module) const {
    auto &context = module->getContext();
    auto *const funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context),
                                                   {getLLVMType(context)},
                                                   false);
    return llvm::cast<llvm::Function>(
            module->getOrInsertFunction("strlen", funcType).getCallee()
            );
}
