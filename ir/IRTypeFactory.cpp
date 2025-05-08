//
// Created by vadim on 09.04.25.
//

#include "IRTypeFactory.h"
#include "BooleanIRType.h"
#include "ByteIRType.h"
#include "CharIRType.h"
#include "DoubleIRType.h"
#include "IntIRType.h"
#include "ModuleContext.h"
#include "StrIRType.h"
#include "VoidIRType.h"

#include "ast/BaseNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/NumberNode.h"
#include "ast/BinOpNode.h"
#include "ast/FieldAccessNode.h"
#include "ast/MemberAccessNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "../type/Type.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/MethodCallNode.h"
#include "type/FunctionType.h"
#include "type/StrType.h"
#include "type/TypeFactory.h"
#include "type/VoidType.h"

std::shared_ptr<IRType> IRTypeFactory::from(const TypePtr &type) {
    if (type->isStr()) {
        return std::make_shared<StrIRType>();
    }
    if (type->isVoid()) {
        return std::make_shared<VoidIRType>();
    }
    if (type->isDouble()) {
        return std::make_shared<DoubleIRType>();
    }
    if (type->isInteger()) {
        return std::make_shared<IntIRType>();
    }

    throw std::logic_error("Unknown type: " + type->getName());
}
