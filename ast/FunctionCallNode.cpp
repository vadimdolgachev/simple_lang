#include "FunctionCallNode.h"
#include "BinOpNode.h"

#include <sstream>

FunctionCallNode::FunctionCallNode(std::unique_ptr<IdentNode> ident,
                                   std::vector<ExprNodePtr> args):
    ident(std::move(ident)),
    args(std::move(args)) {}

std::string FunctionCallNode::toString() const {
    std::stringstream ss;
    ss << "call func: " << ident->name << "(";
    for (const auto &arg: args) {
        const bool isBinOp = isNode<BinOpNode>(arg.get());
        if (isBinOp) {
            ss << "(";
        }
        ss << arg->toString() << ",";
        if (isBinOp) {
            ss << ")";
        }
    }
    ss << ")";
    return ss.str();
}

void FunctionCallNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

TypePtr FunctionCallNode::getType() const {
    return this->type;
}

void FunctionCallNode::setType(TypePtr type) {
    this->type = std::move(type);
}
