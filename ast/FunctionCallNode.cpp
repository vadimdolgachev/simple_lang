#include "FunctionCallNode.h"
#include "BinOpNode.h"

#include <sstream>

FunctionCallNode::FunctionCallNode(std::unique_ptr<IdentNode> ident,
                                   std::vector<std::unique_ptr<ExpressionNode>> args):
    ident(std::move(ident)),
    args(std::move(args)) {}

std::string FunctionCallNode::toString() const {
    std::stringstream ss;
    ss << "call func: " << ident->name << "(";
    for (const auto &arg: args) {
        const bool isBinOp = dynamic_cast<BinOpNode *>(arg.get()) != nullptr;
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

void FunctionCallNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
