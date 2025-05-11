//
// Created by vadim on 27.04.25.
//

#ifndef STRTYPE_H
#define STRTYPE_H

#include "Type.h"

class StrType final : public PrimitiveType {
public:
    StrType();

    ResultType getResultTypeUnary(TokenType op) const override;

    bool operator==(const Type &other) const override;

    std::string getName() const override;

    const std::vector<CallableInfoPtr> &getMethodTypes() const override;

private:
    std::vector<std::shared_ptr<CallableInfo>> methods;
};

#endif //STRTYPE_H
