//===- KaleidoscopeJIT.h - A simple JIT for Kaleidoscope --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Contains a simple JIT definition for use in the kaleidoscope tutorials.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#define LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

namespace llvm::orc {

    class KaleidoscopeJIT {
    private:
        std::unique_ptr<ExecutionSession> executionSession;
        DataLayout dataLayout;
        MangleAndInterner mangleAndInterpret;
        RTDyldObjectLinkingLayer objectLinkingLayer;
        IRCompileLayer compileLayer;
        JITDylib &jitLib;

    public:
        KaleidoscopeJIT(std::unique_ptr<ExecutionSession> executionSession,
                        JITTargetMachineBuilder targetMachineBuilder,
                        const DataLayout &dataLayout)
                : executionSession(std::move(executionSession)),
                  dataLayout(dataLayout),
                  mangleAndInterpret(*this->executionSession, this->dataLayout),
                  objectLinkingLayer(*this->executionSession,
                                     []() { return std::make_unique<SectionMemoryManager>(); }),
                  compileLayer(*this->executionSession, objectLinkingLayer,
                               std::make_unique<ConcurrentIRCompiler>(std::move(targetMachineBuilder))),
                  jitLib(this->executionSession->createBareJITDylib("<main>")) {
            jitLib.addGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
                    dataLayout.getGlobalPrefix())));
            if (targetMachineBuilder.getTargetTriple().isOSBinFormatCOFF()) {
                objectLinkingLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
                objectLinkingLayer.setAutoClaimResponsibilityForObjectSymbols(true);
            }
        }

        ~KaleidoscopeJIT() {
            if (auto Err = executionSession->endSession()) {
                executionSession->reportError(std::move(Err));
            }
        }

        static Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
            auto EPC = SelfExecutorProcessControl::Create();
            if (!EPC) {
                return EPC.takeError();
            }
            auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));
            JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());

            auto DL = JTMB.getDefaultDataLayoutForTarget();
            if (!DL) {
                return DL.takeError();
            }
            return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB), std::move(*DL));
        }

        const DataLayout &getDataLayout() const { return dataLayout; }

        JITDylib &getMainJITDylib() { return jitLib; }

        Error addModule(ThreadSafeModule threadSafeModule,
                        ResourceTrackerSP resTracker) {
            if (resTracker == nullptr) {
                resTracker = jitLib.getDefaultResourceTracker();
            }
            return compileLayer.add(resTracker, std::move(threadSafeModule));
        }

        Expected<ExecutorSymbolDef> lookup(const StringRef name) {
            return executionSession->lookup({&jitLib}, mangleAndInterpret(name.str()));
        }
    };

}  // namespace llvm::orc

#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
