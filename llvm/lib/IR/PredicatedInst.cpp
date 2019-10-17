#include <llvm/IR/PredicatedInst.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/IntrinsicInst.h>

namespace {
  using namespace llvm;
  using ShortValueVec = SmallVector<Value*, 4>;
}

namespace llvm {

void
PredicatedOperator::copyIRFlags(const Value * V, bool IncludeWrapFlags) {
  auto * I = dyn_cast<Instruction>(this);
  if (I) I->copyIRFlags(V, IncludeWrapFlags);
}

Instruction*
PredicatedBinaryOperator::Create(Module * Mod,
                                 Value *Mask, Value *VectorLen,
                                 Instruction::BinaryOps Opc,
                                 Value *V1, Value *V2,
                                 const Twine &Name,
                                 BasicBlock * InsertAtEnd,
                                 Instruction * InsertBefore) {
  assert(!(InsertAtEnd && InsertBefore));
  auto VPID = VPIntrinsic::getForOpcode(Opc);

  // Default Code Path
  if ((!Mod ||
      (!Mask && !VectorLen)) ||
      VPID == Intrinsic::not_intrinsic) {
    if (InsertAtEnd) {
      return BinaryOperator::Create(Opc, V1, V2, Name, InsertAtEnd);
    } else {
      return BinaryOperator::Create(Opc, V1, V2, Name, InsertBefore);
    }
  }

  assert(Mod && "Need a module to emit VP Intrinsics");

  // Fetch the VP intrinsic
  auto & VecTy = cast<VectorType>(*V1->getType());
  auto & ScalarTy = *VecTy.getVectorElementType();
  auto TypeTokens = VPIntrinsic::GetTypeTokens(VPID);
  auto * VPFunc = Intrinsic::getDeclaration(Mod, VPID, VPIntrinsic::EncodeTypeTokens(TypeTokens, &VecTy, VecTy, ScalarTy));

  // Encode default environment fp behavior
  LLVMContext & Ctx = V1->getContext();
  SmallVector<Value*, 6> BinOpArgs({V1, V2});
  if (VPIntrinsic::hasRoundingModeParam(VPID)) {
    BinOpArgs.push_back(GetConstrainedFPRounding(Ctx, RoundingMode::rmToNearest));
  }
  if (VPIntrinsic::hasExceptionBehaviorParam(VPID)) {
    BinOpArgs.push_back(GetConstrainedFPExcept(Ctx, ExceptionBehavior::ebIgnore));
  }

  BinOpArgs.push_back(Mask);
  BinOpArgs.push_back(VectorLen);

  if (InsertAtEnd) {
    return CallInst::Create(VPFunc, BinOpArgs, Name, InsertAtEnd);
  } else {
    return CallInst::Create(VPFunc, BinOpArgs, Name, InsertBefore);
  }
}

}
