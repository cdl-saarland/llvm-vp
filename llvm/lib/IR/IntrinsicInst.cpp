//===-- InstrinsicInst.cpp - Intrinsic Instruction Wrappers ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements methods that make it really easy to deal with intrinsic
// functions.
//
// All intrinsic function calls are instances of the call instruction, so these
// are all subclasses of the CallInst class.  Note that none of these classes
// has state or virtual methods, which is an important part of this gross/neat
// hack working.
//
// In some cases, arguments to intrinsics need to be generic and are defined as
// type pointer to empty struct { }*.  To access the real item of interest the
// cast instruction needs to be stripped away.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Operator.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

//===----------------------------------------------------------------------===//
/// DbgVariableIntrinsic - This is the common base class for debug info
/// intrinsics for variables.
///

Value *DbgVariableIntrinsic::getVariableLocation(bool AllowNullOp) const {
  Value *Op = getArgOperand(0);
  if (AllowNullOp && !Op)
    return nullptr;

  auto *MD = cast<MetadataAsValue>(Op)->getMetadata();
  if (auto *V = dyn_cast<ValueAsMetadata>(MD))
    return V->getValue();

  // When the value goes to null, it gets replaced by an empty MDNode.
  assert(!cast<MDNode>(MD)->getNumOperands() && "Expected an empty MDNode");
  return nullptr;
}

Optional<uint64_t> DbgVariableIntrinsic::getFragmentSizeInBits() const {
  if (auto Fragment = getExpression()->getFragmentInfo())
    return Fragment->SizeInBits;
  return getVariable()->getSizeInBits();
}

int llvm::Intrinsic::lookupLLVMIntrinsicByName(ArrayRef<const char *> NameTable,
                                               StringRef Name) {
  assert(Name.startswith("llvm."));

  // Do successive binary searches of the dotted name components. For
  // "llvm.gc.experimental.statepoint.p1i8.p1i32", we will find the range of
  // intrinsics starting with "llvm.gc", then "llvm.gc.experimental", then
  // "llvm.gc.experimental.statepoint", and then we will stop as the range is
  // size 1. During the search, we can skip the prefix that we already know is
  // identical. By using strncmp we consider names with differing suffixes to
  // be part of the equal range.
  size_t CmpEnd = 4; // Skip the "llvm" component.
  const char *const *Low = NameTable.begin();
  const char *const *High = NameTable.end();
  const char *const *LastLow = Low;
  while (CmpEnd < Name.size() && High - Low > 0) {
    size_t CmpStart = CmpEnd;
    CmpEnd = Name.find('.', CmpStart + 1);
    CmpEnd = CmpEnd == StringRef::npos ? Name.size() : CmpEnd;
    auto Cmp = [CmpStart, CmpEnd](const char *LHS, const char *RHS) {
      return strncmp(LHS + CmpStart, RHS + CmpStart, CmpEnd - CmpStart) < 0;
    };
    LastLow = Low;
    std::tie(Low, High) = std::equal_range(Low, High, Name.data(), Cmp);
  }
  if (High - Low > 0)
    LastLow = Low;

  if (LastLow == NameTable.end())
    return -1;
  StringRef NameFound = *LastLow;
  if (Name == NameFound ||
      (Name.startswith(NameFound) && Name[NameFound.size()] == '.'))
    return LastLow - NameTable.begin();
  return -1;
}

Value *InstrProfIncrementInst::getStep() const {
  if (InstrProfIncrementInstStep::classof(this)) {
    return const_cast<Value *>(getArgOperand(4));
  }
  const Module *M = getModule();
  LLVMContext &Context = M->getContext();
  return ConstantInt::get(Type::getInt64Ty(Context), 1);
}

Optional<ExceptionBehavior>
llvm::StrToExceptionBehavior(StringRef ExceptionArg) {
  return StringSwitch<Optional<ExceptionBehavior>>(ExceptionArg)
    .Case("fpexcept.ignore",  ExceptionBehavior::ebIgnore)
    .Case("fpexcept.maytrap", ExceptionBehavior::ebMayTrap)
    .Case("fpexcept.strict",  ExceptionBehavior::ebStrict)
    .Default(None);
}

Optional<StringRef>
llvm::ExceptionBehaviorToStr(ExceptionBehavior UseExcept) {
  Optional<StringRef> ExceptStr = None;
  switch (UseExcept) {
  default: break;
  case ExceptionBehavior::ebStrict:
    ExceptStr = "fpexcept.strict";
    break;
  case ExceptionBehavior::ebIgnore:
    ExceptStr = "fpexcept.ignore";
    break;
  case ExceptionBehavior::ebMayTrap:
    ExceptStr = "fpexcept.maytrap";
    break;
  }
  return ExceptStr;
}

Optional<RoundingMode>
llvm::StrToRoundingMode(StringRef RoundingArg) {
  // For dynamic rounding mode, we use round to nearest but we will set the
  // 'exact' SDNodeFlag so that the value will not be rounded.
  return StringSwitch<Optional<RoundingMode>>(RoundingArg)
    .Case("round.dynamic",    RoundingMode::rmDynamic)
    .Case("round.tonearest",  RoundingMode::rmToNearest)
    .Case("round.downward",   RoundingMode::rmDownward)
    .Case("round.upward",     RoundingMode::rmUpward)
    .Case("round.towardzero", RoundingMode::rmTowardZero)
    .Default(None);
}

Optional<StringRef>
llvm::RoundingModeToStr(RoundingMode UseRounding) {
  Optional<StringRef> RoundingStr = None;
  switch (UseRounding) {
  default: break;
  case RoundingMode::rmDynamic:
    RoundingStr = "round.dynamic";
    break;
  case RoundingMode::rmToNearest:
    RoundingStr = "round.tonearest";
    break;
  case RoundingMode::rmDownward:
    RoundingStr = "round.downward";
    break;
  case RoundingMode::rmUpward:
    RoundingStr = "round.upward";
    break;
  case RoundingMode::rmTowardZero:
    RoundingStr = "round.towardzero";
    break;
  }
  return RoundingStr;
}

/// Return the IR Value representation of any ExceptionBehavior.
Value*
llvm::GetConstrainedFPExcept(LLVMContext& Context, ExceptionBehavior UseExcept) {
    Optional<StringRef> ExceptStr =
        ExceptionBehaviorToStr(UseExcept);
    assert(ExceptStr.hasValue() && "Garbage strict exception behavior!");
    auto *ExceptMDS = MDString::get(Context, ExceptStr.getValue());

    return MetadataAsValue::get(Context, ExceptMDS);
}

/// Return the IR Value representation of any RoundingMode.
Value*
llvm::GetConstrainedFPRounding(LLVMContext& Context, RoundingMode UseRounding) {
    Optional<StringRef> RoundingStr =
        RoundingModeToStr(UseRounding);
    assert(RoundingStr.hasValue() && "Garbage strict rounding mode!");
    auto *RoundingMDS = MDString::get(Context, RoundingStr.getValue());

    return MetadataAsValue::get(Context, RoundingMDS);
}


Optional<RoundingMode>
ConstrainedFPIntrinsic::getRoundingMode() const {
  unsigned NumOperands = getNumArgOperands();
  assert(NumOperands >= 2 && "underflow");
  Metadata *MD =
      cast<MetadataAsValue>(getArgOperand(NumOperands - 2))->getMetadata();
  if (!MD || !isa<MDString>(MD))
    return None;
  return StrToRoundingMode(cast<MDString>(MD)->getString());
}

Optional<ExceptionBehavior>
ConstrainedFPIntrinsic::getExceptionBehavior() const {
  unsigned NumOperands = getNumArgOperands();
  assert(NumOperands >= 1 && "underflow");
  Metadata *MD =
      cast<MetadataAsValue>(getArgOperand(NumOperands - 1))->getMetadata();
  if (!MD || !isa<MDString>(MD))
    return None;
  return StrToExceptionBehavior(cast<MDString>(MD)->getString());
}


CmpInst::Predicate
VPIntrinsic::getCmpPredicate() const {
  return static_cast<CmpInst::Predicate>(cast<ConstantInt>(getArgOperand(4))->getZExtValue());
}

Optional<RoundingMode>
VPIntrinsic::getRoundingMode() const {
  auto RmParamPos = getRoundingModeParamPos(getIntrinsicID());
  if (!RmParamPos) return None;

  Metadata *MD =
      dyn_cast<MetadataAsValue>(getArgOperand(RmParamPos.getValue()))->getMetadata();
  if (!MD || !isa<MDString>(MD))
    return None;
  StringRef RoundingArg = cast<MDString>(MD)->getString();
  return StrToRoundingMode(RoundingArg);
}

Optional<ExceptionBehavior>
VPIntrinsic::getExceptionBehavior() const {
  auto EbParamPos = getExceptionBehaviorParamPos(getIntrinsicID());
  if (!EbParamPos) return None;

  Metadata *MD =
      dyn_cast<MetadataAsValue>(getArgOperand(EbParamPos.getValue()))->getMetadata();
  if (!MD || !isa<MDString>(MD))
    return None;
  StringRef ExceptionArg = cast<MDString>(MD)->getString();
  return StrToExceptionBehavior(ExceptionArg);
}

bool
VPIntrinsic::hasRoundingModeParam(Intrinsic::ID VPID) {
  switch (VPID) {
    default:
      return false;

    case Intrinsic::vp_ceil:
    case Intrinsic::vp_cos:
    case Intrinsic::vp_exp2:
    case Intrinsic::vp_exp:
    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_floor:
    case Intrinsic::vp_fma:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_fneg:
    case Intrinsic::vp_frem:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_llround:
    case Intrinsic::vp_log10:
    case Intrinsic::vp_log2:
    case Intrinsic::vp_log:
    case Intrinsic::vp_lround:
    case Intrinsic::vp_maxnum:
    case Intrinsic::vp_minnum:
    case Intrinsic::vp_nearbyint:
    case Intrinsic::vp_pow:
    case Intrinsic::vp_powi:
    case Intrinsic::vp_lrint:
    case Intrinsic::vp_llrint:
    case Intrinsic::vp_rint:
    case Intrinsic::vp_round:
    case Intrinsic::vp_sin:
    case Intrinsic::vp_sqrt:
    case Intrinsic::vp_trunc:
      return true;
  }
}

bool
VPIntrinsic::hasExceptionBehaviorParam(Intrinsic::ID VPID) {
  switch (VPID) {
    default:
    case Intrinsic::vp_fpext:
    case Intrinsic::vp_fptosi:
    case Intrinsic::vp_fptoui:
    case Intrinsic::vp_fptrunc:
    case Intrinsic::vp_llround:
    case Intrinsic::vp_lround:
      return false;

    case Intrinsic::vp_ceil:
    case Intrinsic::vp_cos:
    case Intrinsic::vp_exp2:
    case Intrinsic::vp_exp:
    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_floor:
    case Intrinsic::vp_fma:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_frem:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_log10:
    case Intrinsic::vp_log2:
    case Intrinsic::vp_log:
    case Intrinsic::vp_maxnum:
    case Intrinsic::vp_minnum:
    case Intrinsic::vp_nearbyint:
    case Intrinsic::vp_pow:
    case Intrinsic::vp_powi:
    case Intrinsic::vp_lrint:
    case Intrinsic::vp_llrint:
    case Intrinsic::vp_rint:
    case Intrinsic::vp_round:
    case Intrinsic::vp_sin:
    case Intrinsic::vp_sqrt:
    case Intrinsic::vp_trunc:
      return true;
  }
}

Value*
VPIntrinsic::getMask() const {
  auto maskPos = getMaskParamPos(getIntrinsicID());
  if (maskPos) return getArgOperand(maskPos.getValue());
  return nullptr;
}

Value*
VPIntrinsic::getVectorLength() const {
  auto vlenPos = getVectorLengthParamPos(getIntrinsicID());
  if (vlenPos) return getArgOperand(vlenPos.getValue());
  return nullptr;
}

VPIntrinsic::TypeTokenVec
VPIntrinsic::GetTypeTokens(Intrinsic::ID ID) {
  switch (ID) {
    default:
     return TypeTokenVec();

    case Intrinsic::vp_cos:
    case Intrinsic::vp_sin:
    case Intrinsic::vp_exp:
    case Intrinsic::vp_exp2:

    case Intrinsic::vp_log:
    case Intrinsic::vp_log2:
    case Intrinsic::vp_log10:
    case Intrinsic::vp_sqrt:
    case Intrinsic::vp_ceil:
    case Intrinsic::vp_floor:
    case Intrinsic::vp_round:
    case Intrinsic::vp_trunc:
    case Intrinsic::vp_rint:
    case Intrinsic::vp_nearbyint:
    
    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_frem:
    case Intrinsic::vp_pow:
    case Intrinsic::vp_powi:
    case Intrinsic::vp_maxnum:
    case Intrinsic::vp_minnum:
      return TypeTokenVec{VPTypeToken::Returned};

    case Intrinsic::vp_reduce_and:
    case Intrinsic::vp_reduce_or:
    case Intrinsic::vp_reduce_xor:

    case Intrinsic::vp_reduce_add:
    case Intrinsic::vp_reduce_mul:
    case Intrinsic::vp_reduce_fadd:
    case Intrinsic::vp_reduce_fmul:

    case Intrinsic::vp_reduce_fmin:
    case Intrinsic::vp_reduce_fmax:
    case Intrinsic::vp_reduce_smin:
    case Intrinsic::vp_reduce_smax:
    case Intrinsic::vp_reduce_umin:
    case Intrinsic::vp_reduce_umax:
      return TypeTokenVec{VPTypeToken::Returned, VPTypeToken::Vector};

    case Intrinsic::vp_fpext:
    case Intrinsic::vp_fptrunc:
    case Intrinsic::vp_fptoui:
    case Intrinsic::vp_fptosi:
    case Intrinsic::vp_lround:
    case Intrinsic::vp_llround:
    case Intrinsic::vp_lrint:
    case Intrinsic::vp_llrint:
      return TypeTokenVec{VPTypeToken::Returned, VPTypeToken::Vector};

    case Intrinsic::vp_cmp:
      return TypeTokenVec{VPTypeToken::Mask, VPTypeToken::Vector};
  }
}

bool VPIntrinsic::isReductionOp() const {
  switch (getIntrinsicID()) {
    default:
      return false;

    case Intrinsic::vp_reduce_and:
    case Intrinsic::vp_reduce_or:
    case Intrinsic::vp_reduce_xor:

    case Intrinsic::vp_reduce_add:
    case Intrinsic::vp_reduce_mul:
    case Intrinsic::vp_reduce_fadd:
    case Intrinsic::vp_reduce_fmul:

    case Intrinsic::vp_reduce_fmin:
    case Intrinsic::vp_reduce_fmax:
    case Intrinsic::vp_reduce_smin:
    case Intrinsic::vp_reduce_smax:
    case Intrinsic::vp_reduce_umin:
    case Intrinsic::vp_reduce_umax:

      return true;
  }
}

bool VPIntrinsic::isConstrainedOp() const {
  return (getRoundingMode() != None && getRoundingMode() != RoundingMode::rmToNearest) ||
         (getExceptionBehavior() != None && getExceptionBehavior() != ExceptionBehavior::ebIgnore);
}

bool VPIntrinsic::isBinaryOp() const {
  switch (getIntrinsicID()) {
    default:
      return false;

    case Intrinsic::vp_and:
    case Intrinsic::vp_or:
    case Intrinsic::vp_xor:
    case Intrinsic::vp_ashr:
    case Intrinsic::vp_lshr:
    case Intrinsic::vp_shl:

    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_frem:

    case Intrinsic::vp_reduce_or:
    case Intrinsic::vp_reduce_xor:
    case Intrinsic::vp_reduce_add:
    case Intrinsic::vp_reduce_mul:
    case Intrinsic::vp_reduce_smax:
    case Intrinsic::vp_reduce_smin:
    case Intrinsic::vp_reduce_umax:
    case Intrinsic::vp_reduce_umin:

    case Intrinsic::vp_reduce_fadd:
    case Intrinsic::vp_reduce_fmul:
    case Intrinsic::vp_reduce_fmax:
    case Intrinsic::vp_reduce_fmin:

    case Intrinsic::vp_add:
    case Intrinsic::vp_sub:
    case Intrinsic::vp_mul:
    case Intrinsic::vp_udiv:
    case Intrinsic::vp_sdiv:
    case Intrinsic::vp_urem:
    case Intrinsic::vp_srem:
      return true;
  }
}

bool VPIntrinsic::isTernaryOp() const {
  switch (getIntrinsicID()) {
    default:
      return false;
    case Intrinsic::vp_compose:
    case Intrinsic::vp_select:
    case Intrinsic::vp_fma:
      return true;
  }
}

Optional<int>
VPIntrinsic::getMaskParamPos(Intrinsic::ID IntrinsicID) {
  switch (IntrinsicID) {
    default: return None;

    // general cmp
    case Intrinsic::vp_cmp:
      return 2;

    // int arith
    case Intrinsic::vp_and:
    case Intrinsic::vp_or:
    case Intrinsic::vp_xor:
    case Intrinsic::vp_ashr:
    case Intrinsic::vp_lshr:
    case Intrinsic::vp_shl:
    case Intrinsic::vp_add:
    case Intrinsic::vp_sub:
    case Intrinsic::vp_mul:
    case Intrinsic::vp_udiv:
    case Intrinsic::vp_sdiv:
    case Intrinsic::vp_urem:
    case Intrinsic::vp_srem:
      return 2;

    // memory
    case Intrinsic::vp_load:
    case Intrinsic::vp_gather:
      return 1;
    case Intrinsic::vp_store:
    case Intrinsic::vp_scatter:
      return 2;

    // shuffle
    case Intrinsic::vp_select:
      return 0;

    case Intrinsic::vp_compose:
      return None;

    case Intrinsic::vp_compress:
    case Intrinsic::vp_expand:
    case Intrinsic::vp_vshift:
      return 2;

    // fp arith
    case Intrinsic::vp_fneg:
      return 1;

    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_frem:
      return 4;

    case Intrinsic::vp_fma:
      return 5;

    case Intrinsic::vp_ceil:
    case Intrinsic::vp_cos:
    case Intrinsic::vp_exp2:
    case Intrinsic::vp_exp:
    case Intrinsic::vp_floor:
    case Intrinsic::vp_log10:
    case Intrinsic::vp_log2:
    case Intrinsic::vp_log:
      return 3;

    case Intrinsic::vp_maxnum:
    case Intrinsic::vp_minnum:
      return 4;

    case Intrinsic::vp_nearbyint:
    case Intrinsic::vp_pow:
    case Intrinsic::vp_powi:
    case Intrinsic::vp_rint:
    case Intrinsic::vp_round:
    case Intrinsic::vp_sin:
    case Intrinsic::vp_sqrt:
    case Intrinsic::vp_trunc:
      return 3;

    case Intrinsic::vp_fptoui:
    case Intrinsic::vp_fptosi:
    case Intrinsic::vp_lround:
    case Intrinsic::vp_llround:
      return 2;

    case Intrinsic::vp_fpext:
    case Intrinsic::vp_fptrunc:
      return 3;

    // reductions
    case Intrinsic::vp_reduce_add:
    case Intrinsic::vp_reduce_mul:
    case Intrinsic::vp_reduce_umin:
    case Intrinsic::vp_reduce_umax:
    case Intrinsic::vp_reduce_smin:
    case Intrinsic::vp_reduce_smax:
    case Intrinsic::vp_reduce_and:
    case Intrinsic::vp_reduce_or:
    case Intrinsic::vp_reduce_xor:
    case Intrinsic::vp_reduce_fadd:
    case Intrinsic::vp_reduce_fmul:
    case Intrinsic::vp_reduce_fmin:
    case Intrinsic::vp_reduce_fmax:
      return 2;
  }
}

Optional<int>
VPIntrinsic::getVectorLengthParamPos(Intrinsic::ID IntrinsicID) {
  auto maskPos = getMaskParamPos(IntrinsicID);
  if (maskPos) {
    return maskPos.getValue() + 1;
  }

  if (IntrinsicID == Intrinsic::vp_compose) {
    return 3;
  }

  return None;
}

Optional<int>
VPIntrinsic::getExceptionBehaviorParamPos(Intrinsic::ID IntrinsicID) {
  switch (IntrinsicID) {
    default:
      return None;

    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_frem:
      return 3;

    case Intrinsic::vp_fma:
      return 4;

    case Intrinsic::vp_ceil:
    case Intrinsic::vp_cos:
    case Intrinsic::vp_exp2:
    case Intrinsic::vp_exp:
    case Intrinsic::vp_floor:
    case Intrinsic::vp_log10:
    case Intrinsic::vp_log2:
    case Intrinsic::vp_log:
      return 2;

    case Intrinsic::vp_maxnum:
    case Intrinsic::vp_minnum:
      return 3;
    case Intrinsic::vp_nearbyint:
    case Intrinsic::vp_pow:
    case Intrinsic::vp_powi:
    case Intrinsic::vp_rint:
    case Intrinsic::vp_round:
    case Intrinsic::vp_sin:
    case Intrinsic::vp_sqrt:
    case Intrinsic::vp_trunc:
      return 2;

    case Intrinsic::vp_fpext:
    case Intrinsic::vp_fptrunc:
      return 2;

    case Intrinsic::vp_fptoui:
    case Intrinsic::vp_fptosi:
    case Intrinsic::vp_lround:
    case Intrinsic::vp_llround:
      return 1;
  }
}

Optional<int>
VPIntrinsic::getRoundingModeParamPos(Intrinsic::ID IntrinsicID) {
  switch (IntrinsicID) {
    default:
      return None;

    case Intrinsic::vp_fadd:
    case Intrinsic::vp_fsub:
    case Intrinsic::vp_fmul:
    case Intrinsic::vp_fdiv:
    case Intrinsic::vp_frem:
      return 2;

    case Intrinsic::vp_fma:
      return 3;

    case Intrinsic::vp_ceil:
    case Intrinsic::vp_cos:
    case Intrinsic::vp_exp2:
    case Intrinsic::vp_exp:
    case Intrinsic::vp_floor:
    case Intrinsic::vp_log10:
    case Intrinsic::vp_log2:
    case Intrinsic::vp_log:
      return 1;

    case Intrinsic::vp_maxnum:
    case Intrinsic::vp_minnum:
      return 2;
    case Intrinsic::vp_nearbyint:
    case Intrinsic::vp_pow:
    case Intrinsic::vp_powi:
    case Intrinsic::vp_rint:
    case Intrinsic::vp_round:
    case Intrinsic::vp_sin:
    case Intrinsic::vp_sqrt:
    case Intrinsic::vp_trunc:
      return 1;

    case Intrinsic::vp_fptoui:
    case Intrinsic::vp_fptosi:
    case Intrinsic::vp_lround:
    case Intrinsic::vp_llround:
      return None;

    case Intrinsic::vp_fpext:
    case Intrinsic::vp_fptrunc:
      return 2;
  }
}

Intrinsic::ID
VPIntrinsic::getForConstrainedIntrinsic(Intrinsic::ID IntrinsicID) {
  switch (IntrinsicID) {
    default:
      return Intrinsic::not_intrinsic;

    // llvm.experimental.constrained.*
    case Intrinsic::experimental_constrained_cos:       return Intrinsic::vp_cos;
    case Intrinsic::experimental_constrained_sin:       return Intrinsic::vp_sin;
    case Intrinsic::experimental_constrained_exp:       return Intrinsic::vp_exp;
    case Intrinsic::experimental_constrained_exp2:      return Intrinsic::vp_exp2;
    case Intrinsic::experimental_constrained_log:       return Intrinsic::vp_log;
    case Intrinsic::experimental_constrained_log2:      return Intrinsic::vp_log2;
    case Intrinsic::experimental_constrained_log10:     return Intrinsic::vp_log10;
    case Intrinsic::experimental_constrained_sqrt:      return Intrinsic::vp_sqrt;
    case Intrinsic::experimental_constrained_ceil:      return Intrinsic::vp_ceil;
    case Intrinsic::experimental_constrained_floor:     return Intrinsic::vp_floor;
    case Intrinsic::experimental_constrained_round:     return Intrinsic::vp_round;
    case Intrinsic::experimental_constrained_trunc:     return Intrinsic::vp_trunc;
    case Intrinsic::experimental_constrained_rint:      return Intrinsic::vp_rint;
    case Intrinsic::experimental_constrained_nearbyint: return Intrinsic::vp_nearbyint;
  
    case Intrinsic::experimental_constrained_fadd:      return Intrinsic::vp_fadd;
    case Intrinsic::experimental_constrained_fsub:      return Intrinsic::vp_fsub;
    case Intrinsic::experimental_constrained_fmul:      return Intrinsic::vp_fmul;
    case Intrinsic::experimental_constrained_fdiv:      return Intrinsic::vp_fdiv;
    case Intrinsic::experimental_constrained_frem:      return Intrinsic::vp_frem;
    case Intrinsic::experimental_constrained_pow:       return Intrinsic::vp_pow;
    case Intrinsic::experimental_constrained_powi:      return Intrinsic::vp_powi;
    case Intrinsic::experimental_constrained_maxnum:    return Intrinsic::vp_maxnum;
    case Intrinsic::experimental_constrained_minnum:    return Intrinsic::vp_minnum;
  
    case Intrinsic::experimental_constrained_fma:       return Intrinsic::fma;
  }
}

Intrinsic::ID
VPIntrinsic::getForOpcode(unsigned OC) {
  switch (OC) {
  default:
    return Intrinsic::not_intrinsic;

    // fp unary
    case Instruction::FNeg:   return Intrinsic::vp_fneg;

    // fp binary
    case Instruction::FAdd:   return Intrinsic::vp_fadd;
    case Instruction::FSub:   return Intrinsic::vp_fsub;
    case Instruction::FMul:   return Intrinsic::vp_fmul;
    case Instruction::FDiv:   return Intrinsic::vp_fdiv;
    case Instruction::FRem:   return Intrinsic::vp_frem;

    // sign-oblivious int
    case Instruction::Add:    return Intrinsic::vp_add;
    case Instruction::Sub:    return Intrinsic::vp_sub;
    case Instruction::Mul:    return Intrinsic::vp_mul;

    // signed/unsigned int
    case Instruction::SDiv:   return Intrinsic::vp_sdiv;
    case Instruction::UDiv:   return Intrinsic::vp_udiv;
    case Instruction::SRem:   return Intrinsic::vp_srem;
    case Instruction::URem:   return Intrinsic::vp_urem;

    // logical
    case Instruction::Or:     return Intrinsic::vp_or;
    case Instruction::And:    return Intrinsic::vp_and;
    case Instruction::Xor:    return Intrinsic::vp_xor;

    case Instruction::LShr:   return Intrinsic::vp_lshr;
    case Instruction::AShr:   return Intrinsic::vp_ashr;
    case Instruction::Shl:    return Intrinsic::vp_shl;

    // comparison
    case Instruction::ICmp:
    case Instruction::FCmp:
      return Intrinsic::vp_cmp;
  }
}

VPIntrinsic::ShortTypeVec
VPIntrinsic::EncodeTypeTokens(VPIntrinsic::TypeTokenVec TTVec, Type * VecRetTy, Type & VectorTy, Type & ScalarTy) {
  ShortTypeVec STV;

  for (auto Token : TTVec) {
    switch (Token) {
    default:
      llvm_unreachable("unsupported token"); // unsupported VPTypeToken

    case VPIntrinsic::VPTypeToken::Scalar:   STV.push_back(&ScalarTy); break;
    case VPIntrinsic::VPTypeToken::Vector:   STV.push_back(&VectorTy); break;
    case VPIntrinsic::VPTypeToken::Returned:
      assert(VecRetTy);
      STV.push_back(VecRetTy);
      break;
    case VPIntrinsic::VPTypeToken::Mask:
      auto NumElems = VectorTy.getVectorNumElements();
      auto MaskTy = VectorType::get(Type::getInt1Ty(VectorTy.getContext()), NumElems);
      STV.push_back(MaskTy); break;
    }
  }

  return STV;
}


bool ConstrainedFPIntrinsic::isUnaryOp() const {
  switch (getIntrinsicID()) {
    default:
      return false;
    case Intrinsic::experimental_constrained_fptosi:
    case Intrinsic::experimental_constrained_fptoui:
    case Intrinsic::experimental_constrained_fptrunc:
    case Intrinsic::experimental_constrained_fpext:
    case Intrinsic::experimental_constrained_sqrt:
    case Intrinsic::experimental_constrained_sin:
    case Intrinsic::experimental_constrained_cos:
    case Intrinsic::experimental_constrained_exp:
    case Intrinsic::experimental_constrained_exp2:
    case Intrinsic::experimental_constrained_log:
    case Intrinsic::experimental_constrained_log10:
    case Intrinsic::experimental_constrained_log2:
    case Intrinsic::experimental_constrained_lrint:
    case Intrinsic::experimental_constrained_llrint:
    case Intrinsic::experimental_constrained_rint:
    case Intrinsic::experimental_constrained_nearbyint:
    case Intrinsic::experimental_constrained_ceil:
    case Intrinsic::experimental_constrained_floor:
    case Intrinsic::experimental_constrained_lround:
    case Intrinsic::experimental_constrained_llround:
    case Intrinsic::experimental_constrained_round:
    case Intrinsic::experimental_constrained_trunc:
      return true;
  }
}

bool ConstrainedFPIntrinsic::isTernaryOp() const {
  switch (getIntrinsicID()) {
    default:
      return false;
    case Intrinsic::experimental_constrained_fma:
      return true;
  }
}

Instruction::BinaryOps BinaryOpIntrinsic::getBinaryOp() const {
  switch (getIntrinsicID()) {
    case Intrinsic::uadd_with_overflow:
    case Intrinsic::sadd_with_overflow:
    case Intrinsic::uadd_sat:
    case Intrinsic::sadd_sat:
      return Instruction::Add;
    case Intrinsic::usub_with_overflow:
    case Intrinsic::ssub_with_overflow:
    case Intrinsic::usub_sat:
    case Intrinsic::ssub_sat:
      return Instruction::Sub;
    case Intrinsic::umul_with_overflow:
    case Intrinsic::smul_with_overflow:
      return Instruction::Mul;
    default:
      llvm_unreachable("Invalid intrinsic");
  }
}

bool BinaryOpIntrinsic::isSigned() const {
  switch (getIntrinsicID()) {
    case Intrinsic::sadd_with_overflow:
    case Intrinsic::ssub_with_overflow:
    case Intrinsic::smul_with_overflow:
    case Intrinsic::sadd_sat:
    case Intrinsic::ssub_sat:
      return true;
    default:
      return false;
  }
}

unsigned BinaryOpIntrinsic::getNoWrapKind() const {
  if (isSigned())
    return OverflowingBinaryOperator::NoSignedWrap;
  else
    return OverflowingBinaryOperator::NoUnsignedWrap;
}
