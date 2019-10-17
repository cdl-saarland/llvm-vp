; RUN: opt --verify %s -S | FileCheck %s

define void @test_vp_constrainedfp(<8 x double> %f0, <8 x double> %f1, <8 x double> %f2, <8 x double> %f3, <8 x i1> %m, i32 %n) {
  %r0 = call <8 x double> @llvm.vp.fadd.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tonearest", metadata !"fpexcept.ignore", <8 x i1> %m, i32 %n)
  %r1 = call <8 x double> @llvm.vp.fsub.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r2 = call <8 x double> @llvm.vp.fmul.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r3 = call <8 x double> @llvm.vp.fdiv.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r4 = call <8 x double> @llvm.vp.frem.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r5 = call <8 x double> @llvm.vp.fma.v8f64(<8 x double> %f0, <8 x double> %f1, <8 x double> %f2, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  ret void
}

; standard floating point
declare <8 x double> @llvm.vp.fadd.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen) #0
declare <8 x double> @llvm.vp.fsub.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen) #0
declare <8 x double> @llvm.vp.fmul.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen) #0
declare <8 x double> @llvm.vp.fdiv.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen) #0
declare <8 x double> @llvm.vp.frem.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen) #0
declare <8 x double> @llvm.vp.fma.v8f64(<8 x double>, <8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen) #0

; CHECK: attributes #0 = { inaccessiblememonly nounwind willreturn }
; CHECK-NOT: attributes #1 
