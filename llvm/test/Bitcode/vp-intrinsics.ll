; RUN: opt --verify %s -S | FileCheck %s

define void @test_vp_constrainedfp(<8 x double> %f0, <8 x double> %f1, <8 x double> %f2, <8 x double> %f3, <8 x i1> %m, i32 %n) #0 {
  %r0 = call <8 x double> @llvm.vp.fadd.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tonearest", metadata !"fpexcept.ignore", <8 x i1> %m, i32 %n)
  %r1 = call <8 x double> @llvm.vp.fsub.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r2 = call <8 x double> @llvm.vp.fmul.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r3 = call <8 x double> @llvm.vp.fdiv.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r4 = call <8 x double> @llvm.vp.frem.v8f64(<8 x double> %f0, <8 x double> %f1, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r5 = call <8 x double> @llvm.vp.fma.v8f64(<8 x double> %f0, <8 x double> %f1, <8 x double> %f2, metadata !"round.tozero", metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  %r6 = call <8 x double> @llvm.vp.fneg.v8f64(<8 x double> %f2, metadata !"fpexcept.strict", <8 x i1> %m, i32 %n)
  ret void
}

define void @test_vp_int(<8 x i32> %i0, <8 x i32> %i1, <8 x i32> %i2, <8 x i32> %f3, <8 x i1> %m, i32 %n) {
  %r0 = call <8 x i32> @llvm.vp.add.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  %r1 = call <8 x i32> @llvm.vp.sub.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  %r2 = call <8 x i32> @llvm.vp.mul.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  %r3 = call <8 x i32> @llvm.vp.sdiv.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  %r4 = call <8 x i32> @llvm.vp.srem.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  %r5 = call <8 x i32> @llvm.vp.udiv.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  %r6 = call <8 x i32> @llvm.vp.urem.v8i32(<8 x i32> %i0, <8 x i32> %i1, <8 x i1> %m, i32 %n)
  ret void
}

define void @test_mem(<16 x i32*> %p0, <16 x i32>* %p1, <16 x i32> %i0, <16 x i1> %m, i32 %n) {
  call void @llvm.vp.store.v16i32p0v16i32(<16 x i32> %i0, <16 x i32>* %p1, <16 x i1> %m, i32 %n)
  call void @llvm.vp.scatter.v16i32v16p0i32(<16 x i32> %i0 , <16 x i32*> %p0, <16 x i1> %m, i32 %n)
  %l0 = call <16 x i32> @llvm.vp.load.v16i32p0v16i32(<16 x i32>* %p1, <16 x i1> %m, i32 %n)
  %l1 = call <16 x i32> @llvm.vp.gather.v16i32v16p0i32(<16 x i32*> %p0, <16 x i1> %m, i32 %n)
  ret void
}


; standard floating point
declare <8 x double> @llvm.vp.fadd.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
declare <8 x double> @llvm.vp.fsub.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
declare <8 x double> @llvm.vp.fmul.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
declare <8 x double> @llvm.vp.fdiv.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
declare <8 x double> @llvm.vp.frem.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
declare <8 x double> @llvm.vp.fma.v8f64(<8 x double>, <8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
declare <8 x double> @llvm.vp.fneg.v8f64(<8 x double>, metadata, <8 x i1> mask, i32 vlen)

; integer
declare <8 x i32> @llvm.vp.add.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.sub.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.mul.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.sdiv.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.srem.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.udiv.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.urem.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)

declare <8 x i32> @llvm.vp.and.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.xor.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.or.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
declare <8 x i32> @llvm.vp.ashr.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen) 
declare <8 x i32> @llvm.vp.lshr.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen) 
declare <8 x i32> @llvm.vp.shl.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)

; memory
declare void @llvm.vp.store.v16i32p0v16i32(<16 x i32>, <16 x i32>*, <16 x i1> mask, i32 vlen)
declare void @llvm.vp.scatter.v16i32v16p0i32(<16 x i32>, <16 x i32*>, <16 x i1> mask, i32 vlen)
declare <16 x i32> @llvm.vp.load.v16i32p0v16i32(<16 x i32>*, <16 x i1> mask, i32 vlen)
declare <16 x i32> @llvm.vp.gather.v16i32v16p0i32(<16 x i32*>, <16 x i1> mask, i32 vlen)

; TODO reductions
; TODO icmp , fcmp

attributes #0 = { strictfp }


; CHECK: declare <8 x double> @llvm.vp.fadd.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x double> @llvm.vp.fsub.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x double> @llvm.vp.fmul.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x double> @llvm.vp.fdiv.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x double> @llvm.vp.frem.v8f64(<8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x double> @llvm.vp.fma.v8f64(<8 x double>, <8 x double>, <8 x double>, metadata, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x double> @llvm.vp.fneg.v8f64(<8 x double>, metadata, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.add.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.sub.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.mul.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.sdiv.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.srem.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.udiv.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.urem.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.and.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.xor.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.or.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.ashr.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.lshr.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare <8 x i32> @llvm.vp.shl.v8i32(<8 x i32>, <8 x i32>, <8 x i1> mask, i32 vlen)
; CHECK: declare void @llvm.vp.store.v16i32.p0v16i32(<16 x i32>, <16 x i32>*, <16 x i1> mask, i32 vlen)
; CHECK: declare void @llvm.vp.scatter.v16i32.v16p0i32(<16 x i32>, <16 x i32*>, <16 x i1> mask, i32 vlen)
; CHECK: declare <16 x i32> @llvm.vp.load.v16i32.p0v16i32(<16 x i32>*, <16 x i1> mask, i32 vlen)
; CHECK: declare <16 x i32> @llvm.vp.gather.v16i32.v16p0i32(<16 x i32*>, <16 x i1> mask, i32 vlen)
