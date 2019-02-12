; RUN: opt < %s -verify -S | FileCheck %s

define float @reduce_fadd(float %init, <16 x float> %x, <16 x i1> mask %M, i32 vlen %L) {
  %res = call float @llvm.vp.reduce.f32.p0f_f32f32f32i1i32f.v16f32(float (float, float, i1, i32)* @llvm.vp.fadd.f32, float %init, <16 x float> %x, <16 x i1> %M, i32 %L)
  ret float %res
}

; Function Attrs: nounwind readnone
declare float @llvm.vp.fadd.f32(float, float, i1, i32)

; Function Attrs: nounwind readnone
declare float @llvm.vp.reduce.f32.p0f_f32f32f32i1i32f.v16f32(float (float, float, i1, i32)*, float, <16 x float>, <16 x i1>, i32)
