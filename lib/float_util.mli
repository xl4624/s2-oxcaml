(** Small floating-point helpers shared across the library. *)

(** IEEE 754 remainder: rounds the quotient to the nearest even integer, unlike
    [Float.mod_float] / [fmod] which truncates. Matches C [remainder()]. *)
val ieee_remainder : float -> float -> float

(** Unboxed [float#] version of {!ieee_remainder}. *)
val ieee_remainder_u : float# -> float# -> float#
[@@zero_alloc]

(** Branchless [min]/[max] backed by the SSE2 [minsd]/[maxsd] intrinsics on x86-64 (C
    fallback on ARM).

    NaN semantics differ from [Float_u.min]/[max]: if either operand is NaN, the hardware
    instruction returns the second operand. In particular, [min_u nan y = y] and
    [max_u nan y = y]. This matches [Float_u.min]/[max] only when NaN is in the second
    operand (or absent). Call sites in this library pass a finite literal or known-finite
    value as the first operand, so the observable behavior is unchanged. *)
val min_u : float# -> float# -> float#
[@@zero_alloc]

val max_u : float# -> float# -> float# [@@zero_alloc]
