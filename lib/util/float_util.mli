(** Small floating-point helpers shared across the library. *)

(** IEEE 754 remainder: rounds the quotient to the nearest even integer, unlike
    [Float.mod_float] / [fmod] which truncates. *)
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

(** Round a [float#] to the nearest [int] using the FPU's current rounding mode (the
    default is round-half-to-even). Compiles to [cvtsd2si] on x86-64 (C fallback on ARM).

    Semantics differ from [Float_u.iround_nearest_exn], which rounds half away from zero:
    e.g., [iround_half_to_even_u 0.5 = 0] but [Float_u.iround_nearest_exn 0.5 = 1].

    The result is unspecified for NaN, infinity, or values outside [int] range; the caller
    must guarantee a finite, in-range input. *)
val iround_half_to_even_u : float# -> int
[@@zero_alloc]
