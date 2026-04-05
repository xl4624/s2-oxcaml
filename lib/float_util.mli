(** Small floating-point helpers shared across the library. *)

(** IEEE 754 remainder: rounds the quotient to the nearest even integer, unlike
    [Float.mod_float] / [fmod] which truncates. Matches C [remainder()]. *)
val ieee_remainder : float -> float -> float

(** Unboxed [float#] version of {!ieee_remainder}. *)
val ieee_remainder_u : float# -> float# -> float#
[@@zero_alloc]
