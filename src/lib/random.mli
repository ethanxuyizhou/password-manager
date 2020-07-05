open! Core_kernel
open! Async_kernel

include module type of Random

val random_string :
  ?must_include_numbers:unit ->
  ?must_include_uppercase_letters:unit ->
  ?must_include_lowercase_letters:unit ->
  length:int ->
  unit ->
  string Or_error.t
