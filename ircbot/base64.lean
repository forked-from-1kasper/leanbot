import data.bitvec data.buffer

namespace base64

def group_list {α : Type} (n : ℕ) : list α → list (list α) :=
  list.reverse ∘ list.foldl
  (λ accum (elem : α),
    match accum with
    | [] := [[elem]]
    | (x :: xs) :=
      if x.length ≥ n then
        [elem] :: x :: xs
      else (x ++ [elem]) :: xs
    end) []

private def base64_alphabet :=
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".to_list

private def base64_table (id : nat) : string :=
option.get_or_else (string.singleton <$> list.nth base64_alphabet id) ""

private def padding_addition (n : ℕ) (l : list bool) : list bool :=
list.repeat ff (n * (((l.length - 1) / n) + 1) - l.length)

def padding_left (n : ℕ) (l : list bool) : list bool :=
padding_addition n l ++ l

def padding_right (n : ℕ) (l : list bool) : list bool :=
l ++ padding_addition n l

private def char_to_bool_list : char → list bool :=
padding_left 8 ∘ list.reverse ∘ nat.bits ∘ char.to_nat

private def encode_bit_list : list bool → string :=
string.join ∘ list.map
  (base64_table ∘ bitvec.bits_to_nat ∘ padding_right 6) ∘
  group_list 6

def encode (s : string) :=
let bit_buffer := list.join $ list.map char_to_bool_list s.to_list in
let encoded := encode_bit_list bit_buffer in
let final_seq :=
  match (bit_buffer.length / 8) % 3 with
  | 1 := "=="
  | 2 := "="
  | _ := ""
  end in encoded ++ final_seq

def null := string.singleton $ char.of_nat 0

end base64