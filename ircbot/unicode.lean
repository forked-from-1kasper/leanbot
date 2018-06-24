import data.buffer system.io
import data.bitvec

namespace unicode

private def utf_8_convert_bitvec : list (bitvec 8) → option (list char)
-- 0xxxxxxx -- symbols from 0 to 7Fh
| (⟨ff :: tl₁, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat tl₁)) <$> utf_8_convert_bitvec chars
-- 110xxxxx 10xxxxxx -- symbols from 80h to 7FFh
| (⟨tt :: tt :: ff :: tl₁, _⟩ :: ⟨tt :: ff :: tl₂, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat $ tl₁ ++ tl₂)) <$>
    utf_8_convert_bitvec chars
-- 1110xxxx 10xxxxxx 10xxxxxx -- symbols from 800h to FFFFh
| (⟨tt :: tt :: tt :: ff :: tl₁, _⟩ :: ⟨tt :: ff :: tl₂, _⟩ ::
   ⟨tt :: ff :: tl₃, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat $ tl₁ ++ tl₂ ++ tl₃)) <$>
  utf_8_convert_bitvec chars
-- 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx -- symbols from 10000h to 10FFFFh
| (⟨tt :: tt :: tt :: tt :: ff :: tl₁, _⟩ :: ⟨tt :: ff :: tl₂, _⟩ ::
   ⟨tt :: ff :: tl₃, _⟩ :: ⟨tt :: ff :: tl₄, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat $ tl₁ ++ tl₂ ++ tl₃ ++ tl₄)) <$>
  utf_8_convert_bitvec chars
| [] := some []
| _ := none

-- Default Lean’s convert from char_buffer to string is incorrect
-- (when we get a string using get_line, for example),
-- because it does not work correctly with Unicode and UTF-8, in particular.
-- This is simple realisation of correct convert from char_buffer to string
-- for UTF-8 encoding.
def utf8_to_string (buff : char_buffer) : option string :=
list.as_string <$> utf_8_convert_bitvec (list.map (bitvec.of_nat 8 ∘ char.to_nat) buff.to_list)

end unicode
