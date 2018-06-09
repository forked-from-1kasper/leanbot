import data.buffer system.io
import data.bitvec

namespace unicode

private def utf_8_convert_bitvec : list (bitvec 8) → option (list char)
| (⟨ff :: tl₁, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat tl₁)) <$> utf_8_convert_bitvec chars
| (⟨tt :: tt :: ff :: tl₁, _⟩ :: ⟨tt :: ff :: tl₂, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat $ tl₁ ++ tl₂)) <$>
    utf_8_convert_bitvec chars
| (⟨tt :: tt :: tt :: ff :: tl₁, _⟩ :: ⟨tt :: ff :: tl₂, _⟩ ::
   ⟨tt :: ff :: tl₃, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat $ tl₁ ++ tl₂ ++ tl₃)) <$>
  utf_8_convert_bitvec chars
| (⟨tt :: tt :: tt :: tt :: ff :: tl₁, _⟩ :: ⟨tt :: ff :: tl₂, _⟩ ::
   ⟨tt :: ff :: tl₃, _⟩ :: ⟨tt :: ff :: tl₄, _⟩ :: chars) :=
  list.cons (char.of_nat (bitvec.bits_to_nat $ tl₁ ++ tl₂ ++ tl₃ ++ tl₄)) <$>
  utf_8_convert_bitvec chars
| [] := some []
| _ := none

def utf8_to_string (buff : char_buffer) : option string :=
list.as_string <$> utf_8_convert_bitvec (list.map (bitvec.of_nat 8 ∘ char.to_nat) buff.to_list)

end unicode
