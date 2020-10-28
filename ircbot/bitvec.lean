import data.vector

def bitvec := vector bool

def {u} vector.singleton {α : Type u} : α → vector α 1 :=
λ b, ⟨[b], rfl⟩

namespace bitvec
  def of_nat : Π (n : ℕ), nat → bitvec n
  |    0    x := vector.nil
  | (n + 1) x := vector.append (of_nat n (x / 2))
                               (vector.singleton (to_bool (x % 2 = 1)))

  def add_lsb (r : ℕ) (b : bool) := r + r + cond b 1 0

  def bits_to_nat (v : list bool) : nat :=
  v.foldl add_lsb 0
end bitvec