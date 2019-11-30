import ircbot.types

def list.singleton {α : Type} (x : α) : list α := [ x ]

namespace string
  def take_right_while {α : Type} (f : α → Prop) [decidable_pred f] : list α → list α
  | (hd :: tl) := if f hd then take_right_while tl else hd :: tl
  | [] := []

  def take_left_while {α : Type} (f : α → Prop) [decidable_pred f] (xs : list α) :=
  (take_right_while f xs.reverse).reverse

  def trim (c : char → Prop) [decidable_pred c] : string → string :=
  list.as_string ∘ take_left_while c ∘
  take_right_while c ∘ string.to_list
end string

def sequence {f : Type → Type} [applicative f] {α : Type} :
  list (f α) → f (list α)
| [] := pure []
| (x :: xs) := (::) <$> x <*> sequence xs

namespace support

open types

def join (channel : string) : irc_text :=
irc_text.parsed_normal
  { type := message.join,
    text := "",
    args := [channel] }

/-- Returns irc_text.parsed_normal message with “:” in start of message. -/
def privmsg (subject : string) (text : string) : irc_text :=
irc_text.parsed_normal
  { type := message.privmsg,
    text := ":" ++ text,
    args := [subject] }

def notice (subject : string) (text : string) : irc_text :=
irc_text.parsed_normal
  { type := message.notice,
    text := ":" ++ text,
    args := [subject] }

def mode (subject : string) (mode : string) : irc_text :=
irc_text.parsed_normal
  { type := message.mode,
    text := mode,
    args := [subject] }

end support
