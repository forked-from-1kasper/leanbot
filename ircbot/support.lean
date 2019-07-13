import ircbot.types

namespace support

open types

def sequence {f : Type → Type} [applicative f] {α : Type} :
  list (f α) → f (list α)
| [] := pure []
| (x :: xs) := (::) <$> x <*> sequence xs

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
