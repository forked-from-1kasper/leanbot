import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace modules.admin

def CorrectIdent := do
  ch '~', word ← many_char1 $ sat (λ c, c ≠ ' ' ∧ c ≠ '@'),
  ch '@', ip ← sep_by1 (ch '.') parsing.Number,
  pure (word, ip)

def is_admin : person → bool
| ~user!ident :=
  match run_string CorrectIdent ident with
  | (sum.inr (word, [31, 173, _, _])) := tt
  | _ := ff
  end
| _ := ff

def CorrectJoin : parser string := do
  parsing.tok "\\join", many_char1 parsing.WordChar

def join_channel_func (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some object, type := message.privmsg,
    args := [subject], text := text } :=
  if is_admin object then
    match run_string CorrectJoin text with
    | (sum.inr channel) := [join channel]
    | _ := []
    end
  else []
| _ := []
end

def join_channel : bot_function :=
  { name := "join_channel",
    syntax := "\\join [channel]",
    description := "Join channel. Requires root.",
    func := functor.map join_channel_func }

end modules.admin