import ircbot.types ircbot.effects ircbot.support
open types effects support

namespace modules.print_date

def print_date (d : date) (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := "\\date" } :=
  let new_subject :=
    if subject.front = '#' then subject else nick in
  [privmsg new_subject $ sformat! "It's {d.hour}:{d.minute} now."]
| _ := []
end

def print_date_io (dirty_input : io irc_text) : io (list irc_text) := do
  d ← get_date,
  input ← dirty_input,
  match d with
  | some v := pure $ print_date v input
  | none := pure []
  end

end modules.print_date