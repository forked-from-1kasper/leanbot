import ircbot.types ircbot.effects ircbot.support ircbot.datetime
open types effects support datetime

namespace modules.print_date

def print_date_func (d : date) (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := "\\date" } :=
  let new_subject :=
    if subject.front = '#' then subject else nick in
  let leading_zero (n : nat) :=
  if n ≥ 10 then to_string n else "0" ++ (to_string n) in
  [privmsg new_subject $ sformat!
     ("It’s {leading_zero d.hour}:{leading_zero d.minute}, " ++
      "{d.day} {to_string d.month}, {d.weekday} now.")]
| _ := []
end

def print_date_io (input : irc_text) : io (list irc_text) := do
  date ← get_date,
  pure $ match date with
  | some v := print_date_func v input
  | none := []
  end

def print_date : bot_function :=
  { name := "print_date",
    syntax := "\\date",
    description := "Print current date.",
    func := print_date_io }

end modules.print_date
