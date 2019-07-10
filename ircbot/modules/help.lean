import ircbot.types ircbot.support
open types support

namespace modules.help

def help_func (funcs : list bot_function) (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := "\\help" } :=
  [privmsg nick "Loaded modules:"] ++
    list.map (privmsg nick ∘ to_string) funcs
| _ := []
end

/-- Autogenerate and print loaded modules list. -/
def help (funcs : list bot_function) : bot_function :=
  { name := "help",
    syntax := some "\\help",
    description := "Print loaded modules.",
    func := functor.map $ help_func funcs }

end modules.help
