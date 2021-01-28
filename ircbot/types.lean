import system.io
import ircbot.base64

namespace types

inductive message : Type
| notice | privmsg | mode | quit | nick | join | kick

inductive person : Type
| unidentified : string → person
| user : string → string → person
notation `~` nick `!` ident := person.user nick ident

structure normal_message :=
(object : option person := none)
(type   : message)
(args   : list string)
(text   : string)

instance message.has_decidable_eq :
  decidable_eq message :=
by tactic.mk_dec_eq_instance

instance message.has_to_string : has_to_string message :=
⟨λ m,
match m with
| message.notice  := "NOTICE"
| message.privmsg := "PRIVMSG"
| message.mode    := "MODE"
| message.quit    := "QUIT"
| message.nick    := "NICK"
| message.join    := "JOIN"
| message.kick    := "KICK"
end⟩

inductive irc_text : Type
| raw_text : string → irc_text
| parsed_normal : normal_message → irc_text
| ping : string → irc_text

instance normal_message.has_to_string : has_to_string normal_message :=
⟨λ s, let args := string.join $ list.map (++ " ") s.args in
sformat! "{to_string s.type} {args}{s.text}"⟩

instance irc_text.has_to_string : has_to_string irc_text :=
⟨λ it, match it with
| (irc_text.raw_text v)      := v
| (irc_text.parsed_normal v) := to_string v
| (irc_text.ping server)     := sformat! "PONG :{server}"
end⟩

structure account :=
(login : string) (password : string)

def account.get_hash (acc : account) :=
base64.encode $ sformat!
  "{acc.login}{base64.null}{acc.login}{base64.null}{acc.password}"

structure bot_info :=
(nickname    : string)
(not_channel : nickname.front ≠ '#')
(ident       : string)
(server      : string)
(port        : string)
(on_start    : list irc_text := [])

structure bot_function :=
(name        : string)
(syntax      : option string)
(description : string)
(func        : irc_text → io (list irc_text))

instance bot_function.has_to_string : has_to_string bot_function :=
⟨λ it, let syntax := option.get_or_else it.syntax "<none>" in
sformat! "name: {it.name}; syntax: {syntax}; description: {it.description}"⟩ 

structure streams :=
(read write : bool)

structure bot :=
(info  : bot_info)
(funcs : list bot_function)
(fix   : streams := ⟨ff, ff⟩)

structure server_says :=
(server  : string)
(status  : string)
(args    : list string)
(message : option string)

instance : has_to_string server_says :=
⟨λ s, match s with
| { server := server, status := status, args := args, message := message } :=
let args_string := string.join $ list.map (++" ") args in
let message_string := option.get_or_else ((string.append ":") <$> message) "" in
sformat! ":{server} {status} * {args_string}{message_string}"
end⟩

end types
