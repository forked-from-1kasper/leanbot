import data.buffer.parser
import ircbot.types ircbot.support ircbot.parsing

open types support parsing parser

namespace login

def login_messages (nick : string) (ident : string) :=
  [irc_text.raw_text $ sformat! "USER {ident} " ++
     "https://leanprover.github.io/ 1 :A bot written in Lean \n",
   irc_text.raw_text $ sformat! "NICK {nick} \n"]

def relogin (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some _, type := message.kick,
    args := [channel, who], text := _ } :=
    [join channel]
| _ := []
end

def no_login (nick : string) (messages : list irc_text) (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal v :=
  if v.type = message.mode ∧
     v.text = "+i" ∧
     v.args = [nick] then messages
  else []
| _ := []
end

def sasl (info : bot_info) (messages : list irc_text) (acc : account) : irc_text → list irc_text
| (irc_text.raw_text "AUTHENTICATE +") :=
  [irc_text.raw_text $ sformat! "AUTHENTICATE {acc.get_hash}\n"]
| (irc_text.raw_text v) :=
  match run_string LoginWords v with
  | (sum.inr { status := "NOTICE", message := some "*** Checking Ident",
               server := _, args := _ }) :=
    [irc_text.raw_text "CAP REQ :multi-prefix sasl\n"]
  | (sum.inr { status := "CAP", args := ["ACK"],
               message := some "multi-prefix sasl ", server := _ }) :=
    [irc_text.raw_text "AUTHENTICATE PLAIN\n"]
  | (sum.inr { status := "903", message := some "SASL authentication successful",
               server := _, args := _ }) :=
    [irc_text.raw_text "CAP END\n"] ++
    login_messages info.nickname info.ident ++
    messages
  | _ := []
  end
| _ := []

end login