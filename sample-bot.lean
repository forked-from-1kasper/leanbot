import data.buffer.parser
import ircbot ircbot.base64

open types effects support parsing login
open parser

-- constants
def server : string := "chat.freenode.net"
def port : string := "6667"

def ident : string := "lean"
def bot_nickname : string := "leanbot"
-- constants

theorem bot_nickname_is_correct : bot_nickname.front ≠ '#' :=
begin intros contra, cases contra end

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

def messages : list irc_text :=
  [join "#lor",
   privmsg "#lor" "Lean rulet",
   mode bot_nickname "+B"]

def ping_pong (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := "\\ping" } :=
  let new_subject :=
    if subject.front = '#' then subject else nick in
  [privmsg new_subject $ sformat! "{nick}, pong"]
| _ := []
end

theorem ping_pong_is_correct_on_channel (nick ident subject: string)
  (on_channel : subject.front = '#') :
  (ping_pong $ irc_text.parsed_normal
    { object := some ~nick!ident,
      type := message.privmsg,
      args := [subject],
      text := "\\ping" }) =
  [privmsg subject $ sformat! "{nick}, pong"] := begin
  intros, simp [ping_pong], rw [on_channel], trivial
end

theorem ping_pong_is_correct_on_priv (nick ident subject: string)
  (not_on_channel : subject = bot_nickname):
  (ping_pong $ irc_text.parsed_normal
    { object := some ~nick!ident,
      type := message.privmsg,
      args := [bot_nickname],
      text := "\\ping" }) =
  [privmsg nick $ sformat! "{nick}, pong"] := begin
  intros, simp [privmsg], simp [ping_pong],
  simp [privmsg], simp [bot_nickname_is_correct]
end

def my_bot_info : bot_info :=
bot_info.mk bot_nickname ident server port

def my_bot (acc : account) : bot :=
{ info := my_bot_info,
  funcs := [functor.map ping_pong,
            functor.map (sasl my_bot_info messages acc),
            print_date_io,
            functor.map relogin] }

def main := do
  args ← io.cmdline_args,
  match args with
  | (login :: password :: []) :=
    mk_bot (my_bot $ account.mk login password)
  | _ := io.fail "syntax: lean --run file.lean [login] [password]"
  end