import data.buffer.parser
import ircbot ircbot.base64 ircbot.modules.ping_pong ircbot.modules.print_date

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

def messages : list irc_text :=
  [join "#lor",
   privmsg "#lor" "Lean rulet",
   mode bot_nickname "+B"]

def my_bot_info : bot_info :=
bot_info.mk bot_nickname ident server port

def my_bot (acc : account) : bot :=
{ info := my_bot_info,
  funcs := [functor.map modules.ping_pong.ping_pong,
            functor.map (sasl my_bot_info messages acc),
            modules.print_date.print_date_io,
            functor.map relogin] }

def main := do
  args ← io.cmdline_args,
  match args with
  | (login :: password :: []) :=
    mk_bot (my_bot $ account.mk login password)
  | _ := io.fail "syntax: lean --run file.lean [login] [password]"
  end