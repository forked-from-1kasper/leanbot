import data.buffer.parser
import ircbot ircbot.base64
import ircbot.modules

open types effects support parsing login
open parser

-- constants
def server : string := "chat.freenode.net"
def port : string := "6667"

def ident : string := "lean"
def bot_nickname : string := "leanbot"
-- end

theorem bot_nickname_is_correct : bot_nickname.front ≠ '#' :=
begin intros contra, cases contra end

def messages : list irc_text :=
  [ join "#lor",
    privmsg "#lor" "Lean rulet",
    mode bot_nickname "+B" ]

def my_bot_info : bot_info :=
bot_info.mk bot_nickname bot_nickname_is_correct ident server port

def my_funcs (acc : account) : list bot_function :=
  [ modules.ping_pong.ping_pong,
    sasl my_bot_info messages acc,
    modules.print_date.print_date,
    modules.admin.join_channel,
    relogin ]

def my_bot (acc : account) : bot :=
let funcs := my_funcs acc in
{ info := my_bot_info,
  funcs := modules.help.help funcs :: funcs }

def main := do
  args ← io.cmdline_args,
  match args with
  | (login :: password :: []) :=
    mk_bot (my_bot $ account.mk login password)
  | _ := io.fail "syntax: lean --run file.lean [login] [password]"
  end
