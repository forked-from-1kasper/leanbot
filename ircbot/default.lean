import system.io
import data.buffer.parser

import ircbot.types
import ircbot.parsing

def server : string := "irc.mozilla.org"
def port : string := "6667"

def network_provider : string := "nc"

def ident : string := "lean"
def bot_nickname : string := "leanbot"

open parser
open parsing
open types

notation x `&` f := f x

/-
def wait (time : string) : io unit := do
  wait_proc ← io.proc.spawn { cmd := "sleep",
                              args := [time] },
  io.proc.wait wait_proc,
  pure ()
-/

def wrapped_put (h : io.handle) (s : string) : io unit := do
  io.fs.put_str h s,
  io.fs.flush h,
  io.put_str $ sformat! "+ {s}"

def join (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal v :=
  if v.type = message.mode ∧
     v.text = "+x" ∧
     v.subject = bot_nickname then
    list.map irc_text.raw_text
      ["JOIN #borsch\n",
       "PRIVMSG #borsch I can not in fucking unicode!\n",
       sformat! "MODE {bot_nickname} +B\n"]
  else []
| _ := []
end

def ping_pong (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := ~nick!ident,
    type := message.privmsg,
    subject := subject,
    text := "ping" } :=
  [irc_text.parsed_normal
    { object := person.unidentified bot_nickname,
      type := message.privmsg,
      subject :=
        if subject.front = '#' then subject
        else nick,
      text := sformat! "{nick}, pong" }]
| _ := []
end

def funcs : list (irc_text → list irc_text) :=
  [join, ping_pong]

def loop : io.proc.child → io unit
| proc := do
  getted_buffer ← io.fs.get_line proc.stdout,
  let line := buffer.to_string getted_buffer,
  if line.length > 0 then
    io.put_str $ sformat! "- {line}"
  else pure (),

  let maybe_normal := run_string NormalMessage line,
  let return_messages : list string :=
    let text :=
      match maybe_normal with
      | (sum.inr v) := v
      | _ := irc_text.raw_text line
      end in
    list.map (λ (f : irc_text → list irc_text), f text) funcs &
    list.join &
    list.map to_string,

  list.map (wrapped_put proc.stdin) return_messages &
  list.foldl (>>) (pure ()),

  let maybe_ping := run_string Ping line,
  match maybe_ping with
  | (sum.inr v) := wrapped_put proc.stdin $ to_string v
  | _ := pure ()
  end,

  pure ()

def main : io unit := do
  proc ← io.proc.spawn { cmd := network_provider,
                         args := [server, port],
                         stdin := io.process.stdio.piped,
                         stdout := io.process.stdio.piped },

  let out := wrapped_put proc.stdin,

  out $ sformat!
    "USER {ident} " ++
    "https://leanprover.github.io/ 1 :A bot written in Lean \n",
  out $ sformat! "NICK {bot_nickname} \n",

  io.forever $ loop proc,
  io.put_str "* OK"
