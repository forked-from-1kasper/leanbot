import system.io
import data.buffer.parser

import ircbot.types
import ircbot.parsing

namespace effects

open parser
open parsing
open types

notation x `&` f := f x

def network_provider : string := "nc"

def wrapped_put (h : io.handle) (s : string) : io unit := do
  io.fs.put_str h s,
  io.fs.flush h,
  io.put_str $ sformat! "+ {s}"

def loop (funcs : list (irc_text → list irc_text))
         (proc : io.proc.child) : io unit := do
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

structure bot :=
(nickname : string)
(ident : string)
(server : string)
(port : string)
(funcs : list (irc_text → list irc_text))

def mk_bot (bt : bot) : io unit := do
  proc ← io.proc.spawn { cmd := network_provider,
                         args := [bt.server, bt.port],
                         stdin := io.process.stdio.piped,
                         stdout := io.process.stdio.piped },

  let out := wrapped_put proc.stdin,

  out $ sformat!
    "USER {bt.ident} " ++
    "https://leanprover.github.io/ 1 :A bot written in Lean \n",
  out $ sformat! "NICK {bt.nickname} \n",

  io.forever $ loop bt.funcs proc,
  io.put_str "* OK"

end effects
