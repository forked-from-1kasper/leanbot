import system.io
import data.buffer.parser

import ircbot.types ircbot.parsing ircbot.support

namespace effects

open parser
open parsing types support

notation x `&` f := f x

def network_provider : string := "nc"

def date_provider := "date"
def get_date : io $ option date := do
  date_proc ← io.proc.spawn
    { cmd := date_provider,
      args := [date_format],
      stdin := io.process.stdio.piped,
      stdout := io.process.stdio.piped },
  unparsed_date_io ← io.fs.get_line date_proc.stdout,

  io.fs.close date_proc.stdout,
  io.proc.wait date_proc,

  let unparsed := buffer.to_string unparsed_date_io,
  match run_string DateParser unparsed with
  | (sum.inr v) := pure $ some v
  | _ := pure none
  end

def wrapped_put (h : io.handle) (s : string) : io unit := do
  io.fs.put_str h s,
  io.fs.flush h,
  io.put_str $ sformat! "+ {s}"

def sequence_applicative {f : Type → Type} [applicative f] {α : Type} :
  list (f α) → f (list α)
| [] := pure []
| (x :: xs) := (::) <$> x <*> sequence_applicative xs

def application {α β : Type} (f : α → β) (a : α) := f a

def loop (bt : bot)
         (proc : io.proc.child) : io unit := do
  getted_buffer ← io.fs.get_line proc.stdout,
  let line := buffer.to_string getted_buffer,
  if line.length > 0 then
    io.put_str $ sformat! "- {line}"
  else pure (),

  let maybe_normal := run_string NormalMessage line,
  let text :=
    match maybe_normal with
    | (sum.inr v) := v
    | _ := irc_text.raw_text line
    end,

  messages ← list.map (flip application $ pure text) bt.funcs &
             sequence_applicative &
             functor.map list.join,

  list.map (wrapped_put proc.stdin) (list.map to_string messages) &
  list.foldl (>>) (pure ()),

  let maybe_ping := run_string Ping line,
  match maybe_ping with
  | (sum.inr v) := wrapped_put proc.stdin $ to_string v
  | _ := pure ()
  end,

  pure ()

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

  io.forever $ loop bt proc,
  io.put_str "* OK"

end effects
