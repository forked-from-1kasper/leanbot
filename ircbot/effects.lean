import system.io
import data.buffer.parser

import ircbot.types ircbot.parsing ircbot.support ircbot.unicode ircbot.datetime

namespace effects

open parser
open parsing types support datetime

private def network_provider : string := "nc"
private def date_provider := "date"

/-- Return current date. -/
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
  sum.cases_on (run_string DateParser unparsed)
    (λ _, pure none) (pure ∘ some)

private def wrapped_put (h : io.handle) (bt : bot) (s : string) : io unit := do
  match bt.unicode_output_fix with
  | ff := io.fs.put_str_ln h s
  | tt := io.fs.put_str_ln h (unicode.string_to_utf8 s)
  end,
  io.fs.flush h,
  io.put_str_ln (sformat! "+ {s}")

private def loop (bt : bot) (proc : io.proc.child) : io unit := do
  getted_buffer ← io.fs.get_line proc.stdout,
  let line := option.get_or_else
    (unicode.utf8_to_string getted_buffer) "",
  if line.length > 0 then
    io.put_str $ sformat! "- {line}"
  else pure (),

  let text : irc_text :=
    sum.cases_on (run_string NormalMessage line)
      (λ _, irc_text.raw_text $ string.trim_nl line) id,

  messages ← list.join <$>
             (sequence $
              list.map (flip function.app (pure text))
                       (list.map bot_function.func bt.funcs)),

  list.foldl (>>) (pure ()) $
  list.map (wrapped_put proc.stdin bt) (list.map to_string messages),

  sum.cases_on (run_string Ping line)
    (λ _, pure ()) (wrapped_put proc.stdin bt ∘ to_string)

/-- Run a bot. -/
def mk_bot (bt : bot) : io unit := do
  proc ← io.proc.spawn
    { cmd := network_provider,
      args := [bt.info.server, bt.info.port],
      stdin := io.process.stdio.piped,
      stdout := io.process.stdio.piped },

  io.forever $ loop bt proc,
  io.put_str "* OK"

end effects
