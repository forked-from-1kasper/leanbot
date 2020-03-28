import system.io data.buffer.parser
import ircbot.types ircbot.parsing ircbot.support ircbot.unicode ircbot.datetime

namespace effects

open parser
open parsing types support datetime

structure provider :=
(read  : io string)
(write : string → io unit)
(close : io unit)

def string.decode (conf : bot) (buff : char_buffer) :=
match conf.unicode_fix with
| tt := buffer.to_string buff
| ff := option.get_or_else (unicode.utf8_to_string buff) ""
end

def string.encode (conf : bot) (s : string) :=
match conf.unicode_fix with
| ff := s
| tt := unicode.string_to_utf8 s
end

def netcat (conf : bot) : io provider := do
  proc ← io.proc.spawn
    { cmd := "nc",
      args := [conf.info.server, conf.info.port],
      stdin := io.process.stdio.piped,
      stdout := io.process.stdio.piped },
  pure { read := string.decode conf <$> io.fs.get_line proc.stdout,
         write := λ s, io.fs.put_str_ln proc.stdin
           (string.encode conf s) >> io.fs.flush proc.stdin,
         close := io.proc.wait proc >> pure () }

/-- Return current date. -/
def get_date : io (option date) := do
  date_proc ← io.proc.spawn
    { cmd := "date",
      args := [date_format],
      stdin := io.process.stdio.piped,
      stdout := io.process.stdio.piped },
  unparsed_date_io ← io.fs.get_line date_proc.stdout,

  io.fs.close date_proc.stdout,
  io.proc.wait date_proc,

  let unparsed := buffer.to_string unparsed_date_io,
  sum.cases_on (run_string DateParser unparsed)
    (λ _, pure none) (pure ∘ some)

private def wrapped_put (prov : provider)
  (s : string) : io unit :=
prov.write s >> io.put_str_ln (sformat! "+ {s}")

private def loop (conf : bot) (prov : provider) : io unit := do
  line ← prov.read,
  if line.length > 0 then
    io.put_str (sformat! "- {line}")
  else pure (),

  let text : irc_text :=
    sum.cases_on (run_string NormalMessage line)
      (λ _, irc_text.raw_text line.trim_nl) id,

  messages ← list.join <$> (sequence $ flip bot_function.func text <$> conf.funcs),
  list.foldl (>>) (pure ()) $ (wrapped_put prov ∘ to_string) <$> messages,

  sum.cases_on (run_string Ping line)
    (λ _, pure ()) (wrapped_put prov ∘ to_string)

def mk_bot' (conf : bot) (prov : io provider) : io unit :=
prov >>= (λ inst, io.forever (loop conf inst) >> io.put_str "* OK" >> inst.close)

/-- Run a bot. -/
def mk_bot (conf : bot) : io unit :=
mk_bot' conf (netcat conf)

end effects