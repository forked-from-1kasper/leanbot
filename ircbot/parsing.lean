import data.buffer.parser

import ircbot.types ircbot.datetime

namespace string
  def lf := char.of_nat 10
  def cr := char.of_nat 13

  def trim_sym (c : char) (s : string) := if s.back = c then string.pop_back s else s
  def trim_nl := trim_sym cr ∘ trim_sym lf
end string

namespace char
  def is_hex (ch : char) : bool :=
  list.any (string.to_list "abcdefABCDEF") (= ch)
end char

namespace parsing

open types datetime
open parser

def LF := ch string.lf
def CR := ch string.cr

def Nl := CR >> LF <|> LF <|> CR

def date_format : string := "+%Y.%m.%d %H:%M:%S,%N %u"

-- ignores non-numerical characters
private def get_hex_ch (ch : char) : nat :=
if ch.is_digit then ch.to_nat - '0'.to_nat
else if ch.is_hex then ch.to_lower.to_nat - 'a'.to_nat + 10
else 0

private def get_hex_core : string.iterator → ℕ → ℕ → ℕ
| it 0       r := r
| it (i + 1) r := get_hex_core it.next i (r * 16 + get_hex_ch it.curr)

def get_hex (s : string) : nat :=
get_hex_core s.mk_iterator s.length 0

def Numeral : parser char := decorate_error "<digit>" $ sat char.is_digit
def Number := decorate_error "<number>" (string.to_nat <$> many_char1 Numeral)

def HexCh : parser char := decorate_error "<hex digit>" $
  sat (λ c, c.is_digit ∨ c.is_hex)
def HexNumber := decorate_error "<hex number>" (get_hex <$> many_char1 HexCh)

def Integer : parser ℤ :=
int.of_nat <$> Number <|>
(int.neg ∘ int.of_nat) <$> (ch '-' >> Number)

def whitespaces := " \t\n\x0d".to_list

def WordChar : parser char := sat (≠ ' ')

def NarrowWordChar : parser char :=
sat (λ c, list.all (whitespaces ++ [':', '*']) (≠ c))
def NarrowWord := many_char1 NarrowWordChar
def WordNotNl := many_char1 $ sat (λ c, c ≠ string.lf ∧ c ≠ string.cr)

def Ws : parser unit :=
decorate_error "<whitespace>" $
many1 (one_of' whitespaces) >> eps

def Ws' : parser unit :=
decorate_error "<whitespace or not>" $
many' (one_of' whitespaces)

def Word : parser string := many_char1 WordChar <* Ws

def tok (s : string) := str s >> Ws

def OrelseEnum {α β : Type} (p : α → parser unit) : list (α × β) → parser β
| [] := parser.fail "unknown element"
| ((x, y) :: xs) := (p x >> pure y) <|> OrelseEnum xs

def DayOfWeekParser : parser day_of_week :=
OrelseEnum str
  [ ("1", day_of_week.monday),
    ("2", day_of_week.tuesday),
    ("3", day_of_week.wednesday),
    ("4", day_of_week.thursday),
    ("5", day_of_week.friday),
    ("6", day_of_week.saturday),
    ("7", day_of_week.sunday) ]

def MonthParser : parser month :=
OrelseEnum str
  [ ("01", month.jan), ("02", month.feb),
    ("03", month.mar), ("04", month.apr),
    ("05", month.may), ("06", month.jun),
    ("07", month.jul), ("08", month.aug),
    ("09", month.sep), ("10", month.oct),
    ("11", month.nov), ("12", month.dec) ]

def DateParser : parser date := do
  year        ← Number,      ch '.',
  month       ← MonthParser, ch '.',
  day         ← Number,      ch ' ',
  hour        ← Number,      ch ':',
  minute      ← Number,      ch ':',
  seconds     ← Number,      ch ',',
  nanoseconds ← Number,      ch ' ',
  weekday     ← DayOfWeekParser, optional Nl,
  pure (date.mk year month day hour minute seconds nanoseconds weekday)

def MessageType : parser message :=
OrelseEnum tok
  [ ("NOTICE",  message.notice),
    ("PRIVMSG", message.privmsg),
    ("MODE",    message.mode),
    ("QUIT",    message.quit),
    ("NICK",    message.nick),
    ("KICK",    message.kick),
    ("JOIN",    message.join) ]

def PersonIdentified : parser person := do
  nick ← many_char1 $ sat (λ c, c ≠ ' ' ∧ c ≠ '!'),
  ch '!', ident ← Word, pure ~nick!ident

def PersonUnidentified : parser person :=
person.unidentified <$> Word

def Person : parser person :=
PersonIdentified <|> PersonUnidentified

def Ping : parser irc_text := do
  tok "PING",
  optional $ ch ':',
  server ← WordNotNl, optional Nl,
  pure $ irc_text.ping server

def NormalMessage : parser irc_text := do
  ch ':',
  object ← decorate_error "<person>" $ Person,
  type ← MessageType,
  args ← decorate_error "<args>" $ many (NarrowWord <* Ws'),
  text ← decorate_error "<text>" $ optional (ch ':' >> WordNotNl),
  optional Nl,
  pure (irc_text.parsed_normal $
    normal_message.mk (some object) type args $
      option.get_or_else text "")

def LoginWords : parser server_says := do
  ch ':', server ← NarrowWord, Ws,
  status ← NarrowWord, Ws, ch '*', Ws,
  args ← many (NarrowWord <* Ws'),
  message ← optional (ch ':' >> WordNotNl),
  pure $ server_says.mk server status args message

end parsing
