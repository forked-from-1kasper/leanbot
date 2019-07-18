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

private def get_hex_core : string.iterator → nat → nat → nat
| it 0       r := r
| it (i + 1) r := get_hex_core it.next i (r * 16 + get_hex_ch it.curr)

def get_hex (s : string) : nat :=
get_hex_core s.mk_iterator s.length 0

def Numeral : parser char := decorate_error "<digit>" $ sat char.is_digit
def Number := decorate_error "<number>" (many_char1 Numeral >>= pure ∘ string.to_nat)

def HexCh : parser char := decorate_error "<hex digit>" $
  sat (λ c, c.is_digit ∨ c.is_hex)
def HexNumber := decorate_error "<hex number>" (many_char1 HexCh >>= pure ∘ get_hex)

def Integer : parser ℤ :=
int.of_nat <$> Number <|>
(int.neg ∘ int.of_nat) <$> (ch '-' >> Number)

def whitespaces := " \t\x0d".to_list

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

def DayOfWeekParser : parser day_of_week :=
(tok "1" >> pure day_of_week.monday) <|>
(tok "2" >> pure day_of_week.tuesday) <|>
(tok "3" >> pure day_of_week.wednesday) <|>
(tok "4" >> pure day_of_week.thursday) <|>
(tok "5" >> pure day_of_week.friday) <|>
(tok "6" >> pure day_of_week.saturday) <|>
(tok "7" >> pure day_of_week.sunday)

def MonthParser : parser month :=
(tok "01"  >> pure month.jan) <|>
(tok "02" >> pure month.feb) <|>
(tok "03" >> pure month.mar) <|>
(tok "04" >> pure month.apr) <|>
(tok "05" >> pure month.may) <|>
(tok "06" >> pure month.jun) <|>
(tok "07" >> pure month.jul) <|>
(tok "08" >> pure month.aug) <|>
(tok "09" >> pure month.sep) <|>
(tok "10" >> pure month.oct) <|>
(tok "11" >> pure month.nov) <|>
(tok "12" >> pure month.dec)

def DateParser : parser date := do
  year ← Number, ch '.',
  month ← MonthParser, ch '.',
  day ← Number, ch ' ',
  hour ← Number, ch ':',
  minute ← Number, ch ':',
  seconds ← Number, ch ',',
  nanoseconds ← Number, ch ' ',
  weekday ← DayOfWeekParser, optional Nl,
  pure $ date.mk year month day hour minute seconds nanoseconds weekday

def MessageType : parser message :=
(tok "NOTICE" >> return message.notice) <|>
(tok "PRIVMSG" >> return message.privmsg) <|>
(tok "MODE" >> return message.mode) <|>
(tok "QUIT" >> return message.quit) <|>
(tok "NICK" >> return message.nick) <|>
(tok "KICK" >> return message.kick) <|>
(tok "JOIN" >> return message.join)

def PersonIdentified : parser person := do
  nick ← many_char1 $ sat (λ c, c ≠ ' ' ∧ c ≠ '!'),
  ch '!',
  ident ← Word,
  pure ~nick!ident

def PersonUnidentified : parser person := do
  var ← Word,
  pure $ person.unidentified var

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
