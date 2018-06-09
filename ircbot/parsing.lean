import data.buffer.parser

import ircbot.types

namespace parsing

open types
open parser

def lf := char.of_nat 10
def cr := char.of_nat 13

def LF := ch lf
def CR := ch cr

def Nl := CR >> LF <|> LF <|> CR

namespace string
  def trim_sym (c : char) (s : string) := if s.back = c then string.pop_back s else s
  def trim_nl := trim_sym cr ∘ trim_sym lf
end string

def date_format : string := "+%Y.%m.%d %H:%M:%S,%N"

def Numeral : parser char :=
sat $ λ c, list.any "0123456789".to_list (= c)
def Number := many_char1 Numeral >>= pure ∘ string.to_nat

def DateParser : parser date := do
  year ← Number, ch '.',
  month ← Number, ch '.',
  day ← Number, ch ' ',
  hour ← Number, ch ':',
  minute ← Number, ch ':',
  seconds ← Number, ch ',',
  nanoseconds ← Number, optional Nl,
  pure $ date.mk year month day hour minute seconds nanoseconds

def whitespaces := " \t\x0d".to_list

def WordChar : parser char := sat (≠ ' ')

def NarrowWordChar : parser char :=
sat (λ c, list.all (whitespaces ++ [':', '*']) (≠ c))
def NarrowWord := many_char1 NarrowWordChar

def Ws : parser unit :=
decorate_error "<whitespace>" $
many' $ one_of' whitespaces

def Word : parser string := many_char1 WordChar <* Ws

def FreeWord : parser string := many_char1 $ sat (λ c, c ≠ lf ∧ c ≠ cr)

def tok (s : string) := str s >> Ws

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
  server ← Word,
  pure $ irc_text.ping server

def NormalMessage : parser irc_text := do
  ch ':',
  object ← decorate_error "<person>" $ Person,
  type ← MessageType,
  args ← decorate_error "<args>" $ many1 (NarrowWord <* Ws),
  ch ':',
  text ← decorate_error "<text>" $ FreeWord,
  optional Nl,
  pure (irc_text.parsed_normal $
    normal_message.mk (some object) type args text)

def LoginWords : parser server_says := do
  ch ':', server ← NarrowWord, Ws,
  status ← NarrowWord, Ws, ch '*', Ws,
  args ← many (NarrowWord <* Ws),
  message ← optional (ch ':' >> FreeWord),
  pure $ server_says.mk server status args message

end parsing
