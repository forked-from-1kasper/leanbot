import data.buffer.parser

import ircbot.types

namespace parsing

open types
open parser

def LF := ch $ char.of_nat 10
def CR := ch $ char.of_nat 13

def Nl := CR >> LF <|> LF <|> CR

def date_format : string := "+%Y.%m.%d %H:%M:%S,%N"

def Numeral : parser char :=
sat $ λ c, list.any "0123456789".to_list (= c)
def Number := many_char1 Numeral

def DateParser : parser date := do
  year ← Number, ch '.',
  month ← Number, ch '.',
  day ← Number, ch ' ',
  hour ← Number, ch ':',
  minute ← Number, ch ':',
  seconds ← Number, ch ',',
  nanoseconds ← Number, optional Nl,
  pure { year := year.to_nat,
         month := month.to_nat,
         day := day.to_nat,
         hour := hour.to_nat,
         minute := minute.to_nat,
         seconds := seconds.to_nat,
         nanoseconds := nanoseconds.to_nat }

def WordChar : parser char := sat (≠ ' ')

def Ws : parser unit :=
decorate_error "<whitespace>" $
many' $ one_of' " \t\x0d".to_list

def Word : parser string := many_char1 WordChar <* Ws

def tok (s : string) := str s >> Ws

def MessageType : parser message :=
(tok "NOTICE" >> return message.notice) <|>
(tok "PRIVMSG" >> return message.privmsg) <|>
(tok "MODE" >> return message.mode) <|>
(tok "QUIT" >> return message.quit) <|>
(tok "NICK" >> return message.nick) <|>
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
  str ":",
  object ← Person,
  type ← MessageType,
  subject ← Word,
  optional $ ch ':',
  text ← many1 $ sat
    (λ c, c ≠ char.of_nat 10 ∧ c ≠ char.of_nat 13),
  optional Nl,
  pure $ irc_text.parsed_normal
    { object := some object, type := type,
      subject := subject, text := text.as_string }

end parsing
