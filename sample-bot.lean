import ircbot
open types effects

def server : string := "irc.mozilla.org"
def port : string := "6667"

def ident : string := "lean"

def bot_nickname : string := "leanbot"

theorem bot_nickname_is_correct : bot_nickname.front ≠ '#' :=
begin
  intros contra, cases contra
end

/-
def wait (time : string) : io unit := do
  wait_proc ← io.proc.spawn { cmd := "sleep",
                              args := [time] },
  io.proc.wait wait_proc,
  pure ()
-/

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
  { object := ~nick!ident, type := message.privmsg,
    subject := subject, text := "ping" } :=
  [irc_text.parsed_normal
    { object := person.unidentified bot_nickname,
      type := message.privmsg,
      subject :=
        if subject.front = '#' then subject
        else nick,
      text := sformat! "{nick}, pong" }]
| _ := []
end

theorem ping_pong_is_correct_on_channel (nick ident subject: string)
  (on_channel : subject.front = '#') :
  (ping_pong $ irc_text.parsed_normal
    { object := ~nick!ident,
      type := message.privmsg,
      subject := subject,
      text := "ping" }) =
  [irc_text.parsed_normal
    { object := person.unidentified bot_nickname,
      type := message.privmsg,
      subject := subject,
      text := sformat! "{nick}, pong" }] := begin
  intros, simp [ping_pong], rw [on_channel], trivial
end

theorem ping_pong_is_correct_on_priv (nick ident subject: string)
  (not_on_channel : subject = bot_nickname):
  (ping_pong $ irc_text.parsed_normal
    { object := ~nick!ident,
      type := message.privmsg,
      subject := bot_nickname,
      text := "ping" }) =
  [irc_text.parsed_normal
    { object := person.unidentified bot_nickname,
      type := message.privmsg,
      subject := nick,
      text := sformat! "{nick}, pong" }] := begin
  intros, simp [ping_pong], simp [bot_nickname_is_correct]
end

def my_bot : bot :=
{ nickname := bot_nickname,
  ident := ident,
  server := server,
  port := port,
  funcs := [join, ping_pong] }

def main := mk_bot my_bot
