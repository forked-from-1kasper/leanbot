import ircbot.types ircbot.support
open types support

namespace modules.ping_pong

def ping_pong_func (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := "\\ping" } :=
  let new_subject :=
    if subject.front = '#' then subject else nick in
  [privmsg new_subject $ sformat! "{nick}, pong"]
| _ := []
end

def ping_pong : bot_function :=
  { name := "ping-pong",
    syntax := some "\\ping",
    description := "ping-pong game!",
    func := functor.map ping_pong_func }

theorem ping_pong_is_correct_on_channel (nick ident subject: string)
  (on_channel : subject.front = '#') :
  (ping_pong_func $ irc_text.parsed_normal
    { object := some ~nick!ident,
      type := message.privmsg,
      args := [subject],
      text := "\\ping" }) =
  [privmsg subject $ sformat! "{nick}, pong"] := begin
  intros, simp [ping_pong_func], rw [on_channel], trivial
end

theorem ping_pong_is_correct_on_priv (nick ident subject bot_nickname : string)
  (bot_nickname_is_correct : bot_nickname.front ≠ '#')
  (not_on_channel : subject = bot_nickname):
  (ping_pong_func $ irc_text.parsed_normal
    { object := some ~nick!ident,
      type := message.privmsg,
      args := [bot_nickname],
      text := "\\ping" }) =
  [privmsg nick $ sformat! "{nick}, pong"] := begin
  intros, simp [privmsg], simp [ping_pong_func],
  simp [privmsg], simp [bot_nickname_is_correct]
end

end modules.ping_pong