import ircbot
open types effects support

def server : string := "irc.mozilla.org"
def port : string := "6667"

def ident : string := "lean"
def bot_nickname : string := "leanbot"

theorem bot_nickname_is_correct : bot_nickname.front ≠ '#' :=
begin
  intros contra, cases contra
end

def print_date (d : date) (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    subject := subject, text := "\\date" } :=
  let new_subject :=
    if subject.front = '#' then subject else nick in
  [privmsg new_subject $ sformat! "It's now {d.hour}:{d.minute}"]
| _ := []
end

def print_date_io (dirty_input : io irc_text) : io (list irc_text) := do
  d ← get_date,
  input ← dirty_input,
  match d with
  | some v := pure $ print_date v input
  | none := pure []
  end

def join_at_start (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal v :=
  if v.type = message.mode ∧
     v.text = "+x" ∧
     v.subject = bot_nickname then
     [join "#borsch",
      privmsg "#borsch" "Black magic is here!",
      mode bot_nickname "+B"]
  else []
| _ := []
end

def ping_pong (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    subject := subject, text := "ping" } :=
  let new_subject :=
    if subject.front = '#' then subject else nick in
  [privmsg new_subject $ sformat! "{nick}, pong"]
| _ := []
end

theorem ping_pong_is_correct_on_channel (nick ident subject: string)
  (on_channel : subject.front = '#') :
  (ping_pong $ irc_text.parsed_normal
    { object := some ~nick!ident,
      type := message.privmsg,
      subject := subject,
      text := "ping" }) =
  [privmsg subject $ sformat! "{nick}, pong"] := begin
  intros, simp [ping_pong], rw [on_channel], trivial
end

theorem ping_pong_is_correct_on_priv (nick ident subject: string)
  (not_on_channel : subject = bot_nickname):
  (ping_pong $ irc_text.parsed_normal
    { object := some ~nick!ident,
      type := message.privmsg,
      subject := bot_nickname,
      text := "ping" }) =
  [privmsg nick $ sformat! "{nick}, pong"] := begin
  intros, simp [privmsg], simp [ping_pong],
  simp [privmsg], simp [bot_nickname_is_correct]
end

def my_bot : bot :=
{ nickname := bot_nickname,
  ident := ident,
  server := server,
  port := port,
  funcs := [functor.map join_at_start,
            functor.map ping_pong,
            print_date_io] }

def main := mk_bot my_bot
