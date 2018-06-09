import ircbot.types

namespace support

open types

def join (channel : string) : irc_text :=
  irc_text.parsed_normal
    { object := none,
      type := message.join,
      text := "",
      args := [channel] }

def privmsg (subject : string) (text : string) : irc_text :=
  irc_text.parsed_normal
    { object := none,
      type := message.privmsg,
      text := ":" ++ text,
      args := [subject] }

def mode (subject : string) (mode : string) : irc_text :=
  irc_text.parsed_normal
    { object := none,
      type := message.mode,
      text := mode,
      args := [subject] }

end support