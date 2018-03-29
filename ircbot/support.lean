import ircbot.types

namespace support

open types

def join (channel : string) : irc_text :=
  irc_text.parsed_normal
    { object := none,
      type := message.join,
      text := "",
      subject := channel }

def privmsg (subject : string) (text : string) : irc_text :=
  irc_text.parsed_normal
    { object := none,
      type := message.privmsg,
      text := text,
      subject := subject }

def mode (subject : string) (mode : string) : irc_text :=
  irc_text.parsed_normal
    { object := none,
      type := message.mode,
      text := mode,
      subject := subject }

end support
