namespace types

inductive message : Type
| notice | privmsg | mode | quit | nick

inductive person : Type
| unidentified : string → person
| user : string → string → person
notation `~` nick `!` ident := person.user nick ident

structure normal_message :=
(object : person)
(type : message)
(subject : string)
(text : string)

instance message.has_decidable_eq :
  decidable_eq message :=
by tactic.mk_dec_eq_instance

instance message.has_to_string : has_to_string message :=
⟨λ m,
match m with
| message.notice := "NOTICE"
| message.privmsg := "PRIVMSG"
| message.mode := "MODE"
| message.quit := "QUIT"
| message.nick := "NICK"
end⟩

inductive irc_text : Type
| raw_text : string → irc_text
| parsed_normal : normal_message → irc_text
| ping : string → irc_text

instance normal_message.has_to_string : has_to_string normal_message :=
⟨λ s, sformat! "{to_string s.type} {s.subject} :{s.text}\n"⟩

instance irc_text.has_to_string : has_to_string irc_text :=
⟨λ it,
match it with
| (irc_text.raw_text v) := v
| (irc_text.parsed_normal v) := to_string v
| (irc_text.ping server) := sformat! "PONG :{server}"
end⟩

end types
