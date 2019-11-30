import ircbot.support ircbot.parsing
open types support parser

def Word := many_char1 parsing.WordChar
def Words := many_char1 (sat $ function.const char true)

structure speech :=
(object : person) (subject text : string) (type : message)

def router {α : Type} (name desc : string) (syntax : option string)
  (p : parser α) (func : speech → α → io (list irc_text))
  (wherein : list message) : bot_function :=
let p' := parsing.tok ("\\" ++ name) >> p in
{ name := name,
  syntax := syntax,
  description := desc,
  func := λ input,
    match input with
    | irc_text.parsed_normal
      { object := some object, type := type,
        args := [ subject ], text := text } := 
      if subject.front = '#' ∧ type ∈ wherein then
        sum.rec_on (run_string p' text) (λ _, pure [])
          (func ⟨object, subject, text, type⟩)
      else pure []
    | _ := pure []
    end }

def list.singleton {α : Type} (x : α) : list α := [ x ]