ReadText   ← ⊜□⊸≠@\n &fras
ParseRange ← °⋕⊂⟜+⊙⇡₁ ⟜-°⊟ ⊜⋕ ⊸≠@-
IsInvalid  ← ×⊙⋕≍⊸⊃(↘|↙)⌊÷2⊸⧻
IsInvalid "12"

Process ← /+≡(/+≡◇IsInvalid◇ParseRange)
Input   ← °□⊢ ReadText "input.txt"

Process ⊜□ ⊸≠@, Input