ReadText   ← ⊜□⊸≠@\n &fras
ParseRange ← °⋕⊂⟜+⊙⇡₁ ⟜-°⊟ ⊜⋕ ⊸≠@-
IsInvalid  ← ×⊙⋕≍⊸⊃(↘|↙)⌊÷2⊸⧻
IsInvalid "12"

Shapes     ← ▽=0◿1⊸(⊢°⍉)⍉˜⊟⟜÷⇡₁↥1⌊⊸÷2⧻
IsInvalid′ ← /↧≡⌞≍⊸⊢°□
IsInvalid″ ← /↥≡IsInvalid′ ≡⌟(□↯) ⊸Shapes

# Process ← /+≡(/+≡◇IsInvalid◇ParseRange)
Process ← /+≡(/+≡◇(×⊙⋕⊸(×≠1⧻⟜IsInvalid″))◇ParseRange)

Input ← °□⊢ ReadText "input.txt"

Process ⊜□ ⊸≠@, Input
