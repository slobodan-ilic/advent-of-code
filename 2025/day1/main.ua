ReadText ← ⊜□⊸≠@\n &fras
ParseLine ← (
  °□   # unbox string
  ⋕⊸↘1 # parse only number
  ⊙(⨬(¯1|1)=@R↙1)
  /×⊂
)

\+ ⊂ 50 ≡ParseLine ReadText "input.txt"

⧈∘ 2
⍜▽-1 ⊸=0 ⊸≡/≥
⌊÷100×
≡/-
/+
