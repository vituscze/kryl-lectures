-- Všechny tři axiomy dokázené v theorem proveru (tj. můžete vidět, že
-- vám nekecám).
module ParserProof where

open import Data.Maybe
  using (Maybe; nothing; just)
open import Data.Product
  using (_×_; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl)

-- "typová třída" Monad.
record Monad (m : Set → Set) : Set₁ where
  field
    return : ∀ {a}   → a → m a
    _>>=_  : ∀ {a b} → m a → (a → m b) → m b

-- Parser jako ze 4. korekce.
Parser : Set → Set → Set
Parser s a = s → Maybe (a × s)

-- Konkrétní instance třídy Monad (pro libovolný typ s).
monad : ∀ s → Monad (Parser s)
monad s = record
  { return = λ a s → just (a , s)
  ; _>>=_  = bind
  }
  where
  -- Na "with" se můžete dívat jako na "case".
  bind : ∀ {a b} → Parser s a → (a → Parser s b) → Parser s b
  bind p f s₁ with p s₁
  ... | just (a , s₂) = f a s₂
  ... | nothing       = nothing

module Proof (s : Set) where
  open Monad (monad s)

  -- Levá identita.
  idˡ : ∀ {a b} x (f : a → Parser s b) → return x >>= f ≡ f x
  idˡ x f = refl

  -- Pravá identita.
  idʳ : ∀ {a} s′ (m : Parser s a) → (m >>= return) s′ ≡ m s′
  idʳ s′ m with m s′
  ... | just _  = refl
  ... | nothing = refl

  -- Asociativita.
  assoc : ∀ {a b c} s′ m (f : a → Parser s b) (g : b → Parser s c) →
          ((m >>= f) >>= g) s′ ≡ (m >>= λ a → f a >>= g) s′
  assoc s′ m f g with m s′
  ... | nothing = refl
  ... | just (a , s″) with f a s″
  ...   | just _  = refl
  ...   | nothing = refl
