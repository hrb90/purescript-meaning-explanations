module Main where

import Prelude (id, ($))


{-
Meaning explanations in PureScript, or, proof-carrying code is easy

This is a presentation of Per Martin-Lof's meaning explanations for the intuitionistic propositional calculus in PureScript. 
I'm following Martin-Lof's presentation from the 1983 Siena lectures (published as "On the Meanings of the Logical Constants...").

We'll briefly note that in all of the following PureScript code, types should be read as propositions, and terms should be read as proofs or verifications.
(This is usually called the Curry-Howard correspondence for historical reasons, 
but I hope this presentation helps convince you that the connection goes much deeper,
that logic and computation are really two sides of one and the same coin.)
After we've explained the logical constants and justified the logical laws, we'll show how we can write proofs of actual theorems of IPC in PureScript.

There are four logical constants we need to explain: implication, conjunction, disjunction, and "bottom".
For each of these the meaning of the constant will be constituted by its formation rule(s).
We will also introduce, and justify, elimination rules --- the logical laws.

The first thing we have to explain is how to form implications, and what counts as a verification of an implication.

The formation rule is simple:

  Given that we know that a and b are propositions, we know that (a -> b) is a proposition.

(Actually, all we need is that b is a proposition on the hypothesis that a is true, but there is no difference in these semantics.)

So what counts as a verification of a -> b? A verification of a -> b is a hypothetical verification that a is true under the assumption that a is true; 
that is to say that, given a verification of a, you would be able to produce a verification of b;
that is to say that you have a function from verifications of a to verifications of b.

This explanation immediately motivates the introduction rule for implications:

  Given that we know that b is true on the hypothetical that a is true, we know that (a -> b) is true.

The justification is that this is just a restatement of what counts as a verification of (a -> b).

And so, we finally reach our first lines of PureScript code, and they're anticlimactic:
-}

prfImpIntroduction :: forall a b. (a -> b) -> (a -> b)
prfImpIntroduction = id

{-
The thing to note here is that the function type constructor, ->, is doing triple(!) duty. Let's break down the type signature:

  prfImpIntroduction :: forall a b. (a -> b) -> (a -> b)
                                      (1)   (2)   (3)

At position (1), -> represents a hypothetical judgment that b is true given that a is true; 
at position (2), -> separates the premises of our inference rule from its conclusion;
at position (3), -> is the (intuitionistic) implicational logical connective.

(It actually takes on one more role, that of separating the premises of our inference rule from each other. We'll see this soon.)

What the rule of implication introduction tells us is that it's kosher to move from (1) to (3).

And so, we move on to the elimination rule for implication:

  Given that we know that (a -> b) and a are true, we know that b is true.

The justification is that (a -> b) is a way to go from a verification of a to a verification of b;
and we have a verification of a, so we apply our knowledge that (a -> b) to our knowledge that a.

Thinking of elimination as application, let's move to the PureScript rule
-}

prfImpElimination :: forall a b. (a -> b) -> a -> b
prfImpElimination = ($)

{-
Again, let's take special note of the type signature:

  prfImpElimination :: forall a b. (a -> b) -> a -> b
                                     (1)   (2)  (3)

At position (1), -> is the implicational logical connective;
at position (2), it separates the two premises of our inference rule;
and at position (3), it separates the premises of the inference rule from its conclusion.

There's another way we can look at the elimination rule: Suppose we only know (a -> b). Then the elimination rule says that,
hypothetically, if we knew a, we could conclude b. It gives us a hypothetical judgment that b is true on the condition that a is true.
In this case position (1) remains the same, but position (2) separates the premises of the inference rule from the conclusion,
and position (3) becomes a hypothetical judgment.

So the elimination rule tells us that it's kosher to move from reading -> as logical implication to reading it as hypothetical judgment.

I hope the above argument (somewhat) justifies the use of only one type constructor to mean so many overloaded things.

This completes the rules for implication. Now we will move on to conjunction.

Again, we must first explain how to form conjunctions, and what counts as a verification of a conjunction. Again we start with the formation rule:

  Given that we know that a and b are propositions, we know that (a && b) is a proposition.

This corresponds to a type constructor that takes two arguments and returns a new type.

And what counts as a verification of (a && b) is exactly a verification of a and a verification of b taken together.

What immediately follows from this is the rule of introduction:

  Given that we know that a is true and that b is true, we know that (a && b) is true.

The code is again very simple:
-}

data And a b = Both a b

prfAndIntroduction :: forall a b. a -> b -> And a b
prfAndIntroduction = Both

{-
(You may recognize this as isomorphic to Tuple.)

We have not one but two rules for eliminating conjunctions:

  Given that we know that (a && b) is true, we know that a is true.
  Given that we know that (a && b) is true, we know that b is true.

The justification is that we have a verification of (a && b), which is just a verification of a and a verification of b taken together,
so we can "extract" each individual verification and put it into practice.

The code:
-}

prfAndEliminationLeft :: forall a b. And a b -> a
prfAndEliminationLeft (Both prfA _) = prfA

prfAndEliminationRight :: forall a b. And a b -> b
prfAndEliminationRight (Both _ prfB) = prfB

{-
And so we come to disjunction. Again we must explain the formation rule first:

  Given that we know that a and b are propositions, we know that (a || b) is a proposition.

Again this corresponds to a type constructor of kind * -> * -> *.

And what counts as a verification of (a || b)? A verification of (a || b) is simply either a verification of a, or a verification of b.

The code:
-}

data Or p q = A p | OrA q

{-
(This is of course isomorphic to the better-known Either.)

There are two rules of introduction for disjunction:

  Given that we know that a is true, we know that (a || b) is true.
  Given that we know that b is true, we know that (a || b) is true.

The justification is that a verification of (a || b) is either a verification of a or a verification of b;
in the first place, we have a verification of a;
in the second, a verification of b.

Just as with conjunction, the introduction rules are simply data constructors:
-}

prfOrIntroductionLeft :: forall a. a -> (forall b. Or a b)
prfOrIntroductionLeft = A

prfOrIntroductionRight :: forall a. a -> (forall b. Or b a)
prfOrIntroductionRight = OrA

{-
Now we come to the rule of elimination for disjunction, and it's a long one:

  Given that we know (a || b) is true, and we know that c is a proposition on the hypothesis that (a || b) is true,
  and we know that c is true on the hypothesis that a is true, and we know that c is true on the hypothesis that b is true,
  then we know that c is true.

We'll give the justification as we walk through the code step by step.
-}

prfOrElimination :: forall a b. Or a b -> (forall c. (a -> c) -> (b -> c) -> c)
-- We already have a verification of (a || b), which we call prfDisjunction in the code.
prfOrElimination prfDisjunction =
  -- By the definition of disjunction it's either a verification of a or a verification of b.
  case prfDisjunction of
    -- If it's a verification of a, apply our knowledge that c is true on the hypothesis that a is true to obtain a verification of c...
    A prfA -> 
      \prfAImpliesC _ -> prfAImpliesC prfA
    -- If it's a verification of b, apply our knowledge that c is true on the hypothesis that b is true to obtain a verification of c.
    OrA prfB -> 
      \_ prfBImpliesC -> prfBImpliesC prfB

{-
Note that everything on the term level in the above proof is itself a proof.

Now we arrive at the rules for ⊥. As always we begin with the rule of formation:

  ⊥ is a proposition.

For ⊥ to be a proposition we must explain what counts as a verification of it, and it is this: 
Nothing counts as a verification of ⊥. Under no conditions is it true.

So there is no introduction rule; we can only obtain a verification of ⊥ if we already have one.
This motivates the PureScript definition:
-}

newtype Bottom = Bottom Bottom

{-
There is, however, a rule of elimination.

  Given that we know that ⊥ is true, and we know that a is a proposition on the hypothesis that ⊥ is true,
  then we know that a is true.

We can safely make this inference because we know that there is never any condition under which we will have
a verification of a. Martin-Lof compares it to the figure of speech:

  "I'll eat my hat if you do [impossible thing]!"

where you know you are at no risk of actually having to eat your hat.
-}

prfBottomElimination :: forall a. Bottom -> a
prfBottomElimination (Bottom prfBottom) = prfBottomElimination prfBottom

{-
The semantics of the PureScript implementation can be read as follows: If someone comes to you claiming to have a verification of ⊥,
you say, "okay, give it to me [unwrap the newtype constructor] and I'll pass it up the chain to get a verification of a." 
Since the only thing you can get by unwrapping a verification of ⊥ is another verification of ⊥ to unwrap, 
this will go on forever, and you'll never need to provide the promised verification of a.

We come at last to negation. The (informal) justification of ⊥-elimination similarly justifies concluding the truth of any proposition 
given a verification of a false proposition; in particular it verifies ⊥.

Also, if we have a verification that a -> ⊥, and a is true, we could apply our knowledge to obtain a verification of ⊥;
but definitionally we can never have a verification of ⊥.

The above two points justify the definition of ~a as a -> ⊥:
-}

type Not a = a -> Bottom

{-
And that's all there is to intuitionistic propositional logic!

Let's briefly talk about the consistency of this system. Martin-Lof claims that the meaning explanations constitute a
pre-mathematical, or "simple-minded", consistency proof for intuitionistic propositional logic. How is this so? For the 
logic to be inconsistent, we would need to derive a contradiction, i.e., find a proof of ⊥. But there is no introduction 
rule for ⊥; the meaning explanations explicitly say there is no way to verify the judgment that ⊥ is true; there is no
data constructor for Bottom (unless we already have a term of type ⊥, i.e., a proof that ⊥ is true).

Now let's look at how we can prove some propositions in PureScript.

We'll start with double negation introduction, i.e.

  Given that we know that a, we know that ~~a.

This is a theorem of intuitionistic and classical logic; however, the converse,

  Given that we know that ~~a, we know that a.

is *only* a theorem of classical logic.
-}

prfDni :: forall a. a -> Not (Not a)
-- We assumed that we know a, so we are given a term representing a proof of a.
prfDni prfA =
  -- Not (Not a) means that if we are given a proof of ~a, we should give back ⊥.
  -- But a proof of ~a is a proof of (a -> ⊥), so we apply our knowledge of
  -- (a -> ⊥) to our knowledge of a, obtaining ⊥.
  \prfNotA -> prfNotA prfA

{-
We can do the same thing to prove the law of noncontradiction:

  Given that a is a proposition, we know that ~(a && ~a).

So if we're given a proof of (a && ~a), we should give back ⊥.
-}

prfNonContradiction :: forall a. Not (And a (Not a))
prfNonContradiction =
  \(Both prfP prfNotP) -> prfNotP prfP

{-
Let's talk about the law of the excluded middle:

  Given that a is a proposition, we know that (a || ~a).

We can make a type alias:
-}

type Lem a = Or (Not a) a

{-
Now the law of the excluded middle is *not* a theorem of intuitionistic propositional logic. The type is:

  prfLem :: forall a. Lem a

in other words, a term that for every type unifies with either a or a -> ⊥. It should be clear that no
such term exists. But the negation of the law is also not a theorem:

  prfNotLem :: (forall a. Lem a) -> ⊥

However, there is an intuitionistic version of the law of the excluded middle:

  Given that a is a proposition, we know that ~~(a || ~a).

Here's the proof:
-}

prfIntuitionisticLem :: forall a. Not (Not (Lem a))
prfIntuitionisticLem prfNotLem =
  prfNotLem (prfOrIntroductionLeft prfNotP)
  where prfNotP = \p -> prfNotLem (prfOrIntroductionRight p)

{-
It's also a theorem of intuitionistic logic that the law of the excluded middle is equivalent to the law of double negation elimination.

We give proofs below:
-}

prfLemImpliesDne :: (forall a. Lem a) -> (forall b. Not (Not b) -> b)
prfLemImpliesDne prfLem =
  \prfDoubleNeg -> 
    case prfLem of
      A prfNotX -> prfBottomElimination (prfDoubleNeg prfNotX)
      OrA prfX -> prfX

prfDneImpliesLem :: (forall a. Not (Not a) -> a) -> (forall b. Lem b)
prfDneImpliesLem prfDne = prfDne prfIntuitionisticLem