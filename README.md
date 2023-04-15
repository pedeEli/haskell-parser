# Haskell Parser: A deep dive into Parsec

This repo consists of multiple steps each adding another feature from Parsec.
Each step adds the following thing
- [step-1](/step-1): basic structure
- [step-2](/step-2): error handling with different error messages
- [step-3](/step-3): current possition in the input
- [step-4](/step-4): more abstraction by adding a Stream class
- [step-5](/step-5): a bunch of combinaters from Parsec

Everything is well documented and theres even [a example of how to build a json parser](/json-example).

## Structure of Haskell modules

Each step is its own isolated cabal library and always contain these modules
- `Test`: functions for easier testing of the implemented features
- `Text.Parsel`: just exports every sub module
- `Text.Parsel.Primitve`: contains the basic definitions and functions

Each step may add new modules or edit existing ones, but never delete one. If a module is modified it is documented in the README of the step.