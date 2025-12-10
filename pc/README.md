# Parser Combinator

Notes/Code exploring writing a useful set of parser combinators in Rust.

## TODO:
- [ ] Improve error messages
- [x] (maybe) Instead of using bare callbacks, switch to a trait based system,
      which should also allow easier composition of the combinators.

## Examples
* `json.rs` -> A very basic, untested, JSON AST parser. Seems to work well on small examples.

## Resources used:

- [F# for Fun and Profit series on Parser
Combinators](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/)
as guidance.
- [Rust user forum question about combining
   closures](https://users.rust-lang.org/t/unable-to-reduce-over-collection-of-closures/136349/3)
- [Blog post about implementing parsing combinators in
   Rust](https://bodil.lol/parser-combinators/)
