Calculates the [derivatives of regular expressions](https://en.wikipedia.org/wiki/Brzozowski_derivative).

```
% cargo run --example main
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.09s
     Running `target/debug/examples/main`
r: (abc)*
d(r, 'a'): bc(abc)*
d(r, 'b'): ∅
d(d(r, 'a'), 'b'): c(abc)*
```
