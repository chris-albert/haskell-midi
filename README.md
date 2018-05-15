# haskell-midi

```
$ stack test --file-watch

$ stack exec haskell-midi -- drums.mid
```


76543210765432107654321076543210
c0000000c0000000c0000000c0000000
cccc0000000000000000000000000000

remove bit 7,15,23,31
pad with 0's
re-encode
