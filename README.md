# prolog-cfg
The program implements a toy [context-free grammar (CFG)](https://en.wikipedia.org/wiki/Context-free_grammar) parser, that extracts knowledge from text.
The parser understands sentences that follow proper grammar rules of the English language. Also the vocabulary used in the text must be defined in advanced in the knowledge base.

Resources: [1], [2]

# Examples:
You can add facts to the knowledge base:
```
1 ?- say('vaggelis runs fast').
The fact runs(vaggelis,fast) was added to the KB.
true .

2 ?- say('chris gave a program to themis').
The fact gave(chris,program,themis) was added to the KB.
true .
```

[1]: Striegnitz, Kristina, et al. "Algorithms for Computational Linguistics." (2003).

[2]: Blackburn, Patrick, Johannes Bos, and Kristina Striegnitz. Learn prolog now!. Vol. 7. No. 7. London: College Publications, 2006.
