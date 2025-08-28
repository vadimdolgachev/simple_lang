# simple_lang

## Simple program
```
    fn main() {
        fn main() {
            greeting: Greeting = Greeting {hello: "Hello", world: "World"};
            println("%s, %s!", greeting.hello, greeting.world);
        }

        struct Greeting {
            hello: str;
            world: str;
        }
    }
```

Compilation:

```./simple_lang ./text_program | llc -relocation-model=pic | clang -x assembler - -o a.out```

Or

```./simple_lang ./text_program | llc -relocation-model=pic | gcc -x assembler - -o a.out```

Execution:

```
./a.out
```

Base on tutorial https://llvm.org/docs/tutorial/MyFirstLanguageFrontend