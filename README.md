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

Base on tutorial https://llvm.org/docs/tutorial/MyFirstLanguageFrontend