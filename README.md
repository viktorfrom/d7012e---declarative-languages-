# D7012E - Declarative languages
###### 
Project written in in Haskell and Prolog for the Declarative languages course at Luleå University of Technology

## Requirements
* Stack 2.2.0+
* Haskell (ghc) 8.8.3+ 
* swi-prolog 8.0.3-2

# Haskell 
## Build and usage
upgrade/check latest version of stack
```
stack upgrade
```

create new project 
```
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
```

run project 
```
stack build && stack exec my-project-exe
stack ghci
```

pre-defined testing for lab3 
```
stack ghci (:q to quit)
```

# Prolog
## Build and usage
Start Prolog
```
swpl
```

Load program
```
[my-project.pl].
```

## Authors
* Viktor From - vikfro-@student.ltu.se - [viktorfrom](https://github.com/viktorfrom)

## License
Licensed under the MIT license. See [LICENSE](LICENSE) for details.
