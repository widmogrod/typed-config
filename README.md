# typed-config
## Introduction

Experiment with making configuration type safe, self domummenting, easy to extend and dead-simple to use.

## Work in progress

Predefined Types
```
Str
Int
Num
Bool
Binnary
List, Map, Set
```

Special types
```
URL
Path
Port
Password
SHA256
...
```


Defining new types in `config.tcfg`
```
type market = [a-z]{2}
type _name = str
type _number = int where int > 0
```

Using new types in `config.yml`
```yaml
maximum_comments_number: 10
comments_enabled: Bool  """ default true[?]
comments_disabled: Bool """ default false[?]
```

Hinting types in key name:
```
comments_limit_int: 10
```

Validating configuration
- finds invalid types
- missing elements

```
// config.yml
tenant_options:
 - comments_limit_int: 10
   comments_enabled: true

 - comments_limit_int: -1.1
   comments_enabled: fals

 - comments_enabled: true

// cmd
typedconf validate config.yml
typedconf validate --format=yaml --config=config.yml --types=config.tcfg

// Output of invalid file looks like this

Problems:

        5   - comments_limit_int: -1.1
                                  ^^^^^

    Element "comment_limit_int" in list "tenant_options" at possition "1" has invalid value "-1.1", expected type "int" but "float" given


        6     comments_enabled: fals
                                ^^^^

    Element "comment_enabled" in list "tenant_options" at possition "1" has invalid value "fals", expected type "bool" but "string" given

Suggestions:


        8 - comments_enabled: true
            comment_limit_int: 0
            ^^^^^^^^^^^^^^^^ missing element?



    Element "comment_limit_int" in list "tenant_options" at possition "2"  is missing. Not symetric list element

```

Restructuring configuration
- When declared configuration is not structurised
```
typedconf fix
```

Comparing configurations
- When you wan to compare configuration A & B and see whenever types are maching
```
typedconf compare --a=new_config.yml --to=config.yml
```

Other options
```
typedconf help

  - init        - Create configuration file in current directory
  - validate    - Validate configuration file

        --format
        --config
        --types
        --output-hints-as =

  - types       - Extract types information from configuration and output information as json, xml, ...
  - generate    - Generate code from typed configuration to specific language....
  - compare     - Compare two configuration in therms of types
  - fix         - Apply suggested fixes to make configuration consistent
  - server      - For autocomplete suggestions, integration with VIM, and other code editors, IDEs
```

Grammar
```
expression = assigment
assigment = key ":" value

name = "a-z0-1"
key = name "_" type | name

value = value_inline | value_multiline
value_inline = * NOT("\n")
value_multiline = value_inline "\n"
value_list = "-" value
value_map  = assigment
```


```
site_name_str: "something"
site_name: "something"

comments_limit_int: 10
comments_number: 100
```
