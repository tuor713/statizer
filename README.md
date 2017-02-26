# Statizer

A status tracking, alerting and data quality monitoring service. This is very raw and may not 
shape out too much.

## Usage

At the moment all interaction with the system is via 
the REPL, so `lein repl` (or alternative way to start a REPL)

```clojure
(use 'status.core)
(run-dev)
(bootstrap)
```

Open http://localhost:8080.

## License

Copyright © 2017 Valentin Mahrwald
