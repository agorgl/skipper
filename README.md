# skipper

Make deployments simple

## Usage

FIXME: explanation

Run the project directly:

    $ clojure -M:run
    Hello there

Run the project's tests:

    $ clojure -T:build test

Build an uberjar:

    $ clojure -T:build uber

Or run the project's CI pipeline and build an uberjar in a single invocation:

    $ clojure -T:build ci

Run that uberjar:

    $ java -jar target/net.clojars.skipper/skipper-0.1.0-SNAPSHOT.jar

## License

Copyright © 2024 Loukas Agorgianitis

Distributed under the Eclipse Public License version 1.0.
