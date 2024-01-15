(defproject sudoku-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [midje "1.10.9"]
                 [org.clojure/core.logic "1.0.1"]
                 [criterium "0.4.5"]]
  :repl-options {:init-ns sudoku-solver.core})
