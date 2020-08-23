;; Magnus - a Clojure chess library
;; Copyright (C) 2020  Anders Eriksson

;; This file is part of Magnus.

;; Magnus is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Magnus is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with Magnus.  If not, see <https://www.gnu.org/licenses/>.

(defproject magnus "0.1.0"
  :description "A chess library"
  :url "https://github.com/Anders-E/Chess"
  :license {:name "LGPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/lgpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :plugins [[lein-codox "0.10.7"]]
  :repl-options {:init-ns magnus.core})
