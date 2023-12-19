(ns sudoku-solver.core
  (:require [clojure.string :as str]))


(def size 9)

(defn find-zero-indexes [board]
  (for [row (range 9)
        col (range 9)
        :when (= 0 (get-in board [row col]))]
    [row col]))

(defn get-square [p x y]
  (let [square-x (* 3 (quot x 3))
        square-y (* 3 (quot y 3))
        row1 (subvec (nth p square-y) square-x (+ 3 square-x))
        row2 (subvec (nth p (+ 1 square-y)) square-x (+ 3 square-x))
        row3 (subvec (nth p (+ 2 square-y)) square-x (+ 3 square-x))]
    (concat row1 row2 row3)))

(defn missing-numbers [numbers]
  (-> (set (range 1 10))
      (clojure.set/difference (set numbers))
      vec))

(defn solve-helper [indexes i new-board]
  (if (= i (count indexes)
         )
    (print-sudoku new-board)
    (let [current-index (nth indexes i)
          [row col] current-index
          possible-numbers (missing-numbers
                             (concat (get new-board row)
                                     (map #(nth % col) new-board)
                                     (get-square new-board col row)))]
      (println "row :" row "col :" col "Possible numbers:" possible-numbers )
      (if (seq possible-numbers)
        (recur indexes (inc i) (assoc-in new-board [row col] (rand-nth possible-numbers)))
        (recur indexes (inc i) new-board)))))

(defn solve [board]
  (loop [indexes (find-zero-indexes board)
         i 0
         new-board board]
    (solve-helper indexes i new-board)))
;; Example usage:
(def example-board
  [[1 2 3 4 5 6 7 8 0]
   [0 5 6 7 8 9 1 2 3]
   [7 8 0 0 2 3 4 5 6]
   [2 3 1 5 0 4 8 0 7]
   [5 6 4 8 9 7 2 3 1]
   [8 9 7 2 3 1 5 0 4]
   [3 1 2 6 0 5 0 7 8]
   [6 4 0 9 7 8 3 0 2]
   [0 7 8 3 1 2 0 0 5]])

(defn count-filled-cells [board]
  (count (filter #(not= 0 %) (flatten board))))

(defn sudoku-difficulty [board]
  (let [filled-cells (count-filled-cells board)]
    (cond
      (<= filled-cells 20 ) "Hard"
      (<= filled-cells 35 ) "Medium"
      :else "Easy"
      )
    )
  )

(def example-board-1
  [[1 2 0 4 5 6 7 8 0]
   [0 5 0 7 0 9 1 2 3]
   [7 8 0 0 0 3 4 5 6]
   [2 3 0 0 6 4 8 9 0]
   [5 6 0 0 9 7 2 3 0]
   [8 9 0 0 3 1 5 0 0]
   [3 1 0 0 0 5 9 7 0]
   [6 4 0 0 0 8 3 1 0]
   [0 7 8 3 0 2 6 4 5]])

(defn create-user [username password gender age ]
  {:username username
   :password password
   :gender gender
   :age age
   })

(defn -main [u1]
  (let [user u1]
    (println "\n===================================")
    (print (str "SUDOKU ++\t" (:username user) "!"))
    (loop [u user]
      (println "\n===================================")
      (println "1. Solve your sudoku\n2. Solve our sudoku \n3. Find out difficulty level \n4. Exit")
      (println "===================================")
      (print "Select an option: ")
      (flush)
      (let [choice (read-line)]
        (cond
          (= choice "1")
          (do (println "Insert 1. row: ")
              (flush)
              (let [user-board (read-sudoku)]
                (print user-board)
                (solve user-board))
              (recur u))

          (= choice "2")
          (do (println "Solve our sudoku: ")
              (flush)
              ; Implement the logic to solve your Sudoku board
              (println "Solving our Sudoku board...")
              (recur u))

          (= choice "3")
          (do
            (println "Find out difficulty level:")
            (flush)
            (let [user-board board
                  difficulty (sudoku-difficulty user-board)]
              (println "               SUDOKU")
              (print-sudoku user-board)
              (println "#####################################")
              (println (str "Difficulty Level: " difficulty)))
            (println "#####################################")
            (recur u))

          (= choice "4")
          (do (println "Goodbye!")
              u)

          :else
          (do (println "Invalid choice. Try again.")
              (recur u)))))))

(def board
  [[5 3 0 0 7 0 0 0 0]
   [6 0 0 1 9 5 0 0 0]
   [0 9 8 0 0 0 0 6 0]
   [8 0 0 0 6 0 0 0 3]
   [4 0 0 8 0 3 0 0 1]
   [7 0 0 0 2 0 0 0 6]
   [0 6 0 0 0 0 2 8 0]
   [0 0 0 4 1 9 0 0 5]
   [0 0 0 0 8 0 0 7 9]])

(defn login []
  (let [user nil]
    (println "\n===================================")
    (print "SUDOKU ++ login page!")
    (loop [u user]
      (println "\n===================================")
      (println "1. Create account\n2. Login\n3. Exit")
      (println "===================================")
      (print "Select an option: ")
      (flush)
      (let [choice (read-line)]
        (cond
          (= choice "1")
          (do (println "Enter username:")
              (flush)
              (let [username (read-line)]
                (println "Enter password:")
                (flush)
                (let [password (read-line)]
                  (println "Enter gender:")
                  (flush)
                  (let [gender (choose-gender)]
                    (println "Enter age:")
                    (flush)
                    (let [age (Integer/parseInt (read-line))]
                      (recur (create-user username password gender age))
                      )))
                ))

          (= choice "2")
          (do (println "Enter username:")
              (flush)
              (let [username (read-line)]
                (println "Enter password:")
                (flush)
                (let [password (read-line)]
                  (if (and (= username (:username u))
                           (= password (:password u)))
                    (recur (-main u))
                    (do (println "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                        (println "!!!Invalid username or password!!!")
                        (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                        (recur u))))
                ;;(println (add-food u food-name calories))
                ))


          (= choice "3")
          (println "Goodbye!")
          :else
          (do (println "Invalid choice. Try again.")
              (recur u))))))

  )

(defn choose-gender []
  (println "\n===================================")
  (println "1. Male\n2. Female\n")
  (println "===================================")
  (print "Select an option: ")
  (flush)
  (let [choice (read-line)]
    (cond
      (= choice "1")
      "Male"
      (= choice "2")
      "Female"
      :else
      (do (println "Invalid choice. Try again.")
          (recur))
      ))
  )
(login)

(defn remove-spaces [s]
  (str/replace s #"\s" ""))

(defn read-sudoku []
  (loop [i 0
         board (vector)]
    (if (< i 9)
      (let [row (read-line)]
        (if (< (count row) 8)
          (do
            (println "Every row must have 9 numbers!")
            (println "Insert "i". row:")
            (recur i board))
          (do
            (println "Insert "(inc 1)". row:")
            (recur (inc i)  (conj board (vec (map #(Integer/parseInt (str %))(remove-spaces row)))))
            )
          )
        )
      board))
  )

(defn divisible-by-three? [number]
  (zero? (mod number 3)))

(solve (read-sudoku))
(first board)
(defn print-sudoku [board]
  (doseq [[i row] (map vector (range) board)]
    (if (divisible-by-three? i)
      (println "  _________________________________"))
    (print "| ")
    (doseq [[j col] (map vector (range) row)]
      (print col " ")
      (when (zero? (mod (inc (.indexOf (str col) "\n")) 3))
        (print ""))
      (when (= (mod (inc j) 3) 0)
        (print "|  ")))
    (println))
  (println "  _________________________________"))
(print-sudoku board)

(solve example-board)