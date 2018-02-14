(ns yoanna
  (:require [clojure.string :as str]
            [clj-http.client :as client]
            [clojure.data.codec.base64 :as base64]))

(declare who)

(defn decode
  [s]
  (String. (base64/decode (.getBytes s)) "UTF-8"))

(defn get-config!
  "Read dev.secret.edn to get secret api keys
   or any other sensitive config.
   Returns map."
  [filename]
  (read-string (decode (slurp filename))))

(def config (get-config! "prod.secret.edn"))

(def reasons (:reasons config))

(def emojis
  {:cookie 0x1f36a
   :donut 0x1f369
   :burger 0x1f354
   :good 0x1f604
   :pizza 0x1f355
   :ok 0x1f612
   :bad 0x1f62d
   :sleepy 0x1f62a
   :cat 0x1f63a
   :sleepy-cat 0x1f431
   :angry-cat 0x1f63e
   :sleep 0x1f634
   :coffee 0x2615})

(def moods
  {:good  ["good"
           "great"
           "not bad"
           "pretty good"
           "decent"
           "solid"]
   :ok    ["fine"
            "ok"
            "whatever"
            "meh"
            "eh"
            "i dont care"
            "idc"
            "idk"]
   :bad    ["im dead"
            "bad"
             "terrible"
             "fuck you"
             "i hate you"
             "you suck"
             "shut up"
             "mad"
             "angry"
             "upset"
             "grrr"]
   :sleepy ["tired"
            "im tired"
            "sleepy"
            "exhausted"
            "yawn"
            "zzz"
            "so sleepy"]})

(defn prompt
  [prompt-text]
  (println prompt-text)
  (print " ? ")
  (flush)
  (read-line))

(defn emoji
  "Renders a keyword into a unicode emoji char"
  [name]
  (String. (Character/toChars (name emojis))))

(defn chain
  "Runs map and reduce concat to return a single collection"
  [map-fn coll]
  (reduce concat [] (map map-fn coll)))

(defn str->key
  "Converts a string to a lowercase, trimmed keyword"
  [s]
  (reduce #(%2 %1) s [str/lower-case str/trim]))

(defn clean
  [str]
  (str->key (str/replace str #"[^\s\w]" "")))

(defn str->yn
  "Converts a yes-ish & no-ish response to yn"
  [answer]
  (first (str->key (clean answer))))

(defn str->int
  "Converts a string to a number"
  [answer]
  (Integer/parseInt answer 10))

(defn in?
  "Returns true if value is found within collection"
  [coll value]
  (boolean (some #{value} coll)))

(defn is-name?
  "Return true if input string appears to be a name"
  [input]
  (let [name (str->key input)]
    (and (>= (count name) 3)
         (boolean (re-find #"^[\s\w]+$" name)))))

(defn create-name-pred
  "Returns a function that returns true when input includes a given name"
  [name]
  (let [expected-name (str->key name)]
    (fn [actual-name]
      (str/includes? (str->key actual-name) expected-name))))

(def is-yoanna? (create-name-pred "yoanna"))
(def is-jay? (create-name-pred "jay"))

(defn is-mood?
  [mood]
  (in? (chain second moods) (str->key mood)))

(defn is-yoanna-good?
  [mood-type]
  (= mood-type :good))

(defn is-yoanna-ok?
  [mood-type]
  (= mood-type :ok))

(defn is-yoanna-bad?
  [mood-type]
  (= mood-type :bad))

(defn is-yoanna-sleepy?
  [mood-type]
  (= mood-type :sleepy))

(defn is-yes?
  [answer]
  (str/starts-with? (str/lower-case answer) "y"))

(defn is-no?
  [answer]
  (str/starts-with? (str/lower-case answer) "n"))

(defn is-yesno?
  [answer]
  (or (is-yes? answer) (is-no? answer) true))

(defn is-number?
  [answer]
  (boolean (re-find #"^[\d]+$" answer)))

(defn is-within?
  [n min max]
  (and (<= min n) (<= n max)))

(defn prompt-until
  "Takes a prompt-fn & a predicate and keeps prompting until predicate passes"
  [state prompt-fn retry-fn is-complete?]
  (loop [answer (prompt (prompt-fn state))]
    (cond (empty? answer) nil
          (is-complete? answer) answer
          :else (do (print (str (retry-fn state) " "))
                    (recur (prompt (prompt-fn state)))))))

(defn branch
  [selector branches state]
  (let [value (selector state)]
    (loop [[pred-fn branch-fn] (first branches)
           remaining (rest branches)]
      (if (or (= pred-fn :else) (pred-fn value))
          (branch-fn state)
          (recur (first remaining) (rest remaining))))))

(defn cookie-prompt
  [state]
  (prompt-until state
                (fn [state] (format "How many %s would you like?" (emoji :cookie)))
                (fn [state] (format "Try entering a number greater than zero."))
                is-number?))

(defn display-cookies
  [n]
  (doseq [cookie (take n (repeat (emoji :cookie)))]
    (print (str cookie " ")))
  (println (format "\n... yum just for you my cookie cat %s!" (emoji :cat))))

(defn reason-prompt
  [state]
  (let [total-reasons (count reasons)]
    (prompt-until state
                  (fn [state] (format "\nOk! Now pick a reason 1-%d!"
                                      total-reasons))
                  (fn [state] (format "I... don't know?\n\n"))
                  #(or (nil? %)
                       (and (is-number? %)
                            (is-within? (str->int %) 1 total-reasons))))))

(defn exit
  ([]
   (println "Bye"))
  ([state] (System/exit 0)))

(defn get-reason
  [state]
  (loop [reason-index (reason-prompt state)]
    (if (nil? reason-index)
      (exit)
      (do
        (println (get reasons (dec (str->int reason-index))))
        (recur (reason-prompt state))))))

(defn good-yoanna
  [state]
  (println (format "Whoo glad you're feeling %s %s %s!"
                   (:mood state)
                   (emoji (:mood-type state))
                   (:name state)))
  (let [cookies (cookie-prompt state)]
    (if (nil? cookies) (exit) (do
                                (display-cookies (str->int cookies))
                                (get-reason state)))))

(defn pizza-prompt
  [state]
  (prompt-until state
                (fn [state] (format
                              "Would burying you in pizza %s help make it better? (Y/n)"
                              (emoji :pizza)))
                (fn [state] (format "Sorry, this was a yes or no question."))
                is-yesno?))

(defn pizza-pyramid
  [state]
  (let [pizza (emoji :pizza) kitty (emoji :cat)]
    (println (format "        %s" pizza))
    (println (format "      %s  %s" pizza pizza))
    (println (format "    %s  %s  %s" pizza kitty pizza))
    (println (format "  %s  %s  %s  %s" pizza pizza pizza pizza))
    (println (format "%s  %s  %s  %s  %s" pizza pizza pizza pizza pizza))))

(defn ok-yoanna
  [state]
  (println (format "Ah, you're %s. I guess that's Ok... %s"
                   (:mood state)
                   (emoji (:mood-type state))))
  (let [wants-pizza (pizza-prompt state)]
    (cond (nil? wants-pizza) (exit)
          (is-yes? wants-pizza) (pizza-pyramid state))
    (get-reason state)))

(defn gif-prompt
  [state]
  (str/lower-case
    (prompt-until state
                  (fn [state] (format
                                "Would you prefer a husky or kitty gif? %s"
                                (emoji :cat)))
                  (fn [state] (format "Wait. What?"))
                  #(let [answer (str/lower-case %)]
                     (contains? #{"kitty" "husky"} answer)))))

(defn create-giphy-url
  "Build giphy request url.
   Accepts a config map.
   Returns a url string."
  [{:keys [api_key]} tag]
  (format "http://api.giphy.com/v1/gifs/random?tag=%s&api_key=%s&rating=r"
    (str/replace tag #"kitty" "cat")
    api_key))

(defn get-gif
  [tag]
  "Prints a random kitty giphy url"
  (let [giphy-url (create-giphy-url config tag)
        response (client/get giphy-url {:as :json})]
    (println
     (get-in response [:body :data :image_original_url]))))

(defn bad-yoanna
  [state]
  (println (format "\"%s\" eh? Sorry you're feeling bad. %s"
                   (:mood state)
                   (emoji :angry-cat)))
  (let [tag (gif-prompt state)]
    (get-gif tag)
    (get-reason state)))

(defn wait-prompt
  [state]
  (str->int (prompt-until state
                (fn [state]
                  (format "How many minutes should I wait?"))
                (fn [state]
                  (format "Try entering a positive number."))
                #(and (is-number? %) (> (str->int %) -1)))))

(defn any-prompt
  [state]
  (prompt-until state
                (fn [state] (format
                              "Press <enter> when you're ready."
                              (emoji :cat)))
                (fn [state] (format "Uhh are you sure?"))
                (constantly true)))

(defn sleepy-yoanna
  [state]
  (println (format "*Yawn* Aww, sleepy cat! %s%s"
            (emoji :sleepy)
            (emoji :sleepy-cat)))
  (println (format "Go get a %s or take a %s and come back."
            (emoji :coffee)
            (emoji :sleep)))
  (let [minutes (wait-prompt state)]
    (if (> minutes 0)
      (do
        (println (format "Ok! Waiting %d minutes..." minutes))
        (Thread/sleep
          (->> minutes
               (* 60)
               (* 1000)))
        (println "I hope you enjoyed your break. Ready to continue?")
        (any-prompt state))
      (do
        (println "Cool, let's get to it.")))
    (get-reason state)))

(assoc {}
       :mood "ok"
       :name  "Yoanna")

(defn get-mood-type
  [mood-key]
  (some (fn [[mood-type mood-list]]
          (and (some #{mood-key} mood-list) mood-type))
        moods))

(defn set-mood
  [state mood]
  (assoc state
         :mood mood
         :mood-type (get-mood-type (str->key mood))))

(defn prompt-yoanna
  [state]
  (prompt-until state
                (fn [state] (format "Hey %s how are you?" (:name state)))
                (fn [state] (format "Huh?"))
                is-mood?))

(defn greet-yoanna
  [state]
  (branch :mood-type [[is-yoanna-good? good-yoanna]
                      [is-yoanna-ok? ok-yoanna]
                      [is-yoanna-bad? bad-yoanna]
                      [is-yoanna-sleepy? sleepy-yoanna]
                      [:else ok-yoanna]]
          (set-mood state (prompt-yoanna state))))

(defn greet-jay
  [state]
  (println (format "You suck! Go away %s!! %s" (:name state) (emoji :angry-cat))))

(defn greet-person
  [state]
  (println (format "Whatever %s!" (:name state)))
  (exit))

(defn prompt-name
  [state]
  (prompt-until
         state
         (fn [state] "What is your name?")
         (fn [state] "Hmm... I didn't catch that.")
         is-name?))

(defn who
  [state]
  (branch :name [[nil? exit]
                 [is-yoanna? greet-yoanna]
                 [is-jay? greet-jay]
                 [:else greet-person]]
                (assoc state :name (prompt-name state))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (who {}))
