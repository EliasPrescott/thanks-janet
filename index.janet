(defn attrbute-table->string [attributes]
  (string/join (map 
    (fn [pair] (string (get pair 0)
                        "=\""
                        (get pair 1)
                        "\""))
                (pairs attributes))
    " "))

(defn tag->string [tagName attributes content]
  (if (empty? attributes)
    (string/join ["<" tagName ">" content "</" tagName ">"])
    (string/join ["<" tagName " " (attrbute-table->string attributes) ">" content "</" tagName ">"])))

(defn html->string [x]
  (match x
    [tag attributes & rest] (if (keyword? tag)
                              (if (table? attributes)
                                # If the second item is a table, treat it like an attribute dictionary.
                                # We could do the same with lists of tuple pairs, but then we'd have to verify that the list only contains
                                # tuple pairs.
                                (tag->string tag attributes (string/join (map html->string rest)))
                                # I'm just pretending like quasiquoting the tuple back together like 👇 this is correct.
                                (tag->string tag [] (string/join (map html->string ~(,attributes ,;rest)))))
                              (string/join (map html->string ~(,tag ,attributes ,;rest))))
    # Map and then spread/flatten any list-tuples that made it this far.
    [& items] (string/join (map html->string items))
    # Convert everything else to a string and hope for the best.
    _ (string x)))

(defmacro html [& body]
  ~(string/join ["<html>" (string/join (map html->string [,;body])) "</html>"]))

(defn main [&]
  (print (html [:h1 @{:class "alternate-heading"}
                "testing " "a " "html macro"]
               [:main
                [:p @{:id "main-paragraph"}
                 "this is a test program"]
                [:p "this is the second paragraph"]
                ["lots of free text here... " "this is a lot of text... " "maybe too much!"]])))