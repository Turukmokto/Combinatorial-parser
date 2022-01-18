(defn evaluate [expr vars] (.evaluate expr vars))
(defn toString [expr] (.toStr expr))
(defn diff [expr v] (.diff expr v))

(definterface IOperation
  (evaluate [var-map])
  (toStr [])
  (diff [var]))

(declare ZERO)

(deftype Const [value]
  IOperation
  (evaluate [this _] (.value this))
  (toStr [this] (format "%.1f" (double (.value this))))
  (diff [_ _] ZERO))

(defn Constant [val] (Const. val))

(def ZERO (Constant 0))
(def ONE (Constant 1))
(def TWO (Constant 2))

(deftype Var [name]
  IOperation
  (evaluate [this var-map] (var-map (.name this)))
  (toStr [this] (.name this))
  (diff [this var] (if (= var (.name this)) ONE ZERO)))

(defn Variable [name] (Var. name))

(deftype OperationPrototype [op opStr howDiff args]
  IOperation
  (evaluate [this var-map] (apply (.op this) (map (fn [x] (evaluate x var-map)) (.args this))))
  (toStr [this] (str
                  "("
                  (.opStr this)
                  " "
                  (clojure.string/join " " (map toString (.args this)))
                  ")"))
  (diff [this v] ((.howDiff this) (vec (.args this)) v))
  )

(defn makeOp [op opStr howDiff]
  (fn [& args] (OperationPrototype. op opStr howDiff args)))

(def Add (makeOp
           +
           "+"
           (fn [args v] (apply Add (map #(diff % v) args)))
           ))

(def Subtract (makeOp
                -
                "-"
                (fn [args v] (apply Subtract (map #(diff % v) args)))
                ))

(declare Multiply)
(defn diffMultiply [args v]
  (map #(apply Multiply (assoc args % (diff (nth args %) v)))
       (range (count args))))

(def Multiply (makeOp
                *
                "*"
                (fn [args v] (apply Add (diffMultiply args v)))))

(defn multi-arity-fn [func] (fn [& args] (reduce func args)))
(def Divide (makeOp
              (multi-arity-fn #(/ (double %1) (double %2)))
              "/"
              (fn [args v]
                (Divide (apply Subtract (diffMultiply args v))
                        (apply Multiply (concat (rest args) (rest args)))))
              ))

(defn diffUnary [howDiff]
  (fn [arg v] (Multiply (howDiff (first arg)) (diff (first arg) v))))

(def Negate (makeOp
              #(- %)
              "negate"
              (diffUnary (constantly (Constant -1)))
              ))

(def Abs (makeOp
           #(Math/abs %)
           "abs"
           (diffUnary #(Divide % (Abs %)))
           ))

(declare Sinh)

(def Cosh (makeOp
              #(Math/cosh %)
              "cosh"
              (diffUnary #(Sinh %))
              ))

(def Sinh (makeOp
            #(Math/sinh %)
            "sinh"
            (diffUnary #(Cosh %))
            ))

(def operations
  {
   '+      Add,
   '-      Subtract,
   '*      Multiply,
   '/      Divide,
   'negate Negate,
   'sinh   Sinh,
   'cosh   Cosh})

(def var-map
  {
   'x (Variable "x"),
   'y (Variable "y"),
   'z (Variable "z")})

(defn parse [expr]
  (cond
    (seq? expr) (apply (operations (first expr)) (map parse (rest expr)))
    (number? expr) (Constant expr)
    (symbol? expr) (var-map expr)))

(def parseObject (comp parse read-string))