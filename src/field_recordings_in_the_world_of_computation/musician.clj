(ns field-recordings-in-the-world-of-computation.musician
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
           [field-recordings-in-the-world-of-computation.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(l4/initialize {})


(defn update-state [{:keys [count]}]
  (let [c (inc count)
        pairs (fn [prefix f]
                (map (fn [x] { (keyword (str prefix x)) (f x) })
                     (range 4)))]
    (-> {:count c}
        (into (pairs "b" #(bit-test c %)))
        (into (pairs "c" #(->> %
                               (bit-shift-right c)
                               (bit-and 2r0011)))))))

(def option {:swap-option {:switch-dur (* 8 8)}
             :state {:initial (update-state {:count 0})
                     :update update-state}})
(def option-d (merge option {:synth-option {:type :detune}}))

(defn mm-mix [xss]
  (->> (u/transpose xss)
       (map mix)))

(defn mm-map [f & xss]
  (mm-mix (apply map f xss)))

(defcgen pan-lin [pos {:default 0}
                in {:default 0}]
  (:ar (* (cubed (clip:ar [(select (< pos -1/2) [(+ 7/8 (* -1 pos)) (+ 2 (*  2 pos))])
                           (select (> pos  1/2) [(+ 7/8 pos) (+ 2 (* -2 pos))])]
                          0 1)) in)))

(defcgen pan-exp [pos {:default 0}
                in {:default 0}]
  (:ar (let [x (abs pos)
             a (exp (* -14 x))
             b (exp (* -8 x))]
         (* in [(select (not-pos? pos) [a b])
                (select (not-pos? pos) [b a])]))))

(defcgen pan-gau [pos {:default 0}
                  in {:default 0}]
  (:ar (let [x (squared pos)
             a (exp (* -14 x))
             b (exp (* -8 x))]
         (* in [(select (not-pos? pos) [a b])
                (select (not-pos? pos) [b a])]))))

(defsynth-l4 string
  [freq 440 long 1]
  (let [shake 0.0008
        f-env (env-gen (envelope [0 4 1] [0.001 0.18]))
        dur (* long dur)]
    (+ (pan2 (* (sin-osc freq)
                (env-gen (env-perc 0.08 dur) :action FREE)))
       (-> (repeatedly 5 #(saw (* freq (u/rg-exp (pink-noise) (- 1 shake) (+ 1 shake)))))
           (lpf (* freq f-env))
           (* (env-gen (env-perc 0.01 dur)))))))

(defpattern musician
  {:table {:a (synth string)}
   :swap-option {:fade-out-dur 64}
   :period 64}
  (&| (map #(->> (| :a :a|5)
                 (=| :long [2 1.1])
                 (=| :freq (map midi->hz %1)))
           [[77 79] [77 72] ])))

(l4/control :musician :vol {:dur 16 :to 0.3})
(l4/control :forest :vol {:dur 128 :to 1})


(defsound forest
  {:swap-option {:switch-dur (* 8 256)}
   :state {:initial (update-state {:count 0})
           :update update-state}}
  (+ (-> (splay (map #(let [gate (dust 0.8)
                            freq (+ (midicps (round (latch:ar (u/sin-r 1.08 %2 (* 3 %2)) gate) 1))
                                    (u/sin-r 3.3 0.9 1))]
                        (-> (ringz (* (white-noise) gate) freq 0.1)
                            (free-verb 1 1)
                            (free-verb 1 1)))
                     (u/n-range 0 1 8)
                     (shuffle (u/n-range 50 80 8))))
         (* 128)
         (distort)
         (lpf (u/sin-r 0.2 300 1500)))
     (* (u/sin-r 0.08 0.25 1) (sin-osc 240)
        (sin-osc (* 80 (sin-osc 43))))))


(l4/control :flow :vol {:dur 16 :to 1})

(defsound flow option
  (mm-map #(let [freq (latch:ar (u/sin-r 0.3 440 2200) (impulse %))]
             (pan-gau (u/sin-r % -2 2) (sin-osc (* freq %2))))
          [0.05 0.07 0.19 0.09]
          [1 3/2 5/3 7/4 9/5]))

(defsound strong-flow option
  (let [pos (sin-osc 0.05)
        car-freq (u/rg-exp (lf-noise0 (u/rg-exp (sin-osc 0.1) 0.001 16)) 50 800)
        freq (u/rg-exp (sin-osc 0.1) 8200 8210)]
    (-> (pan-gau pos (sin-osc freq (u/rg-lin (sin-osc car-freq) 0 (* 2 Math/PI))))
        (free-verb 0.5 0.1))))

(l4/finish)
(kill-server)
