(ns field-recordings-in-the-world-of-computation.coal_mine
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
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

(def option
  {:period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur (* 8 4)
                 :switch-dur (* 8 1)
                 :fade-out-dur (* 8 4)}})

(l4/control :tracker :vol {:dur 16 :to 0.5})

(defsound tracker
  {:swap-option {:switch-fade-in 0
                 :switch-fade-out (* 8 8)
                 :fade-out-dur (* 8 4)}
   :state {:initial (update-state {:count 0})
           :update update-state}
   :period 32}
  (splay
   (map (fn [count ratio phase]
          (let [gate2 (impulse (* :f4 count) )]
            (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                           [0.1 50 10 1 100])))
                 (* (u/sin-r (* :f8 ratio) 128 5120)
                    (u/dq gate2 [1 1/2 1/4])
                    (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                 (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t2 :t3 :t1])
                 (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.7 0.8 4.3])
                                  (* ratio 500) (* ratio 2000)) 0.001)
                 (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [0.002 0.01 0.02])
                 (tanh)
                 (* 2)
                 )))
        [4 2 3 9/2 8 1]
        [5/4 4 3/2 2 1 1/2]
        (u/n-range 0 1 6))))

(l4/control :drill :vol {:dur 16 :to 0.5})

(defsound drill
  {:period 16
   :synth-option {:type :detune}
   :state {:initial (update-state {:count 0})
           :update update-state}
   :swap-option {:fade-in-dur (* 8 1)
                 :switch-dur (* 8 4)
                 :fade-out-dur (* 8 4)}}
  (let [freq :-freq]
    (-> (sin-osc (* dr freq))
        (u/reduce-> (fn [acc [m1 m2]]
                      (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
                    [[1 2] [1/2 (!! (if b1 7 2))] [1/4 (!! (if b2 7 5))]])
        (u/switch-> (u/sin-r (/ 1 dur) 0 1)
                    (ringz (* (u/rg-exp (lf-saw (!! (if b2 0.28 0.037))) 1/4 2) freq) 0.002))
        (u/reduce-> (fn [acc [mix room]] (free-verb acc mix room))
                    [[(!! (if b2 0.1 1)) 0.1]
                     [1 0.8]])
        (* 8)
        distort)))

(l4/control :work :vol {:dur 16 :to 0.5})

(defsound work
  option
  (let [N 7
        T (u/rg-exp (u/m-map lf-pulse [0.3 0.54 0.8]) 0.00001 0.0025)
        M 0.016
        gate (impulse (/ 1 (+ (* N T) M)))
        env (env-gen (envelope [0 1 1e-4] [0 (* N T)] :exp) gate)
        snd (squared (sin-osc (/ dr T)))]
    (-> (* snd env)
        (free-verb 0.8 :-room))))


(l4/control :collapse :vol {:dur 16 :to 0.5})

(defsound collapse
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  (splay (-> (map (fn [freq1 freq2 i] (* (/ dr i) (rlpf (lf-noise1 freq1) freq2 0.01)))
                  (u/n-range 100 600 8)
                  (shuffle (u/n-range 20 100 8))
                  (iterate inc 1))
             (* 2)
             (u/switch-> (u/rg-lin (lf-tri (* :f1 (!! (if b0 32 4)))) 0 1)
                         (u/reduce-> (fn [acc x] (free-verb acc x 1)) [1 1 1]))
             tanh
             (* (env-gen (env-perc 0.05 2) (* :-gate
                                              (lf-pulse (!! (if (= (mod c2 4) 0) 1/2 1/8)) 0
                                                        (!! (if b3 1/8 1/4)))))))))

(l4/control :bass :vol {:dur 16 :to 0.5})

(defsound bass
  option
  (let [gate (u/throttle :-gate (u/rg-lin (lf-noise0:kr 1) 1 6))]
    (+ (let [f-env (env-gen (envelope [0 3 1] [0.001 0.01]) gate)
             freq 80]
         (-> (env-gen (env-perc 0.05 1) gate)
             (* (sin-osc (* freq dr f-env)))
             (free-verb 1 1)))
       (let [freq 58
             f-env (env-gen (envelope [0 2.5 1] [1e-5 0.04]) gate)]
         (tanh (* 1.2 (sin-osc (* dr freq f-env))
                  (env-gen (envelope [0 1 0.5 0] [0 0.25 0.01]) gate)))))))

(l4/control :bass2 :vol {:dur 16 :to 0.5})
(defsound bass2
  option
  (splay (map #(let [f-env (env-gen (envelope [0 4 1] [0.005 0.01]) (impulse 1 %3))]
                 (* 8 (sin-osc (* %2 f-env (u/rg-lin (lf-pulse % 0 0.1) 40 50)))))
              (u/n-range 1 2 8)
              (u/n-range 1 4 8)
              (shuffle (u/n-range 0 1 8)))))

(l4/control :bass3 :vol {:dur 16 :to 0.5})
(defsound bass3
  (merge option
         {:period 16
          :state {:initial (update-state {:count 0})
                  :update update-state}})
  (-> (* (!! (if (= (mod c1 4) 0) 4 2 ))
         (sin-osc 80) (lf-tri (+ (!! (if b0 -10 -20)) 50))
         (env-gen (env-perc :t8 (* (!! (if b2 4 1)) :t16)) (impulse (* 1/8 :f16))))
      (lpf (u/rg-lin (lf-saw (* 1/4 :f16) -1) 200 800))
      tanh))



(l4/finish)
(kill-server)
