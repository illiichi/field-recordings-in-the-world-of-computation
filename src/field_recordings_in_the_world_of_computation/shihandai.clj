(ns field-recordings-in-the-world-of-computation.shihandai
  (require [llll.core :as l4]
           [llll.clay.clay :as cl]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [field-recordings-in-the-world-of-computation.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(l4/initialize {})

(defn update-state [{:keys [count] :as m}]
  (let [c (inc count)
        pairs (fn [prefix f]
                (map (fn [x] { (keyword (str prefix x)) (f x) })
                     (range 4)))]
    (-> m
        (assoc :count c)
     (into (pairs "b" #(bit-test c %)))
     (into (pairs "c" #(->> %
                            (bit-shift-right c)
                            (bit-and 2r0011)))))))

(def option {:state {:initial (update-state {:count 0})
                     :update update-state}
             :swap-option {:switch-fade-in 0
                           :switch-fade-out 0
                           :fade-out-dur 32}
             :synth-option {:type :detune}
             :period 64})

(do
  (l4/control :bang :vol {:dur 256 :to 0.8})

  (defsynth-l4 bang
    [freq 30 long 2]
    {:type :detune}
    (* (bpf (gray-noise) (* dr freq) 1)
       (env-gen (env-perc 0.01 dur) :action FREE)))
  (def range-freq (partial  u/map-square 100 8000))

  (defpattern bang
    {:table {:a (synth bang)}
     :state {:initial {:n 4}
             :update (fn [m]
                       (update m :n #(if (< % 18) (inc %) 4)))}
     :period 64}
    (cl/->Clay
     (for [x (range 1 n)
           y (range x)]
       (cl/->Child (/ y x)
                   (+ (/ y x) (/ 1 x))
                   :a
                   {:freq (range-freq (/ (+ x y) 16))
                    :long 1
                    :vol 1/2})))))

(l4/control :waza :vol {:set 0.5})

(defsound waza
  option
  [(section
    {:dur 1}
    (-> (u/m-map #(let [gate (lf-pulse (* :f2 %) 1/2 (!! (if b0 1/12 3/4)))
                        freq (latch:ar (u/rg-exp (sin-osc 0.3) 100 1000) gate)]
                    (-> (sin-osc (* dr freq %2))
                        (u/reduce-> (fn [acc x] (sin-osc (* freq acc x))) [1 1/4 1/2])
                        (* gate)))
                 [1/3 1/4 1/7 2/3]
                 [10 30 32 15])
        (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000))
        (* 8)
        distort))
   (section
    {:dur 1}
    (-> (let [f-env (env-gen (envelope [(!! (if b0 2 1/2))
                                        (!! (if b1 2 1/2))
                                        (!! (if b2 2 1/2))]
                                       [(* 1/4 dur) (* 3/4 dur)]))
              freq 1200]
          (u/m-map #(u/switch (line 1 0 dur)
                              (rlpf (white-noise) (* f-env dr %) 0.01)
                              (sin-osc (* f-env dr %)))
                   [freq (* freq 3/2) (* freq (!! (if b0 4 1/4)))]))
        (u/reduce-> (fn [acc x] (comb-l acc x x 0.2)) [0.01 0.03 0.008])
        (u/switch-> (!! b2) (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000)))
        (u/switch-> (!! b1) (moog-ff (u/rg-exp (u/m-map lf-pulse [1.3 0.2 0.5]) 1000 3000)))
        (* 8)
        distort))])

(defsound waza
  option
  [(section
    {:dur 1}
    (-> (u/m-map #(let [gate (lf-pulse (* :f2 %) 1/2 (!! (if b0 1/12 3/4)))
                        freq (latch:ar (u/rg-exp (sin-osc 0.3) 100 1000) gate)]
                    (-> (sin-osc (* dr freq %2))
                        (u/reduce-> (fn [acc x] (sin-osc (* freq acc x))) [1 1/4 1/2])
                        (* gate)))
                 [1/3 1/4 1/7 2/3]
                 [10 30 32 15])
        (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000))
        (* 8)
        distort))
   (section {:dur 7}
            (let [snd (rlpf (white-noise) (* dr [1200 1800]) 0.0001)
                  freq (u/rg-exp (u/m-map lf-pulse [(!! (if b0 10.3 2.8))
                                                    (!! (if b1 0.8 0.6))]) 80 1800)]
              (-> snd
                  (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1/2 1 2])
                  (free-verb (!! (if b1 1 0.3)) (!! (if b2 1 0.5)))
                  (* 8) distort)))])

(defsound waza
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 32}
  [(section
    {:dur 0}
    (let [snd (u/m-map #(sin-osc (* dr (u/rg-lin (lf-pulse % (rand)) 200 %2)))
                       [8 12 4 6]
                       [1800 5600 400 1600])
          freq (u/rg-exp (u/m-map sin-osc [20.2 (!! (if b3 2.02 330)) 40.08]) (!! (if b1 400 1000)) 8000)]
      (-> snd
          (u/reduce-> (fn [acc [x delay]] (ringz acc x delay))
                      [800 1200 1600]
                      [0.08 0.12 0.016])
          (u/switch-> (line 0 1 dur)
                      (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.02]))
          (u/reduce-> (fn [acc x] (sin-osc (* acc x))) [300 (* 1/4 freq) freq])
          (* 8) distort)))
   (section {:dur 1}
    (let [f-env (env-gen (envelope [0 6 1] [(!! (if b0 0.05 0.4)) (!! (if b2 0.1 0.5))]))
          freq (* f-env (u/rg-exp (lf-saw (!! (if b2 -4 16))) 800 (!! (if b0 1600 520))))
          snd (u/switch (lf-pulse 8 0 1/4) (sin-osc (* dr freq))
                        (bpf (gray-noise) (* dr freq)))]
      (-> snd
          (u/switch-> (line 1 0 dur)
                      (u/reduce-> (fn [acc x] (sin-osc (* acc x freq)))
                                  [1 1/4 (!! (if b0 1/2 1/4)) 3]))
          (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.02])
          (* 8) distort)))
   ])

(defsound waza
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 512}
  [(section {:dur 0}
            (let [f-env (env-gen (envelope [0 6 1] [(!! (if b0 0.05 0.4)) (!! (if b2 0.1 0.5))]))
                  freq (* f-env (u/rg-exp (lf-saw (!! (if b2 -4 16))) 800 (!! (if b0 1600 520))))
                  snd (u/switch (lf-pulse 8 0 1/4) (sin-osc (* dr freq))
                                (bpf (gray-noise) (* dr freq)))]
              (-> snd
                  (u/switch-> (line 1 0 dur)
                              (u/reduce-> (fn [acc x] (sin-osc (* acc x freq)))
                                          [1 1/4 (!! (if b0 1/2 1/4)) 3]))
                  (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.02])
                  (* 8) distort)))
   (section {:dur 7}
            (let [freq (u/rg-exp (u/m-map lf-saw [2.3 3.8 2.8])
                                 (!! (if b1 100 800))
                                 (!! (if b0 4880 2800)))
                  snd (mix (sin-osc (* dr [1200 3200 640])))]
              (-> snd
                  (u/switch-> (!! (= c0 3))
                              (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1 3 1/2 4]))
                  (u/switch-> (line 1 0 dur)
                              (u/reduce-> (fn [acc x] (ringz acc x dur)) [(* 2 freq) freq]))
                  (free-verb 1 10)
                  (* 8) distort)))])

(l4/finish)
(kill-server)
