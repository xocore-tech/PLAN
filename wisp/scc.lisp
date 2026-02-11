(require "wisp")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

; a graph is represented as an array of lists, where each list represents
; the set of out-going edges from a node.  For example, a trivial loop
; would be this graph:
;
;     [~[0]]

(defun (foldli-loop f row n i z)
  (@ z2 (f i z (Ix i row)))
  (If (Ge i n) z
    (Seq z2
      (foldli-loop f row n (Inc i) z2))))

(defun (foldli f z row)
  (foldli-loop f row (Sz row) 0 z))

(defun (zeroed row)
  (Rep 0 0 (Sz row)))

(defun (addEdge g i acc v)
  (@ rv (Ix v acc))
  (Seq rv
    (UpUniq v [i rv] acc)))

(defun (addNode g i acc vs)
  (lfoldl (addEdge g i) acc vs))

(defun (transpose g)
  (Force (foldli (addNode g) (zeroed g) g)))

(tests
  ([0 0 0] (zeroed [1 2 3]))

  (20 (foldli (law (0 i z x) (Add i (Add x z))) 5 [3 4 5]))

  ( [~[0]]               (transpose [~[0]]))
  ( [~[1] ~[0]]          (transpose [~[1] ~[0]]))
  ( [~[]  ~[1 0]]        (transpose [~[1] ~[1]]))
  ( [~[2] ~[2 0] ~[2 1]] (transpose [~[1] ~[2] ~[0 1 2]])))

; TODO: Use SetUniq
; TODO: SetUniq variant that does not change the size

(defun (dfsList dfsPost g viz list acc)
  (@ w    (Ix0 list))
  (@ ws   (Ix1 list))
  (@ out  (dfsPost g viz w acc))
  (@ viz2 (Ix0 out))
  (@ acc2 (Ix1 out))
  (Ifz list [viz acc]
    (Seq2 viz2 acc2
      (dfsList dfsPost g viz2 ws acc2))))

(defun (dfsPost g viz v acc)
  (@ out  (dfsList dfsPost g (Set v viz) (Ix v g) acc))
  (@ viz2 (Ix0 out))
  (@ acc2 (Ix1 out))
  (If (Test v viz) [viz acc]
    (Seq2 viz2 acc2
      [viz2 (CONS v acc2)])))

(defun (dfsCollect g viz v acc)
  (If (Test v viz) [viz acc]
    (dfsList dfsCollect g (Set v viz) (Ix v g) [v acc])))

(defun (dfsPostAllLoop g n i inp)
  (@ viz1 (Ix0 inp))
  (@ acc1 (Ix1 inp))
  (@ out  (Force (dfsPost g viz1 i acc1)))
  (If (Ge i n) inp
    (Seq out
      (dfsPostAllLoop g n (Inc i) out))))

(defun (dfsPostAll g)
  (@ n (Sz g))
  (Ix1 (dfsPostAllLoop g n 0 [(Bex n) NIL])))

(tests
  ([7  ~[0 1]]   (dfsPost [~[0 1] ~[0 1]]    4 0 0))  ; first node first
  ([7  ~[1 0]]   (dfsPost [~[0 1] ~[0 1]]    4 1 0))  ; second node first
  ([7  ~[]]      (dfsPost [~[0 1] ~[0 1]]    7 1 0))  ; already visitied
  ([5  ~[0]]     (dfsPost [~[0]   ~[1]]      4 0 0))  ; only first node
  ([6  ~[1]]     (dfsPost [~[0]   ~[1]]      4 1 0))  ; only seond node
  ([11 ~[0 1]]   (dfsPost [~[1]   ~[0] ~[2]] 8 0 0))  ; 1->2
  ([11 ~[1 0]]   (dfsPost [~[1]   ~[0] ~[2]] 8 1 0))  ; 2->1
  ([12 ~[2]]     (dfsPost [~[1]   ~[0] ~[2]] 8 2 0))  ; 2->2
  ([15 ~[0 1 2]] (dfsPost [~[1]   ~[2] ~[0]] 8 0 0))) ; 0->1 1->2

(tests
  (~[0 1]     (dfsPostAll [~[0 1] ~[0 1]]))
  (~[1 0]     (dfsPostAll [~[0]   ~[1]]))
  (~[2 0 1]   (dfsPostAll [~[1]   ~[0] ~[2]]))
  (~[3 2 1 0] (dfsPostAll [~[0]   ~[1] ~[2] ~[0]])))

(tests
  ([7  ~[1 0]]   (dfsCollect [~[0 1] ~[0 1]]    4 0 0))  ; first node first
  ([7  ~[0 1]]   (dfsCollect [~[0 1] ~[0 1]]    4 1 0))  ; second node first
  ([7  ~[]]      (dfsCollect [~[0 1] ~[0 1]]    7 1 0))  ; already visitied
  ([5  ~[0]]     (dfsCollect [~[0]   ~[1]]      4 0 0))  ; only first node
  ([6  ~[1]]     (dfsCollect [~[0]   ~[1]]      4 1 0))  ; only seond node
  ([11 ~[1 0]]   (dfsCollect [~[1]   ~[0] ~[2]] 8 0 0))  ; 1->2
  ([11 ~[0 1]]   (dfsCollect [~[1]   ~[0] ~[2]] 8 1 0))  ; 2->1
  ([12 ~[2]]     (dfsCollect [~[1]   ~[0] ~[2]] 8 2 0))  ; 2->2
  ([15 ~[2 1 0]] (dfsCollect [~[1]   ~[2] ~[0]] 8 0 0))) ; 0->1 1->2

(defun (kosa-loop rev viz acc list)
  (@ node (Ix0 list))
  (@ more (Ix1 list))
  (@ seen (Test node viz))
  (@ out  (dfsCollect rev viz node NIL))
  (@ viz2 (Ix0 out))
  (@ comp (Ix1 out))
  (@ acc2 (CONS comp acc))
  (Ifz list acc
    (If seen
      (kosa-loop rev viz acc more)
      (kosa-loop rev viz2 acc2 more))))

(defun (kosaraju g)
  (@ n (Sz g))
  (@ post (dfsPostAll g))
  (kosa-loop (transpose g) (Bex n) NIL (dfsPostAll g)))

(defun (has-loop e row i rem)
  (And rem
    (Or (Eq e (Ix i row))
      (has-loop e row (Inc i) (Dec rem)))))

(defun (has e row)
  (has-loop e row 0 (Sz row)))

(tests
  1(has 2 [1 2 3])
  0(has 4 [1 2 3]))

(defun (isCyclic g comp)
  (@ a (Ix0 comp))
  (Or (Gt (Sz comp) 1)
    (has a (Ix a g))))

(defun (mark-if-cyclic g comp)
  (Coup (isCyclic g comp) comp))

(defun (scc g)
  (array
    (lmap (mark-if-cyclic g)
      (lrev (lmap array (kosaraju (map stream g)))))))

(defun (cyc elems)
  (Coup 1 elems))

(tests
  ([cyc[1 2 0]]
   (scc [[1] [2] [0]]))

  ([cyc[2] cyc[1] cyc[0]]
   (scc [[0] [1] [2]]))

  ([cyc[2] cyc[1 0]]
   (scc [[1] [0] [2]]))

  ([[2] cyc[1 0]]
   (scc [[1] [0] []]))

  ([cyc[3] cyc[2] [0] cyc[1]]
   (scc [[1] [1] [2] [3]]))

  ([cyc[3] cyc[2] [1] cyc[0]]
   (scc [[0] [0] [2] [3]]))

  ; Broken Diamond shape
  ([[1] [2] [3] [0]]
   (scc [[] [0 2] [3] []]))

  ; Diamond SCC + tail
  ([cyc[1 2 3 0] [4]]
   (scc [[1] [2] [3] [0 4] []]))

  ; Diamond SCC + incoming roots + secondary cycle
  ([[6] [5] cyc[1 2 4 3 0]]
   (scc [[1] [2] [3] [0 4] [3] [1] [1]]))

  ; Large SCC (4-cycle)
  ([cyc[1 2 3 0]]
   (scc [[1] [2] [3] [0]]))

  ; Multiple independent SCCs (2-cycles)
  ([cyc[3 2] cyc[1 0]]
   (scc [[1] [0] [3] [2]]))

  ; Mixed graph: SCC + acyclic + tail
  ([cyc[1 0] [2] [3]]
   (scc [[1] [0 2] [3] []]))

  ([[0] cyc[1] [2]]
   (scc [[1] [2 1] []])))
