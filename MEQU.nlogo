;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MEQU (Market Effects of Quality Uncertainty)
;; MEQU is a model designed to study the effects of quality
;; uncertainty and incomplete information on market dynamics.
;; Copyright (C) 2005 Segismundo S. Izquierdo and Luis R. Izquierdo
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; Contact information:
;; Segis Izquierdo
;;   Department of Industrial Organization, ETS Ingenieros Industriales, 
;;   P del cauce s/n, 47011 Valladolid, Spain. e-mail: segis@eis.uva.es 
;; Luis R. Izquierdo 
;;   University of Burgos, Spain. 
;;   e-mail: luis@izquierdo.name

;;;;;;;;;;;;
;; Breeds ;;
;;;;;;;;;;;;

  ;; sellers, buyers, and links between buyers, are all turtles
  ;; links are not necessarily symmetric
breeds [sellers buyers links]

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

globals [
  list-of-sellers  ;; sellers ordered by marginal cost
  list-of-buyers   ;; buyers ordered by reservation price
  sellers-in-round ;; sellers who have sold a product in the session
  buyers-in-round  ;; buyers who have bought a product in the session
  price
  traded-units     ;; units traded in the session
  
    ;; reference quantities are those that would occur 
    ;; in the absence of quality variability
  reference-price 
  reference-traded-units
  reference-quality
  reference-buyers-surplus
  reference-sellers-surplus
  
    ;; average number of buyers reachable by each buyer
    ;; in an infinite number of steps
  avg-accessibility
  
    ;; variable for drag-and-drop procedure
  clicked-buyer      ;; buyer who was clicked on
  
  my-random-seed
  ]

sellers-own [
  marginal-cost
  surplus
]

buyers-own [
  std-reservation-price
  quality-expectation
  reservation-price       ;; this is the product of the two variables above
  unit-bought?            ;; boolean
  quality-of-unit-bought  ;; quality of last unit bought
  surplus
    ;; stores the list of buyers that this particular buyer links to
  linkees
    ;; these variables are used to do the layout
  force-x
  force-y
]

  ;; The direction of the link does matter in this model.
links-own [
  origin 
  destination
]

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buyers Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
   
to buy-product-of-quality [q]
  set unit-bought? true
  set quality-of-unit-bought q
  set surplus q * std-reservation-price - price
end

to update-quality-expectation
  let nbrs-who-bought filter [unit-bought?-of ?] linkees 

  ; Agents have to update their expectations if and only if:
  ; a. They care something about themselves and they have bought OR
  ; b. They care something about their neighbours and their neighbours have bought 
 
  set quality-expectation 
    quality-expectation 
      + individual-weight * (
        ifelse-value unit-bought? 
          [quality-of-unit-bought - quality-expectation]
          [0] 
        )
      + social-weight * (
        ifelse-value not empty? nbrs-who-bought 
          [(mean map [quality-of-unit-bought-of ?] nbrs-who-bought) - quality-expectation]
          [0] 
        )

  set reservation-price quality-expectation * std-reservation-price
end

  ;; accessibility is the number of social neighbours that the buyer could reach 
  ;; in a number of steps equal to "num-steps". For instance, if buyer A links only to buyer B, 
  ;; who links only to buyer C, who has no social neighbours, then A's accessibility 
  ;; in one step is 1, and in two steps is 2; B's accessibility in any number of steps is 1, 
  ;; and C's accessibility in any number of steps is 0.
to-report accessibility [num-steps]
  let step 1
  let reachable-buyers linkees
  let old-length 0
  let new-length length reachable-buyers
  while [ new-length != old-length AND step < num-steps]
        [ set old-length new-length
          set reachable-buyers 
            remove-duplicates sentence 
              reachable-buyers 
              reduce [sentence ?1 ?2] map [ linkees-of ?] reachable-buyers 
          set new-length (length reachable-buyers)
          set step (step + 1)
        ]
  report new-length - ifelse-value (member? self reachable-buyers) [1] [0]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

  ;; the following procedure is called when the model is first loaded
to startup
  setup
end

to setup
  clear-all
  set my-random-seed new-seed ; generate a new seed
  random-seed my-random-seed  ; use the new seed
  
  make-sellers
  make-buyers
  set-default-shape links "line"
  build-initial-network
 
  calculate-reference-values
  setup-graphs
end

to make-sellers
  set-default-shape sellers "person business"
  let i 1
  create-custom-sellers num-sellers [
    set marginal-cost i 
    set color yellow
      ;; spread them out at the bottom of the screen
    set xcor (i * screen-size-x / num-sellers - screen-edge-x)
    set ycor (- screen-edge-y)
    set i (i + 1) 
  ]
end

to make-buyers
  set-default-shape buyers "person"
  let i 1
  create-custom-buyers num-buyers [
    set std-reservation-price i
    set quality-expectation 1
    set reservation-price i
    set linkees []  ;; empty list
      ;; buyers are automatically created with evenly spaced headings,
      ;; so if they just move forward they will form a circle
    fd screen-edge-x - 2
    set i (i + 1) 
  ]
end

  ;; reference values are those that would occur 
  ;; in the absence of quality variability
to calculate-reference-values
  create-supply
  create-demand
  
    ;; draw reference supply and demand
  set-current-plot "market"
  set-current-plot-pen "ref-demand" 
  foreach list-of-buyers [ plot reservation-price-of ? ]
  set-current-plot-pen "supply" 
  foreach list-of-sellers [ plot marginal-cost-of ? ]
  
  calculate-price-and-traded-units                                 
  set reference-price price
  set reference-traded-units traded-units
  set reference-quality mean map [quality-expectation-of ?] list-of-buyers
  set reference-buyers-surplus sum map [std-reservation-price-of ?] buyers-in-round - price * traded-units
  set reference-sellers-surplus price * traded-units - sum map [marginal-cost-of ?] sellers-in-round  
end

to setup-graphs
  set-current-plot "market"
  set-plot-x-range 0 max list num-buyers num-sellers
  set-plot-y-range 0 max sentence map [reservation-price-of ?] list-of-buyers map [marginal-cost-of ?] list-of-sellers
    ;; drag-and-drop related variables
  set clicked-buyer nobody
end

;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;

to go
  create-supply
  create-demand
  calculate-price-and-traded-units
  exchange-goods
  
  update-graphs
  
  ; BEHAVIORSPACE
  ;if traded-units = 0 [  
  ;  user-message "No more units traded"
  ;  stop
  ;]
  ask buyers [update-quality-expectation]
  
    ;; social network dynamics
    ;; create or delete links, as required
  let actual-num-links count links
  if num-links != actual-num-links [
    ifelse num-links > actual-num-links
      [ make-links (num-links - actual-num-links) ]
      [ delete-links (actual-num-links - num-links) ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-supply
  set list-of-sellers sort-by [marginal-cost-of ?1 < marginal-cost-of ?2] values-from sellers [self]
end

to create-demand
    set list-of-buyers sort-by [reservation-price-of ?1 > reservation-price-of ?2 ] values-from buyers [self] 
end

to calculate-price-and-traded-units
  set sellers-in-round []
  set buyers-in-round []
  set traded-units 0
  ask buyers [ set unit-bought? false ]
  
    ;; calculate the number of traded units, and 
    ;; create the lists of buyers and sellers in the round
  while [traded-units < num-buyers and traded-units < num-sellers and 
         reservation-price-of item traded-units list-of-buyers >= marginal-cost-of item traded-units list-of-sellers ]
        [ 
        set sellers-in-round lput (item traded-units list-of-sellers) sellers-in-round
        set buyers-in-round lput (item traded-units list-of-buyers) buyers-in-round
        set traded-units traded-units + 1 
        ]  
  
    ;; if there's going to be any trade, calculate the price.
    ;; This price-setting formula takes into account the satisfied supply and demand 
    ;; and the pressure of the unsatisfied supply and demand 
  ifelse traded-units > 0 [
    let min-price ifelse-value (num-sellers = traded-units) 
                  [ reservation-price-of item (traded-units - 1) list-of-buyers ]
                  [ min list (reservation-price-of item (traded-units - 1) list-of-buyers)
                             (marginal-cost-of item (traded-units) list-of-sellers) ]
    let max-price ifelse-value (num-buyers = traded-units)
                  [ marginal-cost-of item (traded-units - 1) list-of-sellers ]
                  [ max list (marginal-cost-of item (traded-units - 1) list-of-sellers)
                             (reservation-price-of item (traded-units) list-of-buyers) ]    
    set price (min-price + max-price) / 2 
  ]
  ; BEHAVIORSPACE
  [set price 0] ; should be "NULL"
end

  ;; goods are exchanged and the quality of every product is uncovered
to exchange-goods
  if quality-distribution = "uniform" [ 
    let range 0
      ;; Doing what comes next every time-step is inefficient
      ;;  but it has to be done like this to allow the user to 
      ;; change the distribution at run-time
    set quality-variance min list quality-variance ((2 ^ 2) / 12)
    set range (sqrt (12 * quality-variance)) 
    foreach buyers-in-round [ 
      ask ? [buy-product-of-quality (1 - (range / 2) + random-float range)] 
    ]
    stop
  ]
  if quality-distribution = "(trimmed) normal" [ 
    foreach buyers-in-round [ 
      let quality-of-product random-normal 1 quality-variance
      if quality-of-product < 0 [ set quality-of-product 0 ]
      if quality-of-product > 2 [ set quality-of-product 2 ]  
      ask ? [buy-product-of-quality quality-of-product] 
    ]
    stop
  ]
  if quality-distribution = "exponential" [ 
    set quality-variance 1
    foreach buyers-in-round [ 
      ask ? [buy-product-of-quality random-exponential 1] 
    ]
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Link Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to build-initial-network
  if network-structure = "random"
    [ build-random-network  stop]
  if network-structure = "preferential attachment"
    [ build-preferential-attachment-network pref-attachment-links  stop]
  if network-structure = "double ring"
    [ build-double-ring-network stop]
  if network-structure = "star"
    [ build-star-network stop]
end

to build-random-network
   ;; you cannot create more directed links than (num-buyers * (num-buyers - 1))
 set num-links min list num-links (num-buyers * (num-buyers - 1))
 if-else num-links < (0.72 * (num-buyers * (num-buyers - 1))) 
   [ make-links num-links ]
   [ 
     ;; make a fully connected network

     ask buyers [without-interruption[
       ask buyers with [self != myself] [make-link-from-me-to myself]
     ]]
     
     ;; The following commented code is slower but allows to see the network formation
     ;let buyers-list values-from buyers [self]
     ;foreach buyers-list [ 
       ;let tmp-buyer ?
       ;foreach (remove tmp-buyer buyers-list) [ make-link tmp-buyer ?]
     ;]
     
     ;; and randomly delete the necessary links
     delete-links ((num-buyers * (num-buyers - 1)) - num-links)
   ]   
end

to build-preferential-attachment-network [n-of-edges-per-new-vertex]
  ;; This is Barabási and Albert's "preferential attachment" model of network growth (See Newman, 
  ;; "The structure and function of complex networks", sec. 7B). Basically, each new buyer selects   
  ;; "pref-attachment-links" other buyers to link to, who are selected with probability proportional to 
  ;; their number of existing (incoming and outgoing) links.

  let vertices shuffle values-from buyers [self]
  
  if n-of-edges-per-new-vertex > 0 [
  
  make-link (first vertices) (item 1 vertices)
  let list-of-vertices-repeated list 0 1
  let n-of-vertices-in-the-network 2
  
  repeat (length vertices) - n-of-vertices-in-the-network [
    let vertices-linked []
    while [(length vertices-linked < n-of-edges-per-new-vertex) 
          and (length vertices-linked < n-of-vertices-in-the-network)] [
      let vertex-to-link random-one-of list-of-vertices-repeated 
      if not member? vertex-to-link vertices-linked [
        set vertices-linked lput vertex-to-link vertices-linked
      ]
    ]
    
      ;; now link the vertices
    let new-vertex (item n-of-vertices-in-the-network vertices)
    foreach vertices-linked [make-link new-vertex (item ? vertices)]
    set list-of-vertices-repeated sentence list-of-vertices-repeated vertices-linked
    repeat (length vertices-linked) [
      set list-of-vertices-repeated lput n-of-vertices-in-the-network list-of-vertices-repeated
    ]      
    set n-of-vertices-in-the-network (n-of-vertices-in-the-network + 1)
  ]
  
  ]
  set num-links count links
  plot-accessibility
end

to build-double-ring-network
  ;; randomly seated in a round table and links to the one on the right and the one on the left
  let vertices shuffle values-from buyers [self]
  (foreach vertices
           (fput last vertices but-last vertices)
           (lput first vertices but-first vertices)
    [ make-link ?1 ?2   make-link ?1 ?3 ]
  )
  set num-links count links
  plot-accessibility  
end

to build-star-network
  ;; one buyer is randomly chosen. She links to all the others and all the others link to her.
  let centre random-one-of buyers
  let others values-from (buyers with [self != centre]) [self]
  foreach others [
    make-link ? centre
    make-link centre ?
  ]
  set num-links count links
  plot-accessibility
end

to make-links [n-links]
    ;; you cannot create more directed links than (num-buyers * (num-buyers - 1))
  let links-to-create min list n-links (num-buyers * (num-buyers - 1) - count links)
  repeat links-to-create [
    add-random-link
    set num-links count links
  ] 
  set num-links count links
  plot-accessibility
end

to make-link-from-me-to [linkee]
  if member? linkee linkees
  [
    user-message "There is already a link from me (" + self + ") to " + linkee
    stop
  ]
  hatch-links 1 [
    set origin myself
    set destination linkee
    set color color-of myself
    reposition-link
  ]
    ;; add linkee to the linker's list of linkees
  set linkees fput linkee linkees
  if show-network-formation [ 
    repeat 20 [layout] 
    plot-accessibility
  ]
end

;; The algorithm for the following procedure has been borrowed from the 
;; model "Giant Component", in the "Models library".
;; The copyright for this procedure is included at the bottom of this file

  ;; pick a random missing link and create it
to add-random-link 
  let potential-linker random-one-of buyers
  let potential-linkee random-one-of buyers with [potential-linker != self]
  ifelse member? potential-linkee linkees-of potential-linker
      ;; if there's already a link there, then go back
      ;; and pick two new buyers
    [ add-random-link ]
      ;; else, go ahead and make it
    [ make-link potential-linker potential-linkee ]
end

;; The algorithm for the following procedure has been borrowed from the 
;; model "Giant Component", in the "Models library".
;; The copyright for this procedure is included at the bottom of this file

  ;; makes a link from linker to linkee
to make-link [linker linkee]
  if member? linkee linkees-of linker
  [
    user-message "There is already a link from " + linker + " to " + linkee
    stop
  ]
  create-custom-links 1
  [
    set origin linker
    set destination linkee
    set color color-of linker
    reposition-link
  ]
    ;; add linkee to the linker's list of linkees
  set linkees-of linker fput linkee linkees-of linker
  if show-network-formation [ 
    repeat 20 [layout] 
    plot-accessibility
  ]
end

  ;; deletes n-links links at random
to delete-links [n-links]
  let n-links-to-delete min list n-links (count links)
  let links-to-delete random-n-of n-links-to-delete links
  ask links-to-delete [
    without-interruption [ 
      set (linkees-of origin) remove destination (linkees-of origin)
      die
    ]
  ]
  plot-accessibility
end

;; The algorithm for the following procedure has been borrowed from the 
;; model "Giant Component", in the "Models library".
;; The copyright for this procedure is included at the bottom of this file

  ;; repositions and resizes the link according to the position of the
  ;; buyers
to reposition-link  ;; link procedure
  setxy (xcor-of origin) (ycor-of origin)
  set size distance-nowrap destination
    ;; watch out for special case where origin and destination are
    ;; at the same place
  if size != 0
  [
      ;; position link at midpoint between origin and destination
    set heading towards-nowrap destination
    jump size / 2
  ]
end

;;;;;;;;;;;;;;;;
;;; Plotting ;;;
;;;;;;;;;;;;;;;;

to update-graphs
  plot-market
  plot-price
  plot-volume
  plot-average-quality
  plot-sellers-surplus
  plot-buyers-surplus
end

to plot-market  
  set-current-plot "market"
  set-current-plot-pen "demand" 
  plot-pen-reset
  foreach list-of-buyers
    [ plot reservation-price-of ? ]
end

to plot-price
  set-current-plot "price"  
  set-current-plot-pen "price"  
  ifelse price != "NULL" 
    [ plot price ] 
    [ set-plot-pen-color red
      plot 0 ] 
  set-current-plot-pen "ref-price"  
  plot reference-price 
end

to plot-volume  
  set-current-plot "volume"  
  set-current-plot-pen "volume" 
  plot traded-units
  set-current-plot-pen "ref-volume" 
  plot reference-traded-units
end

to plot-average-quality
  set-current-plot "average expected quality"
  set-current-plot-pen "avg exp q"
  plot avg-exp-quality
  set-current-plot-pen "ref-quality"
  plot reference-quality 
end

to-report avg-exp-quality
  report mean map [quality-expectation-of ?] list-of-buyers
end

to plot-buyers-surplus
  set-current-plot "buyers surplus"
  set-current-plot-pen "buyers surplus"
  plot buyers-surplus
  set-current-plot-pen "ref-surplus"
  plot reference-buyers-surplus 
end

to-report buyers-surplus
  report sum map [surplus-of ?] buyers-in-round
end

to plot-sellers-surplus
  set-current-plot "sellers surplus"
  set-current-plot-pen "sellers surplus"
  plot sellers-surplus
  set-current-plot-pen "ref-surplus"
  plot reference-sellers-surplus 
end

to-report sellers-surplus
  report ifelse-value (price != "NULL")
    [ price * traded-units - sum map [marginal-cost-of ?] sellers-in-round ]
    [ 0 ]
end

to plot-accessibility
  let steps accessibility-steps
  if accessibility-steps = "Infinity" [set steps num-buyers]
  let accessibility-list values-from buyers [accessibility steps]
  let max-accessibility max accessibility-list
  set-current-plot "Accessibility Distribution"
  set-plot-x-range 0 (max-accessibility + 1)  ;; + 1 to make room for the width of the last bar
  histogram-list accessibility-list
  set avg-accessibility mean accessibility-list
end

to-report total-surplus
  report sellers-surplus + buyers-surplus
end

to-report marginal-costs
  report values-from sellers [marginal-cost] 
end

to-report reservation-prices
  report values-from buyers [reservation-price] 
end

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;

;; The algorithm for the following procedure has been borrowed from the 
;; model "Giant Component", in the "Models library".
;; The copyright for this procedure is included at the bottom of this file

to layout
  no-display
    ;; these values are arbitrarily chosen to give a good layout
    ;; for typical model settings
  let spring-constant 0.2
  let natural-length 9.0
  let repulsion-constant 1.0

    ;; reset force-x and force-y
  ask buyers
  [
    set force-x 0
    set force-y 0
  ]

    ;; add the forces due to the springs
  without-interruption     ;; process links one at a time, not concurrently
  [
    ask links
    [
      let spring-force (spring-constant * (size - natural-length))
        ;; take care of zero sized spring
      ifelse size = 0
      [
        set spring-force spring-constant * natural-length

          ;; we know force but dont know the direction in which to apply it
          ;; make an arbitrary choice of direction ( postive and negative x direction)
        ask origin
        [set force-x force-x + spring-force]
        ask destination
        [set force-x force-x - spring-force]
      ]
      [
        ask origin
        [
          set force-x force-x + spring-force * sin towards-nowrap destination-of myself
          set force-y force-y + spring-force * cos towards-nowrap destination-of myself
        ]
        ask destination
        [
          set force-x force-x + spring-force * sin towards-nowrap origin-of myself
          set force-y force-y + spring-force * cos towards-nowrap origin-of myself
        ]
      ]
    ]
  ]

    ;; add a force of repulsion between buyers,
    ;; inversely proportional to square of distance
  without-interruption    ;; process buyers one at a time, not concurrently
  [
      ;; exempt linkless buyers from the force
    let connected-buyers buyers with [not empty? linkees]
    ask connected-buyers
    [
      ask connected-buyers with [self != myself]
      [
        let angle 0
        let force 0
        ifelse xcor = xcor-of myself and ycor = ycor-of myself
          ;; the two buyers are exactly on top of each other.  theoretically
          ;; this shouldn't occur, but in practice, it might because the world
          ;; is bounded and buyers can get forced into the corners.  not clear
          ;; how to handle this, so just apply a small arbitrary force in a
          ;; random direction.
        [
          set angle random-float 360    ;; arbitrary
          set force repulsion-constant  ;; arbitrary
        ]
          ;; normal case where buyers aren't on top of each other
        [
          set angle towards-nowrap myself
          set force repulsion-constant / ((distance-nowrap myself) ^ 2)
        ]
        set force-x force-x - force * sin angle
        set force-y force-y - force * cos angle
      ]
    ]
  ]

    ;; actually move the buyers
  ask buyers
  [
      ;; the current layout scheme has an issue where
      ;; sometimes heavily connected buyers are thrown back and forth.
      ;; to prevent that we cap the movement of buyers
    ifelse force-x > 1
    [ set force-x 1 ]
    [
      if force-x < -1
      [set force-x -1]
    ]
    ifelse force-y > 1
    [ set force-y 1 ]
    [
      if force-y < -1
      [set force-y -1]
    ]
    move (xcor + force-x) (ycor + force-y)
  ]
    ;; reposition all the links
  ask links [ reposition-link ]
    ;; update the display, for smooth animation
  display
end

  ;; move, but take care not to wrap around link of world
to move [x y]  ;; buyer procedure
  ifelse x > screen-edge-x
    [ set x screen-edge-x ]
    [ if x < (- screen-edge-x)
      [ set x (- screen-edge-x) ] ]
  ifelse y > screen-edge-y
    [ set y screen-edge-y ]
    [ if y < (- screen-edge-y + 1)
      [ set y (- screen-edge-y + 1) ] ]
  setxy x y
end

to drag-and-drop-buyers
    ifelse mouse-down? 
    [ ifelse is-agent? clicked-buyer 
        [ ask clicked-buyer [ setxy mouse-xcor mouse-ycor ] ]
        [   ;; no buyer had been clicked
            ;; if there are buyers at the current mouse location, then pick one
          if any? buyers-at round mouse-xcor round mouse-ycor [  
            set clicked-buyer random-one-of buyers-at round mouse-xcor round mouse-ycor
          ]
        ]
      ask links [ reposition-link ] 
    ]
    [   ;; mouse not down
      set clicked-buyer nobody
    ]    
    
end

;; The algorithm for the following procedure has been borrowed from the 
;; model "Preferential Attachment", in the "Models library".
;; The copyright for this procedure is included at the bottom of this file

  ;; resize-buyers, change back and forth from size based on degree to a size of 1
to resize-buyers
  ifelse ( not (any? buyers with [size > 1]) )
  [ ask buyers [ set size sqrt length linkees] ]
  [ ask buyers [ set size 1 ] ]
end

;; The following copyright is included because the coding of some procedures 
;; was inspired or copied from models that included this copyright.

; *** NetLogo Model Copyright Notice ***
;
; This model was created as part of the projects:
; PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN
; CLASSROOMS and INTEGRATED SIMULATION AND MODELING ENVIRONMENT.
; The project gratefully acknowllinks the support of the
; National Science Foundation (REPP & ROLE programs) -- grant numbers
; REC #9814682 and REC-0126227.
;
; Copyright 2005 by Uri Wilensky.  Updated 2005.  All rights reserved.
;
; Permission to use, modify or redistribute this model is hereby granted,
; provided that both of the following requirements are followed:
; a) this copyright notice is included.
; b) this model will not be redistributed for profit without permission
;    from Uri Wilensky.
; Contact Uri Wilensky for appropriate licenses for redistribution for
; profit.
;
; To refer to this model in academic publications, please use:
; Wilensky, U. (2005).  NetLogo Preferential Attachment model.
; http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment.
; Center for Connected Learning and Computer-Based Modeling,
; Northwestern University, Evanston, IL.
;
; In other publications, please use:
; Copyright 2005 by Uri Wilensky.  All rights reserved.  See
; http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment
; for terms of use.
;
; *** End of NetLogo Model Copyright Notice ***
@#$#@#$#@
GRAPHICS-WINDOW
292
12
596
337
10
10
14.0
1
10
1
1
1
0
0
0
1

CC-WINDOW
5
596
866
691
Command Center
0

BUTTON
14
10
80
43
NIL
setup
NIL
1
T
OBSERVER
T
NIL

BUTTON
187
10
248
43
NIL
go
T
1
T
OBSERVER
NIL
NIL

BUTTON
104
10
164
43
go-once
go
NIL
1
T
OBSERVER
T
NIL

BUTTON
677
47
764
80
relax network
layout
T
1
T
OBSERVER
T
NIL

BUTTON
599
47
676
80
resize buyers
resize-buyers
NIL
1
T
OBSERVER
T
NIL

SLIDER
153
110
288
143
num-links
num-links
0
9900
200
1
1
NIL

SLIDER
4
50
108
83
num-sellers
num-sellers
0
100
100
1
1
NIL

SLIDER
4
85
108
118
num-buyers
num-buyers
0
100
100
1
1
NIL

BUTTON
765
47
857
80
drag and drop
drag-and-drop-buyers
T
1
T
OBSERVER
T
NIL

SWITCH
599
12
787
45
show-network-formation
show-network-formation
1
1
-1000

PLOT
292
339
596
460
price
time-step
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"price" 1.0 0 -16777216 true
"ref-price" 1.0 2 -13840069 true

PLOT
292
462
596
582
volume
time-step
units
0.0
10.0
0.0
10.0
true
false
PENS
"volume" 1.0 0 -16777216 true
"ref-volume" 1.0 2 -13840069 true

PLOT
4
239
289
460
market
quantity
price
0.0
10.0
0.0
10.0
false
false
PENS
"supply" 1.0 2 -13345367 true
"demand" 1.0 2 -2674135 true
"ref-demand" 1.0 2 -13840069 true

SLIDER
4
155
128
188
individual-weight
individual-weight
0
1
0.4
0.05
1
NIL

PLOT
4
462
289
582
average expected quality
time-step
NIL
0.0
10.0
0.0
0.0
true
false
PENS
"avg exp q" 1.0 0 -16777216 true
"ref-quality" 1.0 2 -13840069 true

SLIDER
4
120
128
153
social-weight
social-weight
0
1
0.4
0.05
1
NIL

PLOT
599
87
857
246
Accessibility Distribution
# of reachable buyers
# of buyers
0.0
2.0
0.0
10.0
true
false
PENS
"default" 1.0 1 -16777216 false

MONITOR
763
269
857
318
NIL
avg-accessibility
3
1

PLOT
599
339
857
460
sellers surplus
time-step
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"sellers surplus" 1.0 0 -16777216 true
"ref-surplus" 1.0 2 -13840069 true

PLOT
599
462
857
582
buyers surplus
time-step
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"buyers surplus" 1.0 0 -16777216 true
"ref-surplus" 1.0 2 -13840069 true

CHOOSER
4
192
123
237
quality-distribution
quality-distribution
"uniform" "(trimmed) normal" "exponential"
0

SLIDER
126
199
246
232
quality-variance
quality-variance
0
1
0.33
0.01
1
NIL

CHOOSER
153
62
288
107
network-structure
network-structure
"random" "preferential attachment" "double ring" "star"
0

SLIDER
153
146
288
179
pref-attachment-links
pref-attachment-links
0
99
2
1
1
NIL

CHOOSER
599
250
708
295
accessibility-steps
accessibility-steps
1 2 3 4 5 10 20 "Infinity"
0

BUTTON
599
299
708
332
Update accessibility
plot-accessibility
NIL
1
T
OBSERVER
T
NIL

@#$#@#$#@
Detailed documentation for this model is available at http://www.research.izqui.org/
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

coloured line
true
0
Line -11221820 false 150 300 150 225
Line -13791810 false 150 225 150 150
Line -13345367 false 150 150 150 75
Line -8630108 false 150 75 150 0

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="100" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="500"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <metric>avg-accessibility</metric>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="9900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="individual-weight" first="0" step="0.2" last="1.0"/>
    <steppedValueSet variable="social-weight" first="0" step="0.2" last="1.0"/>
  </experiment>
  <experiment name="paper-figures" repetitions="1" runMetricsEveryTick="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="10000"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <metric>marginal-costs</metric>
    <metric>reservation-prices</metric>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-variance">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-weight">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-weight">
      <value value="0.0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fig-4" repetitions="1" runMetricsEveryTick="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="500"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <metric>marginal-costs</metric>
    <metric>reservation-prices</metric>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="9900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-weight">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-weight">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-2" repetitions="1000" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="500"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="9900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-weight">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-weight">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-3" repetitions="100" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="50"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="9900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-weight">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-weight">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-5" repetitions="100" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="50"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="9900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-weight">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-weight">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-4" repetitions="1000" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="500"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="1000"/>
      <value value="9900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-weight">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-weight">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-random" repetitions="100" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="500"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <metric>avg-accessibility</metric>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pref-attachment-links">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
      <value value="99"/>
      <value value="197"/>
      <value value="294"/>
      <value value="390"/>
      <value value="485"/>
      <value value="945"/>
      <value value="4950"/>
    </enumeratedValueSet>
    <steppedValueSet variable="individual-weight" first="0" step="0.2" last="1.0"/>
    <steppedValueSet variable="social-weight" first="0" step="0.2" last="1.0"/>
  </experiment>
  <experiment name="experiment-pref-attachment" repetitions="100" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit ticks="500"/>
    <metric>price</metric>
    <metric>traded-units</metric>
    <metric>sellers-surplus</metric>
    <metric>buyers-surplus</metric>
    <metric>total-surplus</metric>
    <metric>avg-exp-quality</metric>
    <metric>my-random-seed</metric>
    <metric>avg-accessibility</metric>
    <enumeratedValueSet variable="num-buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-sellers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-distribution">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-variance">
      <value value="1.0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-network-formation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;preferential-attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pref-attachment-links">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="individual-weight" first="0" step="0.2" last="1.0"/>
    <steppedValueSet variable="social-weight" first="0" step="0.2" last="1.0"/>
  </experiment>
</experiments>
@#$#@#$#@
