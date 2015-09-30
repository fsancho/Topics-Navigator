;----------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------
; Main Module for the Model
;----------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------

__includes ["Visual.nls" "Measures.nls" "Files.nls" "Query2.nls" "auxiliar.nls" "ContextMenu.nls" "creation.nls" "Timeline.nls"]

extensions [table goo pathdir bitmap network string]

; Breed for marking the selected topics (It is the first to be declared to be drawn on the back)
breed [selectors selector]

; Breed for the Topics Set
breed [topics topic]

; Auxiliary breed for the edges in box selection
breed [edges edge]

; Breed for the pins in the fixed topics
breed [pins pin]

; Breed for the schema
breed [nodes-schema node-schema]

; Auxiliary Breed for the contextual menu
breed [menu-items menu-item]

; Auxiliary breed for Topic information
breed [informers informer]
undirected-link-breed [irelations irelation]

irelations-own
[ RelType]

; Breed for traversals
breed [traversals traversal]
traversals-own
[
  mem-t ; Stores the list of topics it has crossed
  mem-r ; Stores the list of relations it has crossed
  mem-vs
  mem-rs
  mem-eds
  ;mem-reds
  finished? ; Says if it has reached the goal
]

; Relations between Topics
undirected-link-breed [relations relation]
directed-link-breed [drelations drelation]


; Relations between types (schema)
directed-link-breed [srelations srelation]

; Relations provided by queries
directed-link-breed [qrelations qrelation]

; Relations for the contextual menu
undirected-link-breed [links-menu link-menu]

; Attributes for the relations between topics
relations-own
[
  RelType                ; Relation/Edge Type
  relation_attributes    ; Relation attributes (read from NLG file)
  relation_measures      ; Relation attributes (internally used by the system, for example to compute measures)
  original?              ; Indicates if the directed relation is original or obtained from a query (maybe we can remove it)
]

drelations-own
[
  RelType                ; Relation/Edge Type
  relation_attributes    ; Relation attributes (read from NLG file)
  original?              ; Indicates if the directed relation is original or obtained from a query (maybe we can remove it)
  relation_measures      ; Relation attributes (internally used by the system, for example to compute measures)
]

; Attributes for the relations obtained from queries
qrelations-own
[
  RelType
  relation_attributes 
  original?
]

; Attributes for topics
topics-own
[
  ID                     ; ID (unique) for topics
  TType                  ; Node/Topic Type
  topic_attributes       ; Topic attributes (read from NLG file)
  topic_measures         ; Topic attributes (internally used by the system, for example to compute measures)
  selected?              ;
  fixed?                 ;
]

; Attributes for item of contextual menu
menu-items-own
[
  menu-item-Name   ; Name of the item action
  menu-Action      ; Task associated to the item
  Parent           ; Topic that acts as origin of the action
  MType            ; Type of menu to be considered: Browser / Data
]

; Attributes for Selectors
selectors-own
[
  parent ; shows the topic the selector is associated to
]

; Attributes for Pins
pins-own
[
  parent ; shows the topic the selector is associated to
]

; Attributes for the schema nodes
nodes-schema-own
[
  node-Name ; Type of topic it represents
]

globals
[
  Topics-Styles        ; visual features for the several topic types
  vrelations           ; set of visible relations (in any moment)
  Temp-Selected        ; set of Temp-Selected topics while the selection process
  Topic-Types           ; Topics Types in the network
  RelationTypes        ; Relations Types in the network
  Relations-Styles     ; Visual features for the several relations types

  ;Special Families Topics 
  Table:Actions        ; Table of action tasks to be applied over families of topics
  T-Visible            ; Visible Topics in any moment
  T-Layout             ; Topics to be considered in the layout
  Hyper                ; list of topic-types for hyper-edges
  
  Layout:Modes         ; List of Layout Modes
  Buttons:Files        ; Control hide/show for Files
  Button:TI            ; Control hide/show Topic Information
  Buttons:Visual
  Buttons:Queries
  Buttons:Schema
  Buttons:Timeline
  Buttons:Measures
  
  Current-Traversal    ; List with current traversal [[v1... vn] [vi1...vin]]=[P,Q]
  Current-query        ; List with the query that is in constrution
  Query-set            ; List of Queries in memory
  Global-restrictions  ; List of "where" conditions in queries
  
  Run-out              ; Action to be run out of turtle-context
  Customized-label     ; Format for the label to be shown by topics
  
  MinTimeline          ; lower limit for Timeline
  MaxTimeline          ; upper limit for Timeline
  
  topics-load          ; Auxiliary table to load edges faster from files
  
  Processing           ; General reporter to show the % of computation processed
  
]

; Procedure to be called when the model is loaded
to startup
  ca
  no-display
  set query-path "P: "
  set query-set []
  set global-restrictions []
  set current-query []
  set Topic-Types []
  set RelationTypes []
  set Topics-Styles []
  set Relations-Styles []
  set Hyper []
  set Run-out ""
  set Customized-label "ID"
  set MinTimeline 0
  set MaxTimeline 100
  set processing "No Job..."
  
  ;Table of action tasks to be applied over families of topics  
  set Table:Actions table:from-list (list 
    (list "Show" task [Show-topics Selected-Family])
    (list "Hide" task [Hide-topics Selected-Family])
    (list "Expand" task [Expand-topics Selected-Family])
    (list "Leave" task [leave  Selected-Family with [not hidden?]])
    (list "Fix" task [Fix])
    (list "List" task [List-topics Selected-Family])
    (list "Select" task [Select-Family])
    (list "Deselect" task [Unselect-family])
    (list "Show Labels" task [show-labels])
    (list "Hide Labels" task [hide-labels])
    )
  
  ;List of action tasks for layout computing
  set Layout:Modes table:from-list (list
    (list "Spring" task [spring-layout])
    (list "ARF" task [ARF-layout])
    (list "ARF Weighted" task [ARF-Weighted-layout])
    (list "ARF QWeighted" task [ARF-QWeighted-layout])    
    (list "Spring + ARF" task [spring-layout ARF-layout])
    (list "Hyp" task [hyp-layout])
    (list "Spring + Hyp" task [spring-layout hyp-layout])
    (list "Hyp + ARF" task [hyp-layout ARF-layout])
    (list "Circle" task [circle-layout])
    (list "Radial" task [radial-layout])
    (list "C. Bipartite" task [C-Bipartite])
    )
  
  goo:set-chooser-items "Layout-mode" (map [first ?] (table:to-list Layout:Modes))
  goo:set-chooser-items "Family" (sentence "All" "Visible" "Selected" "Fixed")
    
  ; Prepare the background and auxiliar turtles
  ask patches [set pcolor white]
  set-default-shape edges "line"        ; edges for the selection box
  set-default-shape selectors "circle"  ; highlighting for selected topics
  set-default-shape pins "pushpin"      ; pin for fixed topics
  set-default-shape informers "informer";Prepare the informers...
  create-informers 3                    
  [
    set heading 0
    setxy 0 (-.5 * who)
    set color white
    set label-color black
    ht
  ]
  ask turtle 0 [create-irelation-with (turtle 1) [tie hide-link]]
  ask turtle 1 [create-irelation-with (turtle 2) [tie hide-link]]

  ; Initializing of global topics/relations sets
  set Temp-Selected no-turtles
  set T-Visible no-turtles
  set vrelations no-links
     
  set Buttons:Files true
  set Button:TI true
  set Buttons:Visual (list "selected-color" "spring-length" "spring-constant" "repulsion-constant" "tension" "radius" "Show-weights?" "Zoom" "refresh" 
    "Hide-Orphans?" "Show-all-relations?" "V-Sort" "H-Sort" "R-Sort" "Fix?" "Show/Hide Topic Information" "Customize Topic Label" "Gravity" "Show-relation-labels?"
    "Topic-Information?" "Topic-Information")
  set Buttons:Queries (list "Add P" "Build P" "Launch!" "New Q" "Queries:" "Query-Path" "Export" "Q-Import" "Show Query" "Clear" "Output" "Threshold" "Filter" "Consolidate")
  set Buttons:Schema (list "Build" "Layout Schema" "Show/Hide")
  set Buttons:Timeline (list "Start-Interval" "End-Interval" "Visualize" "|<" "<<" "<" ">" ">>" ">|" "Speed")
  set Buttons:Measures (list "Choose-Measure" "Plot-Type" "Plot" "Plot" "Compute" "measure-only-visible?")
  set-buttons Buttons:Visual false
  set-buttons Buttons:Queries false
  set-buttons Buttons:Schema false
  set-buttons Buttons:Timeline false
  set-buttons Buttons:Measures false
  
  ; Initialize of contextual menu
  setup-brwoser-menu
  
  let logo (bitmap:import "My_Logo.png") 
  bitmap:copy-to-drawing logo (patch-size * world-width - (bitmap:width logo)) / 2 (patch-size * world-height - (bitmap:height logo)) / 2 
  
  display
end

; 
to Execute
  run table:get Table:actions Action
end


to topic-inspect
  if mouse-inside?
  [
    let candidate min-one-of T-Visible [distancexy mouse-xcor mouse-ycor]
    if candidate != nobody
    [ ask candidate 
      [if (distancexy mouse-xcor mouse-ycor) < 1 
        [
          let att (table:to-list topic_attributes)
          let p_attr ifelse-value (empty? att) [""][reduce [(word ?1 ?2)] (map [(word "\n" (first ?) ": " (last ?) " | ")] att)]
          ask informer 0 [ move-to candidate st set label (word  "ID: " [ID] of candidate "    Type: " [TType] of candidate)]
          ask informer 1 [ st set label p_attr ]
          ask informer 2 [ st set label (word "\nTotal/Visible Conections: " (count [my-rels] of candidate)  " / " (count [my-rels with [not hidden?]] of candidate))]
        ]
      ]
    ]
  ]
end

to select
  ask edges [die]
  selection-multi
end  

to select-topic
  ifelse selected?
  [
    ask selectors with [Parent = myself] [die]
    set selected? false
  ]
  [
    set selected? true
    ;se añade un selector para él
    no-display
    hatch-selectors 1
    [ 
      move-to myself
      set parent myself
      set size 1.5 * [size] of myself
      set color lput 120 extract-rgb selected-color
      set label ""
    ]
    display
  ]
end

to-report in-box? [x y]
  if not any? edges [ report false ]
  ; Calculamos las cotas del rect�ngulo
  let y-max max [ycor] of edges   
  let y-min min [ycor] of edges   
  let x-max max [xcor] of edges   
  let x-min min [xcor] of edges   
  ; Comprobamos si el punto (x,y) est� dentro de esas cotas
  report (x >= x-min) and (x <= x-max) and (y >= y-min) and (y <= y-max)
end



to select-in-box [x1 y1 x2 y2]   ;; (x1 y1) (x2 y2) esquinas opuestas del cuadrado de selecci�n actual
  ask edges[die]  ; borra el rect�ngulo anterior
  create-edge-box x1 y1 x2 y1
  create-edge-box  x1 y1 x1 y2
  create-edge-box  x1 y2 x2 y2
  create-edge-box  x2 y1 x2 y2
  ; se seleccionan los t�picos visibles que haya dentro
  set Temp-Selected T-Visible with [in-box? xcor ycor]
end

to create-edge-box  [x1 y1 x2 y2]
  ; el edge se crea poniendo una tortuga de tipo edge (lineal) en el centro del edge
  ; orient�ndola en la direcci�n adecuada, y asign�ndole como tama�o la longitud del edge
  no-display
  create-edges 1 [
    set color gray
    setxy (x1 + x2) / 2
          (y1 + y2) / 2
    facexy x1 y1
    set size 2 * distancexy x1 y1
    display
  ]
end

to selection-multi
  ; almacenamos d�nde ha sido pulsado el rat�n
  let old-x mouse-xcor
  let old-y mouse-ycor
  ; Mientras el rat�n se mantenga pulsado
  while [mouse-down?] [
    ; se actualiza el rect�ngulo y se actualiza el conjunto de t�picos Temp-Selected
    select-in-box old-x old-y mouse-xcor mouse-ycor
  ]
  ; una vez soltado el bot�n del rat�n
  ask edges [die] ;se elimina el rect�ngulo
  ; se a�ade un selector por cada t�pico seleccionado (que no estuviera seleccionado)
  ask Temp-Selected with [not selected?]
    [ 
      set selected? true
      hatch-selectors 1
      [ 
        set parent myself
        set size 1.5 * [size] of parent
        set color lput 120 extract-rgb selected-color
        setxy [xcor] of parent [ycor] of parent
        set label ""
      ]
    ]
  ; se actualiza la lista de t�picos general actualizando los t�picos reci�n Temp-Selected
  set Temp-Selected no-turtles
end


; hide-direct todos los nodos salvo el seleccionado
to leave-topic
  let candidate get-with-mouse
  if candidate != nobody
  [
    ask turtles [ht]
    ask links [hide-link]
    set T-visible no-turtles
    set vrelations no-links
    show-topics candidate
    wait .1
  ]
end

; hide-direct todos los t�picos, salvo los que recibe como dato de entrada
to leave [t]
  ask turtles [ht]
  ask links   [hide-link]
  show-topics t
end

; Muuestra los t�picos que recibe como dato de entrada
to show-topics [t]
  ask t [st]
  set T-Visible topics with [not hidden?]
  if Show-all-relations? [show-relations]
  set vrelations rels with [not hidden?]
end

; expand los t�picos que recibe como dato de entrada
to expand-topics [t]
  ; Expandimos los t�picos visibles de los recibidos como entrada
  set t turtle-set t
  ask t with [not hidden?][ expand-topic self ] 
  ; actualizamos los nodos visibles, y las aristas
  refresh-visible
  ; Si est� activada la opci�n, mostramos todas las aristas que se deriven de la aparici�n de los
  ; nuevos t�picos
  if Show-all-relations? [show-relations]
  refresh-visible
end

; Funci�n que se usa para expand-topics t�picos directamente
;to expand-direct
;  let candidate get-with-mouse
;  if candidate != nobody
;  [
;    expand-topic candidate
;  ]
;end

; expand un t�pico en particular
to expand-topic [t]
  ask t
  [
    ;mostramos las aristas que est�n conectadas a �l
    ask my-rels
    [
      show-link
      ask other-end
      [
        st
        ; Si son hiperaristas, iteramos el proceso expandi�ndolos
        if (Hyper? TType)
        [ expand-topic self ]
      ]
    ]
  ]
  refresh-visible
end



; hide-direct los t�picos que recibe como dato de entrada
to hide-topics [t]
  ;La siguiente instrucci�n sirve para convertir un agente aislado en un set... por si se usa con hide-direct
  set t turtle-set t
  let t2 no-turtles
;  set t (turtle-set t (relation-neighbors with [(member? TType Hyper)]))
  ask t
  [
    ht
    ask my-rels [hide-link]
    set t2 (turtle-set t2 my-neighbors with [hyper? ttype])
  ]
  ask t2
  [
    ht 
    ask my-rels [hide-link]
  ]
  ask selectors
  [
    if member? parent t [die]
  ]
  ; Los t�picos de enlace que han quedado con grado 1 se consideran hu�rfanos
  ;Tras hide-topics los t�picos, si la funci�n est� activada, comprueba cu�les son los t�picos que han quedado hu�rfanos
  if Hide-Orphans? [ask topics with [my-rels with [not hidden?] = no-links] [ht]]
  ; Actualiza los t�picos y aristas visibles 
  set T-Visible (topics with [not hidden?])
  set vrelations rels with [not hidden?]
end


; Devuelve el t�pico pulsado por el rat�n, o nobody si se ha pulsado en una zona vac�a
; para que un t�pico se considere pulsado, el clic ha de hacerse a una distancia <1 del t�pico
to-report get-with-mouse
  let candidate nobody
  if mouse-down? 
  [
    let resp 0
    set candidate min-one-of  T-Visible [distancexy mouse-xcor mouse-ycor]
    if candidate != nobody
    [ ask candidate [if (distancexy mouse-xcor mouse-ycor) > 1 [set resp 1]] ]
    if (resp = 1) [set candidate nobody]
  ]
  report candidate
end

; Mueve un t�pico o el conjunto de t�picos Temp-Selected
to move-topics
  let candidate get-with-mouse
  ifelse candidate = nobody
  [
    move-topics-Selected
  ]
  [
    while [mouse-down?]
    [
      ask candidate [setxy mouse-xcor mouse-ycor]
      ask (turtle-set selectors pins) with [parent = myself]
        [move-to parent]
    ]
  ]
end

to move-topic [t]
  while [mouse-down?]
    [
      ask t [setxy mouse-xcor mouse-ycor]
      ask (turtle-set selectors pins) with [parent = t ]
        [move-to parent]
      layout-spring menu-items links-menu 1 1 1
    ]
end

; Mueve los t�picos Temp-Selected
to move-topics-Selected
  ; almacenamos las coordenadas iniciales del rat�n
  let old-x mouse-xcor
  let old-y mouse-ycor
  ; mientras se mantenga pulsado el rat�n
  while [mouse-down?] [
    ; se toman las nuevas coordenadas del rat�n
    let new-x mouse-xcor
    let new-y mouse-ycor
    ; y se actualizan las posiciones relativas de los topics Temp-Selected
    ;   si se salieran de la pantalla, se quedan en el l�mite
    ask (topics with [selected?])
      [ if abs (xcor + new-x - old-x) <= max-pxcor [set xcor (xcor + new-x - old-x)]
        if abs (ycor + new-y - old-y ) <= max-pycor [set ycor  (ycor + new-y - old-y )]
        ask (turtle-set selectors pins) with [parent = myself]
           [ move-to parent]
      ]
    ; se actualizan las coordenadas iniciales del rat�n
    set old-x new-x
    set old-y new-y
    ; se actualiza la representaci�n de los t�picos para que parezca en tiempo real
    display
  ]
end

; Muestra todas las aristas posibles entre los t�picos visibles
to show-relations
  ask rels
  [ if (not [hidden?] of end1) and (not [hidden?] of end2) [show-link]]
end


; Report que devuelve el c�digo asociado a cada una de las familias de t�picos
to-report Type-index [name]
  report (position name Topic-Types)
end

to-report RType-index [name]
  report (position name RelationTypes)
end

to search
  let rep search-aux
  ifelse rep = nobody
  [
    user-message "Your search has produced no results"
  ]
  [
    show-topics rep
  ]
end
    

to-report search-aux
  let goal string:lower-case (user-input "Enter the content to look for:")
  let rep []
  ask topics with [not hyper? ttype]
  [
    let attr (sentence (word ID) (map [(word last ?)] table:to-list topic_attributes))
    ifelse member? goal attr
    [
      set rep fput (map [cut ? 30] attr) rep
    ]
    [
      if reduce [?1 or ?2] (map [member? goal (string:lower-case ?)] attr)
      [
        set rep lput (map [cut ? 30] attr) rep
      ]
    ]
  ]
  ifelse not empty? rep
  [
    set rep sort-by [first ?1 < first ?2] (lput "All" rep)
    let rep2 user-one-of (word "Topics that adjust to your search: " goal) rep
    ifelse rep2 = "All"
    [report topics with [member? (word ID) (map [first ?] rep)]]
    [report one-of topics with [(word ID) = (first rep2)]]
  ]
  [
    report nobody
  ] 
end


; Lista los t�picos seleccionados en el control de informaci�n
to list-topics [t]
  let mes []
  foreach (sort-by [ ([(word ID)] of ?1) < ([(word ID)] of ?2)] t)
  [
    ask ?
    [ 
      let ss ""
      let att (map [(word last ?)] table:to-list topic_attributes)
      ifelse att = []
      [ set ss (word ID ": (" TType")")]
      [ set ss (word ID ": (" TType") " reduce [(word ?1 " | " ?2)] att)]
      if length ss >  60  [ set ss (word (substring ss 0 (min (list 60 (length ss)))) "...")]
      set mes lput ss mes
    ]
  ]
  let rep user-one-of "List of topics:" mes
  ;show rep
end

; Procedimiento de selecci�n de familias
to Select-family
  ask (Selected-Family with [not hidden?]) [set selected? true]
  ask (topics with [selected?])
  [
    ifelse any? selectors with [parent = myself]
    [ 
      ; si ya estaba seleccionado no hacer nada 
    ]
    [ ; si no estaba seleccionado
      ;se a�ade un selector para �l
      hatch-selectors 1
      [ 
        set parent myself
        set size 1.5 * [size] of myself
        set color lput 120 extract-rgb selected-color
        setxy [xcor] of myself [ycor] of myself
        set label ""
      ]
    ]
  ]
end

; Deselecciona una familia
to UnSelect-family
  ask edges [die]
  let temp Selected-Family with [not hidden?]
  ask selectors with [member? parent temp]  [die]
  ask temp [set selected? false]
end

; Devuelve el agentset de una familia
to-report Selected-Family
  ifelse Family = "All" [report topics]
  [ifelse Family = "Visible" [report T-Visible]
    [ifelse Family = "Selected" [report topics with [selected?]]
      [ifelse Family = "Fixed" [report topics with [fixed?]]
        [report topics with [ttype = Family]]]]]
end

to-report Hyper? [t]
  report member? t Hyper
end

to-report rels
  report (link-set relations drelations qrelations)
end

to-report my-rels
  report (link-set my-relations my-in-drelations my-out-drelations my-in-qrelations my-out-qrelations)
end

to-report my-neighbors
  report (turtle-set relation-neighbors in-drelation-neighbors out-drelation-neighbors in-qrelation-neighbors out-qrelation-neighbors)
end


to-report rel-neighbor? [t]
  report (relation-neighbor? t) or (in-qrelation-neighbor? t) or (out-qrelation-neighbor? t) or (in-drelation-neighbor? t) or (out-drelation-neighbor? t)
end

to refresh-visible
  set T-visible topics with [not hidden?]
  set vrelations rels with [not hidden?]
end

to unfix
  ask pins [die]
  ask topics [set fix? false]
end
@#$#@#$#@
GRAPHICS-WINDOW
298
29
1231
578
20
11
22.522
1
12
1
1
1
0
0
0
1
-20
20
-11
11
0
0
0
ticks
30.0

BUTTON
814
10
869
55
Layout
layout
T
1
T
OBSERVER
NIL
Y
NIL
NIL
1

SLIDER
18
387
156
420
spring-constant
spring-constant
0
1
0.7
.01
1
NIL
HORIZONTAL

SLIDER
18
354
156
387
spring-length
spring-length
0
MaxSpringLength
5
.01
1
NIL
HORIZONTAL

SLIDER
18
420
156
453
repulsion-constant
repulsion-constant
0
MaxRepulsionConstant
1.5
.0001
1
NIL
HORIZONTAL

BUTTON
225
154
280
220
NIL
refresh
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
105
187
225
220
Zoom
Zoom
0
200
76
1
1
%
HORIZONTAL

SWITCH
142
220
280
253
Show-all-relations?
Show-all-relations?
0
1
-1000

SWITCH
19
220
142
253
Hide-Orphans?
Hide-Orphans?
0
1
-1000

BUTTON
1105
10
1231
55
Multiple Selection
Select
T
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
523
10
605
55
Search Topic
search
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
156
387
279
420
radius
radius
0
5
2
.01
1
NIL
HORIZONTAL

SLIDER
156
354
279
387
tension
tension
0
30
10
.1
1
NIL
HORIZONTAL

INPUTBOX
19
154
105
220
selected-color
15
1
0
Color

SWITCH
18
321
114
354
Fix?
Fix?
0
1
-1000

INPUTBOX
4
161
298
301
Query-Path
P: 
1
0
String

BUTTON
186
154
241
187
Launch!
query
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
169
321
224
354
V-Sort
sort-vertical
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
224
321
279
354
H-Sort
sort-horizontal
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
105
43
233
88
Family
Family
"All" "Visible" "Selected" "Fixed" "Provincia" "Canton" "Parroquia" "Sensibilidad" "Inmaterial" "Herramientas" "UsoSimbolico" "Alcance" "Periodicidad" "Tecnica" "Producto" "Preparativo" "Elemento" "ElementosSig" "Ambito" "Subambito" "DetalleSubambito" "Lengua" "Comunidad"
0

BUTTON
114
321
169
354
R-Sort
Sort-circular
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
114
10
179
43
Add Data...
add-data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
35
159
90
192
Build
Crea-esquema
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
189
159
261
192
Show/Hide
show-hide-scheme
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
131
154
186
187
Build P
SelectQ
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
92
159
187
192
Layout Schema
LayoutQ
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
76
154
132
187
Add P
New-query
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
4
301
59
334
Export
export
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
5
334
299
494
12

BUTTON
242
301
298
334
Clear
Clear-query
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
241
154
298
187
New Q
new-query-set
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
59
301
120
334
Q-Import
Import-query
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
60
494
299
527
Threshold
Threshold
0
100
25
1
1
NIL
HORIZONTAL

BUTTON
5
494
60
527
Filter
Filter-relations-query threshold
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
19
253
138
286
Show-weights?
Show-weights?
1
1
-1000

BUTTON
120
301
220
334
Show Query
Show-Query
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
298
10
394
55
Active Browser
Active-menu \"browser\"
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
4
43
105
88
Action
Action
"Show" "Hide" "Expand" "Leave" "Fix" "List" "Select" "Deselect" "Show Labels" "Hide Labels"
5

BUTTON
233
43
298
88
Execute!
Execute
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
869
10
962
55
Layout-mode
Layout-mode
"Spring" "ARF" "ARF Weighted" "ARF QWeighted" "Spring + ARF" "Hyp" "Spring + Hyp" "Hyp + ARF" "Circle" "Radial" "C. Bipartite"
1

BUTTON
59
10
114
43
New
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
4
88
59
121
Visual...
Buttons-Visual
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
116
88
173
121
Queries...
Buttons-Queries
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
59
88
116
121
Schema...
Buttons-Schema
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
394
10
490
55
Active Data
Active-menu \"data\"
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
179
10
234
43
Save...
save:nlg-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
4
10
59
43
Files ->
Buttons-toggle (list \"New\" \"Add Data...\" \"Save...\")
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
64
122
242
153
 ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ \nClick here for extended options
12
105.0
1

BUTTON
105
154
226
187
Customize Topic Label
set-customized-label
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
156
420
279
453
Gravity
Gravity
0
1
0.31
.01
1
NIL
HORIZONTAL

SLIDER
44
157
154
190
Start-Interval
Start-Interval
minTimeline
maxTimeline
1348
1
1
NIL
HORIZONTAL

SLIDER
154
157
264
190
End-Interval
End-Interval
minTimeline
maxTimeline
1348
1
1
NIL
HORIZONTAL

BUTTON
44
190
99
223
|<
Start-Timeline
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
99
190
154
223
<
play-back
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
154
190
209
223
>
Play
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
209
190
264
223
>|
End-timeline
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
44
223
154
256
Visualize
Visualize
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
173
88
231
121
Timeline...
Buttons-Timeline
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
154
223
264
256
Speed
Speed
0
100
6
1
1
NIL
HORIZONTAL

SWITCH
137
253
280
286
Show-relation-labels?
Show-relation-labels?
1
1
-1000

CHOOSER
4
162
96
207
Choose-Measure
Choose-Measure
"degree" "in-degree" "out-degree" "Clust.Coef" "rank" "Shimbel" "eccentricity" "closeness"
4

BUTTON
231
88
298
121
Measures...
Buttons-Measures
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
4
207
298
448
Plot
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

BUTTON
243
162
298
207
Plot
plot-measure
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
149
162
243
207
Plot-Type
Plot-Type
"Histogram" "Resize Topics" "X versus Y"
1

BUTTON
95
162
150
207
Compute
Compute-measure
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
4
448
159
481
measure-only-visible?
measure-only-visible?
0
1
-1000

MONITOR
989
10
1075
55
Processing...
Processing
2
1
11

SWITCH
19
286
280
319
Topic-Information?
Topic-Information?
0
1
-1000

BUTTON
5
529
70
562
Consolidate
consolidate
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
644
10
734
43
debug?
debug?
1
1
-1000

@#$#@#$#@
# About Topics Navigator

![Logo](file:my_Logo.png)

**Topics Navigator** is a prototype model to experiment with some uses of _graphs_ and _hypergraphs_ for **storing**, **recovering** and **analyzing** _**connected information**_.

> Note: In this document the terms **topic** and **Node** are used with the same meaning, and so with the terms **relation** and **edge**.

#### Modules and Extensions

In order to work, this NetLogo Model requires the next extensions:

  * _Table_: to manage the attributes of the nodes and edges.
  * _Goo_: to provide a more compact and clean interface.
  * _Pathdir_: to facilitate the access to local files.
  * _Bitmap_: only to show the logo in the main screen of the navigator.
  * _Network_: to manage some functions on Measures module.
  * _String_: for several string manipulation.

The code is divided into several source files:

  * _Main Code_: Abstract layer to manipulate topics and relations.
  * _Visual.nls_: contains the procedures in charge of visual features (layouts of graphs, for example).
  * _Measures.nls_: contains some general procedures for measuring graphs.
  * _Files.nls_: contains the procedures for exporting information.
  * _Query.nls_: contains all the procedures for Schema and Queries operations.
  * _Auxiliar.nls_: for procedures not contained in the other ones.
  * _ContextMenu.nls_: contains the procedures for Context Menu features.
  * _GFiles.nls_: procedures to read/save NLG files.
  * _Creation.nls_: procedures to Data manipulation (add/remove nodes/edges).
  * _Timeline.nls_: procedures to work with time dependent graphs.

The model is an "in development" work... so it is for sure full of errors, and a lot of things could be done better and more robustly. It has been growing from an "ad hoc" primitive tools to manage some connected databases and it lacks of a long term project, that is the reason (and the limitations of the author) there is no perfect matching between all of its pieces.

## Graphs and Hypergraphs

Informally, a **graph** is a ser of objects called **vertices** or **nodes** connected by **linkss** called **edges** or **arcs**, that allow to represent binary relations between elements of the set. Usually, a graph can be represented by a set of points joined by segments in tha plane.

From a practical point of view, the graphs allow us to study the relationships between units that interact with each other. For example, a network of concepts in a domain can be represented and studied by a graph in which the vertices represent the various concepts that are studied and the edges represent connections (which are considered appropriate in the domain, for example, synonymy, antonymy, comes from, etc.). Virtually any problem can be represented by a graph, and their study goes beyond the various areas of sciences and social sciences.

Depending on how we consider the edge joining the vertices, the graphs can be classified as **directed** (when the edge starts from one of the vertices and reaches the other, ie the role of both nodes in the relationship is not same) or **undirected** (where the edge simply connect the vertices together). Typically, the directed edges are represented as an arrow (as seen in the figure below) and the undirected ones as segments.
 
![Fig.1](file:.\doc\fig1.jpg)

A **path** in a graph is a sequence of vertices such that from each of them there is an edge to the next vertex. In the figure below we highlight a path joining the vertices 3 and 4.

![Fig.2](file:.\doc\fig2.jpg)

Additionally, you can consider **weights** on the edges that may indicate some kind of feature of it (length, flow of information, connection strength, etc.).. In this case, we say that the graph is weighted. 

Sometimes the kind information that relates the nodes not just connect the vertices in a binary way, ie, two by two, but the relationships are between larger sets of vertices. For example, suppose we have a network of tourism in which tourists consider traveling and visiting areas, in this case, relate only that tourist v1 visited the area v2 may be insufficient because it depends on the year in which he did it, in this case we introduce a third type of vertex, temporal, and the relationship is between the three simultaneously: the tourist v1 visited the area v2 in the year v3.

![Fig.3](file:.\doc\fig3.jpg)
 
In this case, we have more expressive possibilities, for example: v1 and v4 tourists visited the area v2 together in the year v3. Of course, the repersentation of this kind of relationships is more complicated and we cannot simply represent segments by connecting the dots.

This type of graphs with stronger edges are called **hypergraphs**, and the edges of this type **hyperedges**.

Because representations of hyperedges are much more complex (and confusing) from a technical point of view, and as a demonstration of the versatility you have graphs, we will see there is no need to use hyperedges although our relations are not binary, it is possible to "create" intermediate types of vertices, representing those hiperaristas (artificial vertices, not belonging to the real world being modelled) so that if v1, v2 and v3 are vertices that share an hyperedge, we create the vertex h1 and binary edges linking it to those vertices. Thus we don't lose the information we want to store and continue to work with graphs without having to develop theoretical tools or new representation.

![Fig.4](file:.\doc\fig4.jpg)

In the previous figure we see how an hyperedge linking the three circular nodes is represented by an independent vertex (gray, and highlighted in red) that links together. 

## Playing around with the tool

The look of the interface (with the subinterfaces hidden) of the Navigator is:

![Fig.5](file:.\doc\TopicsNavigator5%20interface.jpg)

Here we introduce some of the main features it provides:

### How it works

This tool can work with hypergraphs that mix edges of several types: undirected, directed and hyperedges in the same hypergraph. 

The hyperedges will be represented by auxiliary nodes connecting all the topics in the hyperedge.

Topics and relations are classified in **Families** or **Types** can hold as many attributes as you want, but some of them are mandatory. See the NLG format file at the end of this document for more information about the structure of the data.

Topics Navigator allows you to **browse** the graph inuitivelly, providing several layouts to represent your connected data. It will also give you contextual menus to **add/modify/erase** information from your graph and **retrieve information** from big graphs by using a kind of **query traversals** to explore automatically the data. In near future, it will provide too different **analysis tools** for your information, as well as **exporters** for other external tools.

### Working with files

The Topic Navigator comes with the usual file options:

  1. **New**: Makes a global reset and unloads everything from memory.
  2. **Add Data...**: Loads an existing NLG file on memory without cleaning the current state. In this way you can load several graphs at a time. In this process, new nodes with the same IDs that nodes in current memory, they will be ignored (but it will add the edges between nodes).
  3. **Save...**: It will save the current graph (in memory graph, not only the visible part) on a file with NLG format.

> Note: Pressing on "Files->" button will show/hide the files menu.

### Contextual Menus

The system provides two different contextual menus acting on the nodes:

![Fig.5](file:.\doc\fig5.jpg)     ![Fig.6](file:.\doc\fig6.jpg)

  1. **Active Browser**: By pressing on this button you can act on every node with the main browser operations you can do over it:
    * **Expand** will show all the nodes connected with it. ![ ](file:.\doc\Expand.jpg) 
    * **Leave** will hide all nodes except this. ![ ](file:.\doc\Leave.jpg) 
    * **Hide** will hide this node from the current representation. ![ ](file:.\doc\hide.jpg)
    * **Select** will select this node. ![ ](file:.\doc\select.jpg)
    * **Fix** will fix this node. ![ ](file:.\doc\Fix.jpg)
    * **Label** will hide/show the label of this node. ![ ](file:.\doc\Label.jpg)
  2. **Active Data**: By pressing on this button you can act on every node with the main data operations you can do over it:
    * **Add Node** will allow to add a new node to the graph. ![ ](file:.\doc\Addnode.jpg) 
    * **Delete Node** will remove this node from the graph. ![ ](file:.\doc\DeleteNode.jpg) 
    * **Add Link** will add a new link between this node and an existing node. ![ ](file:.\doc\AddLink.jpg)
    * **Delete Link** will remove one of the links of this node. ![ ](file:.\doc\DeleteLink.jpg)
    * **Edit** allows to modify the attributes of this node. ![ ](file:.\doc\EditNode.jpg)

### Action on Families

Using the next controls we can act globally on sets of nodes:

![ ](file:.\doc\fig7.jpg)

Together the families (or Nodes Types) defined in the structure of the loaded graph (in its file), there exists the following subset selections:

* **All**: The action will be be applied on all the nodes of the graph (visible or not).
* **Visible**: The action will be applied only on the visible nodes.
* **Selected**: The action will be applied on the selected nodes.
* **Fixed**: The action will be applied on the fixed nodes.

And the actions that can be applied on the selected family are:

* **Show**: The nodes of the family will be shown.
* **Hide**: They will be hidden.
* **Expand**: They will be expanded, showing all the nodes connected to them.
* **Leave**: The selected family will be the only shown.
* **Fix**: They will be fixed.
* **List**: They will be listed on the _Topic Information_ box.
* **Select**: They will be selected.
* **Deselect**: They will be Deselected.
* **Show Label**: Their labels will be shown.
* **Hide Labels**: Their labels will be hidden.

Pressing on **Execute!** button, the action will be applied on the nodes of the selected family.

### Searchs

With the **Search** button you can look for any node in your network by using any piece of information. It will show you all the nodes that have your input string as part of any of theirs attributes. 

Once you have selected the one of your interest, it will be show on the world.

### Layouts

The Navigator provides some useful layouts to represent your graphs:
* **Spring**: It is the layout that comes with NetLogo in the standard distribution.
* **ARF**: Attractive and Repulsion Forces algorithm (http://www.sg.ethz.ch/research/graphlayout).
* **Hyperbolic**: (http://graphics.stanford.edu/papers/munzner_thesis/html/node8.html).
* ... any pair combination of the previous layouts.
* **Circular**
* **Radial**: using as root the node you choose bu clicking on it.
* **Circular Bipartite**: When the graph is a result of a simple query (what implies it is a bipartite graph) this layout can be useful to see the result in a grouped way.

### Modules Operations

Using these buttons:

![ ](file:.\doc\fig8.jpg)

you can access more complex features and controls for analyzing and representing your graph. In the next sections we will give a detailed description on these features.


## Visual options

![ ](file:.\doc\fig9.jpg)

* **Selected-color**: opens a color dialog to choose how the selected nodes will be highlighted.
* **Customize labels**: opens a dialog to choose the attribute to be used as label.
* **Zoom**: Percentage of zoom on the representation (only on topic sizes).
* **Show-weights?**: Decide if the weights of the relations obtained from the queries are shown or not.
* **Hide-Orphans?**: If true, when a node has no visible relations, it will be hidden.
* **Show-all-relations?**: If true, all the relations between visible nodes will be shown.
* **Show-relation-labels?**: If true, the labels (edgetype) of all the relations will be shown.
* **Refresh**: This button will update the representation of the graph when these features are changed.
* **Topic-Information?**: It will show/hide a floating ppiece of information when the mouse is over the topic. It needs **Active Browser** or **Active Data** to be pressed.
* **Fix?**: If true, fixed nodes are not affected by the layout.
* **R-Sort**, **V-Sort**, **H-Sort**: Selected nodes will be arranged radially, vetically, or horizontally.
* **spring-length**, **spring-constant**, **repulsion-constant**: control the _Spring_ and _Hyp_ (Hyperbolical) layouts.
* **tension**, **radius**: control the _ARF_ layout.
* **Gravity**: It produces the nodes to be attracted to the origin. In this way, when there are several islands, they will not be going away in the world.


## The Schema of Connected Information

In order to retrieve information from the graph using queries it is necessary to build previously the **Graph Schema** from the loaded data. Since this schema is obtained from the data you must do it after loading the graph and after main changes in your graph. 

![ ](file:.\doc\fig12.jpg)

Obtaining this schema is really simple from the data:
* One _Schema-Node_ is added for every _NodeType_ of your graph (and every hyperedge) and 
* Two Scheme-Nodes are _connected_ if there exists at least one connection between nodes of these types in your graph.

Press on **Build** button to generate ths schema. **Layout Schema** to represent it in a mor confortable way, and **Show/Hide** to show/hide the schema.

![ ](file:.\doc\fig14.jpg)

Remember that if you change the data graph (node or edges) probably you must generate the schema again.


## Retrieving Information: Query Traversals

Just as in the _Relational DataBases_ (**RDB**) there is relational language that allows us to have a query system (**SQL** language) in the _Graph DataBases_ (**GDB**) is in development a set of languages that allow querying and graph transformations. The **traversals** are becoming the most common and useful operations for querying these databases, becoming so important to the GDB as the SQL for the RDB.

![ ](file:.\doc\fig18.jpg)

A **traversal* is formed by 3 main parts:
  1. A **path** on the schema that indicates the traversal that has to cover the query to connect the two ends of the path. In the Topics Navigator, this path is achieved as the sequence of nodes that comprise it.
  2. A **constraint** for each node in the path. That is, a condition that must verify the nodes such that, even existing a path as indicated in 1 that passes through them, is considered a valid path. This is what is called **local conditions** affecting each node individually.
  3. A **global constraint** that can link the different nodes of a valid path and that is the last filter that must pass to be considered as a valid result for the query.

The only mandatory part of a query is part 1, the other two are optional and provide additional filtering processes. An example that uses the 3 parts is the following query:

![ ](file:.\doc\fig13.jpg)
>Connect Obra with Clas.Calvo if there exists a path verifying: 
>(&1) Obra -> 
>(&2) Localización -> 
>(&3) Jornada, verifying nombre = "I",  -> 
>(&4) Localización -> 
>(&5) Acto -> 
>(&6) Verbo -> 
>(&7) Clas.Calvo -> 
>Where &2 = &4

Let us see step by step how to generate the previous query:
  1. To start a new compound query (you can simultaneously launch multiple queries of the above type) press the button **New Q**. Then click on **Build P** to start creating a single query. With this button pressed will go on clicking on the nodes of the path to be constructed, for each selected node a popup dialog appears where you can enter the node's local constraint, which may use the attributes of the topic. For example, in the above query will leave all blank except for the node of type _Jornada_, for which we introduce the text of the condition:
![ ](file:.\doc\fig15.jpg)
  2. Once we have finished generating the path, we must add it to the current compund query by clicking the **Add P** and then the system will ask for the global constraint. To reference the various nodes of the path the system provides a numeric label for each one, _&n_ in the order they are generated. If, as in the present case, we look for nodes 2 and 4 in the path to be the same, just enter the following text in the dialog:

![ ](file:.\doc\fig16.jpg)
 
If you want to see in a more natural format the query generated just click on the button **Show Query**:
 
The result is shown below, and it is achieved by launching the query with the **Launch!** button:

![ ](file:.\doc\fig17.jpg)

Keep in mind that if you perform another query without clearing previous one the reult is added (and then the weights of the possible new connections if they exist) to the existing one. If you want to run the query on a clean state, you must press the **"Clear**.

The visualization of the results of the query is performed automatically after the execution of the query. Using the standard **Layout** button you can control the representation of the resulting graph. 

### Exporting

Topics Navigator allows (right now) three exporting methods in the queries (to be chosen after pressing **Export** button):
  1. **Query Format**: Saves the query (not the result) in a text file to be retrieved later.
  2. **Table for Excel CSV (bipartite)**: Exports a table in CSV format in which nodes of origin / target are distributed by rows / columns storing the corresponding edge weights. Tools like _Excel_ can import it in order to obtain more representations for easy interpretation of the results of the query.
  3. **CSV Files for Gephi**: Export nodes and edges in two CSV files that are ready to be imported into _Gephi_ for a higher quality representation of the graph and to perform some standard measures of network theories.


## Networks with Time dependency

In order to work with graphs that change with time, Topics Navigator include a module to manage relations with an special attribute (**Timeline(X)**) storing time intervals where the relation is "active".

Once you have this attribute in your relations, you can store theses active intervals as a list of pair lists (see the example at the end of this document):

> [... [t<sub>1</sub> t<sub>2</sub>] ...], with t<sub>1</sub> <= t<sub>2</sub>

The controls under the module **Timeline** will allow you to obtain a dinamical representation of your graph in time:

![ ](file:.\doc\fig11.jpg)

* **Start-Interval - End-Interval**: Defines the active interval inside the total time of live of your graph (mnimum and maximun times are extracted automatically from the file)
* **Visualize**: Show the active relations (and associated nodes) regarded to the actve interval.
* **Play (>)**, **Play-Back (<)**: These buttons moves automatically the active interval forward and backward in an endless loop.
* **Speed**: Set the speed of reproduction.
* **|<**, **>|**: moves respectivally to the beginning and the end of the time.

## Measures on graphs

THe system provides some common measures on graphs. For every visible node/topic:

* **Degree**: measures the number of undirected relations connecting the node
* **Indegree**: measures the number of directed relations entering the node
* **Outdegree**: measures the number of directed relations exiting from the node
* **Clustering Coefficient**: A measure of neighborhood indicating the type of environment in which the node situates. A node with low order (degree) may be surrounded by a variety of other nodes, small or large, which has a direct influence on its own centrality and growth potential. A network is assortative or disassortative depending on the similarity of the order (degree) among neighboring nodes, which can be tested by means of Pearson correlation (assortativity coefficient). Neighbor connectivity is the correlation between the order (degree) of nodes and the average order (degree) of their neighbors.
* **Rank**: by applying the Page Rank algorithm.
* **Shimbel Index**: (or Shimbel distance, nodal accessibility, nodality). A measure of accessibility representing the sum of the length of all shortest paths connecting all other nodes in the graph. The inverse measure is also called closeness centrality or distance centrality.
* **Eccentricity**: A measure of farness based on the number of links needed to reach the most distant node in the graph.
* **Closeness**:  Closeness can be regarded as a measure of how long it will take to spread information from s to all other nodes sequentially.

![ ](file:.\doc\fig19.jpg)

Also with this module we can give several representations of these measures:

* **Histogram**
* **Resizing** the nodes according to their values.
* **X Versus Y** to show relations between measures (the system ask for the second measure).

## NLG Format File

The format for the **NLG** (NetLogo Graph) files are strictly the following:

    % Types of Nodes of the graph.
    <NodesTypes>
    Name(string)   color   shape(string)   size
    ...
    <EndNodesTypes>
    
    % Types of Edges: Name color shape thickness Type(0: Undirected, 1: Directed, 2-Hyperedge)
    %   if Hyperdege: Name color shape size Type
    <EdgesTypes>
    Name   color   shape   thickness   Type
    ...
    <EndEdgesTypes>
    
    % Nodes of the graph. Structure: ID NodeType Attributes...
    <Nodes>
    "ID(X)"   "NodeType(S)"   "Att1(S)"   "Att2(X)" ...
    id1       "Type1"         "value1"     value2
    ...
    <EndNodes>
    
    % Edges & HyperEdges of the graph.
    <Edges>
    "ID-List(X)"   "EdgeType(S)"   "Attr1(X)"   "Att2(S)" ...
    [id1 id2 id3]  "Type1"          value1      "value2"
    ....
    <EndEdges>

Following you have an example:

    % Types of Nodes of the graph. Structure: Name color shape size
    <NodesTypes>
    "Mammal"    brown  "circle"   1
    "Bird"      blue   "square"   1	
    "Reptile"   red    "triangle" 1
    <EndNodesTypes>
    
    % Types of Edges: Name color shape thickness Type(0: Undirected, 1: Directed, 2-Hyperedge)
    %   if Hyperdege: Name color shape size Type
    <EdgesTypes>
    "enemy"      red     "straight"   0      0
    "eats"       blue    "curve1.0"   0.05   1
    "escapes"    green   "curve-3.0"  0.1    1
    "group"      gray    "conector"   0.5    2
    <EndEdgesTypes>
    
    % Nodes of the graph. Structure: ID NodeType Attributes...
    <Nodes>
    "ID(X)"   "NodeType(S)"   "label(S)"
    1         "Mammal"        "Dog"
    2         "Mammal"        "Cat"
    3         "Mammal"        "Horse"
    4         "Bird"          "Sparrow"
    5         "Reptile"       "Crocodile"
    6         "Bird"          "Eagle"
    <EndNodes>
    
    % Edges & HyperEdges of the graph. Structure: [ID-list] EdgeType Attributes...
    % Special Attribute "Timeline"
    <Edges>
    "ID-List(X)"   "EdgeType(S)"   "Peso(X)"   "Timeline(X)"
    [1 2 3]        "group"          1           [[0 20]]
    [4 5]          "group"          1           [[10 20] [130 160]]
    [3 5]          "group"          2           [[20 30]]
    [1 2]          "enemy"          1.0         [[30 50]]
    [2 4]          "eats"           1           [[50 70] [100 120]]
    [3 1]          "enemy"          1           [[60 90] [120 130]]
    [5 1]          "eats"           1           [[70 80]]
    [5 3]          "eats"           1           [[100 120] [130 160]]
    [4 5]          "escapes"        1           [[110 120]]
    [6 4]          "eats"           1           [[190 200]]
    <EndEdges>

some things to be considered in the file:

  * You can **comment lines** (the parser will not consider them) by using the **%** symbol.
  * If you want to explicite the type of data an attribute will storage, you must add in the name:
    * **(S)**: for strings.
    * **(X)**: for evaluable expressions.
  * **NodeTypes** and **EdgeTypes** blocks must be before than **Nodes** and **Edges** blocks.
  * **NodeTypes** and **EdgeTypes** blocks has fixed structure, while **Nodes** and **Edges** only have the first two attributes (_"ID"_ and _"NodeType"_ for Nodes, _"ID-List"_ and _"EdgeType"_ for Edges) fixed, after them you can add as many attributes as yo want.

## About the author

Fernando Sancho Caparrini
_Dpto. Ciencias de la Computación e Inteligencia Artificial_
E.T.S. Ingeniería Informática
Universidad de Sevilla
e-mail: fsancho@us.es
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

abierto
false
0
Rectangle -13791810 true false 0 0 300 225
Circle -1 true false 54 24 42
Polygon -10899396 true false 0 225 90 60 105 75 135 30 180 75 225 60 285 225
Polygon -1 true false 225 60 180 75 225 90
Polygon -6459832 true false 90 90 45 165 105 105
Polygon -6459832 true false 150 105 180 120 225 105 180 90
Rectangle -14835848 true false 0 225 300 300
Circle -1 true false 9 24 42
Circle -1 true false 24 24 42
Circle -1 true false 39 9 42
Polygon -6459832 true false 120 165 30 210 90 210 105 195 120 195 150 180 135 165
Polygon -10899396 true false 240 150 300 165 300 225 255 225
Polygon -1 true false 135 30 105 75 135 60 150 75 150 60 180 75
Rectangle -16777216 false false 0 0 300 300

acto
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 false false -2 -2 302
Line -16777216 false 15 150 285 150

ant
true
0
Polygon -7500403 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7500403 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7500403 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7500403 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7500403 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7500403 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7500403 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7500403 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7500403 true true 249 107 211 147 168 147 168 150 213 150

austin
false
0
Polygon -1 true false 0 240 23 9 112 9 138 171 299 173 296 291 1 291
Polygon -7500403 true true 0 285 30 15 105 15 135 285 105 285 90 165 45 165 30 285
Polygon -7500403 true true 150 180 150 285 225 285 225 180 195 180 195 255 180 255 180 180
Polygon -7500403 true true 300 180 240 180 240 240 285 255 285 270 240 255 240 285 300 285 300 240 255 225 255 210 300 210
Polygon -1 true false 60 135 90 135 75 45 60 45 45 135
Polygon -16777216 false false 150 180 150 285 225 285 225 180 195 180 195 255 180 255 180 180
Polygon -16777216 false false 240 180 300 180 300 210 255 210 255 225 300 240 300 285 240 285 240 255 285 270 285 255 240 240
Polygon -16777216 false false 30 15 105 15 135 285 105 285 90 165 45 165 30 285 0 285
Polygon -16777216 false false 60 45 75 45 90 135 45 135

book
false
0
Polygon -6459832 true false 30 195 150 255 270 135 150 75
Polygon -6459832 true false 30 135 150 195 270 75 150 15
Polygon -6459832 true false 30 135 30 195 90 150
Polygon -1 true false 39 139 39 184 151 239 156 199
Polygon -1 true false 151 239 254 135 254 90 151 197
Line -7500403 true 150 196 150 247
Line -7500403 true 43 159 138 207
Line -7500403 true 43 174 138 222
Line -7500403 true 153 206 248 113
Line -7500403 true 153 221 248 128
Polygon -1 true false 159 52 144 67 204 97 219 82
Polygon -16777216 false false 150 15 30 135 30 195 150 255 270 135 255 120 255 90 270 75

box 2
false
0
Polygon -7500403 true true 150 285 270 225 270 90 150 150
Polygon -13791810 true false 150 150 30 90 150 30 270 90
Polygon -13345367 true false 30 90 30 225 150 285 150 150

bread
false
0
Polygon -16777216 true false 140 145 170 250 245 190 234 122 247 107 260 79 260 55 245 40 215 32 185 40 155 31 122 41 108 53 28 118 110 115 140 130
Polygon -1 true false 135 151 165 256 240 196 225 121 241 105 255 76 255 61 240 46 210 38 180 46 150 37 120 46 105 61 47 108 105 121 135 136
Polygon -1 true false 60 181 45 256 165 256 150 181 165 166 180 136 180 121 165 106 135 98 105 106 75 97 46 107 29 118 30 136 45 166 60 181
Polygon -16777216 false false 45 255 165 255 150 180 165 165 180 135 180 120 165 105 135 97 105 105 76 96 46 106 29 118 30 135 45 165 60 180
Line -16777216 false 165 255 239 195
Line -16777216 false 150 180 225 120
Line -16777216 false 180 135 255 75
Line -16777216 false 180 120 255 60
Line -16777216 false 165 105 240 45
Line -16777216 false 105 105 180 45

building institution
false
0
Rectangle -7500403 true true 0 60 300 270
Rectangle -16777216 true false 130 196 168 256
Rectangle -16777216 false false 0 255 300 270
Polygon -7500403 true true 0 60 150 15 300 60
Polygon -16777216 false false 0 60 150 15 300 60
Circle -1 true false 135 26 30
Circle -16777216 false false 135 25 30
Rectangle -16777216 false false 0 60 300 75
Rectangle -16777216 false false 218 75 255 90
Rectangle -16777216 false false 218 240 255 255
Rectangle -16777216 false false 224 90 249 240
Rectangle -16777216 false false 45 75 82 90
Rectangle -16777216 false false 45 240 82 255
Rectangle -16777216 false false 51 90 76 240
Rectangle -16777216 false false 90 240 127 255
Rectangle -16777216 false false 90 75 127 90
Rectangle -16777216 false false 96 90 121 240
Rectangle -16777216 false false 179 90 204 240
Rectangle -16777216 false false 173 75 210 90
Rectangle -16777216 false false 173 240 210 255
Rectangle -16777216 false false 269 90 294 240
Rectangle -16777216 false false 263 75 300 90
Rectangle -16777216 false false 263 240 300 255
Rectangle -16777216 false false 0 240 37 255
Rectangle -16777216 false false 6 90 31 240
Rectangle -16777216 false false 0 75 37 90
Line -16777216 false 112 260 184 260
Line -16777216 false 105 265 196 265

c mortero
false
0
Rectangle -1 true false 0 0 300 300
Polygon -1 true false 60 150 30 150 30 165 45 165 45 195 60 225 90 255 150 270 210 255 240 225 255 195 255 165 270 165 270 150
Polygon -1 true false 75 150 30 105 15 105 15 90 45 60 60 60 60 75 150 150
Line -16777216 false 45 120 75 90
Line -16777216 false 105 150 125 128
Polygon -16777216 false false 30 150 30 165 45 165 45 195 60 225 90 255 150 270 210 255 240 225 255 195 255 165 270 165 270 150
Polygon -16777216 false false 15 90 45 60 60 60 60 75 150 150 75 150 30 105 15 105
Rectangle -16777216 false false 0 0 300 300

c petals
false
0
Rectangle -1 true false 0 0 300 300
Circle -1 true false 117 12 66
Circle -1 true false 116 221 67
Circle -1 true false 41 41 67
Circle -1 true false 11 116 67
Circle -1 true false 41 191 67
Circle -1 true false 191 191 67
Circle -1 true false 221 116 67
Circle -1 true false 191 41 67
Circle -16777216 false false 116 11 67
Circle -16777216 false false 41 41 67
Circle -16777216 false false 11 116 67
Circle -16777216 false false 41 191 67
Circle -16777216 false false 116 221 67
Circle -16777216 false false 191 191 67
Circle -16777216 false false 221 116 67
Circle -16777216 false false 191 41 67
Circle -1 true false 60 60 180
Rectangle -16777216 false false 0 0 300 300

c sun
false
0
Rectangle -1 true false 0 0 300 300
Circle -1 true false 75 75 150
Polygon -1 true false 300 150 240 120 240 180
Polygon -1 true false 150 0 120 60 180 60
Polygon -1 true false 150 300 120 240 180 240
Polygon -1 true false 0 150 60 120 60 180
Polygon -1 true false 60 195 105 240 45 255
Polygon -1 true false 60 105 105 60 45 45
Polygon -1 true false 195 60 240 105 255 45
Polygon -1 true false 240 195 195 240 255 255
Circle -16777216 false false 75 75 150
Polygon -16777216 false false 120 60 150 0 180 60
Polygon -16777216 false false 60 105 45 45 105 60
Polygon -16777216 false false 60 120 0 150 60 180
Polygon -16777216 false false 60 195 45 255 105 240
Polygon -16777216 false false 120 240 180 240 150 300
Polygon -16777216 false false 195 240 240 195 255 255
Polygon -16777216 false false 240 180 300 150 240 120
Polygon -16777216 false false 240 105 255 45 195 60
Rectangle -16777216 false false 0 0 300 300

calvo
false
0
Polygon -1 true false 10 257 7 65 39 34 137 37 144 76 188 81 196 141 299 143 298 210 273 291 46 291
Polygon -7500403 true true 150 285 150 90 180 90 180 285
Polygon -7500403 true true 210 150 240 150 255 240 270 150 300 150 270 285 240 285
Polygon -16777216 false false 150 90 180 90 180 285 150 285
Polygon -16777216 false false 210 150 240 150 255 240 270 150 300 150 270 285 240 285
Polygon -7500403 true true 135 45 105 90 60 90 45 105 45 225 60 240 105 240 120 285 45 285 15 255 15 75 45 45
Polygon -16777216 false false 135 45 45 45 15 75 15 255 45 285 120 285 105 240 60 240 45 225 45 105 60 90 105 90

cargo
false
0
Circle -1 true false 45 45 210
Circle -16777216 false false 45 45 210
Rectangle -6459832 true false 135 75 165 225
Rectangle -6459832 true false 105 105 195 120
Circle -6459832 true false 26 57 67
Circle -6459832 true false 206 57 67
Circle -16777216 false false 26 56 67
Circle -16777216 false false 206 56 67
Polygon -16777216 false false 135 75 165 75 165 105 195 105 195 120 165 120 165 225 135 225 135 120 105 120 105 105 135 105
Circle -6459832 true false 116 222 67
Circle -16777216 false false 116 221 67

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
true
8
Circle -11221820 true true 0 0 300
Circle -1 true false 30 30 240
Line -7500403 false 150 30 150 270
Line -7500403 false 30 150 270 150

clasificacion milagro
false
0
Rectangle -955883 true false 0 0 300 300
Circle -13345367 true false 29 44 242
Circle -1 true false 44 59 212
Circle -13345367 true false 103 13 92
Circle -13345367 true false 13 178 92
Circle -13345367 true false 193 178 92
Rectangle -16777216 false false 0 0 300 300

clock
true
0
Circle -7500403 true true 30 30 240
Polygon -16777216 true false 150 31 128 75 143 75 143 150 158 150 158 75 173 75
Circle -16777216 true false 135 135 30

conector
true
0
Circle -7500403 true true 180 90 120
Circle -16777216 false false 180 90 120
Circle -7500403 true true 0 90 120
Circle -16777216 false false 0 90 120
Circle -7500403 true true 90 0 120
Circle -16777216 false false 90 0 120
Circle -7500403 true true 90 180 120
Circle -16777216 false false 90 180 120
Circle -7500403 true true 60 60 180

crate
false
0
Rectangle -7500403 true true 45 45 255 255
Rectangle -16777216 false false 45 45 255 255
Rectangle -16777216 false false 60 60 240 240
Line -16777216 false 180 60 180 240
Line -16777216 false 150 60 150 240
Line -16777216 false 120 60 120 240
Line -16777216 false 210 60 210 240
Line -16777216 false 90 60 90 240
Polygon -7500403 true true 75 240 240 75 240 60 225 60 60 225 60 240
Polygon -16777216 false false 60 225 60 240 75 240 240 75 240 60 225 60

cuad
true
0
Circle -7500403 false true 13 13 272
Rectangle -1 true false 60 0 240 300

date
false
0
Rectangle -1 true false 30 45 270 240
Polygon -16777216 true false 105 75 180 75 180 105 150 195 120 195 150 105 105 105
Rectangle -16777216 false false 30 45 270 240
Rectangle -1 true false 210 15 240 75
Rectangle -1 true false 60 15 90 75
Rectangle -16777216 false false 60 15 90 75
Rectangle -16777216 false false 210 15 240 75

deletelink
false
2
Circle -10899396 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -10899396 true false 135 75 225 75 225 105 135 105
Rectangle -7500403 true false 75 105 75 120
Polygon -10899396 true false 60 105 75 90 225 210 210 225
Circle -10899396 true false 45 75 60
Circle -10899396 true false 180 180 60

deletenode
false
2
Circle -10899396 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -10899396 true false 135 75 225 75 225 105 135 105
Rectangle -7500403 true false 75 105 75 120
Circle -10899396 true false 75 135 90

disposicion
false
0
Circle -7500403 true true 135 43 122
Circle -7500403 true true 43 43 122
Polygon -7500403 true true 255 120 240 150 210 180 180 210 150 240 146 135
Line -7500403 true 150 209 151 80
Polygon -7500403 true true 45 120 60 150 90 180 120 210 150 240 154 135

expand
false
0
Circle -13345367 true false 0 0 300
Circle -1 true false 15 15 270
Circle -13345367 true false 118 118 62
Circle -13345367 true false 225 135 28
Circle -13345367 true false 180 210 28
Circle -13345367 true false 90 210 28
Circle -13345367 true false 43 133 32
Circle -13345367 true false 88 58 32
Circle -13345367 true false 178 58 32
Line -13345367 false 150 150 195 75
Line -13345367 false 150 150 240 150
Line -13345367 false 150 150 195 225
Line -13345367 false 150 150 105 225
Line -13345367 false 150 150 60 150
Line -13345367 false 150 150 105 75

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

fantastico
true
0
Circle -13345367 true false 29 29 242
Circle -1 true false 44 44 212
Circle -13345367 true false 103 -2 92
Circle -13345367 true false 13 163 92
Circle -13345367 true false 193 163 92

fecha
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 45 105 75 135
Rectangle -16777216 true false 90 105 120 135
Rectangle -16777216 true false 135 105 165 135
Rectangle -16777216 true false 180 105 210 135
Rectangle -16777216 true false 45 150 75 180
Rectangle -16777216 true false 90 150 120 180
Rectangle -16777216 true false 135 150 165 180
Rectangle -16777216 true false 180 150 210 180
Rectangle -16777216 true false 225 105 255 135
Rectangle -16777216 true false 225 150 255 180
Polygon -16777216 true false 203 45 233 45 218 60 233 75 203 90 203 75 218 75 203 60 203 45
Polygon -16777216 true false 135 60 150 45 150 75 150 90 135 90 135 60
Rectangle -16777216 false false 30 30 270 270
Polygon -16777216 true false 165 45 195 45 180 60 195 75 165 90 165 75 180 75 165 60 165 45
Polygon -16777216 true false 240 60 255 45 255 75 255 90 240 90 240 60

fire
false
0
Polygon -955883 true false 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -1184463 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282
Polygon -16777216 false false 104 283 58 248 40 211 30 156 36 109 67 145 71 108 83 70 110 29 126 54 148 12 169 42 179 112 195 56 216 91 228 124 228 201 255 157 256 201 239 262 212 279 163 286 125 284 122 282

fix
false
2
Circle -13345367 true false 0 0 300
Circle -1 true false 14 14 272
Circle -13345367 true false 135 66 102
Circle -1 true false 153 69 82
Polygon -13345367 true false 150 135 105 180 60 240 119 196 165 150

folio
false
0
Rectangle -1 true false 30 0 270 300
Rectangle -16777216 true false 45 15 105 30
Rectangle -16777216 true false 60 45 240 60
Rectangle -16777216 true false 60 75 195 90
Rectangle -16777216 true false 60 105 210 120
Rectangle -16777216 true false 45 135 105 150
Rectangle -16777216 true false 60 165 210 180
Rectangle -16777216 true false 60 195 195 210
Rectangle -16777216 true false 45 225 90 240
Rectangle -16777216 true false 60 255 240 270
Rectangle -16777216 false false 30 0 270 300

hex
false
0
Polygon -7500403 true true 0 150 75 30 225 30 300 150 225 270 75 270

hide
false
0
Circle -13345367 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -13345367 false false 60 90 90 60 150 120 210 60 240 90 180 150 240 210 210 240 150 180 90 240 60 210 120 150

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

hyper-tipo
false
15
Circle -7500403 true false 180 90 120
Circle -7500403 true false 0 90 120
Circle -7500403 true false 90 180 120
Circle -7500403 true false 60 60 180
Circle -7500403 true false 90 0 120
Circle -1 true true 105 15 90
Circle -1 true true 195 105 90
Circle -1 true true 15 105 90
Circle -1 true true 105 195 90
Circle -1 true true 75 75 150

informer
false
0
Rectangle -16777216 true false 300 0 315 300

institucion
false
0
Polygon -6459832 true false 90 90 75 300 225 300 210 90
Rectangle -16777216 true false 165 105 195 135
Polygon -955883 true false 90 90 225 90 150 60 75 90
Rectangle -16777216 true false 105 150 135 180
Circle -16777216 true false 120 210 60
Rectangle -16777216 true false 120 240 180 300
Line -16777216 false 90 90 210 90
Polygon -16777216 false false 75 300 90 90 75 90 150 60 225 90 210 90 225 300

jornada
false
0
Polygon -7500403 true true 45 45 60 75 135 90 135 240 120 255 105 255 90 240 90 225 30 225 45 270 75 300 150 300 180 270 180 90 240 75 255 45
Polygon -16777216 false false 30 225 90 225 90 240 105 255 120 255 135 240 135 90 60 75 45 45 255 45 240 75 180 90 180 270 150 300 75 300 45 270

label
false
1
Circle -13345367 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -13345367 true false 45 195 45 165 60 180 75 195 90 180 75 180 90 165 105 180 120 180 135 180 150 165 165 180 180 180 180 195 195 195 195 210 180 210 180 195
Circle -13345367 false false 104 59 153

labios
false
0
Polygon -1 true false 0 150 105 75 150 90 195 75 300 150 195 210 105 210
Line -16777216 false 0 150 300 150
Polygon -16777216 false false 0 150 105 75 150 90 195 75 300 150 195 210 105 210

leave
false
2
Circle -13345367 true false 0 0 300
Circle -1 true false 15 15 270
Circle -7500403 true false 225 135 28
Circle -7500403 true false 180 210 28
Circle -7500403 true false 90 210 28
Circle -7500403 true false 43 133 32
Circle -7500403 true false 88 58 32
Circle -7500403 true false 178 58 32
Circle -13345367 true false 118 118 62

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

llave inglesa
false
0
Polygon -1 true false 270 90 165 195 165 225 150 240 120 255 105 240 105 225 135 210 135 195 120 180 240 60
Polygon -1 true false 225 45 120 150 90 150 75 165 60 195 75 210 90 210 105 180 120 180 135 195 255 75
Polygon -16777216 false false 225 45 120 150 90 150 75 165 60 195 75 210 90 210 105 180 120 180 135 195 135 210 105 225 105 240 120 255 150 240 165 225 165 195 270 90

logs
false
0
Polygon -7500403 true true 15 241 75 271 89 245 135 271 150 246 195 271 285 121 235 96 255 61 195 31 181 55 135 31 45 181 49 183
Circle -1 true false 132 222 66
Circle -16777216 false false 132 222 66
Circle -1 true false 72 222 66
Circle -1 true false 102 162 66
Circle -7500403 true true 222 72 66
Circle -7500403 true true 192 12 66
Circle -7500403 true true 132 12 66
Circle -16777216 false false 102 162 66
Circle -16777216 false false 72 222 66
Circle -1 true false 12 222 66
Circle -16777216 false false 30 240 30
Circle -1 true false 42 162 66
Circle -16777216 false false 42 162 66
Line -16777216 false 195 30 105 180
Line -16777216 false 255 60 165 210
Circle -16777216 false false 12 222 66
Circle -16777216 false false 90 240 30
Circle -16777216 false false 150 240 30
Circle -16777216 false false 120 180 30
Circle -16777216 false false 60 180 30
Line -16777216 false 195 270 285 120
Line -16777216 false 15 240 45 180
Line -16777216 false 45 180 135 30

lugar
false
0
Circle -13345367 true false 0 0 300
Polygon -13345367 true false 165 60 195 120 150 75 135 75 90 105 60 165 75 195 120 210 165 195 210 150 225 105 225 90 120 180 90 120
Polygon -6459832 true false 30 120 135 135 90 210 75 270 60 165
Polygon -6459832 true false 45 105 60 75 45 45 75 30 135 30 150 45 120 60 135 90 135 120 120 75 105 105 75 105 90 135 30 120 60 105
Polygon -6459832 true false 225 45 210 60 210 90 225 75 225 90 240 75 240 105 225 105 210 90 210 105 180 105 195 135 195 150 180 150 210 165 210 150 210 135 225 135 225 150 225 165 240 165 240 135 255 150 255 180 225 180 195 165 165 180 165 225 210 240 195 285 255 255 240 210 225 195 255 195 270 210 270 225 285 195 300 165 300 120 285 90 255 75 240 45
Polygon -1 true false 120 15 150 30 210 30 195 15 150 0
Circle -16777216 false false 0 0 300

lugarbn
false
0
Circle -1 true false 0 0 300
Polygon -1 true false 165 60 195 120 150 75 135 75 90 105 60 165 75 195 120 210 165 195 210 150 225 105 225 90 120 180 90 120
Polygon -16777216 true false 30 120 135 135 90 210 75 270 60 165
Polygon -16777216 true false 45 105 60 75 45 45 75 30 135 30 150 45 120 60 135 90 135 120 120 75 105 105 75 105 90 135 30 120 60 105
Polygon -16777216 true false 225 45 210 60 210 90 225 75 225 90 240 75 240 105 225 105 210 90 210 105 180 105 195 135 195 150 180 150 210 165 210 150 210 135 225 135 225 150 225 165 240 165 240 135 255 150 255 180 225 180 195 165 165 180 165 225 210 240 195 285 255 255 240 210 225 195 255 195 270 210 270 225 285 195 300 165 300 120 285 90 255 75 240 45
Polygon -16777216 true false 120 15 150 30 210 30 195 15 150 0
Circle -16777216 false false 0 0 300
Polygon -1 true false 120 60 135 90 180 105 210 105 165 45
Polygon -1 true false 120 75 135 120 135 135 90 120 75 105 105 105

milagro cronica
false
5
Rectangle -16777216 true false 15 60 285 240
Rectangle -7500403 true false 30 75 270 225
Circle -1184463 true false 180 106 90
Circle -7500403 true false 195 120 60
Rectangle -7500403 true false 240 105 270 195
Circle -1184463 true false 60 105 60
Circle -1184463 true false 105 105 60
Circle -7500403 true false 75 120 30
Circle -7500403 true false 120 120 30
Rectangle -7500403 true false 60 135 150 180
Rectangle -1184463 true false 60 135 75 195
Rectangle -1184463 true false 150 135 165 195

milagroso
false
2
Circle -16777216 false false 86 -34 127
Circle -955883 true true 110 5 80
Rectangle -7500403 true false 127 79 172 94
Polygon -8630108 true false 120 90 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90
Polygon -2674135 true false 135 90 120 90 150 135 180 90 165 90 150 105
Line -2674135 false 195 90 150 135
Line -2674135 false 105 90 150 135
Polygon -8630108 true false 135 90 150 105 165 90
Polygon -16777216 false false 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90

mortero
false
0
Polygon -1 true false 60 150 30 150 30 165 45 165 45 195 60 225 90 255 150 270 210 255 240 225 255 195 255 165 270 165 270 150
Polygon -1 true false 75 150 30 105 15 105 15 90 45 60 60 60 60 75 150 150
Line -16777216 false 45 120 75 90
Line -16777216 false 105 150 125 128
Polygon -16777216 false false 30 150 30 165 45 165 45 195 60 225 90 255 150 270 210 255 240 225 255 195 255 165 270 165 270 150
Polygon -16777216 false false 15 90 45 60 60 60 60 75 150 150 75 150 30 105 15 105

move
false
0
Circle -16777216 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -13840069 true false 45 135 210 135 210 105 255 150 210 195 210 165 45 165

mundo
false
0
Polygon -1 true false 0 240 45 60 135 105 225 240
Polygon -16777216 false false 0 240 45 60 135 105 225 240
Polygon -1 true false 150 240 225 75 300 240
Polygon -16777216 false false 225 75 300 240 150 240

newhyper
false
8
Circle -7500403 true false 180 90 120
Circle -7500403 true false 0 90 120
Circle -7500403 true false 90 180 120
Circle -7500403 true false 60 60 180
Circle -7500403 true false 90 0 120
Circle -1 true false 105 15 90
Circle -1 true false 195 105 90
Circle -1 true false 15 105 90
Circle -1 true false 105 195 90
Circle -1 true false 75 75 150
Rectangle -13345367 true false 135 90 165 210
Rectangle -13345367 true false 90 135 210 165

newlink
false
1
Circle -10899396 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -10899396 true false 135 75 225 75 225 105 135 105
Polygon -10899396 true false 165 45 195 45 195 135 165 135
Rectangle -7500403 true false 75 105 75 120
Polygon -10899396 true false 60 105 75 90 225 210 210 225
Circle -10899396 true false 45 75 60
Circle -10899396 true false 180 180 60

newnode
false
1
Circle -10899396 true false 0 0 300
Circle -1 true false 15 15 270
Polygon -10899396 true false 135 75 225 75 225 105 135 105
Polygon -10899396 true false 165 45 195 45 195 135 165 135
Rectangle -7500403 true false 75 105 75 120
Circle -10899396 true false 75 135 90

obra
false
0
Rectangle -7500403 true true 30 90 270 225
Rectangle -16777216 false false 30 90 270 225
Line -16777216 false 150 30 270 105
Line -16777216 false 30 105 150 30
Line -16777216 false 270 225 181 161
Line -16777216 false 30 225 119 161
Polygon -6459832 true false 30 105 150 30 270 105 150 180
Line -16777216 false 30 105 270 105
Line -16777216 false 270 105 150 180
Line -16777216 false 30 105 150 180

obrabn
false
0
Polygon -16777216 true false 0 60 210 0 285 255 165 300 75 300
Polygon -1 true false 15 75 195 15 270 255 150 300 90 300
Polygon -16777216 true false 105 60 135 120 210 90 180 30
Polygon -16777216 true false 45 90 90 255 165 225 75 90
Polygon -16777216 true false 150 150 195 270 255 240 210 120

paisbn
false
0
Circle -1 true false 0 0 300
Polygon -1 true false 165 60 195 120 150 75 135 75 90 105 60 165 75 195 120 210 165 195 210 150 225 105 225 90 120 180 90 120
Polygon -16777216 true false 30 120 135 135 90 210 75 270 60 165
Polygon -16777216 true false 45 105 60 75 45 45 75 30 135 30 150 45 120 60 135 90 135 120 120 75 105 105 75 105 90 135 30 120 60 105
Polygon -16777216 true false 225 45 210 60 210 90 225 75 225 90 240 75 240 105 225 105 210 90 210 105 180 105 195 135 195 150 180 150 210 165 210 150 210 135 225 135 225 150 225 165 240 165 240 135 255 150 255 180 225 180 195 165 165 180 165 225 210 240 195 285 255 255 240 210 225 195 255 195 270 210 270 225 285 195 300 165 300 120 285 90 255 75 240 45
Polygon -16777216 true false 120 15 150 30 210 30 195 15 150 0
Circle -16777216 false false 0 0 300
Polygon -1 true false 120 60 135 90 180 105 210 105 165 45
Polygon -1 true false 120 75 135 120 135 135 90 120 75 105 105 105
Rectangle -16777216 false false 0 0 300 300

pencil
false
0
Circle -10899396 true false 0 0 300
Circle -1 true false 14 14 272
Polygon -10899396 true false 240 75 240 105 120 225 90 225
Polygon -10899396 true false 75 180 75 210 225 60 195 60
Polygon -1 true false 90 195 105 210 240 75 225 60
Polygon -1 true false 105 180 75 180 60 240 120 225 120 195
Polygon -10899396 true false 60 240 89 233 90 225 75 210 66 210
Polygon -10899396 false false 60 240 75 180 105 180 120 195 120 225
Line -10899396 false 225 60 240 75

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person farmer
false
0
Circle -7500403 true true 56 194 30
Circle -16777216 false false 56 194 30
Rectangle -6459832 true false 225 90 240 300
Circle -7500403 true true 213 190 30
Circle -16777216 false false 213 190 30
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -6459832 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 109 4 82
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -16777216 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -6459832 false 210 90 255 90
Line -6459832 false 210 15 210 90
Line -6459832 false 255 15 255 90
Line -6459832 false 232 15 232 90
Polygon -16777216 false false 105 90 60 195 90 210 113 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 187 156 210 210 240 195 195 90 173 90 165 135 135 135 129 90
Circle -16777216 false false 109 4 82

persona
false
0
Polygon -13791810 true false 135 90 150 105 135 165 150 180 165 165 150 105 165 90
Polygon -1 true false 195 90 240 195 210 210 165 105
Circle -1 true false 110 5 80
Polygon -1 true false 105 90 60 195 90 210 135 105
Circle -16777216 false false 108 3 85
Rectangle -16777216 false false 126 80 171 90
Polygon -16777216 false false 105 90 60 195 90 210 120 135
Polygon -16777216 false false 195 90 240 195 210 210 180 150
Rectangle -1 true false 127 79 172 94
Polygon -1 true false 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -16777216 false false 90 285 120 195 105 90 195 90 180 195 210 285 195 300 165 300 150 225 135 300 105 300

personaje
false
0
Circle -955883 true false 110 5 80
Circle -16777216 false false 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -6459832 true false 120 90 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90
Polygon -2674135 true false 135 90 120 90 150 135 180 90 165 90 150 105
Line -2674135 false 195 90 150 135
Line -2674135 false 105 90 150 135
Polygon -6459832 true false 135 90 150 105 165 90
Polygon -16777216 false false 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90

petals
false
0
Circle -1 true false 117 12 66
Circle -1 true false 116 221 67
Circle -1 true false 41 41 67
Circle -1 true false 11 116 67
Circle -1 true false 41 191 67
Circle -1 true false 191 191 67
Circle -1 true false 221 116 67
Circle -1 true false 191 41 67
Circle -16777216 false false 116 11 67
Circle -16777216 false false 41 41 67
Circle -16777216 false false 11 116 67
Circle -16777216 false false 41 191 67
Circle -16777216 false false 116 221 67
Circle -16777216 false false 191 191 67
Circle -16777216 false false 221 116 67
Circle -16777216 false false 191 41 67
Circle -1 true false 60 60 180

plant
false
0
Rectangle -1 true false 135 90 165 300
Polygon -1 true false 135 255 90 210 45 195 75 255 135 285
Polygon -1 true false 165 255 210 210 255 195 225 255 165 285
Polygon -1 true false 135 180 90 135 45 120 75 180 135 210
Polygon -1 true false 165 180 165 210 225 180 255 120 210 135
Polygon -1 true false 135 105 90 60 45 45 75 105 135 135
Polygon -1 true false 165 105 165 135 225 105 255 45 210 60
Polygon -1 true false 135 90 120 45 150 15 180 45 165 90
Polygon -16777216 false false 135 300 135 285 75 255 45 195 90 210 135 255 135 210 75 180 45 120 90 135 135 180 135 135 75 105 45 45 90 60 135 105 135 90 120 45 150 15 180 45 165 90 165 105 210 60 255 45 225 105 165 135 165 180 210 135 255 120 225 180 165 210 165 255 210 210 255 195 225 255 165 285 165 300

plato
false
0
Rectangle -1 true false 0 0 300 300
Rectangle -16777216 false false 0 0 300 300
Circle -16777216 false false 45 45 210
Circle -16777216 false false 74 74 153

pushpin
false
0
Polygon -16777216 true false 210 75 165 120 153 145 179 136 225 90
Circle -16777216 true false 195 6 102
Circle -7500403 true true 213 9 82

rep_type
false
0
Polygon -16777216 true false 0 60 210 0 285 255 165 300 75 300
Polygon -1 true false 15 75 195 15 270 255 150 300 90 300
Polygon -16777216 true false 240 75 240 105 120 225 90 225
Polygon -16777216 true false 75 180 75 210 225 60 195 60
Polygon -955883 true false 90 195 105 210 240 75 225 60
Polygon -6459832 true false 105 180 75 180 60 240 120 225 120 195
Polygon -16777216 true false 60 240 89 233 90 225 75 210 66 210

revista
false
0
Polygon -16777216 true false 0 60 210 0 285 255 165 300 75 300
Polygon -1 true false 15 75 195 15 270 255 150 300 90 300
Polygon -13345367 true false 105 60 135 120 210 90 180 30
Polygon -6459832 true false 45 90 90 255 165 225 75 90
Polygon -5825686 true false 150 150 195 270 255 240 210 120

ritual
false
0
Rectangle -6459832 true false 120 15 180 285
Rectangle -6459832 true false 30 75 270 120
Rectangle -16777216 true false 180 120 195 300
Rectangle -16777216 true false 270 60 285 135
Rectangle -16777216 true false 180 0 195 75
Rectangle -16777216 true false 30 60 120 75
Rectangle -16777216 true false 105 0 120 60
Rectangle -16777216 true false 15 60 30 135
Rectangle -16777216 true false 30 120 120 135
Rectangle -16777216 true false 105 120 120 300
Rectangle -16777216 true false 195 60 285 75
Rectangle -16777216 true false 180 120 270 135
Rectangle -16777216 true false 120 0 180 15
Rectangle -16777216 true false 120 285 180 300

rotulo
false
0
Rectangle -13345367 true false 0 0 285 15
Rectangle -13345367 true false 285 0 300 300
Rectangle -1 true false 0 15 285 300

select
false
0
Circle -13345367 true false 0 0 300
Circle -1 true false 15 15 270
Circle -13345367 true false 120 120 58
Circle -13345367 false false 75 75 150

selector
false
0
Circle -7500403 false true 0 0 300
Circle -7500403 false true 14 14 272
Circle -7500403 false true 29 29 242

situacion
false
0
Line -16777216 false 75 255 225 255
Polygon -7500403 true true 90 255 60 255 60 225 75 180 75 165 60 135 45 90 60 75 60 45 90 30 120 30 135 45 240 60 255 75 255 90 255 105 240 120 225 105 180 120 210 150 225 195 225 210 210 255
Polygon -16777216 false false 210 255 60 255 60 225 75 180 75 165 60 135 45 90 60 75 60 45 90 30 120 30 135 45 240 60 255 75 255 90 255 105 240 120 225 105 180 120 210 150 225 195 225 210
Line -16777216 false 255 90 240 90
Circle -16777216 true false 134 63 24
Line -16777216 false 103 34 108 45
Line -16777216 false 80 41 88 49
Line -16777216 false 61 53 70 58
Line -16777216 false 64 75 79 75
Line -16777216 false 53 100 67 98
Line -16777216 false 63 126 69 123
Line -16777216 false 71 148 77 145
Rectangle -7500403 true true 90 255 210 300
Rectangle -16777216 false false 90 255 210 300

square
false
0
Rectangle -7500403 true true 30 30 270 270

sun
false
0
Circle -1 true false 75 75 150
Polygon -1 true false 300 150 240 120 240 180
Polygon -1 true false 150 0 120 60 180 60
Polygon -1 true false 150 300 120 240 180 240
Polygon -1 true false 0 150 60 120 60 180
Polygon -1 true false 60 195 105 240 45 255
Polygon -1 true false 60 105 105 60 45 45
Polygon -1 true false 195 60 240 105 255 45
Polygon -1 true false 240 195 195 240 255 255
Circle -16777216 false false 75 75 150
Polygon -16777216 false false 120 60 150 0 180 60
Polygon -16777216 false false 60 105 45 45 105 60
Polygon -16777216 false false 60 120 0 150 60 180
Polygon -16777216 false false 60 195 45 255 105 240
Polygon -16777216 false false 120 240 180 240 150 300
Polygon -16777216 false false 195 240 240 195 255 255
Polygon -16777216 false false 240 180 300 150 240 120
Polygon -16777216 false false 240 105 255 45 195 60

termometro
false
0
Circle -1 true false 45 180 120
Circle -16777216 false false 45 180 120
Rectangle -1 true false 75 30 135 210
Line -16777216 false 75 195 75 30
Line -16777216 false 135 195 135 30
Line -16777216 false 75 30 135 30
Line -16777216 false 105 60 135 60
Line -16777216 false 105 90 135 90
Line -16777216 false 105 120 135 120
Line -16777216 false 105 150 135 150
Line -16777216 false 105 180 135 180
Circle -1 true false 165 0 60
Circle -16777216 false false 165 0 60
Polygon -1 true false 285 60 210 60 210 150 285 150 285 120 240 120 240 90 285 90
Polygon -16777216 false false 285 60 210 60 210 150 285 150 285 120 240 120 240 90 285 90

tipo
false
1
Circle -7500403 true false 0 0 300
Circle -2674135 true true 15 15 270

tipo institucion
false
0
Rectangle -1184463 true false 30 15 270 315
Polygon -6459832 true false 90 90 75 300 225 300 210 90
Rectangle -16777216 true false 165 105 195 135
Rectangle -6459832 true false 176 29 188 79
Rectangle -6459832 true false 159 41 204 52
Polygon -955883 true false 90 90 225 90 150 60 75 90
Rectangle -16777216 true false 105 150 135 180
Circle -16777216 true false 120 210 60
Rectangle -16777216 true false 120 240 180 300
Polygon -16777216 false false 89 90 74 90 149 60 176 68 176 53 160 52 158 41 175 41 175 30 187 29 187 41 204 40 204 53 189 53 188 73 224 90 209 90 225 300 75 300
Rectangle -16777216 false false 30 15 270 315
Line -7500403 true 90 90 210 90
Line -16777216 false 90 90 210 90

tipo lugar
false
0
Rectangle -2674135 true false 0 0 300 300
Circle -13345367 true false 0 0 300
Polygon -13345367 true false 165 60 195 120 150 75 135 75 90 105 60 165 75 195 120 210 165 195 210 150 225 105 225 90 120 180 90 120
Polygon -6459832 true false 30 120 135 135 90 210 75 270 60 165
Polygon -6459832 true false 45 105 60 75 45 45 75 30 135 30 150 45 120 60 135 90 135 120 120 75 105 105 75 105 90 135 30 120 60 105
Polygon -6459832 true false 225 45 210 60 210 90 225 75 225 90 240 75 240 105 225 105 210 90 210 105 180 105 195 135 195 150 180 150 210 165 210 150 210 135 225 135 225 150 225 165 240 165 240 135 255 150 255 180 225 180 195 165 165 180 165 225 210 240 195 285 255 255 240 210 225 195 255 195 270 210 270 225 285 195 298 169 297 121 285 90 255 75 240 45
Polygon -1 true false 120 15 150 30 210 30 195 15 150 0
Circle -16777216 false false 0 0 300
Rectangle -16777216 false false 0 0 300 300

tipo personaje milagroso
false
2
Rectangle -1184463 true false 0 255 300 300
Rectangle -1184463 true false 0 -45 315 255
Circle -16777216 false false 86 -34 127
Circle -955883 true true 110 5 80
Rectangle -7500403 true false 127 79 172 94
Polygon -8630108 true false 120 90 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90
Polygon -2674135 true false 135 90 120 90 150 135 180 90 165 90 150 105
Line -2674135 false 195 90 150 135
Line -2674135 false 105 90 150 135
Polygon -8630108 true false 135 90 150 105 165 90
Polygon -16777216 false false 105 90 60 195 90 210 120 165 90 285 105 300 195 300 210 285 180 165 210 210 240 195 195 90
Rectangle -16777216 false false 0 -45 315 300

tipo ritual
false
0
Rectangle -10899396 true false 0 0 300 300
Rectangle -6459832 true false 120 15 180 285
Rectangle -6459832 true false 30 75 270 120
Rectangle -16777216 true false 180 120 195 300
Rectangle -16777216 true false 270 60 285 135
Rectangle -16777216 true false 180 0 195 75
Rectangle -16777216 true false 30 60 120 75
Rectangle -16777216 true false 105 0 120 60
Rectangle -16777216 true false 15 60 30 135
Rectangle -16777216 true false 30 120 120 135
Rectangle -16777216 true false 105 120 120 300
Rectangle -16777216 true false 195 60 285 75
Rectangle -16777216 true false 180 120 270 135
Rectangle -16777216 true false 120 0 180 15
Rectangle -16777216 true false 120 285 180 300
Rectangle -16777216 false false 0 0 300 300

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

verbo
false
0
Polygon -1 true false 78 299 9 42 205 41 191 95 225 99 220 204 262 204 293 234 292 279 270 299
Polygon -7500403 true true 15 45 90 300 120 300 195 45 150 45 105 240 60 45
Polygon -7500403 true true 180 105 180 300 255 300 285 270 285 240 255 210 210 210 210 105
Polygon -1 true false 210 240 210 255 210 270 240 270 255 255 240 240
Polygon -16777216 false false 15 45 60 45 105 240 150 45 195 45 120 300 90 300
Polygon -16777216 false false 180 105 210 105 210 210 255 210 285 240 285 270 255 300 180 300
Polygon -16777216 false false 210 240 240 240 255 255 240 270 210 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 150 150 135 180 165 180

curve
0.4
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 135 165 150 75
Line -7500403 true 165 165 150 75

curve-1.0
-1.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 150 150 120 240 150 210 180 240

curve-2.0
-2.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

curve-3.0
-3.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 150 105 90 225 150 195 210 225

curve0.5
1.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

curve1.0
1.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 150 105 90 240 150 210 210 240

curve2.0
2.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

curve3.0
3.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dotcurve-2.0
2.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dotcurve2.0
2.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dotcurve3.0
3.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

double
0.0
-0.2 1 1.0 0.0
0.0 0 0.0 1.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 210 195 195 150
Circle -7500403 true true 165 135 30
Line -7500403 true 165 150 150 195

menu
1.0
-0.2 1 4.0 4.0
0.0 1 4.0 4.0
0.2 1 4.0 4.0
link direction
true
0

query
1.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 120 195 150 150 180 195

simple
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 135 165
Line -7500403 true 150 150 165 165

straight
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 150 150 135 210 150 180 165 210

@#$#@#$#@
0
@#$#@#$#@
