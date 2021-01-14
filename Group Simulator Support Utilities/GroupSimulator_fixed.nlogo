extensions [ array ]
globals [
  model-definition                      ;; First line of output files
  start-number-for-random               ;; The random seed
  members-from-file-input               ;; Boolean set in SETUP and SETUP-MODEL-FROM-FILE
  re-playing                            ;; Boolean set by button on interface
  abo-coefficients                      ;; EXPRESSIVE ORDER array
  emotion-coefficients                  ;; EXPRESSIVE ORDER array
  I-beta                                ;; EXPRESSIVE ORDER array
  S-beta                                ;; EXPRESSIVE ORDER array
  g                                     ;; EXPRESSIVE ORDER arra y
  testing-against-Interact              ;; Boolean set at beginning of SETUP procedure
  variable-vector                       ;; Pre-event transients and cross-products
  scale-max-EPA                         ;; Max polarization in graphing
  max-random-vector-length              ;; Max length of random EPA vector
  actor-size                            ;; Basic size of faces in pictorial view
  male-faces                            ;; Names of male emotion displays
  female-faces                          ;; Names of female emotion displays
  whole-group-faces                     ;; Names of emotion displays signaling group state
  interactants                          ;; Agentset: males plus females
  society                               ;; Agentset: interactants plus whole-group
  old-last-action                       ;; List of actor, object, behavior IPA
  last-action                           ;; List of actor, object, behavior IPA
  action-now                            ;; List of actor, object, behavior IPA
  original-fundamentals-list            ;; List of fundamentals input from file
  actor-reciprocating                   ;; Boolean set in SELECT-ACTOR procedure
  whole-group-is-object                 ;; Boolean set in SELECT-OBJECT procedure
  link-types                            ;; Kinds of lines between agents
  IPA-EPAs                              ;; Center points of IPA sectors in EPA space
  IPA-counts                            ;; Running tally of actions in each IPA category
  network-matrix                        ;; Markovian matrix of who-to-whom relations
  IPA-categories                        ;; Names of IPA categories - NOT USED
  SYMLOG-topics                         ;; Names of topics/values in SYMLOG categories
  SYMLOG-directions                     ;; Acronyms for SYMLOG categories
  SYMLOG-verbs                          ;; Names of behaviors in SYMLOG catgeories
  SYMLOG-physical-verbs                 ;; Names of contact behaviors in SYMLOG catgeories
  SYMLOG-directions-EPA-coordinates     ;; Coordinates of 26 points, 3 units from origin
  run-number                            ;; Index incremented in transition-to-new-run procedure
  ]

breed [graph-elements graph-element]    ;; Axes, circles
breed [whole-groups whole-group]        ;; Agent
breed [males male]                      ;; Agents
breed [females female]                  ;; Agents
turtles-own [fundamentals current-transients emotion behavior alter
             ego-is-actor event-deflection personal-deflection
             actorBasis attractionBasis acts-originated acts-received potential-event-deflection
             D original-transients best-object best-D p-n u-d f-b personal-value temp-index]
            ;; ego-is-actor consists of true-false answer to This turtle is actor (not object) in next event?
undirected-link-breed [past-links past-link]
directed-link-breed [behavior-arrows behavior-arrow]
past-links-own [repetitions negative-acts]



to setup
  clear-all
  set testing-against-Interact false ;; Set TRUE to check calculations
  set members-from-file-input false

  set scale-max-EPA 4.5  ;; Max polarization in graphing
  set max-random-vector-length 4 ;; Used in RANDOM-EPA procedure

  ;; DEFINE FACES AND LINKS
  set actor-size   min list (100 / (group-size + 2)) 18 ;; Smaller faces with more agents
  set male-faces [ "male neutral" "male elated" "male calm" "male bedazzled" "male beatific"
    "male angry" "male disgusted" "male scared" "male sad" ]
  set female-faces [ "female neutral" "female elated" "female calm" "female bedazzled" "female beatific"
    "female angry" "female disgusted" "female scared" "female sad" ]
  set whole-group-faces [ "topic neutral" "topic elated" "topic calm" "topic bedazzled" "topic beatific"
    "topic angry" "topic disgusted" "topic scared" "topic sad" ]
  set link-types [ "friend" "foe" "back-1" "back-2" "back-3" "topic-link" ]

  ;; CREATE BASIS FOR ACT COMPUTATIONS
  load-math-arrays
  ifelse member? "unisex" equations or equations = "random acts" or equations = "identity echoes"
    [ load-unisex-equations ]
    [ ifelse member? "male" equations
      [ load-male-equations ]
      [ ifelse member? "female" equations
        [ load-female-equations ]
        [load-special-equations ]
      ]
    ]


  ;; CREATE BASIS FOR IPA and SYMLOG ANALYSES
  setup-Bales-categories

  ;; MAKE WHOLE-GROUP = TURTLE NUMBER 0
  create-whole-groups 1
    [
    setxy 0 0
    set size actor-size
    set shape (item 0 whole-group-faces)
    set ego-is-actor false
    ]

  ;; MAKE INTERACTANTS
  let number-of-females floor ((group-size * percent-females) / 100)
  create-males (group-size - number-of-females)
    [
    ;; Define the individual
    set size actor-size
    set shape (item 0 male-faces)
    ]
  create-females (number-of-females)
    [
    ;; Define the individual
    set size actor-size
    set shape (item 0 female-faces)
    ]
  ask turtle 0 [set interactants other turtles] ;; "Interactants" consist of males and females.
  define-interactants ;; Sets fundamentals and transients; defines "society" as whole-group plus interactants.
  ifelse dynamics [plot-transient-faces-on-axes][plot-faces-on-axes]

  set IPA-counts [0 0 0 0 0 0 0 0 0 0 0 0]
  set network-matrix n-values ((count interactants + 1) * (count interactants + 1)) [0]
  set run-number 0
  set re-playing false
  set start-number-for-random new-seed
  random-seed start-number-for-random


  set model-definition (sentence "MODEL::"
                        (word "Males: " (list male-goodness male-dominance male-activation) )
                        (word "Females: " (list female-goodness female-dominance female-activation) )
                        word "Actor choice=" actor-choice
                        word "Object choice=" object-choice
                        word "Equations=" equations   word "N=" group-size   word"female%=" percent-females
                        word "Individuality=" individuality   word "Initial tension=" initial-tension
                        word "Reciprocal action probability=" reciprocal-act-Pr
                        word "Address group probability=" address-group-Pr word "Group act to all=" grp-act-to-all
                        word "IPA coding=" IPA-coding-basis
                        word "Run size=" run-size word "Next group changes " change-next-group's
                        word "Random seed=" start-number-for-random
                        )
  if testing-against-Interact [X_run-test-case]

  set actor-reciprocating false ;; Variable reported in saved output
  set whole-group-is-object false ;; Variable reported in saved output
  reset-ticks
  tick ;; Start with event 1
end ;; setup



to re-start
  ask links [die] ;; Clear old relationships.

  ;; CREATE BASIS FOR IPA and SYMLOG ANALYSES
  setup-Bales-categories

  if members-from-file-input and (change-next-group's = "sentiments") and not re-playing ;; If false fundamentals stay the same.
    [
    ;; Reset fundamentals with origianl fundamentals as center for random draws.
    foreach sort-by [ [?1 ?2] -> [who] of ?1 < [who] of ?2 ] interactants
      [ ?1 ->
      ask ?1
        [
        set fundamentals random-EPA item (([who] of ?1) - 1) original-fundamentals-list individuality
        ]
      ]
    ]
  ;; Clear individual variables.
  ask society
    [
    set emotion [-10 -10 -10]
    set behavior [-10 -10 -10]
    set potential-event-deflection -1
    set acts-originated -1
    set acts-received -1
    set alter -1
    set ego-is-actor false
    set event-deflection -1
    set D -1
    set best-object -1
    set best-D -1
    set p-n -10
    set f-b -10
    set u-d -10
    ifelse re-playing
      [
      set current-transients original-transients
      ]
      [
      ifelse initial-tension > 0
        [
        set current-transients random-EPA fundamentals initial-tension ;; EPA profile around fundamental profile
        ]
        [
        set current-transients fundamentals ;; Zero starting stress.
        ]
      ]
    set original-transients current-transients
    compute-personal-deflection who
    ;; Display emotion
    display-emotion (who)
    ]
  clear-all-plots
  clear-output
  set actor-reciprocating false ;; Variable reported in saved output
  set whole-group-is-object false ;; Variable reported in saved output

  set action-now [ 0 0 0 ]
  set last-action [ 0 0 0 ]
  set old-last-action [ 0 0 0 ]
  set IPA-counts [0 0 0 0 0 0 0 0 0 0 0 0]
  set network-matrix n-values ((count interactants + 1) * (count interactants + 1)) [0]
  set run-number 0
  reset-ticks
  ifelse dynamics [plot-transient-faces-on-axes][plot-faces-on-axes]
  if re-playing
    [
    set re-playing false
    random-seed start-number-for-random
    ]
end ;; re-start



to define-interactants
  ;; SET FUNDAMENTALS AND TRANSIENTS AND SHOW EMOTIONS
  ask interactants
    [
    set fundamentals [0 0 0]
    set current-transients fundamentals
    ;; Assign random EPA fundamentals between -scale-max-EPA and +scale-max-EPA to each individual.
    ;; Mean of multinormal distribution is from GOODNESS-DOMINANCE-ACTIVATION sliders on interface.
    ;; Standard deviation of multinormal distribution is from INDIVIDUALITY slider on interface.
    ;; (INDIVIDUALITY of 0.9 would reflect StanDev of trait EPAs; see COMPUTEMODIFIERVARIANCE.XLSX.)
    ;;
    ;; Also set the interactant's basis for choosing actors, and for choosing objects
    ifelse is-male? self
      [
      let centers-list (list male-goodness male-dominance male-activation)
      set fundamentals random-EPA centers-list individuality
      ]
      [
      let centers-list (list female-goodness female-dominance female-activation)
      set fundamentals random-EPA centers-list individuality
      ]
    ;;
    set actorBasis actor-choice
    set attractionBasis object-choice
    set personal-value find-SYMLOG-personality-type [fundamentals] of self
    let deflection .5
    ifelse initial-tension > 0
      [
      set current-transients random-EPA fundamentals initial-tension ;; EPA profile around fundamental profile
      ]
      [
      set current-transients fundamentals ;; Zero starting stress.
      ]
    ]
  ;; whole-group fundamental averages all interactants' fundamentals.
  ask whole-groups
    [
    set fundamentals (list  precision mean [item 0 fundamentals] of interactants 2
                            precision mean [item 1 fundamentals] of interactants 2
                            precision mean [item 2 fundamentals] of interactants 2)
    set current-transients fundamentals
    ;; If ADDRESS-GROUP-PR slider is negative, remove whole-group entity so it never is an object of action.
    if address-group-Pr < 0 [die] ;;
    ]
  set society (turtle-set whole-groups males females) ;; Consists of the whole-group turtle, plus males, plus females
  ask society
    [
    set original-transients current-transients
    compute-personal-deflection who
    ;; Declare lists
    set emotion [0 0 0]
    set behavior [0 0 0]
    ;; Display emotion
    display-emotion (who)

    ]

  set action-now [ 0 0 0 ]
  set last-action [ 0 0 0 ]
  set old-last-action [ 0 0 0 ]
end ;; define-interactants


to setup-model-from-file
  ;; Obtain a set of agents from a file.
  setup ;; Establish equations, etc.
  let specification "" ;; Declaration
  let line-of-data ""  ;; Declaration
  let number-of-symlog-specifications 0 ;; Initialization
  set original-fundamentals-list [] ;; Initialization
  set members-from-file-input true ;; SETUP made it false.
  clear-turtles
  ;; MAKE whole-group = TURTLE NUMBER 0
  create-whole-groups 1
    [
    setxy 0 0
    set shape (item 0 whole-group-faces)
    ]
  ;; Close any open file, and get file name
  file-close-all
  let infoFile user-file
  ;; Generate a header for output files to record the source of specifictions.
  set model-definition (sentence model-definition word "MODEL FROM FILE: " infoFile)
  if ( infoFile != false ) ;; Lack of file name - skip to end.
    [
    file-open infoFile
    ;; Check to see if first line is a random seed.
    set line-of-data file-read-line
    set specification (read-from-string line-of-data)
    ifelse is-number? specification
      [
      set start-number-for-random specification
      set line-of-data file-read-line ;; Read 2nd line.
      ]
      [
      set start-number-for-random new-seed
      ]
    ;; Check to see if current line concerns WHOLE-GROUP, and if so skip it.
    set specification (read-from-string line-of-data)
    if (first specification) = "whole-group" [set line-of-data file-read-line]
    ;; The next WHILE loop processes input data until end of file, or an empty line.
    ;; An empty line can be used to separate data from notes at the end of the input file.
    while [line-of-data != ""]
      [
      set specification (read-from-string line-of-data)
      ;; A specification line gives an agent's fundamentals, originals-transients, and perhaps value directions.
      ;; The line may be of the form ["male" [2.75  -1.07  -0.36] [1.85 -0.94 -1.59]]  or
      ;; ["male" [2.75 -1.07 -0.36] [1.85 -0.94 -1.59] "UN"] if specifying a SYMLOG direction for actor choice  or
      ;; ["female" [2.50 0.18 1.95] [2.0 0.76 0.85] "UP" "DB"] if specifying a SYMLOG direction for both actor and object choice.
      ;; Actor choice SYMLOG directions from file are ignored unless they are given for all agents.
      ;; The interface ACTOR-CHOICE is assigned to interactants whose actor direction is unspecified in the file.
      ;; The interface ATTRACTION-BASIS is assigned to interactants whose object basis is unspecified in the file.
      ;; The object basis can be any of the ATTRACTION-BASIS options.
      ifelse (first specification) = "male"
        [
        create-males 1
          [
          setxy 0 0
          set shape (item 0 male-faces)
          set fundamentals item 1 specification
          set original-fundamentals-list lput fundamentals original-fundamentals-list
          set original-transients item 2 specification
          ifelse (length specification > 3) and (item 3 specification != "")
            [
            ;; A basis for actor choice is included. Set this agent's actorBasis to it.
            set actorBasis item 3 specification
            if member? item 3 specification SYMLOG-directions
              [set number-of-symlog-specifications (number-of-symlog-specifications + 1)]
            ]
            [ set actorBasis actor-choice ] ;; Set actor choice to selection on actor-choice menu.
          ifelse (length specification > 4)
            [ set attractionBasis item 4 specification ] ;; Set this agent's attractionBasis to the given entry.
            [ set attractionBasis object-choice ] ;; Set object choice to selection on object-choice menu.
          ]
        ]
        [
        create-females 1
          [
          setxy 0 0
          set shape (item 0 female-faces)
          set fundamentals item 1 specification
          set original-fundamentals-list lput fundamentals original-fundamentals-list
          set original-transients item 2 specification
          ifelse (length specification > 3) and (item 3 specification != "")
            [
            ;; A basis for actor choice is included. Set this agent's actorBasis to it.
            set actorBasis item 3 specification
            if member? item 3 specification SYMLOG-directions
              [set number-of-symlog-specifications (number-of-symlog-specifications + 1)]
            ]
            [ set actorBasis actor-choice ] ;; Set actor choice to selection on actor-choice menu.
          ifelse (length specification > 4)
            [ set attractionBasis item 4 specification ] ;; Set this agent's attractionBasis to the given entry.
            [ set attractionBasis object-choice ] ;; Set object choice to selection on object-choice menu.
          ]
        ]
      ifelse file-at-end?
        [set line-of-data ""]
        [set line-of-data file-read-line] ;; Get the next specification
      ]
    file-close
    ask turtle 0 ;; whole-group
      [
      set interactants other turtles
      ]
    ;; Next two lines change sliders on interface.
    set group-size count interactants
    set percent-females ceiling ((100 * count females) / (count males + count females))
    set actor-size   min list (100 / (group-size + 2)) 18 ;; Smaller faces with more agents
    set society (turtle-set whole-groups males females) ;; Society consists of the whole-group turtle, plus males, plus females.
    ;; Make sure symlog specifications are complete.
    if (number-of-symlog-specifications > 0) and (number-of-symlog-specifications < group-size)
      [
      ;; Symlog specifications for actor choice are incomplete, so replace them.
      ask interactants
        [
        set actorBasis actor-choice
        ;; But no need for to reset attractionBasis because an individual can use symlog without others doing so.
        ]
      show-inputs
      user-message "INPUT ERROR IN ACTOR-CHOICE CRITERIA. ALL CHANGED TO ACTOR-CHOICE VALUE. "
      ]
    ask whole-groups ;; Finish specifying the whole-group entity.
      [
      set fundamentals (list  precision (mean [item 0 fundamentals] of interactants) 2
                              precision (mean [item 1 fundamentals] of interactants) 2
                              precision (mean [item 2 fundamentals] of interactants) 2)
      ;; No deflection for whole-group entity
      set current-transients fundamentals
      set original-transients current-transients
      compute-personal-deflection who
      ;; Declare lists.
      set emotion [0 0 0]
      set behavior [0 0 0]
      ;; Display characteristic emotion.
      display-emotion (who)
      ;; If ADDRESS-GROUP-PR slider is negative, remove whole-group entity so it never is an object of action.
      set ego-is-actor false
      if address-group-Pr < 0 [die] ;;
      ]
    ask interactants ;; Finish specifying each interactant.
      [
      set personal-value find-SYMLOG-personality-type [fundamentals] of self
      set current-transients original-transients
      compute-personal-deflection who
      ;; Declare lists.
      set emotion [0 0 0]
      set behavior [0 0 0]
      ;; Display characteristic emotion.
      display-emotion (who)
      set ego-is-actor false
      ]
    ifelse dynamics [plot-transient-faces-on-axes][plot-faces-on-axes]
    random-seed start-number-for-random
    set run-number 0
    set re-playing false
    set model-definition (sentence "MODEL From file:: " infoFile
                          (word "Generic actor-choice=" actor-choice ", Gemeric object-choice=" object-choice)
                          word "Equations=" equations   word "N=" group-size   word"female%=" percent-females
                          word "StartingStress=" initial-tension
                          word "ReciprocalActionPr=" reciprocal-act-Pr
                          word "AddressGroupPr=" address-group-Pr "GroupActToAll=" grp-act-to-all
                          word "IPAcoding=" IPA-coding-basis
                          word "RunSize=" run-size word "change-next-group's " change-next-group's
                          word "RandomSeed=" start-number-for-random
                          )
    ]
end ;; setup-model-from-file



to next
  go
  ;;  To export a movie of the group space:
  ;;  SETUP the group to analyze.
  ;;  Comment out the above GO.
  ;;  Uncomment the lines below.
  ;;  Change REPEAT 300 to the number of frames you want recorded.
  ;;  Click the NEXT button on the interface to start recording.
;  movie-start "out.mov"
;  ;movie-grab-view ;; show the initial state of the view box
;  movie-grab-interface ;; show the initial state of the whole interface
;  repeat 300
;  [ go
;  ;movie-grab-view ]
;  movie-grab-interface ]
;  movie-close
end



to on-off
  go
end



to go
  save-file-headers
  ifelse fast [no-display] [display] ;; Shut off display if interface FAST switch is on.
  if ticks = 1 [display]
  ;; put current conditions into past
  update-links
  set old-last-action last-action
  set last-action action-now
  ;; Identify new actor and object
  ask interactants  [ set ego-is-actor false ]
  select-actor ;; This routine calls select-object. New actor and object are returned in action-now.
  let theActor item 0 action-now
  let theObject item 1 action-now
  ;;ask turtle theActor [save-event-data]

  ;; Implement next event
  ask turtle theActor
    [
    set alter theObject
    if not fast
      [
      create-behavior-arrow-to turtle theObject
        [
        set shape (item 0 link-types)
        set color green
        ]
      ]
    compute-behavior-epa theActor
    ifelse (alter = 0) and grp-act-to-all
      [
      ;; Distribute whole-group behavior to all non-Actor interactants
      let actor-current-transients current-transients
      let tempObject group-size ;; Start processing at highest agent number
      while [tempObject >= 0] ;; Sequence ends with alter = 0 applying behavior to all agents other than actor
        [
        set current-transients actor-current-transients ;; Reset theActor's transients after processing prior object
        if tempObject != theActor
          [
          set alter tempObject
          compute-transients theActor ;; Uses the behavior chosen for theActor to the whole group
          ask turtle tempObject
            [
            set behavior [behavior] of myself
            set event-deflection [potential-event-deflection] of myself
            compute-personal-deflection tempObject
            display-emotion tempObject
            ]
          ]
        set tempObject (tempObject - 1)
        ]
        ;; actor-transients are now the ones after action on the group (alter = 0) which is what we want
      set event-deflection potential-event-deflection ;; Actor's event deflection when alter = 0
      ]
      [
      ;; Object is not the whole group
      compute-transients theActor
      set event-deflection potential-event-deflection
      ask turtle theObject
        [
        set behavior [behavior] of myself
        set event-deflection [potential-event-deflection] of myself
        ]
      compute-personal-deflection alter
      display-emotion alter
      ]
    update-matricies
    show-Bales-outputs theActor
    compute-personal-deflection theActor
    display-emotion theActor
    ]
  ifelse dynamics [plot-transient-faces-on-axes][plot-faces-on-axes]
  if save-io [ask turtle theActor [save-event-data] ]   ;; Record results
  ifelse ticks = run-size
    [transition-to-new-run]
    [tick]
end ;; go



to update-links
  if fast [stop]
  ;; Move link display back one unit of time
  ask behavior-arrows
    [
    let oneside end1 ;; Originating turtle
    let otherside end2
    ;; set up hostility links
    let theActor item 0 action-now
    let bad-behavior 0
    if (theActor = [who] of oneside) and (item 0 [behavior] of turtle (theActor) <= -0.5)
      [ set bad-behavior 1 ]
    ask oneside
      [
      ifelse not past-link-neighbor? otherside
        [
        ;; Create a new undirected link.
        create-past-link-with otherside
          [
          set repetitions repetitions + 1
          set negative-acts negative-acts + bad-behavior
          ifelse bad-behavior = 0
            [set color scale-color cyan repetitions 0 10]
            [set color scale-color red repetitions 0 10]
          if [who] of otherside = 0  ;; Give a different appearance to links to whole-group
            [
            set shape (item 5 link-types)
            set color turquoise
            if not lines-to-group [die]
            ]
          ]
        ]
        [
        ask past-link [who] of oneside  [who] of otherside
          [
          ;; Modify an existing link.
          set repetitions repetitions + 1
          set negative-acts negative-acts + bad-behavior
          set color scale-color cyan repetitions 0 10
          if [who] of otherside = 0 [set color turquoise]
          if negative-acts / repetitions > 0.5 [set color red]
          ifelse repetitions > 20
            [ set shape (item 2 link-types) ]
            [ if repetitions > 15 [ set shape (item 3 link-types) ] ]
          ]
        ]
      ]
    die
    ]
 end ;; update-links



to display-emotion [ theOne ]
  ;; Compute the emotion
  set I-beta array:from-list sentence(n-values 9 [1]) (array:to-list variable-vector) ;; Expand I-beta to include fundamentals
  let combo (sentence [fundamentals] of turtle theOne [current-transients] of turtle theOne)
  ask turtle theOne [set emotion compute-emotion combo]
  let theEmotion  [emotion] of turtle theOne

  ;; Translate the emotion EPA to a facial expression:
  ;; neutral elated calm bedazzled beatific angry disgusted scared sad
  let emotion-category-EPAs  [0 0 0   2 2 2   2 2 -2   2 -2 2   2 -2 -2   -2 2 2   -2 2 -2   -2 -2 2   -2 -2 -2]
  let distances []
  let i 0
  while [i < 9]
    [
    ;; Make a list of distances from the emotion-epa to each of the emotion-category-EPAs.
    set distances sentence distances   squared-EPA-distance theEmotion   sublist emotion-category-EPAs (3 * i) ((3 * i) + 3)
    set i  i + 1
    ]
  let index position (min distances) distances ;; The chosen category is the minimum distance in the list.
  ifelse theOne = 0
    [ ask turtle theOne [set shape (item index whole-group-faces)] ]
    [ifelse ( [breed] of turtle theOne = males )
      [ ask turtle theOne [set shape (item index male-faces)] ]
      [ ask turtle theOne [set shape (item index female-faces)] ] ]
end ; display-emotion



to select-actor
  ;; Initialize and declare variables
  set action-now [-1 -1 -1]
  set actor-reciprocating false
  let potential-actors nobody
  let actorCriterion actor-choice

  ;; With reciprocal-act-Pr, the next actor is the object of last action, unless the prior object was the whole-group.
  ifelse (random 10 < 10 * reciprocal-act-Pr) and (not (item 1 last-action <= 0))
    [
    set action-now (list (item 1 last-action) (item 0 last-action) -1) ;; reverse actor-object of last action
    set actor-reciprocating true
    ]
    [
    ;; Not reciprocating so make a fresh selection of actor.
    ;; Below actorCriterion is set from interface. The actor-choice must be set to a SYMLOG option to get
    ;; SYMLOG actor choice, even if different SYMLOG actor directions have been read from a file. Otherwise
    ;; the drop-down menu definition of actorCriterion is applied to all possible actors.
    set potential-actors interactants
    ]
  let theActor item 0 action-now

  ;; If an actor hasn't been selected as a reciprocator, find a new actor.
  if theActor = -1
    [
    ifelse member? [actorBasis] of turtle 1 SYMLOG-directions
      ;; Use SYMLOG values to select actor.
      [
      ;; Check that all agents have a symlog actorBasis
      ask potential-actors
        [if not member? actorBasis SYMLOG-directions
          [
          show-inputs
          user-message "INPUT ERROR IN ACTOR-CHOICE CRITERIA. "
          ]
        ]
      ;; Actor will be the one with the longest vector in the direction of the interactant's SYMLOG actor direction.
      ask potential-actors [ set temp-index position actorBasis SYMLOG-directions
         ] ;; Which direction is selected?
      set theActor [who] of max-one-of potential-actors [
        ;; (3 * temp-index) is index to coordinates of a 3-unit vector in the desired SYMLOG direction.
        ;; Multiply direction coordinates with coordinates of the potential actor's current-transients,
        ;; and sum to get length of the potential actor's vector in the desired direction.
        item (3 * temp-index) SYMLOG-directions-EPA-coordinates * item 0 current-transients +
        item ((3 * temp-index) + 1) SYMLOG-directions-EPA-coordinates * item 1 current-transients +
        item ((3 * temp-index) + 2) SYMLOG-directions-EPA-coordinates * item 2 current-transients]
      ]
      [
      ;; Not a symlog basis so all agents must have the same actorBasis.
      let actorBasis-error false
      ask potential-actors
        [if actorBasis !=  [actorBasis] of turtle 1  [set actorBasis-error true] ]
      if  actorBasis-error
        [
        show-inputs
        user-message "INPUT ERROR IN ACTOR-CHOICE CRITERIA. "
        ]
      ifelse actorCriterion = "min event tension"
        [
        ;; Reset object-choice criterion to min event tension, since object will be determined that way
        ;; while solving for the optimal actor.
        set object-choice "min event tension"
        ;; Actor will be chosen in combination with object and behavior to generate minimum-deflection event.
        ask potential-actors ;; Try every feasible actor.
          [
          ;; Save the current transients of the actor for later restoration.
          let actor-current-transients current-transients
          set ego-is-actor true
          let testActor who
          let possible-objects (turtle-set  other society)
          ask possible-objects ;; With this actor, try every feasible object.
            [
            ;; Save the current transients of the object for later restoration.
            let object-current-transients current-transients
            let testObject [who] of self
            ask myself [set alter testObject]  ;; For testActor, alter is the current test object
            compute-behavior-epa testActor ;; Find this actor's optimal action toward this test object.
            ;; The next call re-sets current-transients of test actor and test object,
            ;; and also potential-event-deflection and personal-deflection of test actor
            compute-transients testActor
            ;; Store the actor's event deflection for this actor-object combo in object's potential-event-deflection
            set potential-event-deflection [potential-event-deflection] of myself
            set current-transients object-current-transients ;; Restore this object's original transients.
            ]
          ;; Set the best-object and best deflection for this actor
          set best-object [who] of min-one-of possible-objects [potential-event-deflection]
          set best-D  [potential-event-deflection] of turtle best-object
          set current-transients actor-current-transients ;; Restore this actor's original transients.
          ]
        set theActor [who] of min-one-of potential-actors [best-D]
        let theObject [best-object] of turtle theActor
        if (random 10 < 10 * address-group-Pr) ;; Allow chosen actor to address whole group instead of optimal object.
          [
          set theObject 0
          set whole-group-is-object true
          ]
        set action-now (list theActor theObject -1) ;; Selection completed of both actor and object
        ]
        [
        ifelse actorCriterion = "max self-tension"
          [
          ;; Actor will be the one whose identity is most stressed.
          set theActor [who] of max-one-of potential-actors [personal-deflection]
          ]
          [
          ifelse actorCriterion = "min self-tension"
            [
            ;; Actor will be the one whose identity is least stressed.
            set theActor [who] of min-one-of potential-actors [personal-deflection]
            ]
            [
            ;; Random choice of turtles numbered above 0
            set theActor [who] of one-of potential-actors
            ]
          ]
        ]
      ]
    set action-now (replace-item 0 action-now theActor) ;; Specify actor in action-now
    ]
  if item 1 action-now = -1
    [
    ;; Find an object unless it was chosen by reciprocation or for least-event-stress.
    select-object
    ]
  let theObject item 1 action-now
  ask turtle theActor
    [
    set alter  theObject
    set ego-is-actor true
    set acts-originated  acts-originated + 1
    ]
  ask turtle theObject
    [
    set alter  theActor
    set acts-received  acts-received + 1
    ]
end ;; select-actor



to select-object
  ;; Called from select-actor, unless object already has been chosen for minimum future deflection.
  let theActor  item 0 action-now
  let theObject item 1 action-now
  let actorAttractionBasis object-choice
  let object-candidates nobody
  let rejection-level 0
  set whole-group-is-object false
  let actor-current-transients [current-transients] of turtle theActor

  ;; Object candidates are group entities other than theActor.
  ask turtle theActor
    [
      set ego-is-actor true ;; <---- Niko CHanged this!! This is the error that caused the wrong object selection. since the function compute-transients checks this variable to determine who is the actor.
    set object-candidates  other society
    ;; If attractionBasis was read from file, SYMLOG directions can be mixed with other criteria.
    set actorAttractionBasis attractionBasis
    ]

  ;; Prevent a repeat dyad
  if theActor = item 0 last-action [ ask turtle item 1 last-action [set object-candidates other object-candidates] ]

  ifelse (random 10 < 10 * address-group-Pr)
    [
    ;; With address-group-Pr, the next object is the whole-group
    set theObject 0
    set action-now (list theActor theObject 0)
    set whole-group-is-object true
    ]
    [
    ;; Otherwise select an object according to actor's attraction basis
    ;;print "find object and behavior"
    ask object-candidates
      [
      ifelse member? actorAttractionBasis SYMLOG-directions
        [
        set temp-index position actorAttractionBasis SYMLOG-directions
        ;; Compute candidate's SYMLOG direction vector, but negate it so a long vector turns into small "distance"
        set D (-( item (3 * temp-index) SYMLOG-directions-EPA-coordinates * item 0 current-transients +
          item ((3 * temp-index) + 1) SYMLOG-directions-EPA-coordinates * item 1 current-transients +
          item ((3 * temp-index) + 2) SYMLOG-directions-EPA-coordinates * item 2 current-transients))
        set rejection-level 4
        ]
        [
        ifelse actorAttractionBasis = "emotion similarity"
          [
          set D squared-EPA-distance [emotion] of turtle who  [emotion] of turtle theActor
          set rejection-level 20
          ]
          [
          ifelse actorAttractionBasis = "similar self-EPA"
            [
            set D squared-EPA-distance [fundamentals] of turtle who  [fundamentals] of turtle theActor
            set rejection-level 20
            ]
            [
            ifelse actorAttractionBasis = "min event tension"
              [
              ;; Restore actor's transients after prior object tests
              ask turtle theActor [set current-transients actor-current-transients] ;;--> this could probably be deleted now, because their are restored further down, but it also doesn't hurt.
              let object-current-transients current-transients
              ask turtle theActor
                [
                set alter [who] of myself  ;; for theActor, alter is the current object candidate
                ]
              compute-behavior-epa theActor ;; Find the actor's optimal action toward this test object.
              ;; The next call re-sets current-transients of actor and test object,
              ;; and also potential-event-deflection and personal-deflection of test actor
              compute-transients theActor
              set D  [potential-event-deflection] of turtle theActor

              ;; Restore actor's transients after prior object tests
              ;; Niko changed this! The actor's transients have to be restored at the end as well. Otherwise the transients of the last object candidate in the loop
              ;; would be changed permanently.
              ask turtle theActor [set current-transients actor-current-transients]
              set current-transients object-current-transients ;; Restore test object's transients
              set rejection-level 60
              ]
              [
              ifelse actorAttractionBasis = "max alter tension"
                [
                set D (- [personal-deflection] of turtle who) ;; Invert stress so that minimum will indicate maximum
                set rejection-level -0 ;; Rejection disabled for this option
                ]
                [
                ifelse actorAttractionBasis = "min alter tension"
                  [
                  set D personal-deflection
                  set rejection-level 20
                  ]
                  [
                  ;; Random choice of partner
                  set D random 25
                  set rejection-level 20
                  ]
                ]
              ]
            ]
          ]
        set D  precision (D) 2
        ]
      ]
    if any?  object-candidates
      [
      ;; Choose the candidate with lowest D as object
      set theObject [who] of  min-one-of object-candidates [D]
      ]
    ]
  ifelse theObject = -1 or theObject = theActor
    [
    ;; No object obtained.
    show "OBJECT PROBLEM."
    ]
    [
    ;; Set the new object in action-now, unless object was set by reciprocation or was set to whole-group
    if item 1 action-now = -1 [set action-now (list theActor theObject -1)]
    ]
end ;; select-object



to plot-faces-on-axes
  clear-patches ;; Clear axis labels
  ask graph-elements [die] ;; Clear axes and circles
  let x-axis 0 ;; Declaration

  ;; Install axes and outer-limits circle.
  create-graph-elements 1 ;; Vertical axis
    [
    set size world-height - 4
    set color grey
    set shape "line"
    facexy 0 2
    ]
  create-graph-elements 1 ;; Horizontal axis
    [
    set x-axis who
    set size world-width - 6
    set color grey
    set shape "line"
    facexy 2 0
    ]
  create-graph-elements 1 ;; Circle showing outer bound
    [
    set size world-width
    set color grey
    set shape "orbit"
    ]

  ;; Set up specific graphs.
  ifelse (view = "symlog") or (view = "SYMLOG")
    [
    ;; Enter labels
    ask turtle x-axis [set size world-width - 14] ;; Make room for labels
    ask patch 1 max-pycor [set plabel "Forward"]
    ask patch 3 min-pycor [set plabel "Backward"]
    ask patch (max-pxcor ) 0 [set plabel "Positive"]
    ask patch (min-pycor + 5) 0 [set plabel "Negative"]
    ;; Create a multiplier
    let multiplier 1
    if view = "SYMLOG" ;; Expand the graph: move the interactant furthest out all the way to edge of {P-N by F-B} graph.
      [
      let outlier [who] of max-one-of society
            [squared-EPA-distance (list 0 0 0) (list  0  (item 0 fundamentals) (0.707 * item 1 fundamentals - 0.707 * item 2 fundamentals))]
      ask turtle outlier
        [
        let outlier-length sqrt (squared-EPA-distance
               (list 0 0 0) (list  0  (item 0 fundamentals) (0.707 * item 1 fundamentals - 0.707 * item 2 fundamentals)))
        set u-d 0.707 * item 1 fundamentals + 0.707 * item 2 fundamentals ;; Create SYMLOG U-D from P and A
        let face-size actor-size * (u-d + scale-max-EPA) / (2 * scale-max-EPA)
        set face-size face-size / max-pxcor
        set multiplier (scale-max-EPA - face-size) / outlier-length
        ]
      ]
    ;; Use the multiplier and EPA to compute SYMLOG coordinates of all social entities.
    ask society ;; Interactants plus whole-group
      [
      set p-n multiplier * (item 0 fundamentals) * (max-pxcor / scale-max-EPA) ;; Create SYMLOG P-N from E
      set u-d 0.707 * item 1 fundamentals + 0.707 * item 2 fundamentals  ;; Create SYMLOG U-D from P and A
      set u-d 0.707 * item 1 fundamentals + 0.707 * item 2 fundamentals  ;; Create SYMLOG U-D from P and A
      ;; Create SYMLOG F-B from P and A
      set f-b multiplier * (0.707 * item 1 fundamentals - 0.707 * item 2 fundamentals) * (max-pxcor / scale-max-EPA)
      ]
    ;; Plot the interactants.
    ask society
      [
      set xcor p-n  ;; Positive-Negative, which is the same as Evaluation
      set ycor f-b ;; Forward-Backward
      set size actor-size * (u-d + scale-max-EPA) / (2 * scale-max-EPA) ;; Up-Down
      ]
    ]

    ;; Draw an EPA graph
    [
    ifelse view = "E x A (P)"
      [
      ask patch 0 max-pycor [set plabel "E+"]
      ask patch 0 min-pycor [set plabel "E-"]
      ask patch (max-pxcor ) 0 [set plabel "A+"]
      ask patch (min-pycor + 1) 0 [set plabel "A-"]
      ask society
        [
        set ycor (item 0 fundamentals / scale-max-EPA) * max-pycor ;; Evaluation
        set xcor (item 2 fundamentals / scale-max-EPA) * max-pxcor ;; Activity
        set size actor-size * (item 1 fundamentals + scale-max-EPA) / (2 * scale-max-EPA) ;; Potency
        ]
      ]
      [
      ifelse view = "P x A (E)"
        [
        ask patch 0 max-pycor [set plabel "P+"]
        ask patch 0 min-pycor [set plabel "P-"]
        ask patch (max-pxcor ) 0 [set plabel "A+"]
        ask patch (min-pycor + 1) 0 [set plabel "A-"]
        ask society
          [
          set ycor (item 1 fundamentals / scale-max-EPA) * max-pycor ;; Potency
          set xcor (item 2 fundamentals / scale-max-EPA) * max-pxcor ;; Activity
          set size actor-size * (item 0 fundamentals + scale-max-EPA) / (2 * scale-max-EPA) ;; Evaluation
          ]
        ]
        [
        ;; "P x E (A)"
        ask patch 0 max-pycor [set plabel "P+"]
        ask patch 0 min-pycor [set plabel "P-"]
        ask patch (max-pxcor ) 0 [set plabel "E+"]
        ask patch (min-pycor + 1) 0 [set plabel "E-"]
        ask society
          [
          set ycor (item 1 fundamentals / scale-max-EPA) * max-pycor ;; Potency
          set xcor (item 0 fundamentals / scale-max-EPA) * max-pxcor ;; Evaluation
          set size actor-size * (item 2 fundamentals + scale-max-EPA) / (2 * scale-max-EPA) ;; Activity
          ]
        ]
      ]
    ]
end ;; plot-faces-on-axes



to plot-transient-faces-on-axes
  clear-patches ;; Clear axis labels
  ask graph-elements [die] ;; Clear axes and circles
  let x-axis 0 ;; Declaration

  ;; Install axes and outer-limits circle.
  create-graph-elements 1 ;; Vertical axis
    [
    set size world-height - 4
    set color grey
    set shape "line"
    facexy 0 2
    ]
  create-graph-elements 1 ;; Horizontal axis
    [
    set x-axis who
    set size world-width - 6
    set color grey
    set shape "line"
    facexy 2 0
    ]
  create-graph-elements 1 ;; Circle showing outer bound
    [
    set size world-width
    set color grey
    set shape "orbit"
    ]

  ;; Set up specific graphs.
  ifelse (view = "symlog") or (view = "SYMLOG")
    [
    ;; Enter labels
    ask turtle x-axis [set size world-width - 14] ;; Make room for labels
    ask patch 1 max-pycor [set plabel "Forward"]
    ask patch 3 min-pycor [set plabel "Backward"]
    ask patch (max-pxcor ) 0 [set plabel "Positive"]
    ask patch (min-pycor + 5) 0 [set plabel "Negative"]
    ;; Create a multiplier
    let multiplier 1
    if view = "SYMLOG" ;; Expand the graph: move the interactant furthest out all the way to edge of {P-N by F-B} graph.
      [
      let outlier [who] of max-one-of society
            [squared-EPA-distance (list 0 0 0) (list  0  (item 0 current-transients) (0.707 * item 1 current-transients - 0.707 * item 2 current-transients))]
      ask turtle outlier
        [
        let outlier-length sqrt (squared-EPA-distance
               (list 0 0 0) (list  0  (item 0 current-transients) (0.707 * item 1 current-transients - 0.707 * item 2 current-transients)))
        set u-d 0.707 * item 1 current-transients + 0.707 * item 2 current-transients ;; Create SYMLOG U-D from P and A
        let face-size actor-size * (u-d + scale-max-EPA) / (2 * scale-max-EPA)
        set face-size face-size / max-pxcor
        set multiplier (scale-max-EPA - face-size) / outlier-length
        ]
      ]
    ;; Use the multiplier and EPA to compute SYMLOG coordinates of all social entities.
    ask society ;; Interactants plus whole-group
      [
      set p-n multiplier * (item 0 current-transients) * (max-pxcor / scale-max-EPA) ;; Create SYMLOG P-N from E
      set u-d 0.707 * item 1 current-transients + 0.707 * item 2 current-transients  ;; Create SYMLOG U-D from P and A
      set u-d 0.707 * item 1 current-transients + 0.707 * item 2 current-transients  ;; Create SYMLOG U-D from P and A
      ;; Create SYMLOG F-B from P and A
      set f-b multiplier * (0.707 * item 1 current-transients - 0.707 * item 2 current-transients) * (max-pxcor / scale-max-EPA)
      ]
    ;; Plot the interactants.
    ask society
      [
      set xcor p-n  ;; Positive-Negative, which is the same as Evaluation
      set ycor f-b ;; Forward-Backward
      set size actor-size * (u-d + scale-max-EPA) / (2 * scale-max-EPA) ;; Up-Down
      ]
    ]

    ;; Draw an EPA graph
    [
    ifelse view = "E x A (P)"
      [
      ask patch 0 max-pycor [set plabel "E+"]
      ask patch 0 min-pycor [set plabel "E-"]
      ask patch (max-pxcor ) 0 [set plabel "A+"]
      ask patch (min-pycor + 1) 0 [set plabel "A-"]
      ask society
        [
        set ycor (item 0 current-transients / scale-max-EPA) * max-pycor ;; Evaluation
        set xcor (item 2 current-transients / scale-max-EPA) * max-pxcor ;; Activity
        set size actor-size * (item 1 current-transients + scale-max-EPA) / (2 * scale-max-EPA) ;; Potency
        ]
      ]
      [
      ifelse view = "P x A (E)"
        [
        ask patch 0 max-pycor [set plabel "P+"]
        ask patch 0 min-pycor [set plabel "P-"]
        ask patch (max-pxcor ) 0 [set plabel "A+"]
        ask patch (min-pycor + 1) 0 [set plabel "A-"]
        ask society
          [
          set ycor (item 1 current-transients / scale-max-EPA) * max-pycor ;; Potency
          set xcor (item 2 current-transients / scale-max-EPA) * max-pxcor ;; Activity
          set size actor-size * (item 0 current-transients + scale-max-EPA) / (2 * scale-max-EPA) ;; Evaluation
          ]
        ]
        [
        ;; "P x E (A)"
        ask patch 0 max-pycor [set plabel "P+"]
        ask patch 0 min-pycor [set plabel "P-"]
        ask patch (max-pxcor ) 0 [set plabel "E+"]
        ask patch (min-pycor + 1) 0 [set plabel "E-"]
        ask society
          [
          set ycor (item 1 current-transients / scale-max-EPA) * max-pycor ;; Potency
          set xcor (item 0 current-transients / scale-max-EPA) * max-pxcor ;; Evaluation
          set size actor-size * (item 2 current-transients + scale-max-EPA) / (2 * scale-max-EPA) ;; Activity
          ]
        ]
      ]
    ]
end ;; plot-transient-faces-on-axes



to X_run-test-case
  ;; For checking results against results produced by program Interact
  SHOW "TEST CASE: FRIEND BERATE FREELOADER"
  SHOW "ACTOR FUNDAMENTAL:   2.75   1.88   1.38. BEHAVIOR FUNDAMENTAL:  -1.89  -0.19   0.83. OBJECT PERSON FUNDAMENTAL:  -1.44  -1.73  -0.47"
  SHOW "TRANSIENT OUTCOMES - ACTOR:   0.11   0.95   1.39. BEHAVIOR:  -0.71   0.52   0.75. OBJECT:  -1.15  -1.81  -0.23"
  SHOW "DEFLECTION:   9.90"
  SHOW "ACTOR EMOTIONS:   -0.88  -0.29   1.32"
  SHOW "OBJECT EMOTIONS:   -1.21  -1.26   0.29"

  load-male-equations
  set action-now [1 2 -1]
  ask turtle 1
    [
    create-behavior-arrow-to turtle 2
      [
      set shape (item 0 link-types)
      set color red
      ]
    set alter 2
    set ego-is-actor true
    set fundamentals (list 2.75   1.88   1.38) ;; friend
    set current-transients (list 2.75   1.88   1.38)
    set behavior (list -1.89  -0.19   0.83) ;; berate
    show word "Actor fundamental and input transient: " fundamentals
    show word "Behavior: " behavior
    ]
  ask turtle 2
    [
    set alter 1
    set ego-is-actor false
    set fundamentals (list -1.44  -1.73  -0.47) ;; freeloader
    set current-transients (list -1.44  -1.73  -0.47)
    show word "Object fundamental and input transient: " fundamentals
    ]
  ask turtle 1
    [
    compute-transients 1 ;; Gets transients for both actor and object
    show word "Event deflection: " potential-event-deflection
    show word "Actor outcome transient: " current-transients
    ]
  ask turtle 2
    [
    show word "Object outcome transient: " current-transients
    ]
  ask turtle 1
    [
    display-emotion 1
    show word "Actor emotion: " emotion
    ]
  ask turtle 2
    [
    display-emotion 2
    show word "Object emotion: " emotion
    ]

  SHOW "Test optimal behavior:"


  compute-behavior-epa 1
  ask turtle 1
  [
    show word "best behavior for agent 1 acting on agent 2" behavior

  ]
  ask turtle 2
  [
    set ego-is-actor TRUE
  ]
  compute-behavior-epa 2
  ask turtle 2
  [
    show word "best behavior for agent 2 acting on agent 1" behavior
  ]

  stop
  ;  Interact results for comparison (obtained with male sentiments and equations):
  ;  Person 1[_,friend],berate,Person 2[_,freeloader]
  ;  Actor Fundamental:   2.75   1.88   1.38. Behavior Fundamental:  -1.89  -0.19   0.83. Object person Fundamental:  -1.44  -1.73  -0.47. Setting Fundamental:  99.99  99.99  99.99.
  ;  Actor Transient inputs:   2.75   1.88   1.38. Behavior Transient inputs:  -1.89  -0.19   0.83. Object person Transient inputs:  -1.44  -1.73  -0.47. Setting Transient inputs:  99.99  99.99  99.99.
  ;  Actor Transient outcomes:   0.11   0.95   1.39. Behavior Transient outcomes:  -0.71   0.52   0.75. Object person Transient outcomes:  -1.15  -1.81  -0.23. Setting Transient outcomes:  99.99  99.99  99.99.
  ;  Deflection:   9.90.
  ;  Actor emotions:   -0.88  -0.29   1.32.    0.32, alarmed.
  ;  Object emotions:   -1.21  -1.26   0.29.    0.22, terrified.
  ;  Actor behaviors:    1.43  -0.75  -0.97.    0.88, obey.
  ;  Object behaviors:    0.02  -1.55  -0.81.    0.76, mumble to.
  ;  Actor labels:   -1.81  -0.09   0.64.    0.30, sexist.
  ;  Actor attributes:   -3.57  -2.62  -0.01.   99.99, No words in range.
  ;  Object labels:   -2.19   0.07  -0.62.    0.29, scrooge.
  ;  Object attributes:   -1.18   1.65  -0.82.   99.99, No words in range.
end ;; setup-test-case



to save-file-headers
  if run-number = 0 ;; Do this only once, immediately after a setup.
    [
    clear-output
;    if view-input [ show-inputs ]
    show-inputs ;; in Output box, every time setup is run
    set run-number 1
    if save-IO
      [
      file-open "data_GroupSimulator_actions.txt"
      file-print model-definition
       ;; List event variables that are being saved
      file-print (word "OUTPUT VARIABLES:: RUN#, EVENT#, IPA#, BEHAVIOR-EPA, DEFLECTION, ACTOR-RECIPROCATING, "
                     "ACTOR, ACTOR-FUNDAMENTAL-EPA, ACTOR-TRANSIENT-EPA, ACTOR-EMOTION-EPA, ACTOR-DEFLECTION, ACTS-ORIGINATED#, ACTS-RECEIVED#, "
                     "OBJECT, OBJECT-FUNDAMENTAL-EPA, OBJECT-TRANSIENT-EPA, OBJECT-EMOTION-EPA, OBJECT-DEFLECTION")
      file-close
      ]
    ]
end ;; save-file-headers



to save-event-data
  let object turtle alter
  if (who = item 0 action-now) ;; Record results for actor of current event
    [
    file-open "data_GroupSimulator_actions.txt"
      file-type  run-number file-type ", "
      file-type   ticks  file-type ", "
      file-type   (last action-now + 1)    file-type ", "
      file-type   behavior  file-type ", "
      file-type event-deflection  file-type ", "
      file-type actor-reciprocating  file-type ",     "
      file-type self file-type ", "
      file-type fundamentals  file-type ", "
      file-type to-two-decimals (current-transients)  file-type ", "
      file-type emotion  file-type ", "
      file-type personal-deflection  file-type ", "
      file-type acts-originated    file-type ", "
      file-type acts-received  file-type ",     "
      file-type [self] of turtle alter  file-type ", "
      file-type [fundamentals] of object  file-type ", "
      file-type to-two-decimals ([current-transients] of object) file-type ", "
      file-type [emotion] of object  file-type ", "
      file-type [personal-deflection] of object
      file-print " "
    file-close
    ]
end ;; save-event-data



to transition-to-new-run
  if run-size <= 0 [stop]
  if run-size > 0
    [
    if run-number = 1
      ;; Include model and variable information at top of RUNS data file.
      [
      file-open "data_GroupSimulator_runs.txt"
      file-print model-definition
      file-print (sentence (word "VARIABLES:: " "percents in IPA categories; ")
                           (word "for each agent: " "gender, ID#,")
                           (word "fundamentals, # of acts-initiated, " "# of acts-received; ")
                           (word "% of possible ties implemented;  " "network from-to matrix "))
      file-close
      ]
    let IPA-percents []
    foreach IPA-counts [ ?1 -> set IPA-percents (sentence IPA-percents  precision (100 * ( ?1 / sum IPA-counts)) 2) ]
    file-open "data_GroupSimulator_runs.txt"
    ;; list-of-who-originated-received shows each turtle's breed, number, fundamentals, # of acts-originated, # of acts-received
    file-print (list (IPA-percents) list-of-who-originated-received networking-percent network-matrix)
    file-close
    ]
  let temp-run-number run-number + 1
  ;; Start a new analysis.
  ifelse change-next-group's = "sentiments"
    [
    ifelse members-from-file-input
      [
      ;; Change sentiments by random draws with centers at original-fundamentals-list
      re-start
      ]
      [
      setup ;; Create a new group.
      ]
    ]
    [
    ifelse change-next-group's = "starting feelings"
      [
      re-start ;; Run the group again with new starting stresses and different random choices.
      ]
      [
      ;; Change just randomization.
      set start-number-for-random new-seed
      set re-playing true
      re-start
      ]
    ]
  set run-number temp-run-number
end ;; transition-to-new-run



to show-inputs
  if fast [stop]
  output-print start-number-for-random
  let sex ""
  let actor-object-choices ""
  foreach sort-by [ [?1 ?2] -> [who] of ?1 < [who] of ?2 ] society
    [ ?1 ->
    ask ?1
      [
        ifelse is-male? self
          [set sex "male"]
          [ifelse is-female? self
              [set sex "female"]
              [set sex "whole-group"]
          ]
        set actor-object-choices (word " \"" actorBasis "\"" )
        set actor-object-choices (word actor-object-choices " \"" attractionBasis "\"")
        output-print (word "[ \"" sex  "\" " fundamentals  " " original-transients actor-object-choices " ]")
      ]
    ]
  output-print "" ;; blank line before action descriptions
end ;; show-inputs



to update-matricies
  ;; See IPA profile information in SETUP-BALES-CATEGORIES
  let theBehavior [behavior] of (turtle (item 0 action-now))
  let distances []
  let i 0
  while [i < 12]
    [
    ;; Make a list of distances from the behavior-epa to each of the IPA-EPAs.
    set distances sentence distances   squared-EPA-distance theBehavior   sublist IPA-EPAs (3 * i) ((3 * i) + 3)
    set i  i + 1
    ]
  let current-behavior-category position (min distances) distances ;; The chosen category is the minimum distance in the list.
  ;; Save the IPA category of the current behavior as last item in action-now list
  set action-now replace-item 2 action-now current-behavior-category
  ;; Add this act to tally of acts in IPA categories
  set IPA-counts   replace-item current-behavior-category IPA-counts  ((item current-behavior-category IPA-counts) + 1)
  ;; Update who-to-whom matrix
  let row item 0 action-now ;; Actor
  let col item 1 action-now ;; Object
  let multiplier count society
  set network-matrix replace-item  (multiplier * row + col)  network-matrix  (item (multiplier * row + col) network-matrix + 1)
end ;; update-matricies



to show-Bales-outputs [theActor]
  if fast [stop]
  ;; Summarize discourse
  let i 0
  let agent ""
  let object ""
  ifelse is-male? turtle theActor ;; Actor is either male or female,never whole-group
    [set agent word "Male " theActor] ;; E.g., gives Male 3
    [set agent word "Female " theActor] ;; E.g., gives Female 1
  let recipient [alter] of turtle theActor
  ifelse is-male? turtle recipient
    [set object word "male " recipient] ;; E.g., gives Male 4
    [ifelse is-female? turtle recipient
        [set object word "female " recipient] ;; E.g., gives Female 2
        [set object "whole group"]
    ]
  let projections []
  set i 0
  while [i < 26]
    [
    ;; Make a list of projections from the agent's behavior-epa to each of the SYMLOG-category-EPAs.
    set projections sentence projections   SYMLOG-projection [behavior] of turtle theActor
                                                             sublist SYMLOG-directions-EPA-coordinates (3 * i) ((3 * i) + 3)
    set i  i + 1
    ]
  let region 0
  let bearing ""
  let behavior-set ""
  ifelse squared-EPA-distance  [behavior] of turtle theActor  [0 0 0] < 1.25
    [
    ;; Too close to neutral to warrant projection
    set bearing "neutral"
    set region 26 ;; Neutral verbs are last in list
    ]
    [
    set region position (max projections) projections
    set bearing  (item region SYMLOG-directions)
    ]
    ifelse behavior-type = "verbal"
      [
      set behavior-set item region SYMLOG-verbs
      ]
      [
      set behavior-set item region SYMLOG-physical-verbs
      ]
  let message (word agent " to " object  ". Act = IPA_" (1 + item 2 action-now) "; "
                             behavior-set " (" bearing "). " "Allusions: "  (item (find-SYMLOG-personality-type [current-transients] of turtle theActor) SYMLOG-topics) )
  output-print message
end ;; show-Bales-outputs



to-report list-of-who-originated-received
  ;; Create a list of each turtle's breed, number, fundamentals, number of acts-originated, and number of acts-received for saving in a file.
  let theList []
  let i 0
  if address-group-Pr < 0 [set i 1]  ;; No whole group entity, so no turtle 0
  ask turtle i [set theList (sentence self fundamentals acts-originated acts-received)]  ;; Start the list
  set i  i + 1
  loop
    [
    ask turtle i [set theList sentence theList (sentence self fundamentals acts-originated acts-received)]
    if i = count interactants [report theList]
    set i  i + 1
    ]
end ;;list-of-who-originated-received



to-report find-SYMLOG-personality-type [EPA-list]
  let distances []
  let i 0
  while [i < 27]
    [
    ;; Make a list of distances from the agent's fundamental-epa to each of the SYMLOG-category-EPAs.
    set distances sentence distances   squared-EPA-distance EPA-list
                                                             sublist SYMLOG-directions-EPA-coordinates (3 * i) ((3 * i) + 3)
    set i  i + 1
    ]
  report  position (min distances) distances ;; The chosen category is the minimum distance in the list.
end ;;SYMLOG-value



to-report networking-percent
  let group-links 0
  if any? whole-groups [ask whole-group 0 [set group-links count my-past-links]]
  let possible-interpersonal-links count interactants * (count interactants - 1) / 2
  report precision ((100 * (count past-links - group-links)) / possible-interpersonal-links) 2 ;; % of possible links
end; ;; networking-percent



to setup-Bales-categories
  set SYMLOG-directions ["U" "UP" "UPF"   "UF" "UNF" "UN"   "UNB" "UB" "UPB"   "P" "PF" "F"   "NF" "N" "NB"
    "B" "PB" "DP"   "DPF" "DF" "DNF"   "DN" "DNB" "DB"   "DPB" "D" "0"]
  set SYMLOG-topics ["success, power, the future" "popularity, personal involvement" "teamwork, solidarity"
    "efficiency, loyalty, harmony" "authority, rules, obedience" "adventure, challenges, ambition"
    "sensualism, nonconformity, being anti-authority" "relativism, expressiveness" "being protective, supportive"
    "equality, democracy, tolerance" "benevolence, altruism" "established rules, morality, religion"
    "conscience, self-control" "self-sufficiency, individualism, isolationism" "cynicism, subversiveness"
    "creativity, innovation, anti-religion" "friendship, collaboration" "trust, appreciation"
    "faithfulness, dedication" "introspection, moderation" "self-sacrifice, asceticism"
    "reclusion, being anti-social" "detachment, resignation" "noncooperation, uncertainty"
    "permissiveness, indulgence" "self-denial, self-distain, abstinence" "ambivalence, uncertainty"] ;; Extracted from Bales 1999.
  ;; SYMLOG-verbs are selections from IU EPA dictionary, to match P-N, U-D, F-B profiles. Words in prens are supplements from Bales & Cohen.
  set SYMLOG-verbs [
    ;;"U" "UP" "UPF" "UF"
    "commands, hollers at, disagrees with"   "asks about __, tells __ to, answers"   "corrects, advises, confers with"   "orders, persuades, reprimands"
    ;; "UNF" "UN" "UNB" "UB" "UPB"
    "silences, forbids, ~interrogates"   "criticizes, scolds, quarrels with"   "dares, giggles at, taunts"   "joshes, banters with, is saracastic toward"   "chatters to, ~encourages, ~jokes with"
    ;; "P" "PF" "F" "NF" "N"
     "admits __ to, consults,agrees with"   "explains to, listens to, confides in"   "~prays for, ~assures, ~calms"   "~demeans, ~threatens, ~talks down to"   "nags, ridicules, insults"
    ;; "NB" "B" "PB" "DP" "DPF"
    "wheedles, quibbles with, grouses at"   "~babbles to, ~banters with, ~fusses over"   "~chats with, ~cheers up, ~applauds"   "~offers __ to, ~apologizes to, ~prays with"   "~makes up with, ~reassures, ~thanks"
    ;; "DF" "DNF" "DN" "DNB" "DB"
    "whispers to, ~murmurs to, ~bows to"   "defers to,~fibs to, ~scoffs at"   "begs from. mumbles to, whines to"   "begs, sucks up to, gibes"   "~drones on at, ~pleads with, ~toadies to"
    ;; "DPB" "D"   "Neutral"
    "gives in to, ~chit chats with, ~flatters"   "murmurs to, ~whispers to, ~stammers at"    "implore, yield to, request _ from"]
  set SYMLOG-physical-verbs [
    ;;"U" "UP" "UPF" "UF"
    "wrestle with, tackle, overthrow, force"    "save, have sex with, kiss"    "heal,guide, medicate"    "jail, stop, subdue"
    ;; "UNF" "UN" "UNB" "UB"
    "stare down, confine constrain"    "rape, sock, shove"    "brawl with, slap, jump on"    "chase, rough-house with, hurry"
    ;; "UPB" "P" "PF" "F"
    "lust for, ~tickle, ~horse around with"    "snuggle, observe, suckle, feel"    "comfort, cuddle, draw near to"    "~mesmerize, ~test, ~fetter, ~inject with medicine"
    ;; "NF" "N" "NB" "B"
    "glare at, ~stare down, ~leer at, ~fetter"    "torture, molest, injure"    "maim, paw, run away from"    "~fuss over, ~flee, ~imitate, ~run away from"
    ;; "PB" "DP" "DPF" "DF"
    "~look_at, ~visit, ~adorn, ~lust for"    "lie_on, ~bow to, ~adorn, ~dress"    "bow to, ~wash, ~lie_on, ~dress"    "~fetter, ~bow to, ~stare at, ~inject with medicine"
    ;; "DNF" "DN" "DNB" "DB"
    "frown at, stare at, leer at"    "peep at, cling to,  submit to, look away from"    "spit on, hide from, avoid"    "~submit to, ~hide from, ~avoid, ~cling to"
    ;; "DPB" "D"    "Neutral"
    "~lie_on, ~adorn, ~visit, ~__"    "~cling to, ~submit to, ~fetter, ~look away from"     "tap, nudge, gaze at"]
  set SYMLOG-directions-EPA-coordinates [
     0.000 2.121 2.121   1.732 1.732 1.732   2.121 2.121 0.000
     0.000 3.000 0.000   -2.121 2.121 0.000   -1.732 1.732 1.732
     -2.121 0.000 2.121   0.000 0.000 3.000   2.121 0.000 2.121
     3.000 0.000 0.000   1.732 1.732 -1.732   0.000 2.121 -2.121
     -1.732 1.732 -1.732   -3.000 0.000 0.000   -1.732 -1.732 1.732
     0.000 -2.121 2.121   1.732 -1.732 1.732   1.732 -1.732 -1.732
     2.121 0.000 2.121   0.000 0.000 -3.000   -2.121 0.000 -2.121
     -1.732 -1.732 -1.732   -2.121 -2.121 0.000   0.000 -3.000 0.000
     2.121 -2.121 0.000   0.000 -2.121 -2.121  0 0 0]
  set IPA-categories [ " gives reinforcement to " " provides tension release for " " offers agreement with " " makes suggestion to "
    " gives opinion to " " gives orientation to " " requests orientation from " " requests opinion from " " requests suggestion from "
    " disagrees with " " shows tension toward " " is antagonistic toward " ] ;;  - NOT USED
  ;; IPA-EPAs computed from sample verbs
  ifelse IPA-coding-basis = "1978 sentiments"
    [
    ;; 1978 Unisex EPAs for IPA categories
    set IPA-EPAs [1.78 1.29 0.21        1.48 0.91 1.12        1.60 0.78 0.01         1.28 1.18 0.25         0.88  1.48  0.46        1.68 1.62 -0.14
                  0.50 0.62 0.45        0.48 0.74 0.16        0.30 0.24 0.09        -1.00 0.35 0.45        -0.89 -0.16 0.35        -0.82 0.71 1.32]
    ]
    [
    ifelse IPA-coding-basis = "2004 sentiments"
      [
      ;; 2004 Unisex EPAs for IPA categories
      set IPA-EPAs [2.78 2.22 1.20   2.71 1.90 1.78   1.24 0.64 0.32   1.03 1.07 0.64   1.12 0.96 0.02   1.98 1.54 0.42
                    0.68 0.81 0.38   0.22 0.25 0.27   0.21  0.38  0.64   -0.86 -0.01 -0.22   -0.89 0.19 0.13   -1.27 0.68 0.93]
      ]
      [ ;; IPA-coding-basis = "Canada 2001 sentiments"
      ;; 2001 Canada Unisex EPAs for IPA categories
      set IPA-EPAs [2.46  1.56  0.64    1.72  0.77  0.73    1.70  0.80  0.34    1.29  1.20  0.35    0.78  0.89  0.18    1.70  1.25  0.37
                    0.67  0.54  0.20    0.69  0.67  0.23    0.38  0.39  0.10    -0.94  0.85  0.05    -0.76  0.40  0.28    -0.77  0.91  0.26]
      ]
    ]
end ;; setup-Bales-categories



;; COMPUTATIONS

to-report compute-emotion [fundamental-and-transient]
  ;; Terms for emotions-coefficients array: c Me Mp Ma Ie Ip Ia MeIe MeIp MeIa MpIe MpIp MpIa MaIe MaIp MaIa
  let emoCoeffs array:from-list (n-values 9 [0]) ;; Declaration
  let transients array:from-list [0 0 0] ;; Declaration
  let vector array:from-list [0 0 0] ;; Declaration
  let emo array:from-list [0 0 0] ;; Declaration
  let i 0 ;; Declaration
  let j 0 ;; Declaration
  let k 0 ;; Declaration

  ;; Create the 3X3 matix to be inverted in Heise (2007) eq 14.13. Call it emoCoeffs
  set i 0
  while [i < 48] ;; Select rows
    [
    set j 0
    while [j < 3]
      [
      ;; start with M coefficients defining E matrix
      array:set emoCoeffs ((3 * (i / 16)) + j) array:item emotion-coefficients (i + 1 + j)
      ;; Add in interaction coefficients times fundamental
      set k 7 ;; Start with coefficient for MeIe
      while [k < 16]
        [
        array:set emoCoeffs ((3 * (i / 16)) + j)  array:item emoCoeffs ((3 * (i / 16)) + j) +
                                                  (item ((k - 7) / 3) fundamental-and-transient) * array:item emotion-coefficients (i + j + k)
        set k (k + 3)
        ]
      set j (j + 1)
      ]
    set i (i + 16)
    ]
  ;; Invert emoCoeffs
  let Q (invert-3-by-3 emoCoeffs)

  ;; Create the 3-element vector that is post-multiplied with emoCoeffs-inverse in eq 14.13. Call it vector
  set i 0
  while [i < 3]
    [
    array:set vector i (item (i + 3) fundamental-and-transient) ;; Begin with transient
    array:set vector i (array:item vector i) - (array:item emotion-coefficients (16 * i)) ;; Subtract the constants
    set j 0
    while [j < 3]
      [
      ;; Subtract Rr
      array:set vector i (array:item vector i) -
                         ((item (j) fundamental-and-transient) * (array:item emotion-coefficients ((16 * i) + 4 + j) ) )
      set j (j + 1)
      ]
    set i (i + 1)
    ]
  ;; Multiply the inverted matrix times the vector to get the emotion, e
  set i 0
  while [i < 3]
    [
    set j 0
    while [j < 3]
      [
      array:set emo i (array:item emo i) + array:item Q ((i * 3) + j) * array:item vector j
      set j (j + 1)
      ]
    array:set emo i precision (array:item emo i) 2
    set i (i + 1)
    ]
  report array:to-list emo
end ;; compute-emotion



to compute-transients [ anAgent ]
  if equations = "identity echoes" [stop] ;; Transients can explode when behavior EPAs randomly echo identity EPAs.
  let event-transients array:from-list  (n-values 9 [0]) ;; Declaration and initialization
  make-ACT-products anAgent true ;; "True" means we want a vector of transients and their cross-products.
  let equation 0
  while [equation < 9]
    [
    ;; Compute outcome transients for Ae, Ap, Aa, Be, Bp, Ba, Oe, Op, Oa.
    let i 0
    while [i < 64]
      [
      ;; Comput sum of coefficients times transient terms.
      ;; Skip first 9 coefficients since they constitute identity matrix used in finding behavior epa
      array:set event-transients equation (array:item event-transients equation +
                                    ((array:item abo-coefficients ((equation * 73) + 9 + i)) * (array:item variable-vector i)))
      set i (i + 1)
      ]
    array:set event-transients equation  (array:item event-transients equation)
    set equation (equation + 1)
    ]
  ifelse [ego-is-actor] of turtle anAgent ;; Compute current transients for actor and object.
    [
    ask turtle anAgent [set current-transients sublist array:to-list event-transients 0 3] ;; actor
    ask turtle [alter] of turtle anAgent [set current-transients sublist array:to-list event-transients 6 9] ;; object
    ]
    [
    ask turtle [alter] of turtle anAgent [set current-transients sublist array:to-list event-transients 0 3] ;; object
    ask turtle anAgent [set current-transients sublist array:to-list event-transients 6 9] ;; actor
    ]
  ;; Compute event deflection and store it in a temporary place.
  let i 0
  ask turtle anAgent [set potential-event-deflection 0]
  while [i < 3]
    [
    ask turtle anAgent [set potential-event-deflection ([potential-event-deflection] of turtle anAgent +
                                                  square (array:item event-transients i -
                                                  item i ([fundamentals] of turtle anAgent)))]
    ask turtle anAgent [set potential-event-deflection ([potential-event-deflection] of turtle anAgent +
                                                   square (array:item event-transients (i + 3) -
                                                   item i ([behavior] of turtle anAgent)))]
    ask turtle anAgent [set potential-event-deflection ([potential-event-deflection] of turtle anAgent +
                                                   square (array:item event-transients (i + 6) -
                                                   item i ([fundamentals] of turtle [alter] of turtle anAgent)))]
    set i (i + 1)
    ]
  ask turtle anAgent [set potential-event-deflection precision ([potential-event-deflection] of turtle anAgent) 2]
  compute-personal-deflection anAgent
end ;; compute-transients



to compute-personal-deflection [anAgent]
  ask turtle anAgent [set personal-deflection precision (squared-EPA-distance fundamentals current-transients) 2]
end ;; compute-personal-deflection



to compute-behavior-epa [ anActor ]
  if equations = "random acts" ;; Center random acts around origin with SD=initial-tension
    [
    ask turtle anActor [set behavior  random-EPA (list 0 0 0) initial-tension]
    stop ;; Leave the compute-behavior-epa procedure
    ]
  if equations = "identity echoes" ;; Behavior EPAs randomly echo identity EPAs. Transients disabled because of instability.
    [
    ask turtle anActor [set behavior  random-EPA fundamentals initial-tension]
    stop ;; Leave the compute-behavior-epa procedure
    ]
  ;; Set up I-beta
  make-ACT-products anActor false ;; Create the transient part of I-beta
  set I-beta array:from-list sentence(n-values 9 [1]) (array:to-list variable-vector) ;; Expand I-beta with fundamentals
  let i 0
  while [i < 3] ;; Add fundamentals to I-beta
    [
    ifelse [ego-is-actor] of turtle anActor
      [
      array:set I-beta i (item i ([fundamentals] of turtle anActor) )
      array:set I-beta (i + 6) (item i ([fundamentals] of turtle [alter] of turtle anActor) )
      set i (i + 1)
      ]
      [
      array:set I-beta i (item i ([fundamentals] of turtle [alter] of turtle anActor) )
      array:set I-beta (i + 6) (item i ([fundamentals] of turtle anActor) )
      set i (i + 1)
      ]
    ]

  let SIM array:from-list (n-values 27 [0]) ;; Initialize the 3X9 pre-multiplier matrix in eq 12.21
  let GIM array:from-list (n-values 9 [0]) ;; Initialize the 1X9 post-multiplier matrix in eq 12.21, transposed
  let SIMSIM array:from-list (n-values 9 [0]) ;; Initialize the 3X3 matrix to be inverted
  let SIMGIM array:from-list (n-values 3 [0]) ;; Initialize the 1X3 final matrix in eq 12.21
  let epa 0
  while [epa < 3]
    [
    let sim-row epa * 9
    let beta-row epa * 73
    let aboEPA 0
    while [aboEPA < 9]
      [
      let coefs-col aboEPA * 73
      set i 0
      while [i < 73]
        [
        array:set SIM (sim-row + aboEPA) (array:item SIM (sim-row + aboEPA) +
                                         ((array:item I-beta i) * array:item S-beta (beta-row + i) *
                                         (- array:item abo-coefficients (coefs-col + i)) ) )
        if (epa = 0)
          [
          array:set GIM aboEPA ((array:item GIM aboEPA) +
                               ((array:item I-beta i) * (array:item g i) * (- array:item abo-coefficients (coefs-col + i)) ) )
          ]
        set i (i + 1)
        ]
      set aboEPA (aboEPA + 1)
      ]
    set epa (epa + 1)
    ]
  let row 0
  while [row < 3]
    [
    let col 0
    while [col < 3]
      [
      let aboEPA 0
      while [aboEPA < 9]
        [
        array:set SIMSIM ((row * 3) + col) (array:item SIMSIM ((row * 3) + col) +
                                           ((array:item SIM ((row * 9) + aboEPA)) * array:item SIM ((col * 9) + aboEPA) ) )
        if (row = 0)
          [
          array:set SIMGIM col ((array:item SIMGIM col) + ((array:item SIM ((col * 9) + aboEPA)) * (array:item GIM aboEPA) ) )
          ]
        set aboEPA (aboEPA + 1)
        ]
      set col (col + 1)
      ]
    set row (row + 1)
    ]
  let y  invert-3-by-3 SIMSIM
  ;; Multiply inverted matrix times SIMGIN vector
  let beh array:from-list (n-values 3 [0]) ;; Initialization
  set row 0
  while [row < 3]
    [
    let col 0
    while [col < 3]
      [
      array:set beh row (array:item beh row - (array:item y ((row * 3) + col) * array:item SIMGIM col ) )
      set col (col + 1)
      ]
    array:set beh row precision (array:item beh row) 2
    set row (row + 1)
    ]
  ask turtle anActor [set behavior array:to-list beh]
end ;; compute-behavior-epa



to make-ACT-products  [ anAgent making-t ] ;; anAgent is a turtle; making-t is
                                       ;; true for constructing vector t, versus
                                       ;; false for constructing transient part of I-beta
  let i 0
  let j 0
  array:set variable-vector 0 1
  ;; Fill terms 1 to 9 with transients. Use ones in place of behavior EPA for I-beta
  while [i < 3]
    [
    ifelse making-t
      [ array:set variable-vector (i + 4) item i ([behavior] of turtle anAgent) ]
      [ array:set variable-vector (i + 4) 1 ]
    ifelse [ego-is-actor] of turtle anAgent
      [
      array:set variable-vector (i + 1) (item i ([current-transients] of turtle anAgent) )
      array:set variable-vector (i + 7) (item i ([current-transients] of turtle [alter] of turtle anAgent) )
      ]
      [
      array:set variable-vector (i + 1) (item i ([current-transients] of turtle [alter] of turtle anAgent) )
      array:set variable-vector (i + 7) (item i ([current-transients] of turtle anAgent) )
      ]
    set i (i + 1)
    ]
  ;; Fill terms 10 to 36 with 2nd-order terms:
  ;; (10)AeBe AeBp AeBa AeOe AeOp AeOa (16)ApBe ApBp ApBa ApOe ApOp ApOa
  ;; (22)AaBe AaBp AaBa AaOe AaOp AaOa (28)BeOe BeOp BeOa BpOe BpOp BpOa BaOe BaOp BaOa
  set i 0
  while [i < 3]
    [
    set j 0
    while [j < 3]
      [  ;; First A_B_ terms
      array:set variable-vector (10 + (i * 6) + j) array:item variable-vector (1 + i) * array:item variable-vector (4 + j)
      set j (j + 1)
      ]
    set j 0
    while [j < 3]
      [  ;; Next A_O_ terms
      array:set variable-vector (13 + (i * 6) + j) array:item variable-vector (1 + i) * array:item variable-vector (7 + j)
      set j (j + 1)
      ]
    set i (i + 1)
    ]
  set i 0
  while [i < 3]
    [
    set j 0
    while [j < 3]
      [ ;; Now B_O_ terms
      array:set variable-vector (28 + (i * 3) + j) array:item variable-vector (4 + i) * array:item variable-vector (7 + j)
      set j (j + 1)
      ]
    set i (i + 1)
    ]
  ;; Fill terms 37 to 63 with 3rd-order terms
  ;; (37)AeBeOe AeBeOp AeBeOa AeBpOe AeBpOp AeBpOa AeBaOe AeBaOp AeBaOa
  ;; (46)ApBeOe ApBeOp ApBeOa ApBpOe ApBpOp ApBpOa ApBaOe ApBaOp ApBaOa
  ;; (55)AaBeOe AaBeOp AaBeOa AaBpOe AaBpOp AaBpOa AaBaOe AaBaOp AaBaOa
  set i 0
  while [i < 3]
    [
    set j 0
    while [j < 3]
      [ ;; AeB_O_ terms
      array:set variable-vector (37 + (i * 3) + j) array:item variable-vector (10 + i) * array:item variable-vector (7 + j)
      set j (j + 1)
      ]
    set i (i + 1)
    ]
  set i 0
  while [i < 3]
    [
    set j 0
    while [j < 3]
      [ ;; ApB_O_ terms
      array:set variable-vector (46 + (i * 3) + j) array:item variable-vector (16 + i) * array:item variable-vector (7 + j)
      set j (j + 1)
      ]
    set i (i + 1)
    ]
  set i 0
  while [i < 3]
    [
    set j 0
    while [j < 3]
      [ ;; AaB_O_ terms
      array:set variable-vector (55 + (i * 3) + j) array:item variable-vector (22 + i) * array:item variable-vector (7 + j)
      set j (j + 1)
      ]
    set i (i + 1)
    ]
end ;; make-ACT-products



;; MATHEMATICAL ROUTINES


to-report square [ x ]
  report x * x
end ;; square



to-report SYMLOG-projection  [ listA listB]
  report ( (item 0 listA * item 0 listB) + (item 1 listA * item 1 listB) + (item 2 listA * item 2 listB) )
end ;; to-report SYMLOG-projection



to-report squared-EPA-distance [ listA listB]
  report ( square (item 0 listA - item 0 listB) + square (item 1 listA - item 1 listB) + square (item 2 listA - item 2 listB) )
end ;; to-report squared-EPA-distance



to-report to-two-decimals [ number-list ]
  let i 0
  let new-list []
  while [i < length number-list]
    [
    set new-list  sentence  new-list  precision item i number-list 2
    set i  (i + 1)
    ]
    report new-list
end ;; to-report to-two-decimals



to-report random-EPA [centers-list sd]
  ;; Draw a random EPA profile from distributions with midpoints defined by the three numbers in the centers list
  ;; and standard deviations of sd. But discard and draw again if the polarization of the profile is too large.
  let theProfile [-100 -100 -100] ;; This profile has too large a polarization.
  while [(squared-EPA-distance theProfile (list 0 0 0)) > max-random-vector-length * max-random-vector-length]
    [
    set theProfile replace-item 0 theProfile precision (random-normal (item 0 centers-list) sd ) 2
    set theProfile replace-item 1 theProfile precision (random-normal (item 1 centers-list) sd ) 2
    set theProfile replace-item 2 theProfile precision (random-normal (item 2 centers-list) sd ) 2
    ]
  report theProfile
end ;; random-EPA



to-report invert-3-by-3 [ theMatrix ] ;; Invert a 3X3 matrix sent as a 9-item array with rows in sequence
  let d1 0 ;; variable declaration
  let d2 0 ;; variable declaration
  let i 0
  while [ i < 9 ] ;; select rows in turn by incrementing by 3
    [
    set d1 array:item theMatrix i ;; Set d = theMatrix[i][0]
    let j 0
    while [ j < 2 ] ;; Iterate through the first two cells of the current row
      [
      array:set theMatrix (i + j) (array:item theMatrix (i + j + 1)) / d1  ;; Set theMatrix[i][j] = theMatrix[i][j + 1] / d
      set j (j + 1) ;; Increment j index
      ]
    array:set theMatrix (i + 2) (1 / d1) ;; Set theMatrix[i][c - 1] = 1 / d
    let k 0
    while [ k < 9 ] ;; Select rows in turn again, within the first row selection
      [
      if (k != i)
        [
        set d2 array:item theMatrix k ;; Set d = theMatrix[k][0]
        let m 0
        while [ m < 2 ] ;; Iterate through the first two cells of the current row
          [
          ;; Set theMatrix[k][m] = theMatrix[k][m + 1] - theMatrix[i][m] * d
          array:set theMatrix (k + m) array:item theMatrix (k + m + 1) - ((array:item theMatrix (i + m)) * d2) ;; Reset the value of the cell
          set m (m + 1)
          ]
        array:set theMatrix (k + 2) (- array:item theMatrix (i + 2)) * d2 ;; Set theMatrix[k][c - 1] = -theMatrix[i][c - 1] * d
        ]
      set k (k + 3) ;; Increment k index
      ]
    set i (i + 3) ;; Increment i index
    ]
    report  theMatrix
end ;; invert-3-by-3



to load-unisex-equations
  ;; SET UNISEX COEFFICIENTS FOR IMPRESSION-FORMATION EQUATIONS
  ;; Terms corresponding to columns in coefficient array are as follows:
  ;;   C Ae Ap Aa Be Bp Ba Oe Op Oa
  ;;   AeBe AeBp AeBa AeOe AeOp AeOa ApBe ApBp ApBa ApOe ApOp ApOa AaBe AaBp AaBa AaOe AaOp AaOa BeOe BeOp BeOa BpOe BpOp BpOa BaOe BaOp BaOa
  ;;   AeBeOe AeBeOp AeBeOa AeBpOe AeBpOp AeBpOa AeBaOe AeBaOp AeBaOa ApBeOe ApBeOp ApBeOa ApBpOe ApBpOp ApBpOa ApBaOe ApBaOp ApBaOa AaBeOe AaBeOp AaBeOa AaBpOe AaBpOp AaBpOa AaBaOe AaBaOp AaBaOa
  ;; Rows are coefficients in equation for Ae', Ap', Aa', Be', Bp', Ba', Oe', Op', or Oa'
  ;; Coefficients are based on Stata SEM (Struc Eq Model) analysis of UNC515 sentences, sexes constrained to equality.
  ifelse member? "US" equations ;; Does equations string include "US"
  [ ;; load US unisex equations
    set abo-coefficients array:from-list
    ;; Two-phase equations from UNC515, median split for ANOVA, sexes constrained equal in Stata SEM
    [
      -1  0 0 0 0 0 0 0 0 -0.18 0.41 0 0 0.42 0 -0.11 0.03 0.06 0 0.05 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0.12 -0.05 0 -0.05 0 0 0 0 0 0.03 -0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0  -1  0 0 0 0 0 0 0 -0.10 0 0.56 0.06 -0.10 0.44 0 0.04 0 0 0 0 0 0 0 0 0 -0.05 0 0 0 0 0 0 0 0 0 0 0.01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0  -1  0 0 0 0 0 0 0.15 0.05 0 0.69 -0.06 0 0.29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0  -1  0 0 0 0 0 -0.14 0.11 0 0 0.55 0 -0.13 0 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.11 -0.05 0 -0.02 0 0 0 0 0 0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0  -1  0 0 0 0 0.05 0 0.15 0 -0.14 0.69 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0  -1  0 0 0 0.17 0.02 -0.05 0.30 0.04 0 0.63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0  -1  0 0 0.00 0 0 0 0.11 0 0 0.62 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.05 0 0 -0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0  -1  0 -0.40 0 0 0 0.16 -0.11 0 -0.11 0.66 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0.02 0 0 -0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0  -1  -0.02 0 0 0 0.02 0 0 0.02 -0.05 0.72 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ]
    ;; EMOTION-IDENTITY EQUATIONS
    ;; Terms corresponding to columns in coefficients matrix are as follows:
    ;;   c Me Mp Ma Ie Ip Ia MeIe MeIp MeIa MpIe MpIp MpIa MaIe MaIp MaIa
    ;; Rows are 16 coefficients in equation for evaluation amalgamation, potency amalgamation, and activity amalgamation
    ;; Coefficients are based on IU emotion-identity impression-formation equations, males and females averaged.
    set emotion-coefficients array:from-list
    [  -0.32 0.69 -0.36 0 0.47 0.01 -0.07 0.12 0 0 0 0 0 0 0 0
       -0.18 -0.18 0.65 0.01 -0.01 0.59 0.05 0 0 0 0 0 0 0 0 0
       -0.11 -0.04 0.07 0.53 -0.02 -0.02 0.64 0 0 0 0 0 0 0 0 0 ]
  ]
  [ ifelse member? "Canada" equations ;; Does equations string include "Canada"
    [  ;; load Canadian unisex equations
      set abo-coefficients array:from-list
      ;; Male coefficients from two-phase analyses of Canada (ANOVA then SEM). Male-female coefficient constrained equal if not significantly different.
      [
        -1 0 0 0 0 0 0 0 0 -0.24 0.45 0 0 0.51 0 0 0 0 0 0.09 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 -1 0 0 0 0 0 0 0 -0.21 0 0.54 0 0 0.45 0.12 0 -0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 -1 0 0 0 0 0 0 -0.07 0 0 0.71 0 0.16 0.29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 -1 0 0 0 0 0 0.00 0.08 0 0 0.73 0 0 0 0 0 0.06 0 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 -1 0 0 0 0 0.00 0 0 0 0 0.63 0.19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 -1 0 0 0 0.02 0 0 0.21 -0.04 0.12 0.52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 -1 0 0 -0.11 0 0 0 0.18 0 0 0.52 -0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 -1 0 -0.21 0 -0.09 0 0 0 -0.28 0 0.54 0.13 0 0 0.05 0 0 -0.04 0 0 0 0 0 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 -1 -0.12 0 0 0 0 0 0 0 0.09 0.59 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      ]
      set emotion-coefficients array:from-list ;;Canada unisex
      [
        -0.26 0.67 -0.29 -0.11 0.47 -0.02 0 0.12 0 0 0 0 0 0 0 0
        -0.18 -0.15 0.76 0.06 -0.02 0.56 0.07 0 0 0 0 0 0 0 0 0
        0.07 0.05 -0.09 0.67 0.01 -0.09 0.67 0 0 0 0 0 0 0 0 0
      ]
    ]
    [ ifelse member? "China" equations ;; Does equations string include "China"
    [  ;; load China unisex equations
      set abo-coefficients array:from-list
      ;; Chinese coefficients estimated by Jun Zhao using BMS
      [
       -1 0 0 0 0 0 0 0 0 -0.13 0.54 0 0 0.64 0 -0.09 0 0 0 0.13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.3 -0.1 0 -0.05 0.08 0 0 0 0 0.11 -0.06 0 0 0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.03 0
        0 -1 0 0 0 0 0 0 0 -0.12 -0.07 0.77 0.06 -0.26 0.44 0 0.05 0 0 0 0 0 0 0 0 0.11 -0.12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 -1 0 0 0 0 0 0 0.08 0.09 -0.07 0.86 -0.17 0.09 0.31 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 -1 0 0 0 0 0 -0.08 0.13 0 0 0.85 0 -0.1 0 0 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.28 -0.07 0 0 0.03 0 0 0.04 0 0.09 -0.03 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 -1 0 0 0 0 0.07 0 0.21 0 -0.37 0.78 0 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0.05 0 0 0 0 0 0 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 -1 0 0 0 0.06 0 -0.07 0.38 -0.14 0.13 0.72 0 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 -1 0 0 0.02 0 0 0 0.2 0 0 0.89 0 0 0.09 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.12 0 0 0 0 0 0 0 0 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 -1 0 -0.04 0 -0.04 0 0.32 -0.11 0.05 -0.21 0.8 0.09 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0 0 0 0 0 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 -1 -0.04 0 0 0 0 0 0 0 -0.04 0.86 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      ]
      set emotion-coefficients array:from-list ;;China unisex
      [
        -0.5 0.6 -0.1 -0.13 0.39 0.06 0 0.08 0 0 0 0 0 0.02 -0.05 0
        -0.36 0 0.6 0 0.09 0.48 0.07 0 0 0 0 0 0 0 0 0
        0.09 -0.12 0.11 0.59 0 0 0.36 0 0 0 0 0 0 0 -0.05 0.07
      ]
    ]
    [  ;; load German unisexx equations
      set abo-coefficients array:from-list
      [
        -1.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  -0.38  0.42  0  -0.11  0.47  0  0  0.11  0  0  0.05  0  0  0  0  0.06  0  0  0  0  0  0  0  0  0  0.09  0  0.09  0.04  0  -0.07  -0.13  0  0  0  0  0  0  -0.03  0  0.02  0  0  0  0  0  0  0  0.03  -0.02  0  0  0  0  0  0  0  0  0  0  0  0  0  0
        0.00  -1.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  -0.03  0  0.39  0.08  -0.07  0.57  0  0  -0.2  0.16  0  0  0  0  0  0  0  0  -0.04  0  0  0  0  0  0  0  -0.07  0  0  0  0  0  0  0  0.03  0.06  0  0  0  0  0  0  0.02  0  0  0  0  0  0  0  0  0  0  0  0.02  0  0  0  0  0  0  0  0  0
        0.00  0.00  -1.00  0.00  0.00  0.00  0.00  0.00  0.00  0.1  0  0  0.39  -0.13  0.14  0.52  0  0  0  0  0  0  0  0  0  0  0  -0.03  -0.03  0  0  0  0  -0.06  0  0.04  0  0  0  0  0  0.07  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  -0.04  0
        0.00  0.00  0.00  -1.00  0.00  0.00  0.00  0.00  0.00  -0.72  0.23  0  0  0.51  0  0  0.2  0  0  0.06  0.08  0  0.04  -0.04  0  0  0  0  0  0  0  0  0  0  0  0.05  0.09  0.06  0  -0.09  -0.1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0.03  0  0  0  0  0  0  0  0  0  0  0  0  0  0  -0.05
        0.00  0.00  0.00  0.00  -1.00  0.00  0.00  0.00  0.00  -0.05  0  0.17  0.1  0  0.66  0  0  0  0  0  0  0.02  0  0  0.04  0  0  0  0  0  0  0  -0.09  0  0  0  0  0  0  -0.05  0.02  0  0  0  0  0  0  0  0  0  0  0  0  -0.01  0  0  0  0  0  0  0.02  0  0  0  0  0  0.03  0  0  0  0  0  0
        0.00  0.00  0.00  0.00  0.00  -1.00  0.00  0.00  0.00  0.18  0  0  0.28  -0.06  0  0.62  0  0  0  -0.02  0  0  0  0  0  0  0  0  -0.03  0  0  0  0  -0.07  0  0  0  0  0.04  0.04  0  0  0  0  0.08  0  0  0  0  0  0  0  0.02  0  0  0  0  0  0  0  0  0.02  0  0  0  0  0  0  0  0  0  -0.03  0.03
        0.00  0.00  0.00  0.00  0.00  0.00  -1.00  0.00  0.00  -0.15  0  0.1  0  0.13  0  0  0.38  0  0  0.06  0  0  0.03  0  0  0  -0.04  0  0  0  0  -0.03  0  0  0  0.04  0  0  0  0  0  0  -0.06  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
        0.00  0.00  0.00  0.00  0.00  0.00  0.00  -1.00  0.00  -0.26  0  -0.28  0  0.17  -0.54  0.15  0  0.4  0  0  0  0  0  0.03  0  0  0  0.08  0  0  0  0  0  0  0.09  0.06  0  0  0  0  -0.06  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  -0.02  0  0  0  0  0  0  0  0  0
        0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  -1.00  -0.57  0  -0.18  0  0  0  0  0  0  0.28  0  0  0  0  0  0  0  0  0.05  0  0.05  0  0.03  0  0  0  0  0  0  0  0  -0.08  0.08  0  0  0  0  0  0  0.01  0  0  0  0.01  0  0  0  0  0  0  -0.03  0  0  0  0  0  0  -0.03  0  0  0  0  0  -0.02
      ]
      set emotion-coefficients array:from-list
      [
        -0.50  0.60  -0.10  -0.13  0.39  0.06  0.00  0.08  0.00  0.00  0.00  0.00  0.00  0.02  -0.05  0.00
        -0.36  0.00  0.60  0.00  0.09  0.48  0.07  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
        0.09  -0.12  0.11  0.59  0.00  0.00  0.36  0.00  0.00  0.00  0.00  0.00  0.00  0.00  -0.05  0.07
      ]
     ]
    ]
  ]
end ;; load-unisex-equations



to load-male-equations
  ;; SET MALE COEFFICIENTS FOR IMPRESSION-FORMATION EQUATIONS
  ;; Terms corresponding to columns in coefficient array are as follows:
  ;;   C Ae Ap Aa Be Bp Ba Oe Op Oa
  ;;   AeBe AeBp AeBa AeOe AeOp AeOa ApBe ApBp ApBa ApOe ApOp ApOa AaBe AaBp AaBa AaOe AaOp AaOa BeOe BeOp BeOa BpOe BpOp BpOa BaOe BaOp BaOa
  ;;   AeBeOe AeBeOp AeBeOa AeBpOe AeBpOp AeBpOa AeBaOe AeBaOp AeBaOa ApBeOe ApBeOp ApBeOa ApBpOe ApBpOp ApBpOa ApBaOe ApBaOp ApBaOa AaBeOe AaBeOp AaBeOa AaBpOe AaBpOp AaBpOa AaBaOe AaBaOp AaBaOa
  ;; Rows are coefficients in equation for Ae', Ap', Aa', Be', Bp', Ba', Oe', Op', or Oa'
  ;; Coefficients are based on UNC ABO impression-formation equations
  ;; males
;;  ifelse equations != "Canadian multisex" ;; Load U.S. equations
  ifelse member? "US" equations ;; Does equations string include "US"
  [
    set abo-coefficients array:from-list ;; Identity matrix concatenated above M is negated to allow for negation of M in formulas
  ;; Coefficients from two-phase analyses of UNC515 (ANOVA then SEM). Male-female coefs constrained equal when not significantly different.
  [
    -1 0 0 0 0 0 0 0 0 -0.26 0.41 0 0 0.42 -0.02 -0.10 0.03 0.06 0 0.05 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0.12 -0.05 0 -0.05 0 0 0 0 0 0.03 -0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 -1 0 0 0 0 0 0 0 -0.10 0 0.56 0.06 -0.07 0.44 0 0.04 0 0 0 0 0 0 0 0 0 -0.05 0 0 0 0 0 0 0 0 0 0 0.01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 -1 0 0 0 0 0 0 0.14 0.05 0 0.64 -0.06 0 0.29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 -1 0 0 0 0 0 -0.19 0.11 0 0 0.53 0 -0.12 0 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.11 -0.05 0 -0.02 0 0 0 0 0 0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 -1 0 0 0 0 0.06 0 0.16 0 -0.13 0.70 0 0.03 0.01 0 0.01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 -1 0 0 0 0.11 0.02 -0.06 0.27 0.04 0 0.64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 -1 0 0 -0.11 0 0 0 0.11 0 0 0.61 0 0.03 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.04 0 0 -0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 -1 0 -0.37 0 0 0 0.18 -0.11 0 -0.08 0.66 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0.03 0 0 -0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 -1 0.02 0 0 0 0.02 0 0 0.03 -0.05 0.66 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  ]
    ;; EMOTION-IDENTITY EQUATIONS
    ;; Terms corresponding to columns in coefficients matrix are as follows:
    ;;   c Me Mp Ma Ie Ip Ia MeIe MeIp MeIa MpIe MpIp MpIa MaIe MaIp MaIa
    ;; Rows are 16 coefficients in equation for evaluation amalgamation, potency amalgamation, and activity amalgamation
    ;; Coefficients are based on IU emotion-identity impression-formation equations, males and females averaged.
    set emotion-coefficients array:from-list ;; U.S. unisex
    [
      -0.32 0.69 -0.36 0 0.47 0.01 -0.07 0.12 0 0 0 0 0 0 0 0
      -0.18 -0.18 0.65 0.01 -0.01 0.59 0.05 0 0 0 0 0 0 0 0 0
      -0.11 -0.04 0.07 0.53 -0.02 -0.02 0.64 0 0 0 0 0 0 0 0 0
    ]
  ]
  [
    set abo-coefficients array:from-list ;; Identity matrix concatenated above M is negated to allow for negation of M in formulas
    ;; Male coefficients from two-phase analyses of Canada (ANOVA then SEM). Male-female coefficient constrained equal if not significantly different.
    [
      -1 0 0 0 0 0 0 0 0 -0.24 0.45 0 0 0.51 0 0 0 0 0 0.09 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 -1 0 0 0 0 0 0 0 -0.21 0 0.54 0 0 0.45 0.12 0 -0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 -1 0 0 0 0 0 0 -0.07 0 0 0.71 0 0.16 0.29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 -1 0 0 0 0 0 0.00 0.08 0 0 0.73 0 0 0 0 0 0.06 0 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 -1 0 0 0 0 0.00 0 0 0 0 0.63 0.19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 -1 0 0 0 0.02 0 0 0.21 -0.04 0.12 0.52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 -1 0 0 -0.11 0 0 0 0.18 0 0 0.52 -0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 -1 0 -0.21 0 -0.09 0 0 0 -0.28 0 0.54 0.13 0 0 0.05 0 0 -0.04 0 0 0 0 0 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 -1 -0.12 0 0 0 0 0 0 0 0.09 0.59 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ]
    set emotion-coefficients array:from-list ;;Canada unisex
    [
      -0.26 0.67 -0.29 -0.11 0.47 -0.02 0 0.12 0 0 0 0 0 0 0 0
      -0.18 -0.15 0.76 0.06 -0.02 0.56 0.07 0 0 0 0 0 0 0 0 0
      0.07 0.05 -0.09 0.67 0.01 -0.09 0.67 0 0 0 0 0 0 0 0 0
    ]
  ]
end ;; load-male-equations



to load-female-equations
  ;; SET FEMALE COEFFICIENTS FOR IMPRESSION-FORMATION EQUATIONS
  ;; Terms corresponding to columns in coefficient array are as follows:
  ;;   C Ae Ap Aa Be Bp Ba Oe Op Oa
  ;;   AeBe AeBp AeBa AeOe AeOp AeOa ApBe ApBp ApBa ApOe ApOp ApOa AaBe AaBp AaBa AaOe AaOp AaOa BeOe BeOp BeOa BpOe BpOp BpOa BaOe BaOp BaOa
  ;;   AeBeOe AeBeOp AeBeOa AeBpOe AeBpOp AeBpOa AeBaOe AeBaOp AeBaOa ApBeOe ApBeOp ApBeOa ApBpOe ApBpOp ApBpOa ApBaOe ApBaOp ApBaOa AaBeOe AaBeOp AaBeOa AaBpOe AaBpOp AaBpOa AaBaOe AaBaOp AaBaOa
  ;; Rows are coefficients in equation for Ae', Ap', Aa', Be', Bp', Ba', Oe', Op', or Oa'
  ;; Coefficients are based on UNC ABO impression-formation equations
  ;; females
  ifelse member? "US" equations ;; Does equations string include "US"
  [
    set abo-coefficients array:from-list ;; Identity matrix concatenated above M is negated to allow for negation of M in formulas
    ;; Coefficients from two-phase analyses of UNC515 (ANOVA then SEM). Male-female coefs constrained equal when not significantly different.
    [
      -1 0 0 0 0 0 0 0 0 -0.05 0.41 0 0 0.42 -0.02 -0.10 0.03 0.06 0 0.05 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0.12 -0.05 0 -0.05 0 0 0 0 0 0.03 -0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 -1 0 0 0 0 0 0 0 -0.10 0 0.56 0.06 -0.14 0.44 0 0.04 0 0 0 0 0 0 0 0 0 -0.05 0 0 0 0 0 0 0 0 0 0 0.01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 -1 0 0 0 0 0 0 0.14 0.05 0 0.77 -0.06 0 0.29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 -1 0 0 0 0 0 -0.09 0.11 0 0 0.58 0 -0.12 0 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.11 -0.05 0 -0.02 0 0 0 0 0 0.02 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 -1 0 0 0 0 0.06 0 0.16 0 -0.17 0.67 0 0.03 -0.04 0 0.01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.01 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 -1 0 0 0 0.23 0.02 -0.06 0.33 0.04 0 0.64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 -1 0 0 0.16 0 0 0 0.11 0 0 0.61 0 -0.05 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.04 0 0 -0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 -1 0 -0.42 0 0 0 0.12 -0.11 0 -0.15 0.66 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0.03 0 0 -0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 -1 -0.09 0 0 0 0.02 0 0 0.03 -0.05 0.83 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ]
    ;; EMOTION-IDENTITY EQUATIONS
    ;; Terms corresponding to columns in coefficients matrix are as follows:
    ;;   c Me Mp Ma Ie Ip Ia MeIe MeIp MeIa MpIe MpIp MpIa MaIe MaIp MaIa
    ;; Rows are 16 coefficients in equation for evaluation amalgamation, potency amalgamation, and activity amalgamation
    ;; Coefficients are based on IU emotion-identity impression-formation equations, males and females averaged.
    set emotion-coefficients array:from-list ;; U.S. unisex
    [
      -0.32 0.69 -0.36 0 0.47 0.01 -0.07 0.12 0 0 0 0 0 0 0 0
      -0.18 -0.18 0.65 0.01 -0.01 0.59 0.05 0 0 0 0 0 0 0 0 0
      -0.11 -0.04 0.07 0.53 -0.02 -0.02 0.64 0 0 0 0 0 0 0 0 0
    ]
  ]
  [
    set abo-coefficients array:from-list ;; Identity matrix concatenated above M is negated to allow for negation of M in formulas
    ;; Female coefficients from two-phase analyses of Canada (ANOVA then SEM). Male-female coefficient constrained equal if not significantly different.
    [
      -1 0 0 0 0 0 0 0 0 -0.24 0.45 0 0 0.51 0 0 0 0 0 0.09 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 -1 0 0 0 0 0 0 0 -0.21 0 0.63 0 0 0.45 0.21 0 -0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 -1 0 0 0 0 0 0 -0.07 0 0 0.71 0 0.16 0.29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 -1 0 0 0 0 0 0.00 0.08 0 0 0.83 0 0 0 0 0 0.06 0 0.07 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 -1 0 0 0 0 0.00 0 0 0 0 0.63 0.19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 -1 0 0 0 0.02 0 0 0.21 -0.04 0.12 0.52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 -1 0 0 0.02 0 0 0 0.12 0 0 0.65 -0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 -1 0 -0.21 0 -0.09 0 0 0 -0.28 0 0.70 0.13 0 0 0.05 0 0 -0.04 0 0 0 0 0 0 0.04 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.03 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 -1 -0.12 0 0 0 0 0 0 0 0.09 0.68 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ]
    set emotion-coefficients array:from-list ;;Canada unisex
    [
      -0.26 0.67 -0.29 -0.11 0.47 -0.02 0 0.12 0 0 0 0 0 0 0 0
      -0.18 -0.15 0.76 0.06 -0.02 0.56 0.07 0 0 0 0 0 0 0 0 0
      0.07 0.05 -0.09 0.67 0.01 -0.09 0.67 0 0 0 0 0 0 0 0 0
    ]
  ]
end ;; load-female-equations



to load-special-equations
  set abo-coefficients array:from-list ;; Identity matrix concatenated above M is negated to allow for negation of M in formulas
    ;; Two-phase equations from Dave, median split for ANOVA, SEM on Dave only
  [
    -1 0 0 0 0 0 0 0 0 -0.49 0.63 0 0 0.33 0 0 0 0 0 0.08 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 -1 0 0 0 0 0 0 0 0.01 0 0.72 0 0 0.45 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 -1 0 0 0 0 0 0 0.18 0 0 0.25 0 0.12 0.64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 -1 0 0 0 0 0 -0.37 0.32 0 0 0.44 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 -1 0 0 0 0 -0.21 0 0.35 0 0 0.62 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 -1 0 0 0 0.23 0 0 0 0 0 0.76 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 -1 0 0 -0.25 0 0 0 0 0 0 0.75 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 -1 0 -0.28 0.17 -0.15 0 0.18 -0.31 0 0 0.71 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 -1 -0.43 0 0 0 0 0 0 0 0 0.52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  ]
  ;; EMOTION-IDENTITY EQUATIONS
  ;; Terms corresponding to columns in coefficients matrix are as follows:
  ;;   c Me Mp Ma Ie Ip Ia MeIe MeIp MeIa MpIe MpIp MpIa MaIe MaIp MaIa
  ;; Rows are 16 coefficients in equation for evaluation amalgamation, potency amalgamation, and activity amalgamation
  ;; Coefficients are based on IU emotion-identity impression-formation equations, males and females averaged.
  set emotion-coefficients array:from-list ;; From Dave
  [
    -0.89   0.49 0 0   0.57 0 0   0.13 0 0 0 0 0 0 0 0
    -0.41   0 0.55 0   0 0.81 0    0 0 0 0 0 0 0 0 0
     0.17   0 0 0.78   0 0 0.35    0 0 0 0 0 0 0 0 0
  ]
end ;; load-special-equations



to load-math-arrays
  ;; SET S-BETA AND G ARRAYS
  set S-beta array:from-list
  [ 0 0 0 1 0 0 0 0 0  0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0
    0 0 0 0 1 0 0 0 0  0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0
    0 0 0 0 0 1 0 0 0  0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 ]
  set g array:from-list
  [ 1 1 1 0 0 0 1 1 1  1 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
  set variable-vector array:from-list n-values 64 [0]
end ;; load-math-arrays
@#$#@#$#@
GRAPHICS-WINDOW
430
10
871
452
-1
-1
6.662
1
10
1
1
1
0
0
0
1
-32
32
-32
32
1
1
1
ticks
30.0

BUTTON
265
165
320
198
setup
setup
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
375
165
430
198
NIL
next
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
265
10
423
43
group-size
group-size
3
25
3.0
1
1
NIL
HORIZONTAL

SLIDER
0
115
255
148
percent-females
percent-females
0
100
70.0
1
1
NIL
HORIZONTAL

OUTPUT
415
475
1075
550
9

CHOOSER
0
160
125
205
actor-choice
actor-choice
"min event tension" "min self-tension" "max self-tension" "random" "U" "UP" "UPF" "UF" "UNF" "UN" "UNB" "UB" "UPB" "P" "PF" "F" "NF" "N" "NB" "B" "PB" "DP" "DPF" "DF" "DNF" "DN" "DNB" "DB" "DPB" "D"
2

CHOOSER
130
160
255
205
object-choice
object-choice
"min event tension" "min alter tension" "max alter tension" "emotion similarity" "similar self-EPA" "random" "U" "UP" "UPF" "UF" "UNF" "UN" "UNB" "UB" "UPB" "P" "PF" "F" "NF" "N" "NB" "B" "PB" "DP" "DPF" "DF" "DNF" "DN" "DNB" "DB" "DPB" "D"
0

SLIDER
0
10
125
43
male-goodness
male-goodness
-3
3
1.0
.1
1
NIL
HORIZONTAL

SLIDER
130
10
255
43
female-goodness
female-goodness
-3
3
1.0
.1
1
NIL
HORIZONTAL

PLOT
-5
315
405
590
Processes
NIL
NIL
0.0
10.0
-2.0
10.0
true
true
"if fast [stop]\n  set-plot-y-range  -2 5\n  auto-plot-off\n  set-current-plot-pen \"axis\"\n  plotxy 0 0\n  ;; for axis, plot to a point that's far to the right\n  plotxy 1000000000 0\n  auto-plot-on\n  set-plot-y-range -2 count society\n  set-plot-x-range 0 count society + 1\n  set-current-plot \"Originated, Received\"\n  set-plot-y-range 0 count society\n  set-plot-x-range 0 2 * (count society + 1)" ""
PENS
"Tension" 1.0 0 -16777216 true "" "if fast [stop]\nlet acting-individual  ( turtle item 0 action-now )\nif-else acting-individual = nobody\n  [ plot 0 ]\n  [ plot  [event-deflection] of acting-individual ]"
"E" 1.0 0 -2674135 true "" "if fast [stop]\nlet acting-individual  ( turtle item 0 action-now )\nifelse graph-EPA-variables = \"actor emotion\"\n  [plot  [item 0 emotion] of acting-individual]\n  [ifelse graph-EPA-variables = \"actor impresion\"\n    [plot  [item 0 current-transients] of acting-individual]\n    [ifelse graph-EPA-variables = \"behavior\"\n      [plot  [item 0 behavior] of acting-individual]\n      [];; graph-variables = \"none\"\n    ]\n  ]"
"P" 1.0 0 -13791810 true "" "if fast [stop]\nlet acting-individual  turtle item 0 action-now\nifelse graph-EPA-variables = \"actor emotion\"\n  [plot  [item 1 emotion] of acting-individual]\n  [ifelse graph-EPA-variables = \"actor impresion\"\n    [plot  [item 1 current-transients] of acting-individual]\n    [ifelse graph-EPA-variables = \"behavior\"\n      [plot  [item 1 behavior] of acting-individual]\n      [];; graph-variables = \"none\"\n    ]\n  ]"
"A" 1.0 0 -8630108 true "" "if fast [stop]\nlet acting-individual  ( turtle item 0 action-now )\nifelse graph-EPA-variables = \"actor emotion\"\n  [plot  [item 2 emotion] of acting-individual]\n  [ifelse graph-EPA-variables = \"actor impresion\"\n    [plot  [item 2 current-transients] of acting-individual]\n    [ifelse graph-EPA-variables = \"behavior\"\n      [plot  [item 2 behavior] of acting-individual]\n      [];; graph-variables = \"none\"\n    ]\n  ]"
"Links" 1.0 2 -5825686 true "" "if fast [stop]\nplot networking-percent / 5"
"axis" 1.0 0 -1184463 true "if fast [stop]\nset-plot-y-range  -2 5" "if fast [stop]\nauto-plot-off\nplotxy 0 0\n;; for axis, plot to a point that's far to the right\nplotxy 1000000000 0\nauto-plot-on"

SLIDER
0
45
125
78
male-dominance
male-dominance
-3
3
2.5
.1
1
NIL
HORIZONTAL

SLIDER
0
80
125
113
male-activation
male-activation
-3
3
1.0
.1
1
NIL
HORIZONTAL

SLIDER
130
45
255
78
female-dominance
female-dominance
-3
3
0.5
.1
1
NIL
HORIZONTAL

SLIDER
130
80
255
113
female-activation
female-activation
-3
3
1.0
.1
1
NIL
HORIZONTAL

CHOOSER
405
550
530
595
graph-EPA-variables
graph-EPA-variables
"actor emotion" "actor impresion" "behavior" "none"
3

SWITCH
895
550
988
583
save-IO
save-IO
0
1
-1000

BUTTON
320
200
375
233
NIL
re-start
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
816
550
871
583
read-file
setup-model-from-file
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
875
55
1075
175
Originated, Received
rank
freq
0.0
2.0
0.0
10.0
true
false
"set-plot-y-range 0 count society\nset-plot-x-range 0 2 * (count society + 1)" "clear-plot\n  let originators (sort-by [ [?1 ?2] -> [acts-originated] of ?1 > [acts-originated] of ?2 ] society)\n  let recipients (sort-by [ [?1 ?2] -> [acts-received] of ?1 > [acts-received] of ?2 ] society)\n  let in-both-graphs sentence originators recipients\n  let index 2 ;; Put a space at the beginning of the histograms\n  foreach in-both-graphs\n    [ ?1 ->\n    ifelse [who] of ?1 = 0 [set-current-plot-pen \"whole-group\"] [set-current-plot-pen \"agent\"] ;; Color the whole-group line orange\n    plot-pen-up\n    plotxy index 0\n    plot-pen-down\n    ifelse index <= 1 + length originators [plotxy index [acts-originated] of ?1] [plotxy index [acts-received] of ?1]\n    if index = 1 + length originators [set index index + 1] ;; Put a space between the histograms\n    set index index + 1\n    ]"
PENS
"agent" 1.0 0 -16777216 true "" ""
"whole-group" 1.0 0 -955883 true "" ""

SLIDER
0
210
125
243
reciprocal-act-Pr
reciprocal-act-Pr
0
1
1.0
.1
1
NIL
HORIZONTAL

SLIDER
130
210
255
243
address-group-Pr
address-group-Pr
-.1
1
0.4
.1
1
NIL
HORIZONTAL

PLOT
875
219
1075
474
IPA Codes
IPA category
Percents
0.0
13.0
0.0
100.0
false
true
"" "clear-plot"
PENS
"1_solidarity" 1.0 1 -10899396 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 1 100 * (item 0 IPA-counts / sum IPA-counts)]"
"2_release" 1.0 1 -10899396 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 2 100 * (item 1 IPA-counts / sum IPA-counts)]"
"3_agrees" 1.0 1 -10899396 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 3 100 * (item 2 IPA-counts / sum IPA-counts)]"
"4_suggests" 1.0 1 -13345367 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 4 100 * (item 3 IPA-counts / sum IPA-counts)]"
"5_opines" 1.0 1 -13345367 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 5 100 * (item 4 IPA-counts / sum IPA-counts)]"
"6_orients" 1.0 1 -13345367 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 6 100 * (item 5 IPA-counts / sum IPA-counts)]"
"7_questions" 1.0 1 -16777216 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 7 100 * (item 6 IPA-counts / sum IPA-counts)]"
"8_prompts" 1.0 1 -16777216 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 8 100 * (item 7 IPA-counts / sum IPA-counts)]"
"9_entreats" 1.0 1 -16777216 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 9 100 * (item 8 IPA-counts / sum IPA-counts)]"
"10_disagrees" 1.0 1 -2674135 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 10 100 * (item 9 IPA-counts / sum IPA-counts)]"
"11_tension" 1.0 1 -2674135 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 11 100 * (item 10 IPA-counts / sum IPA-counts)]"
"12_argues" 1.0 1 -2674135 true "" "if fast [stop]\nif sum IPA-counts > 0\n  [plotxy 12 100 * (item 11 IPA-counts / sum IPA-counts)]"

SLIDER
265
235
430
268
run-size
run-size
0
2000
500.0
50
1
NIL
HORIZONTAL

CHOOSER
265
115
420
160
equations
equations
"US unisex" "US male" "US female" "Canadian unisex" "Canadian male" "Canadian female" "German unisex" "China unisex" "random acts" "identity echoes" "one individual"
0

CHOOSER
875
175
1075
220
IPA-coding-basis
IPA-coding-basis
"1978 sentiments" "2004 sentiments" "Canada 2001 sentiments"
0

CHOOSER
875
10
967
55
view
view
"E x A (P)" "P x A (E)" "P x E (A)" "symlog" "SYMLOG"
3

SLIDER
265
81
422
114
initial-tension
initial-tension
0.0
2
1.0
.1
1
NIL
HORIZONTAL

BUTTON
375
200
430
233
re-plot
plot-faces-on-axes
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
265
200
320
233
re-play
set re-playing true\nre-start
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
985
550
1075
583
fast
fast
0
1
-1000

SLIDER
264
46
422
79
individuality
individuality
0.0
2
1.0
.1
1
NIL
HORIZONTAL

SWITCH
0
245
125
278
grp-act-to-all
grp-act-to-all
0
1
-1000

CHOOSER
645
550
737
595
behavior-type
behavior-type
"verbal" "physical"
1

BUTTON
320
165
375
198
NIL
on-off
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
130
245
255
278
lines-to-group
lines-to-group
1
1
-1000

CHOOSER
265
270
430
315
change-next-group's
change-next-group's
"randomization" "starting feelings" "sentiments"
2

SWITCH
970
20
1072
53
dynamics
dynamics
1
1
-1000

@#$#@#$#@
## WEBSITES

The GroupSimulator program and documentation were created by David R. Heise. For terms and conditions of use, see http://www.indiana.edu/~socpsy/ACT/legal.htm .

GroupSimulator.nlogo can be downloaded from:  
http://www.indiana.edu/~socpsy/GroupSimulator/ .  
The model runs on your computer within NetLogo, a multi-agent modeling program available without charge at:  
http://ccl.northwestern.edu/netlogo/download.shtml .

You also can run GroupSimulator as an applet in your web browser by going to:  
http://www.indiana.edu/~socpsy/GroupSimulator/GroupSimulator.html  
However, not all of the functions mentioned below are available in the applet.

Basic documentation concerning the program is given below. More extensive documentation is provided at:
http://www.indiana.edu/~socpsy/public_files/GroupSimulatorGuide.pdf

## WHAT IS IT?

GroupSimulator uses affect control theory to generate interpersonal behaviors, and to track emotions and tensiones in multi-agent human groups ranging in size from three to 25 interactants. The program has three kinds of displays: a pictorial view showing interactants' facial expressions; graphs where group variables are plotted; and a text box for displaying information regarding actions and individuals. Additionally the program provides many controls for defining and analyzing groups in different ways.

The Credits and References section below gives information on affect control theory. The terminology and symbols used in the GroupSimulator program depart somewhat from standard usages in affect control theory. In this program dimensions of Evaluation, Potency, and Activity (EPA) are translated to goodness, dominance, and activation when talking about people, an individual's fundamental EPA profile is called stable character, and the deflection construct is called tension.

## HOW IT WORKS

Interactants are selected randomly from a normal distribution centered on specified values of goodness, dominance, and activation. An interactant's goodness, dominance, and activation constitutes the person's stable character that he or she tries to maintain in social interaction. Individuals' stable characters correspond to role-identities combined with personal traits, or to self-sentiments in informal situations like a party. Each individual presumably knows the stable character of every other individual at the scene, either because of a shared definition of the situation, or because the individuals are acquainted with one another personally.

Individual character is displayed graphically in the pictorial view with each individual's face positioned with regard to two dimensions of individual character, and the size of the individual's face reflecting the third dimension. The pictorial view's scatterplot of characters in three dimensions can be viewed from several different perspectives.

A blue image representing the group as a whole also is plotted in the pictorial view, positioned on each dimension at the mean of all individuals' characters. An action toward the group as a whole impacts impressions of the group entity, and the group image has a facial expression that indicates how the meaning of the group entity is faring.  An actor's action toward the group can be directed at other members individually to change impressions of them, too.

A relative balance of males and females is chosen for the group. Sex partitions the group into subgroups of individuals who can be assigned psychological differences influencing their social interaction.

On each round of interaction, an individual is selected on the basis of some criterion, and that person chooses another individual for dyadic interaction, or else behaves towards the group as a whole. Then another individual chooses a partner for dyadic interaction, or engages the whole group. This process continues until the analyst ends it.

The actor can be chosen to be the one who can produce the least future tension, or the one who has maximum personal tension or minimum personal tension, or the choice can be random.   
Alternatively, the actor can be the one whose transient impression is most extreme in one of 26 different directions in the affective space. The directions are defined in the SYMLOG system (see Robert Freed Bales, SOCIAL INTERACTION SYSTEMS, Transaction Publishers, 1999.)

Bales' Positive-Negative (P-N) dimension is the same thing as the goodness dimension here. Bales' Forward-Backward (F-B) dimension is a rotation of the dominance and activation dimensions, corresponding to quiet and strong as opposed to noisy and weak.  Bales' Up-Down (U-P) dimension rotates the dominance and activation dimensions to noisy and strong as opposed to quiet and weak (see Bales 'SOCIAL INTERACTION SYSTEMS, pp. 141-3). 

The list below shows the SYMLOG label for each of the 26 directions in the affective space, then the values and interests that are characteristic of individuals in that direction, then kinds of behavior that are characteristic, then specific noncorporeal interpersonal acts that characterize the direction, and then the direction's EPA profile * a point three units away from the origin of the EPA space in the given direction. Interests and Behavior are from Polley (1987; 1989). 

Acts characterizing directions were selected from 1,330 behaviors rated on EPA in various English-language databases. Coordinates were computed for each act on all 26 directional axes, and an act was characteristic of a direction if its coordinate on that direction was higher than its coordinates on other directions. (If a direction has less than three characteristic acts, then acts that are somewhat in the given direction are included, marked with a tilde.) 

U. Interests: Individual financial success, personal prominence and power.   
* Behavior: Active, dominant, talkative.   
* Acts: commands, hollers at, disagrees with.   
* EPA: 0.00, 2.12, 2.12  
UP. Interests: Popularity and social success, being liked and admired.   
* Behavior: Supportive, nurturant.   
* Acts: asks about something (_?_), tells something (_?_) to, answers.   
* EPA: 2.12, 1.50, 1.50  
UPF. Interests: Active teamwork toward common goals, organizational unity.   
* Behavior: Purposeful and considerate.   
* Acts: corrects, advises, confers with.   
* EPA: 1.73, 2.45, 0.00  
UF. Interests: Efficiency, strong impartial management.   
* Behavior: Assertive, business-like, impartial.   
* Acts: orders, persuades, reprimands.   
* EPA: 0.00, 3.00, 0.00  
UNF. Interests: Dogmatic enforcement of authority, rules, and regulations.   
* Behavior: Authoritarian, controlling.   
* Acts: silences, forbids, ~interrogates.   
* EPA: -1.73, 2.45, 0.00  
UN. Interests: Tough-minded, self-oriented assertiveness.   
* Behavior: Egocentric, tough-minded, powerful.   
* Acts: criticizes, scolds, quarrels with.   
* EPA: -2.12, 1.50, 1.50  
UNB. Interests: Rugged, self-oriented individualism, resistance to authority.   
* Behavior: Rebellious, provocative.   
* Acts: dares, giggles at, taunts.   
* EPA: -1.73, 0.00, 2.45  
UB. Interests: Active pursuit of change, new and unorthodox ideas.   
* Behavior: Unconventional, daring, risk-oriented.   
* Acts: joshes, banters with, is saracastic toward.   
* EPA: 0.00, 0.00, 3.00  
UPB. Interests: Leading group-centered efforts to change and seek new solutions.   
* Behavior: Innovative, progressive.   
* Acts: chatters to, ~encourages, ~jokes with.   
* EPA: 1.73, 0.00, 2.45  
P. Interests: Friendship, mutual pleasure, recreation.   
* Behavior: Friendly, warm, caring.   
* Acts: admits something (_?_) to, consults,agrees with.   
* EPA: 3.00, 0.00, 0.00  
PF. Interests: Responsible idealism, collaborative work.   
* Behavior: Cooperative, idealistic.   
* Acts: explains to, listens to, confides in.   
* EPA: 2.12, 1.50, -1.50  
F. Interests: Conservative, established, "correct" ways of doing things.   
* Behavior: Conforming, conventional.   
* Acts: ~prays for, ~assures, ~calms.   
* EPA: 0.00, 2.12, -2.12  
NF. Interests: Rigid adherence to organizational expections and rules.   
* Behavior: Closed-minded, rigid.   
* Acts: ~demeans, ~threatens, ~talks down to.   
* EPA: -2.12, 1.50, -1.50  
N. Interests: Self-protection, self-interest first, self-sufficiency.   
* Behavior: Unfriendly, cold, uncaring.   
* Acts: nags, ridicules, insults.   
* EPA: -3.00, 0.00, 0.00  
NB. Interests: Rejection of established procedures, rejection of conformity.   
* Behavior: Uncooperative, cynical.   
* Acts: wheedles, quibbles with, grouses at.   
* EPA: -2.12, -1.50, 1.50  
B. Interests: Change to new procedures, different ideas .   
* Behavior: Nonconforming, change-oriented.   
* Acts: ~babbles to, ~banters with, ~fusses over.   
* EPA: 0.00, -2.12, 2.12  
PB. Interests: Group-centered approaches to new ideas and new procedures.   
* Behavior: Open-minded, flexible.   
* Acts: ~chats with, ~cheers up, ~applauds.   
* EPA: 2.12, -1.50, 1.50  
DP. Interests: Trust in the goodness of others.   
* Behavior: Trustful, appreciative, dependent.   
* Acts: ~offers something (_?_) to, ~apologizes to, ~prays with.   
* EPA: 2.12, -1.50, -1.50  
DPF. Interests: Dedication, faithfulness, loyalty to the organization.   
* Behavior: Helpful, accepting of authority.   
* Acts: ~makes up with, ~reassures, ~thanks.   
* EPA: 1.73, 0.00, -2.45  
DF. Interests: Obedience to the chain of command, compliance with authority.   
* Behavior: Obiedient, accepting of authority.   
* Acts: whispers to, ~murmurs to, ~bows to.   
* EPA: 0.00, 0.00, -3.00  
DNF. Interests: Grudging self-sacrifice in the interests of the organization.   
* Behavior: Martyred, self-punishing.   
* Acts: defers to,~fibs to, ~scoffs at.   
* EPA: -1.73, 0.00, -2.45  
DN. Interests: Passive rejection of popularity, going it alone.   
* Behavior: Resentful, uninvolved.   
* Acts: begs from. mumbles to, whines to.   
* EPA: -2.12, -1.50, -1.50  
DNB. Interests: Admission of failure, withdrawal of effort from the task.   
* Behavior: Withdrawn, alienated.   
* Acts: begs, sucks up to, gibes.   
* EPA: -1.73, -2.45, 0.00  
DB. Interests: Tolerance for new ideas and different procedures.   
* Behavior: Tolerant of change and new ideas.   
* Acts: ~drones on at, ~pleads with, ~toadies to.   
* EPA: 0.00, -3.00, 0.00  
DPB. Interests: Comfort with coworker's new ideas and suggestions for change.   
* Behavior: Accepting of change and innovation.   
* Acts: gives in to, ~chit chats with, ~flatters.   
* EPA: 1.73, -2.45, 0.00  
D. Interests: Giving up personal needs and desires, passivity.   
* Behavior: Passive, submissive, quiet.   
* Acts: murmurs to, ~whispers to, ~stammers at.   
* EPA: 0.00, -2.12, -2.12
 

Choices of partner are based on similarity in emotion, or similarity in character, or by selecting the other who has maximum personal tension or minimum personal tension, or by a determination of who might generate minimum future tension in an interpersonal event; or as a random choice. Alternatively, the partner can be the one who is most extreme in one of the 26 affective directions.

Each event is displayed in the pictorial view with a green arrow linking the actor to the object of action. Links on past rounds of interaction are displayed as aqua-colored lines. The density of ties among individuals based on actions between them is shown by a dotted line in a graph that records process over time: a dot in the process graph shows the percentage of actual ties relative to the possible number, divided by five. The value of the dot times 20 gives the percentage of occurring links relative to the maximum possible links.

A behavior is generated for the chosen actor and object that minimizes actor, object, and behavior tensiones in accordance with affect control theory. The goodness, dominance, and activation of behaviors can be examined over time in the process graph by selecting "Behavior EPA" as the variables to be plotted. The profile of the behavior generated at a particular moment can be examined by choosing to view outputs. 

Emotions are displayed as expressions on the faces of individuals in the pictorial view. The facial expressions consist of just nine variations: neutral, elated, calm, bedazzled, beatific, angry, disgusted, scared, and sad. The average emotion of the two interactants in each acting dyad can be plotted over time in the process graph by selecting "Emotion EPA" as the variables to be plotted. A numerical EPA profile for each interactant's emotion at a particular moment can be obtained by choosing to view outputs. 

"EPA impressions" can be selected as the variables to be plotted in the process graph, in which case the average impressions of each acting dyad's goodness, dominance, and activation are plotted over time. Impressions of interactants as a result of the current event can be seen by choosing to view outputs. Impressions of individuals are used to select actors on the next round if the basis for choosing actors is "most esteemed," "most potent," "most active," or one of the 26 affective directions. 

The total tension produced by the current event is plotted over time in the process graph. Numerical levels of tension for individuals can be seen by choosing to view outputs.

The right side of the display shows output related to Interaction Process Analysis (IPA), a methodology for observing group interactions developed in the middle of the 20th century by Robert Bales. (For more information, see his SOCIAL INTERACTION SYSTEMS,ibid.) One graph shows interactants' relative frequencies of performing and receiving actions. Another graph shows the distribution of simulated acts categorized into IPA categories relating to instrumental and expressive aspects of group process. A text box shows the actor and recipient of the current event, the behavior's SYMLOG direction translated into words for behaviors of that type, and a possible topic that the actor might be forwarding, based on the SYMLOG value direction of the actor's character. 



## HOW TO USE IT

Clicking the SETUP button populates the pictorial view with a set of individuals. Clicking the ON-OFF button generates continuous rounds of social interaction among the individuals until the button is clicked again. The SPEED slider above the pictorial view controls the rate at which rounds of interaction occur. (You can edit the GO button so that clicking the button produces just one round of interaction.) Clicking the NEXT button causes just one action to be generated.

The emotional expressions on agents' faces show their current emotions. If the INITIAL-TENSION slider is set to zero, then the faces initially show characteristic emotions experienced when reality perfectly confirms characters. Otherwise the beginning emotions reflect individuals' characters along with tensions randomly assigned to each individual.

You can view the group from different perspectives by making a selection on the VIEW menu, and then clicking the RE-PLOT button. (You also can use the RE-PLAY or the RE-START button, but this will erase interactions that occurred previously.) The VIEW menu's option of "E x A (P)" shows individuals arrayed in terms of goodness on the vertical dimension and activation horizontally, with size of face representing dominance. The "P x A (E)" option shows dominance versus activation, with size representing goodness. "P x E (A)" shows dominance versus goodness, with size representing activation. The "SYMLOG (UD)" option shows individuals distributed in terms of Forward-Backward versus Positive-Negative, with size representing Up-Down. "MAX SYMLOG" shows the same thing but expanded to maximally fill the graph, as suggested in Bales and Cohen's (1979) drawing instructions.

Clicking the RE-PLAY button erases effects of all rounds of interaction and returns individuals to their starting states, with each individual retaining the same stable character and the same initial tension. Clicking ON-OFF then reproduces the prior rounds of interaction. Clicking the RE-START button also returns individuals to their starting states, but clicking ON-OFF produces fresh rounds of interaction in the same group, with individuals perhaps choosing affiliations in a different random order than on the previous run.

You determine the number of individuals�from three to 25�using the NUMBER-OF-INDIVIDUALS slider. Faces become smaller as more individuals are selected.

The proportion of individuals who are female is set with the PROPORTION-FEMALES slider.

The number of individuals and the proportion who are female must be decided before clicking the SETUP button. 

The expected character of males or of females can be set using the GOODNESS, DOMINANCE, and ACTIVATION sliders for each sex. Individuals of a given sex are drawn from a multivariate normal distribution centered on the numbers set in the character sliders for that sex. Individuals with character values beyond realistic limits of -4 and +4 on each scale are discarded, and a replacement is obtained by drawing again.

The ACTOR-CHOICE drop-down menus, one for each sex, determine the basis for deciding which person in the group will act next. The "least future tension" option is implemented by computing the optimal event among every possible pairing of actors and objects. Maximal or minimal personal tension relate to the distance between an individual's fundamental character and the prevailing impression of the individual in the group. The criteria specified by SYMLOG directions are applied to impressions created by the last event (not to stable character variables). 

The basis for attraction between individuals is set on the OBJECT-CHOICE drop-down menu. 

The EQUATIONS drop-down menu determines which impression-formation equations are used in calculations: male equations for everyone, female equations for everyone, or the average of male and female equations for everyone (unisex). The option of "random acts" obtains EPA profiles for behaviors by drawing from a multinormal distribution centered at the origin of the EPA space,  with a standard deviation set on the INITIAL-TENSION slider; unisex equations are used to compute impressions and emotions. The option of "identity echos" generates EPA profiles for an agent's behavior by random draws from a multinormal distribution centered in the EPA space at the agent's character, or EPA sentiment, with a standard deviation set on the INITIAL-TENSION slider; impressions and emotions are disabled with this option because they explode to infinity for certain types of characters when behaviors are chosen this way. The option of "one individual" uses an equation set describing impression processes of a specific person, and this is used for everyone in the group.

The RECIPROCAL-ACTION-PROBABILITY slider determines how frequently a dyad exchanges actions back and forth, at the expense of actions involving others. Dyadic exchanges of this sort occur in question-answer sequences, and other conversational structures. The probability should be set higher in groups experiencing conflict (Pincus et al. 2008). The ADDRESS-GROUP-PROBABILITY slider determines how frequently actors address the group as a whole, even if interacting with some individual would produce less tension. The group entity can be removed as a possible object of interaction by setting the slider to a negative value. The GRP-ACT-TO-ALL switch determines whether actions on the group affect impressions of the group entity alone, or whether the actor's behavior is directed to all other members of the group as well.

Green arrows in the pictorial view show which dyads are operative on the current round of interaction. (The faces of some interactants may hide the faces of others, in which case arrows for events pairing the interactants also may be hidden.) Interacting dyads initially are linked with dim connecting lines, but the lines gradually become brighter if interaction occurs over and over in the dyad. Dyads with pleasant relations are connected with aqua or white lines, those with hostile relations are connected with red lines.

Right-clicking a face in the pictorial view raises a menu of options. One line of this menu specifies the sex and identification number of the topmost face. Other lines may specify the sex and identification numbers of faces hidden by the top one. Moving the mouse pointer over an identification line produces a sub-menu. Choosing the "follow" option on the sub-menu will keep the selected face at the middle of the pictorial view in future rounds of interaction. The "watch" option highlights the selected face in future rounds of interaction. The "inspect" option opens a window with a blow-up of the individual's face on top and various kinds of information below. This window can be left open to track technical details regarding an individual during subsequent rounds of interaction. The window also can be used to change an individual's character by entering new values for the individual's fundamental EPA profile and pressing Enter. (Changing most other variables would be futile because the new values would be discarded by the program on the next round of interaction.)

The process graph shows over-time development of average tension and density of links. Exact coordinates of any point on the graph can be seen by positioning the mouse pointer over the point. Three additional lines (identified as E, P, and A in the legend) display over-time averages of Evaluation, Potency, and Activity components of individuals' emotions, or of impressions of the individuals, or of behaviors being enacted in dyads. You select which you want to see with the GRAPH-VARIABLES drop-down menu. *Note* While the scales of the horizontal and vertical axes of the graph are initialized to fit a variety of analyses, the scales change automatically when greater ranges are needed.

Rank-frequency histograms display the proportion of times that each individual has been an actor, and the proportion of times that each individual has been the recipient of others' actions. The group as a whole is included in the recipient graph, colored orange.

The IPA histogram shows the proportions of actions in Bales' twelve IPA categories. The drop-down menu named IPA-CODING-BASIS provides different bases for coding behavior EPAs into IPA categories. For example, the option of "2004 sentiments" defines each IPA category with mean EPA profiles for three signature acts in that category, averaged over ratings obtained from Indiana University males and females in 2004. 

The INPUT-OUTPUT text box gives fundamentals and initial transients for each actor at the top. Thereafter each line shows the actor and the target of an implemented action, and extreme representatives of actions in the given SYMLOG direction, drawn from 1,330 behaviors rated on EPA in various English dictionaries. A possible topic is indicated, corresponding to interests associated with the SYMLOG value direction of the actor's stable character. The BEHAVIOR-TYPE drop-down menu allows selecting between verbal acts and acts that encroach on the personal space or privacy of the target person.

##File Options

File input or output cannot be employed with the applet version of GroupSimulator, only with the downloaded version.

Clicking the SAVE-IO switch "on" causes simulation data to be written to two files named Data_GroupSimulator_actions.txt and Data_GroupSimulator_runs.txt. The files are written in the same directory as the GroupSimulator.nlogo file. The first line of each file identifies the model that produced the results. The "actions file contains information on each simulated action. The "runs" file contains summary results for each complete run, with run size set using the RUN-SIZE slider. 

Clicking the FAST switch to "on" stops updating of all visual displays in order to speed up processing when conducting multiple runs. Data continue to be saved in files, and the "ticks" counter above the pictorial view continues to operate, showing progress of the current simulation.

Clicking the READ-FILE button opens a file directory window so that you can load a set of interactants from a file that you have prepared in a text processing program. You also can re-load a group that you analyzed previously, if you saved the contents of the INPUT-OUTPUT box for that analysis to a file.

## REFERENCES

Bales, Robert Freed. 1999. Social Interaction Systems: Theory and Measurement. New York: Transaction.

Bales, Robert Freed and Stephen P. Cohen. 1979. SYMLOG: A System for the Multiple Level Observation of Groups. New York: Free Press.

Heise, David R. 2007. Expressive Order: Confirming Sentiments in Social Actions. New York: Springer. (The book has three sections. Part 1  provides a plain-language exposition of the theory. Part 2 presents the mathematical derivations that define sentiment-confirming behavior, labeling, attribution, and emotion. Part 3 describes the research program associated with the theory, and the Interact computer simulation program that is used in research.)

Heise, David R. 2010. SURVEYING CULTURES: DISCOVERING SHARED CONCEPTIONS AND SENTIMENTS, Hoboken NJ: Wiley. (Covers measurement of sentiments and the development of equations describing impression formation.)

Pincus, David, Kristen M. Fox, Katherine A. Perez, Jaclyn S. Turner, and Andrew R. McGeehan. 2008. "Nonlinear Dynamics of Individual and Interpersonal Conflict in an Experimental Group." Small Group Research 39:150-178.

Polley, Richard Brian. 1987. "The dimensions of social interaction: A method for improving rating scales." Social Psychology Quarterly 50:72-82.

Polley, Richard Brian. 1989. "On the dimensionality of interpersonal behavior: A reply to Lustig." Small Group Behavior 20:270-278.
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

female angry
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 21 47 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Rectangle -2064490 true false 186 164 213 176
Circle -16777216 true false 187 137 22
Rectangle -2064490 true false 89 162 116 174
Polygon -16777216 true false 184 245 165 255 149 252 148 240
Rectangle -2064490 true false 191 125 218 137
Rectangle -2064490 true false 82 125 109 137
Polygon -16777216 true false 197 103 197 114 234 119
Polygon -16777216 true false 197 103 198 114 165 138 168 118 180 109
Circle -16777216 true false 91 137 22
Polygon -16777216 true false 116 245 135 255 151 252 152 240
Polygon -16777216 true false 174 223 150 225 150 215 178 215 183 245
Polygon -16777216 true false 103 103 102 114 135 138 132 118 120 109
Polygon -16777216 true false 103 103 103 114 66 119
Polygon -16777216 true false 103 103 103 114 66 119
Polygon -16777216 true false 126 223 150 225 150 215 122 215 117 245
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 240
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 240

female beatific
false
0
Circle -2064490 true false 24 48 252
Circle -2064490 true false 9 5 280
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Circle -16777216 true false 92 139 22
Polygon -6459832 true false 30 255 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 225
Rectangle -2064490 true false 184 159 211 171
Rectangle -2064490 true false 182 135 212 154
Polygon -16777216 true false 195 225 147 240 150 255 175 246 190 238
Polygon -16777216 true false 105 225 153 240 150 255 125 246 110 238
Polygon -16777216 true false 195 225 182 235 149 242 150 225 177 228
Polygon -16777216 true false 105 225 118 235 151 242 150 225 123 228
Rectangle -2064490 true false 88 135 118 154
Rectangle -2064490 true false 89 159 116 171
Polygon -16777216 true false 210 90 210 105 165 120 180 105
Polygon -16777216 true false 210 90 210 105 240 114
Polygon -6459832 true false 270 255 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 225
Polygon -16777216 true false 90 90 90 105 135 120 120 105
Polygon -16777216 true false 90 90 90 105 60 114

female bedazzled
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Rectangle -2064490 true false 182 164 209 176
Rectangle -2064490 true false 183 123 210 135
Rectangle -2064490 true false 87 125 114 137
Circle -16777216 true false 92 139 22
Rectangle -2064490 true false 87 164 114 176
Polygon -16777216 true false 198 211 178 236 150 242 150 253 179 246
Polygon -16777216 true false 214 96 214 109 180 105 180 90
Polygon -16777216 true false 199 211 170 228 148 230 149 219 173 217
Polygon -16777216 true false 86 96 86 109 120 105 120 90
Polygon -16777216 true false 213 95 212 109 242 133
Polygon -16777216 true false 101 211 130 228 152 230 151 219 127 217
Polygon -16777216 true false 87 95 88 109 58 133
Polygon -16777216 true false 102 211 122 236 150 242 150 253 121 246
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 240
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 240

female calm
false
0
Circle -2064490 true false 24 48 252
Circle -2064490 true false 9 4 280
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Circle -16777216 true false 92 139 22
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 240
Polygon -16777216 true false 195 225 177 231 149 240 149 251 181 240
Rectangle -2064490 true false 184 138 211 150
Rectangle -2064490 true false 184 156 211 168
Polygon -16777216 true false 195 225 176 230 150 238 150 223 171 224
Polygon -16777216 true false 105 225 123 231 151 240 151 251 119 240
Polygon -16777216 true false 105 225 124 230 150 238 150 223 129 224
Rectangle -2064490 true false 89 156 116 168
Rectangle -2064490 true false 89 138 116 150
Polygon -16777216 true false 210 109 210 123 240 135
Polygon -16777216 true false 90 109 90 123 60 135
Polygon -16777216 true false 211 109 211 123 177 132 187 114
Polygon -16777216 true false 89 109 89 123 123 132 113 114
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 240

female disgusted
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 22 49 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 91 137 22
Circle -16777216 true false 187 137 22
Rectangle -2064490 true false 185 154 212 166
Rectangle -2064490 true false 186 133 213 145
Rectangle -2064490 true false 88 154 115 166
Polygon -16777216 true false 168 218 149 220 150 208 176 209 180 230
Polygon -16777216 true false 132 218 151 220 150 208 124 209 120 230
Rectangle -2064490 true false 87 133 114 145
Polygon -16777216 true false 207 117 206 126 173 134 170 123 189 121
Polygon -16777216 true false 203 116 201 126 235 127
Polygon -16777216 true false 97 116 99 126 65 127
Polygon -16777216 true false 93 117 94 126 127 134 130 123 111 121
Polygon -16777216 true false 181 232 150 230 150 220 172 220
Polygon -16777216 true false 119 232 150 230 150 220 128 220
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 225
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 225

female elated
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Rectangle -2064490 true false 182 164 209 176
Rectangle -2064490 true false 183 123 210 135
Polygon -16777216 true false 210 105 210 120 249 137
Polygon -16777216 true false 210 105 210 120 183 136 186 123
Rectangle -2064490 true false 87 125 114 137
Circle -16777216 true false 92 139 22
Rectangle -2064490 true false 87 164 114 176
Polygon -16777216 true false 90 105 90 120 51 137
Polygon -16777216 true false 90 105 90 120 117 136 114 123
Polygon -16777216 true false 199 211 169 226 150 226 150 215 173 217
Polygon -16777216 true false 101 211 131 226 150 226 150 215 127 217
Polygon -16777216 true false 198 211 176 233 150 242 150 253 179 243
Polygon -16777216 true false 102 211 124 233 150 242 150 253 121 243
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 225
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 225

female neutral
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 22 49 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Polygon -16777216 true false 213 106 213 121 249 122
Polygon -16777216 true false 213 106 213 121 183 121 186 108
Circle -16777216 true false 186 139 22
Rectangle -2064490 true false 184 160 211 172
Polygon -16777216 true false 87 106 87 121 51 122
Polygon -16777216 true false 87 106 87 121 117 121 114 108
Circle -16777216 true false 92 139 22
Rectangle -2064490 true false 89 160 116 172
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 225
Rectangle -2064490 true false 183 132 210 144
Rectangle -2064490 true false 90 132 117 144
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 225
Polygon -16777216 true false 195 234 155 222 149 225 149 231
Polygon -16777216 true false 190 232 165 244 148 244 148 232
Polygon -16777216 true false 105 234 145 222 151 225 151 231
Polygon -16777216 true false 110 232 135 244 152 244 152 232

female sad
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 23 45 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Polygon -16777216 true false 213 109 213 121 249 122
Circle -16777216 true false 92 139 22
Polygon -16777216 true false 215 111 213 121 190 108 173 85 189 93
Rectangle -2064490 true false 184 135 211 147
Polygon -16777216 true false 199 249 180 229 149 223 149 231 171 235
Polygon -16777216 true false 87 109 87 121 51 122
Polygon -16777216 true false 85 111 87 121 110 108 127 85 111 93
Polygon -16777216 true false 203 252 150 239 150 231 172 237
Polygon -16777216 true false 101 249 120 229 151 223 151 231 129 235
Polygon -16777216 true false 97 252 150 239 150 231 128 237
Rectangle -2064490 true false 89 135 116 147
Rectangle -2064490 true false 186 152 213 164
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 225
Rectangle -2064490 true false 87 152 114 164
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 225

female scared
false
0
Circle -2064490 true false 9 4 280
Circle -2064490 true false 23 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Polygon -16777216 true false 213 109 213 121 249 122
Circle -16777216 true false 92 139 22
Polygon -16777216 true false 215 111 213 121 190 108 173 85 189 93
Rectangle -2064490 true false 90 124 117 136
Rectangle -2064490 true false 184 162 211 174
Polygon -16777216 true false 87 109 87 121 51 122
Polygon -16777216 true false 85 111 87 121 110 108 127 85 111 93
Rectangle -2064490 true false 182 124 209 136
Rectangle -2064490 true false 89 163 116 175
Polygon -16777216 true false 199 249 186 227 149 223 149 231 180 234
Polygon -16777216 true false 201 251 147 256 149 245 184 246
Polygon -16777216 true false 99 251 153 256 151 245 116 246
Polygon -16777216 true false 101 249 114 227 151 223 151 231 120 234
Polygon -6459832 true false 255 270 263 82 231 51 150 53 150 1 221 7 268 39 288 81 300 225
Polygon -6459832 true false 45 270 37 82 69 51 150 53 150 1 79 7 32 39 12 81 0 225

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

male angry
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 22 50 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Rectangle -955883 true false 186 164 213 176
Circle -16777216 true false 187 137 22
Rectangle -955883 true false 89 162 116 174
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Polygon -16777216 true false 184 245 165 255 149 252 148 240
Rectangle -955883 true false 191 125 218 137
Rectangle -955883 true false 82 125 109 137
Polygon -16777216 true false 197 103 197 114 234 119
Polygon -16777216 true false 197 103 198 114 165 138 168 118 180 109
Circle -16777216 true false 91 137 22
Polygon -16777216 true false 116 245 135 255 151 252 152 240
Polygon -16777216 true false 174 223 150 225 150 215 178 215 183 245
Polygon -16777216 true false 103 103 102 114 135 138 132 118 120 109
Polygon -16777216 true false 103 103 103 114 66 119
Polygon -16777216 true false 103 103 103 114 66 119
Polygon -16777216 true false 126 223 150 225 150 215 122 215 117 245

male beatific
false
0
Circle -955883 true false 24 48 252
Circle -955883 true false 9 5 280
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Circle -16777216 true false 92 139 22
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Rectangle -955883 true false 184 159 211 171
Rectangle -955883 true false 182 135 212 154
Polygon -16777216 true false 195 225 147 240 150 255 175 246 190 238
Polygon -16777216 true false 105 225 153 240 150 255 125 246 110 238
Polygon -16777216 true false 195 225 182 235 149 242 150 225 177 228
Polygon -16777216 true false 105 225 118 235 151 242 150 225 123 228
Rectangle -955883 true false 88 135 118 154
Rectangle -955883 true false 89 159 116 171
Polygon -16777216 true false 210 90 211 101 165 120 180 105
Polygon -16777216 true false 210 90 207 99 240 114
Polygon -16777216 true false 90 90 89 101 135 120 120 105
Polygon -16777216 true false 90 90 93 99 60 114

male bedazzled
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Rectangle -955883 true false 182 164 209 176
Rectangle -955883 true false 183 123 210 135
Rectangle -955883 true false 87 125 114 137
Circle -16777216 true false 92 139 22
Rectangle -955883 true false 87 164 114 176
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Polygon -16777216 true false 198 211 178 236 150 242 150 253 179 246
Polygon -16777216 true false 214 96 214 109 180 105 180 90
Polygon -16777216 true false 199 211 170 228 148 230 149 219 173 217
Polygon -16777216 true false 86 96 86 109 120 105 120 90
Polygon -16777216 true false 213 95 212 109 242 133
Polygon -16777216 true false 101 211 130 228 152 230 151 219 127 217
Polygon -16777216 true false 87 95 88 109 58 133
Polygon -16777216 true false 102 211 122 236 150 242 150 253 121 246

male calm
false
0
Circle -955883 true false 24 48 252
Circle -955883 true false 9 4 280
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Circle -16777216 true false 92 139 22
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Rectangle -955883 true false 184 138 211 150
Rectangle -955883 true false 184 156 211 168
Polygon -16777216 true false 195 225 177 231 149 240 149 251 181 240
Polygon -16777216 true false 195 225 176 230 150 238 150 223 171 224
Rectangle -955883 true false 89 156 116 168
Rectangle -955883 true false 89 138 116 150
Polygon -16777216 true false 210 109 210 123 240 135
Polygon -16777216 true false 90 109 90 123 60 135
Polygon -16777216 true false 211 109 211 123 177 132 187 114
Polygon -16777216 true false 89 109 89 123 123 132 113 114
Polygon -16777216 true false 105 225 124 230 150 238 150 223 129 224
Polygon -16777216 true false 105 225 123 231 151 240 151 251 119 240

male disgusted
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 22 49 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 91 137 22
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Circle -16777216 true false 187 137 22
Rectangle -955883 true false 185 154 212 166
Rectangle -955883 true false 186 133 213 145
Rectangle -955883 true false 88 154 115 166
Polygon -16777216 true false 168 218 149 220 150 208 176 209 180 230
Polygon -16777216 true false 132 218 151 220 150 208 124 209 120 230
Rectangle -955883 true false 87 133 114 145
Polygon -16777216 true false 207 117 206 126 173 134 170 123 189 121
Polygon -16777216 true false 203 116 201 126 235 127
Polygon -16777216 true false 97 116 99 126 65 127
Polygon -16777216 true false 93 117 94 126 127 134 130 123 111 121
Polygon -16777216 true false 181 232 150 230 150 220 172 220
Polygon -16777216 true false 119 232 150 230 150 220 128 220

male elated
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Rectangle -955883 true false 182 164 209 176
Rectangle -955883 true false 183 123 210 135
Polygon -16777216 true false 210 105 210 120 249 137
Polygon -16777216 true false 210 105 210 120 183 136 186 123
Rectangle -955883 true false 87 125 114 137
Circle -16777216 true false 92 139 22
Rectangle -955883 true false 87 164 114 176
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Polygon -16777216 true false 90 105 90 120 51 137
Polygon -16777216 true false 90 105 90 120 117 136 114 123
Polygon -16777216 true false 199 211 169 226 150 226 150 215 173 217
Polygon -16777216 true false 101 211 131 226 150 226 150 215 127 217
Polygon -16777216 true false 198 211 176 233 150 242 150 253 179 243
Polygon -16777216 true false 102 211 124 233 150 242 150 253 121 243

male neutral
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 22 49 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Polygon -16777216 true false 213 106 213 121 249 122
Polygon -16777216 true false 213 106 213 121 183 121 186 108
Circle -16777216 true false 186 139 22
Rectangle -955883 true false 184 160 211 172
Polygon -16777216 true false 87 106 87 121 51 122
Polygon -16777216 true false 87 106 87 121 117 121 114 108
Circle -16777216 true false 92 139 22
Rectangle -955883 true false 89 160 116 172
Rectangle -955883 true false 183 132 210 144
Rectangle -955883 true false 90 132 117 144
Polygon -6459832 true false 280 146 263 82 231 51 150 53 150 1 221 7 268 39 288 81 292 150
Polygon -16777216 true false 195 234 155 222 149 225 149 231
Polygon -16777216 true false 190 232 165 244 148 244 148 232
Polygon -16777216 true false 105 234 145 222 151 225 151 231
Polygon -16777216 true false 110 232 135 244 152 244 152 232
Polygon -6459832 true false 20 146 37 82 69 51 150 53 150 1 79 7 32 39 12 81 8 150

male sad
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 23 45 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Polygon -16777216 true false 213 109 213 121 249 122
Circle -16777216 true false 92 139 22
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Polygon -16777216 true false 215 111 213 121 190 108 173 85 189 93
Rectangle -955883 true false 184 135 211 147
Polygon -16777216 true false 199 249 180 229 149 223 149 231 171 235
Polygon -16777216 true false 87 109 87 121 51 122
Polygon -16777216 true false 85 111 87 121 110 108 127 85 111 93
Rectangle -955883 true false 187 152 214 164
Polygon -16777216 true false 203 252 150 239 150 231 172 237
Polygon -16777216 true false 101 249 120 229 151 223 151 231 129 235
Polygon -16777216 true false 97 252 150 239 150 231 128 237
Rectangle -955883 true false 89 135 116 147
Rectangle -955883 true false 86 152 113 164

male scared
false
0
Circle -955883 true false 9 4 280
Circle -955883 true false 23 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Polygon -16777216 true false 213 109 213 121 249 122
Circle -16777216 true false 92 139 22
Polygon -6459832 true false 278 151 263 82 231 51 150 53 150 1 221 7 268 39 288 81 293 158
Polygon -6459832 true false 22 151 37 82 69 51 150 53 150 1 79 7 32 39 12 81 7 158
Polygon -16777216 true false 215 111 213 121 190 108 173 85 189 93
Rectangle -955883 true false 90 124 117 136
Rectangle -955883 true false 184 162 211 174
Polygon -16777216 true false 87 109 87 121 51 122
Polygon -16777216 true false 85 111 87 121 110 108 127 85 111 93
Rectangle -955883 true false 182 124 209 136
Rectangle -955883 true false 89 163 116 175
Polygon -16777216 true false 199 249 186 227 149 223 149 231 180 234
Polygon -16777216 true false 201 251 147 256 149 245 184 246
Polygon -16777216 true false 99 251 153 256 151 245 116 246
Polygon -16777216 true false 101 249 114 227 151 223 151 231 120 234

orbit
true
0
Circle -8630108 false false 0 0 300

overlay
true
0
Circle -7500403 false true 75 0 150
Circle -7500403 false true 75 150 150

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

topic angry
false
0
Circle -13345367 true false 22 50 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Rectangle -13345367 true false 186 164 213 176
Circle -16777216 true false 187 137 22
Rectangle -13345367 true false 89 162 116 174
Polygon -16777216 true false 184 245 165 255 149 252 148 240
Rectangle -13345367 true false 191 125 218 137
Rectangle -13345367 true false 82 125 109 137
Polygon -16777216 true false 197 103 197 114 234 119
Polygon -16777216 true false 197 103 198 114 165 138 168 118 180 109
Circle -16777216 true false 91 137 22
Polygon -16777216 true false 116 245 135 255 151 252 152 240
Polygon -16777216 true false 174 223 150 225 150 215 178 215 183 245
Polygon -16777216 true false 103 103 102 114 135 138 132 118 120 109
Polygon -16777216 true false 103 103 103 114 66 119
Polygon -16777216 true false 103 103 103 114 66 119
Polygon -16777216 true false 126 223 150 225 150 215 122 215 117 245

topic beatific
false
0
Circle -13345367 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Circle -16777216 true false 92 139 22
Rectangle -13345367 true false 184 159 211 171
Rectangle -13345367 true false 182 135 212 154
Polygon -16777216 true false 195 225 147 240 150 255 175 246 190 238
Polygon -16777216 true false 105 225 153 240 150 255 125 246 110 238
Polygon -16777216 true false 195 225 182 235 149 242 150 225 177 228
Polygon -16777216 true false 105 225 118 235 151 242 150 225 123 228
Rectangle -13345367 true false 88 135 118 154
Rectangle -13345367 true false 89 159 116 171
Polygon -16777216 true false 210 90 211 101 165 120 180 105
Polygon -16777216 true false 210 90 207 99 240 114
Polygon -16777216 true false 90 90 89 101 135 120 120 105
Polygon -16777216 true false 90 90 93 99 60 114

topic bedazzled
false
0
Circle -13345367 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Rectangle -13345367 true false 182 164 209 176
Rectangle -13345367 true false 183 123 210 135
Rectangle -13345367 true false 87 125 114 137
Circle -16777216 true false 92 139 22
Rectangle -13345367 true false 87 164 114 176
Polygon -16777216 true false 198 211 178 236 150 242 150 253 179 246
Polygon -16777216 true false 214 96 214 109 180 105 180 90
Polygon -16777216 true false 199 211 170 228 148 230 149 219 173 217
Polygon -16777216 true false 86 96 86 109 120 105 120 90
Polygon -16777216 true false 213 95 212 109 242 133
Polygon -16777216 true false 101 211 130 228 152 230 151 219 127 217
Polygon -16777216 true false 87 95 88 109 58 133
Polygon -16777216 true false 102 211 122 236 150 242 150 253 121 246

topic calm
false
0
Circle -13345367 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Circle -16777216 true false 92 139 22
Rectangle -13345367 true false 184 138 211 150
Rectangle -13345367 true false 184 156 211 168
Polygon -16777216 true false 195 225 177 231 149 240 149 251 181 240
Polygon -16777216 true false 195 225 176 230 150 238 150 223 171 224
Rectangle -13345367 true false 89 156 116 168
Rectangle -13345367 true false 89 138 116 150
Polygon -16777216 true false 210 109 210 123 240 135
Polygon -16777216 true false 90 109 90 123 60 135
Polygon -16777216 true false 211 109 211 123 177 132 187 114
Polygon -16777216 true false 89 109 89 123 123 132 113 114
Polygon -16777216 true false 105 225 124 230 150 238 150 223 129 224
Polygon -16777216 true false 105 225 123 231 151 240 151 251 119 240

topic disgusted
false
0
Circle -13345367 true false 22 49 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 91 137 22
Circle -16777216 true false 187 137 22
Rectangle -13345367 true false 185 154 212 166
Rectangle -13345367 true false 186 133 213 145
Rectangle -13345367 true false 88 154 115 166
Polygon -16777216 true false 168 218 149 220 150 208 176 209 180 230
Polygon -16777216 true false 132 218 151 220 150 208 124 209 120 230
Rectangle -13345367 true false 87 133 114 145
Polygon -16777216 true false 207 117 206 126 173 134 170 123 189 121
Polygon -16777216 true false 203 116 201 126 235 127
Polygon -16777216 true false 97 116 99 126 65 127
Polygon -16777216 true false 93 117 94 126 127 134 130 123 111 121
Polygon -16777216 true false 181 232 150 230 150 220 172 220
Polygon -16777216 true false 119 232 150 230 150 220 128 220

topic elated
false
0
Circle -13345367 true false 24 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Rectangle -13345367 true false 182 164 209 176
Rectangle -13345367 true false 183 123 210 135
Polygon -16777216 true false 210 105 210 120 249 137
Polygon -16777216 true false 210 105 210 120 183 136 186 123
Rectangle -13345367 true false 87 125 114 137
Circle -16777216 true false 92 139 22
Rectangle -13345367 true false 87 164 114 176
Polygon -16777216 true false 90 105 90 120 51 137
Polygon -16777216 true false 90 105 90 120 117 136 114 123
Polygon -16777216 true false 199 211 169 226 150 226 150 215 173 217
Polygon -16777216 true false 101 211 131 226 150 226 150 215 127 217
Polygon -16777216 true false 198 211 176 233 150 242 150 253 179 243
Polygon -16777216 true false 102 211 124 233 150 242 150 253 121 243

topic neutral
false
0
Circle -13345367 true false 22 49 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Polygon -16777216 true false 213 106 213 121 249 122
Polygon -16777216 true false 213 106 213 121 183 121 186 108
Circle -16777216 true false 186 139 22
Rectangle -13345367 true false 184 160 211 172
Polygon -16777216 true false 87 106 87 121 51 122
Polygon -16777216 true false 87 106 87 121 117 121 114 108
Circle -16777216 true false 92 139 22
Rectangle -13345367 true false 89 160 116 172
Rectangle -13345367 true false 183 132 210 144
Rectangle -13345367 true false 90 132 117 144
Polygon -16777216 true false 195 234 155 222 149 225 149 231
Polygon -16777216 true false 190 232 165 244 148 244 148 232
Polygon -16777216 true false 105 234 145 222 151 225 151 231
Polygon -16777216 true false 110 232 135 244 152 244 152 232

topic sad
false
0
Circle -13345367 true false 23 45 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Polygon -16777216 true false 213 109 213 121 249 122
Circle -16777216 true false 92 139 22
Polygon -16777216 true false 215 111 213 121 190 108 173 85 189 93
Rectangle -13345367 true false 184 135 211 147
Polygon -16777216 true false 199 249 180 229 149 223 149 231 171 235
Polygon -16777216 true false 87 109 87 121 51 122
Polygon -16777216 true false 85 111 87 121 110 108 127 85 111 93
Rectangle -13345367 true false 187 152 214 164
Polygon -16777216 true false 203 252 150 239 150 231 172 237
Polygon -16777216 true false 101 249 120 229 151 223 151 231 129 235
Polygon -16777216 true false 97 252 150 239 150 231 128 237
Rectangle -13345367 true false 89 135 116 147
Rectangle -13345367 true false 86 152 113 164

topic scared
false
0
Circle -13345367 true false 23 48 252
Line -16777216 false 150 165 150 184
Circle -16777216 false false 154 178 18
Circle -16777216 false false 128 178 18
Circle -16777216 true false 186 139 22
Polygon -16777216 true false 213 109 213 121 249 122
Circle -16777216 true false 92 139 22
Polygon -16777216 true false 215 111 213 121 190 108 173 85 189 93
Rectangle -13345367 true false 90 124 117 136
Rectangle -13345367 true false 184 162 211 174
Polygon -16777216 true false 87 109 87 121 51 122
Polygon -16777216 true false 85 111 87 121 110 108 127 85 111 93
Rectangle -13345367 true false 182 124 209 136
Rectangle -13345367 true false 89 163 116 175
Polygon -16777216 true false 199 249 186 227 149 223 149 231 180 234
Polygon -16777216 true false 201 251 147 256 149 245 184 246
Polygon -16777216 true false 99 251 153 256 151 245 116 246
Polygon -16777216 true false 101 249 114 227 151 223 151 231 120 234

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
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="General Survey" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count males</metric>
    <metric>mean [item 0 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of males with [alter != nobody]</metric>
    <metric>count females</metric>
    <metric>mean [item 0 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of females with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of females with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of females with [alter != nobody]</metric>
    <enumeratedValueSet variable="extra-lines">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-variables">
      <value value="&quot;Emotion EPA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-individuals">
      <value value="6"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-actor-choice">
      <value value="&quot;more esteemed&quot;"/>
      <value value="&quot;more potent&quot;"/>
      <value value="&quot;more active&quot;"/>
      <value value="&quot;least future deflection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-potency">
      <value value="-2"/>
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-inputs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attraction-cutoff">
      <value value="2"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-activity">
      <value value="-2"/>
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proportion-females">
      <value value="25"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-potency">
      <value value="-2"/>
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-attraction-basis">
      <value value="&quot;emotion similarity&quot;"/>
      <value value="&quot;similarity of fundamentals&quot;"/>
      <value value="&quot;least future deflection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-outputs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-actor-choice">
      <value value="&quot;more esteemed&quot;"/>
      <value value="&quot;more potent&quot;"/>
      <value value="&quot;more active&quot;"/>
      <value value="&quot;least future deflection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-goodness">
      <value value="-2"/>
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-activity">
      <value value="-2"/>
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-goodness">
      <value value="-2"/>
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-attraction-basis">
      <value value="&quot;emotion similarity&quot;"/>
      <value value="&quot;similarity of fundamentals&quot;"/>
      <value value="&quot;least future deflection&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GS_Egalitarian_AddressGroup_Comparisons" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>run-number &gt;= 201</exitCondition>
    <metric>mean [personal-deflection] of turtles with [who != ego-is-actor]</metric>
    <metric>count males</metric>
    <metric>mean [personal-deflection] of males</metric>
    <metric>mean [item 0 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of males with [alter != nobody]</metric>
    <enumeratedValueSet variable="save-IO">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grp-act-to-all">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IPA-coding-basis">
      <value value="&quot;1978 sentiments&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fast">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-dominance">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dominance">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-females">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individuality">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-tension">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="actor-choice">
      <value value="&quot;max self-tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="object-choice">
      <value value="&quot;min event tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="address-group-Pr">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reciprocal-act-Pr">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equations">
      <value value="&quot;US unisex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change-next-group's">
      <value value="&quot;sentiments&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GS_Egalitarian_Reciprocity_Comparisons" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>run-number &gt;= 201</exitCondition>
    <metric>mean [personal-deflection] of turtles with [who != ego-is-actor]</metric>
    <metric>count males</metric>
    <metric>mean [personal-deflection] of males</metric>
    <metric>mean [item 0 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of males with [alter != nobody]</metric>
    <enumeratedValueSet variable="save-IO">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grp-act-to-all">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IPA-coding-basis">
      <value value="&quot;1978 sentiments&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fast">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-dominance">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dominance">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-females">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individuality">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-tension">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="actor-choice">
      <value value="&quot;max self-tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="object-choice">
      <value value="&quot;min event tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="address-group-Pr">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reciprocal-act-Pr">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equations">
      <value value="&quot;US unisex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change-next-group's">
      <value value="&quot;sentiments&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GS_Hierarchical_AddressGroup_Comparisons" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>run-number &gt;= 201</exitCondition>
    <metric>mean [personal-deflection] of turtles with [who != ego-is-actor]</metric>
    <metric>count males</metric>
    <metric>mean [personal-deflection] of males</metric>
    <metric>mean [item 0 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of males with [alter != nobody]</metric>
    <metric>count females</metric>
    <metric>mean [personal-deflection] of females</metric>
    <metric>mean [item 0 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of females with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of females with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of females with [alter != nobody]</metric>
    <enumeratedValueSet variable="save-IO">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grp-act-to-all">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IPA-coding-basis">
      <value value="&quot;1978 sentiments&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fast">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-dominance">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dominance">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-females">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individuality">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-tension">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="actor-choice">
      <value value="&quot;max self-tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="object-choice">
      <value value="&quot;min event tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="address-group-Pr">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reciprocal-act-Pr">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equations">
      <value value="&quot;US unisex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change-next-group's">
      <value value="&quot;sentiments&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GS_Hierarchical_Reciprocity_Comparisons" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>run-number &gt;= 201</exitCondition>
    <metric>mean [personal-deflection] of turtles with [who != ego-is-actor]</metric>
    <metric>count males</metric>
    <metric>mean [personal-deflection] of males</metric>
    <metric>mean [item 0 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of males with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of males with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of males with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of males with [alter != nobody]</metric>
    <metric>count females</metric>
    <metric>mean [personal-deflection] of females</metric>
    <metric>mean [item 0 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 1 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 2 behavior] of females with [alter != nobody]</metric>
    <metric>mean [item 0 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 1 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 2 current-transients] of females with [alter != nobody]</metric>
    <metric>mean [item 0 emotion] of females with [alter != nobody]</metric>
    <metric>mean [item 1 emotion] of females with [alter != nobody]</metric>
    <metric>mean [item 2 emotion] of females with [alter != nobody]</metric>
    <enumeratedValueSet variable="save-IO">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grp-act-to-all">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IPA-coding-basis">
      <value value="&quot;1978 sentiments&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fast">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-dominance">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-goodness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dominance">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-activation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-females">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individuality">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-tension">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="actor-choice">
      <value value="&quot;max self-tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="object-choice">
      <value value="&quot;min event tension&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="address-group-Pr">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reciprocal-act-Pr">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equations">
      <value value="&quot;US unisex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change-next-group's">
      <value value="&quot;sentiments&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

back-1
0.0
-0.2 1 1.0 0.0
0.0 1 1.0 0.0
0.2 1 1.0 0.0
link direction
true
0

back-2
0.0
-0.2 1 4.0 4.0
0.0 1 4.0 4.0
0.2 1 4.0 4.0
link direction
true
0

back-3
0.0
-0.2 0 0.0 1.0
0.0 0 0.0 1.0
0.2 1 2.0 2.0
link direction
true
0

foe
0.8
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -955883 false 0 285 300 285
Polygon -2674135 true false 150 150 90 240 210 240 150 150

friend
0.8
-0.2 1 1.0 0.0
0.0 1 1.0 0.0
0.2 1 1.0 0.0
link direction
true
0
Polygon -10899396 true false 150 150 30 300 270 300 150 150

topic-link
0.0
-0.2 1 2.0 2.0
0.0 0 0.0 1.0
0.2 0 0.0 1.0
link direction
true
0
@#$#@#$#@
1
@#$#@#$#@
