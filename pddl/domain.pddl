(define (domain grid-world)
  (:requirements :strips :typing)
  (:types location)
  (:predicates 
    (at ?l - location)
    (connected ?l1 ?l2 - location)
    (treasure-at ?l - location)
  )

  (:action move
    :parameters (?from ?to - location)
    :precondition (and (at ?from) (connected ?from ?to))
    :effect (and (not (at ?from)) (at ?to))
  )
)
