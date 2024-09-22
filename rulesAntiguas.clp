(defrule seleccionar-platos-desayuno
    (declare (salience 90))
    (not (Menu-diario (dia 8)))
    ?menu <- (Menu-diario (desayuno $?desayuno))
    (test (eq (length$ ?desayuno) 0))
    ?restante <- (Cantidades-rest (calorias-des ?calrest))
    ?usados <-(usados (plato-desayuno $?ps))
    =>
    (bind ?des-platos (find-all-instances ((?plato desayuno_plato)) (and (not (member$ ?plato:nombre ?ps)) (> ?calrest ?plato:calorias))))
    (bind ?calorias-aux ?calrest)
    (bind ?lista ?desayuno)
    (foreach ?des-plato ?des-platos do
        ;(printout t "Nombre: " (send ?des-plato get-nombre) crlf)
        (if (> (- ?calorias-aux (send ?des-plato get-calorias)) 0) then
            (bind ?calorias-aux (- ?calorias-aux (send ?des-plato get-calorias)))
            (printout t "Plato añadido al desayuno: " (send ?des-plato get-nombre) crlf)
            (bind ?lista (create$ $?lista (send ?des-plato get-nombre)))
        )
    )
    (modify ?restante (calorias-des ?calorias-aux))
    (modify ?menu (desayuno ?lista))
    (modify ?usados (plato-desayuno (create$ ?ps ?lista)))
)

(defrule seleccionar-platos-comida
    (declare (salience 89))
    (not (Menu-diario (dia 8)))
    ?menu <- (Menu-diario (comida $?comida))
    (test (eq (length$ ?comida) 0))
    ?restante <- (Cantidades-rest (calorias-com ?calrest))
    ?usados <-(usados (plato-principal $?ps) (postre $?pp))
    =>
    (bind ?calorias-aux ?calrest)
    (bind ?lista ?comida)
    (bind ?des-platos (find-all-instances ((?plato plato_principal)) (and (not (member$ ?plato:nombre ?ps)) (> ?calrest ?plato:calorias))))
    (foreach ?des-plato ?des-platos do
        ;(printout t "Nombre: " (send ?des-plato get-nombre) crlf)
        (if (and (> (- ?calorias-aux (send ?des-plato get-calorias)) 0) (< (length$ ?lista) 2)) then
            (bind ?calorias-aux (- ?calorias-aux (send ?des-plato get-calorias)))
            (printout t "Plato añadido a la comida: " (send ?des-plato get-nombre) crlf)
            (bind ?lista (create$ $?lista (send ?des-plato get-nombre)))
        )
    )
    ;;; PILLA UN POSTRE
    (bind ?cont 1)
    (bind ?pos-platos (find-all-instances ((?plato postre)) (and (not (member$ ?plato:nombre ?pp))(> ?calrest ?plato:calorias))))
    (foreach ?pos-plato ?pos-platos do
            ;(printout t "Nombre: " (send ?pos-plato get-nombre) crlf)
            (if (and (> (- ?calorias-aux (send ?pos-plato get-calorias)) 0) (eq ?cont 1)) then
                (bind ?calorias-aux (- ?calorias-aux (send ?pos-plato get-calorias)))
                (printout t "Postre añadido a la comida: " (send ?pos-plato get-nombre) crlf)
                (bind ?lista (create$ $?lista (send ?pos-plato get-nombre)))
                (bind ?cont 0)
            )
        )
    (modify ?restante (calorias-com ?calorias-aux))
    (modify ?menu (comida ?lista))
    (modify ?usados (plato-principal (create$ ?ps (delete$ ?lista (length$ ?lista) (length$ ?lista)))) (postre (create$ ?pp (nth$ (length$ ?lista) ?lista))))
)


(defrule seleccionar-platos-cena
    (declare (salience 87))
    (not (Menu-diario (dia 8)))
    ?menu <- (Menu-diario (cena $?cena))
    (test (eq (length$ ?cena) 0))
    ?restante <- (Cantidades-rest (calorias-cen ?calrest))
    ?usados <-(usados (plato-principal $?ps) (postre $?pp))
    =>
    (bind ?calorias-aux ?calrest)
    (bind ?lista ?cena)
    (bind ?des-platos (find-all-instances ((?plato plato_principal)) (and (not (member$ ?plato:nombre ?ps)) (> ?calrest ?plato:calorias))))
    (foreach ?des-plato ?des-platos do
        ;(printout t "Nombre: " (send ?des-plato get-nombre) crlf)
        (if (and (> (- ?calorias-aux (send ?des-plato get-calorias)) 0) (< (length$ ?lista) 2)) then
            (bind ?calorias-aux (- ?calorias-aux (send ?des-plato get-calorias)))
            (printout t "Plato añadido a la cena: " (send ?des-plato get-nombre) crlf)
            (bind ?lista (create$ $?lista (send ?des-plato get-nombre)))
        )
    )
    ;;; PILLA UN POSTRE
    (bind ?cont 1)
    (bind ?pos-platos (find-all-instances ((?plato postre)) (and (not (member$ ?plato:nombre ?pp))(> ?calrest ?plato:calorias))))
    (foreach ?pos-plato ?pos-platos do
            ;(printout t "Nombre: " (send ?pos-plato get-nombre) crlf)
            (if (and (> (- ?calorias-aux (send ?pos-plato get-calorias)) 0) (eq ?cont 1)) then
                (bind ?calorias-aux (- ?calorias-aux (send ?pos-plato get-calorias)))
                (printout t "Postre añadido a la cena: " (send ?pos-plato get-nombre) crlf)
                (bind ?lista (create$ $?lista (send ?pos-plato get-nombre)))
                (bind ?cont 0)
            )
        )
    (modify ?restante (calorias-cen ?calorias-aux))
    (modify ?menu (cena ?lista))
	(modify ?usados (plato-principal (create$ ?ps (delete$ ?lista (length$ ?lista) (length$ ?lista)))) (postre (create$ ?pp (nth$ (length$ ?lista) ?lista))))
)

(defrule resetear-cantidades-rest "Reseteamos las cantidades de los elementos diarios una vez se completa el menu de un día"
    (not (Menu-diario (dia 8)))
    ?menu <- (Menu-diario (dia ?dia) (desayuno $?desayuno) (comida $?comida) (cena $?cena))
    (test (neq (length$ ?cena) 0))
    ?c <- (Cantidades-rest)
    ?p <- (Persona (calorias ?calo))
    =>
    ;;; Reseteamos cantidades diarias
    (modify ?c (calorias-des (round (* 0.3 ?calo))) (calorias-com (round (* 0.4 ?calo))) (calorias-cen (round (* 0.3 ?calo))))


    ;;;Reseteamos platos
    (printout t crlf "MENU DEL DIA " ?dia  crlf)
    (printout t  "Platos asignados al desayuno: "  ?desayuno crlf)
    (printout t  "Platos asignados a la comida: " ?comida crlf)
    (printout t  "Platos asignados a la cena: " ?cena crlf crlf)

    (modify ?menu (dia (+ ?dia 1)) (desayuno ) (comida ) (cena ))
)