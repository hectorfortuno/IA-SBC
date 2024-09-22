;;*******************************
;;* DEFINICION DE LAS FUNCIONES *
;;*******************************


(deffunction ask-question (?question $?allowed-values)
    (printout t ?question)
    (bind ?answer (read))
    (if (lexemep ?answer)
        then (bind ?answer (lowcase ?answer)))
    (while (not (member$ ?answer ?allowed-values)) do
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer)
            then (bind ?answer (lowcase ?answer))))
    ?answer)

(deffunction question (?question)
    (format t "%s" ?question)
    (bind ?answer (read))
    ?answer
)

(deffunction yes-or-no-p (?question)
    (bind ?response (ask-question ?question yes no y n))
    (if (or (eq ?response yes) (eq ?response y)) then 1 else 0)
)

(deffunction lista-ingredientes (?plato)
  (bind ?lista-ingredientes (create$)) ; Crear una lista vacía para los ingredientes

  (foreach ?plato ?plato
    (foreach ?ingrediente (send ?plato get-formado_por)
        (bind ?lista-ingredientes (create$ ?lista-ingredientes ?ingrediente))
    )
  )
  (return ?lista-ingredientes)
)

(deffunction lista-ingredientes-principales (?plato)
    (bind ?i1 (first$ (send (nth$ 3 ?plato) get-formado_por)))
    (bind ?i2 (first$ (send (nth$ 4 ?plato) get-formado_por)))
    (bind ?i3 (first$ (send (nth$ 6 ?plato) get-formado_por)))
    (bind ?i4 (first$ (send (nth$ 7 ?plato) get-formado_por)))
    (bind ?lista-ingredientes-principales (create$ ?i1 ?i2 ?i3 ?i4))
  (return ?lista-ingredientes-principales)
)

(deffunction repetidos-ingredientes (?plato)
  (bind ?lista-ingredientes (create$)) ; Crear una lista vacía para los ingredientes

  (foreach ?plato ?plato
    (foreach ?ingrediente (send ?plato get-formado_por)
      (if (member$ ?ingrediente ?lista-ingredientes)
        then
        (return TRUE)
      )
      (bind ?lista-ingredientes (create$ ?lista-ingredientes ?ingrediente))
    )
  )
  (return FALSE)
)

(deffunction ingredientes-principales (?plato)
  (bind ?lista-ingredientes (create$)) ; Crear una lista vacía para los ingredientes
  (bind ?indices (create$ 3 4 6 7))
  (foreach ?idx ?indices
    (bind ?ing (first$ (send (nth$ ?idx ?plato) get-formado_por)))
    (if (member$ ?ing ?lista-ingredientes)
        then
        (return TRUE)
    )
    (bind ?lista-ingredientes (create$ ?lista-ingredientes ?ing))
  )
  (return FALSE)
)

(deffunction generar-combinacion (?des-plato ?pri-plato ?seg-plato ?pos-plato)

    ;Tamaño de la lista de plato
    (bind ?tam-des (length$ ?des-plato))
    (bind ?tam-pri (length$ ?pri-plato))
    (bind ?tam-seg (length$ ?seg-plato))
    (bind ?tam-pos (length$ ?pos-plato))

    ;Indices para escoger platos
    ;Desayuno
    (bind ?d1 (nth$ (random 1 ?tam-des) ?des-plato))
    (bind ?d2 (nth$ (random 1 ?tam-des) ?des-plato))
    (while (eq (send ?d1 get-nombre) (send ?d2 get-nombre)) do (bind ?d2 (nth$ (random 1 ?tam-des) ?des-plato)))

    ;Comida
    (bind ?c1 (nth$ (random 1 ?tam-pri) ?pri-plato))
    (while (or (eq (send ?c1 get-nombre) (send ?d1 get-nombre)) (eq (send ?c1 get-nombre) (send ?d2 get-nombre))) do (bind ?c1 (nth$ (random 1 ?tam-pri) ?pri-plato)))
    (bind ?c2 (nth$ (random 1 ?tam-seg) ?seg-plato))
    (while (or (eq (send ?c1 get-nombre) (send ?c2 get-nombre)) (eq (send ?c2 get-nombre) (send ?d1 get-nombre)) (eq (send ?c2 get-nombre) (send ?d2 get-nombre))
        (and (eq "true" (send ?c1 get-contundente)) (eq "true" (send ?c2 get-contundente)))) do
            (bind ?c2 (nth$ (random 1 ?tam-seg) ?seg-plato)))
    (bind ?p1 (nth$ (random 1 ?tam-pos) ?pos-plato))
    (while (or (eq (send ?p1 get-nombre) (send ?d1 get-nombre)) (eq (send ?p1 get-nombre) (send ?d2 get-nombre))) do (bind ?p1 (nth$ (random 1 ?tam-pos) ?pos-plato)))

    ;Cena
    (bind ?ce1 (nth$ (random 1 ?tam-pri) ?pri-plato))
    (while (or (eq (send ?ce1 get-nombre) (send ?c2 get-nombre)) (eq (send ?ce1 get-nombre) (send ?c1 get-nombre)) (eq (send ?ce1 get-nombre) (send ?d1 get-nombre))
        (eq (send ?ce1 get-nombre) (send ?d2 get-nombre)) (eq "true" (send ?ce1 get-contundente))) do
            (bind ?ce1 (nth$ (random 1 ?tam-pri) ?pri-plato)))
    (bind ?ce2 (nth$ (random 1 ?tam-seg) ?seg-plato))
    (while (or (eq (send ?ce2 get-nombre) (send ?c2 get-nombre)) (eq (send ?ce2 get-nombre) (send ?c1 get-nombre)) (eq (send ?ce2 get-nombre) (send ?ce1 get-nombre))
        (eq (send ?ce2 get-nombre) (send ?d1 get-nombre)) (eq (send ?ce2 get-nombre) (send ?d2 get-nombre)) (eq "true" (send ?ce2 get-contundente))) do
            (bind ?ce2 (nth$ (random 1 ?tam-seg) ?seg-plato)))
    (bind ?p2 (nth$ (random 1 ?tam-pos) ?pos-plato))
    (while (or (eq (send ?p2 get-nombre) (send ?p1 get-nombre)) (eq (send ?p2 get-nombre) (send ?d1 get-nombre)) (eq (send ?p2 get-nombre) (send ?d2 get-nombre))
        (eq "true" (send ?p2 get-contundente))) do (bind ?p2 (nth$ (random 1 ?tam-pos) ?pos-plato)))

    (bind ?menu (create$ ?d1 ?d2 ?c1 ?c2 ?p1 ?ce1 ?ce2 ?p2))
    (return ?menu)
)

(deffunction contar-nutrientes ($?plato)
    (bind ?cal 0)
    (bind ?hid 0)
    (bind ?gra 0)
    (bind ?pro 0)
    (bind ?fib 0)
    (foreach ?plato ?plato
        (bind ?cal (+ ?cal (send ?plato get-calorias)))
        (bind ?hid (+ ?hid (send ?plato get-cantidad_hidratos)))
        (bind ?gra (+ ?gra (send ?plato get-cantidad_grasas)))
        (bind ?pro (+ ?pro (send ?plato get-cantidad_proteinas)))
        (bind ?fib (+ ?fib (send ?plato get-cantidad_fibras)))
    )
    (return (create$ ?cal ?hid ?gra ?pro ?fib))
)


(deffunction menu-correcto (?posible-menu ?cal)
    (bind ?nutrientes-menu (contar-nutrientes ?posible-menu))

    (if (and
        (not (repetidos-ingredientes (subseq$ ?posible-menu 3 4)))
        (not (repetidos-ingredientes (subseq$ ?posible-menu 6 7)))
        (not (ingredientes-principales ?posible-menu))
        (>= (+ ?cal 150) (nth$ 1 ?nutrientes-menu))
        (<= (- ?cal 150) (nth$ 1 ?nutrientes-menu))
        ; Hidratos
        (>= (* ?cal 0.55) (* 4 (nth$ 2 ?nutrientes-menu)))
        (<= (* ?cal 0.45) (* 4 (nth$ 2 ?nutrientes-menu)))
        ; Grasas
        (>= (* ?cal 0.35) (* 9 (nth$ 3 ?nutrientes-menu)))
        (<= (* ?cal 0.3) (* 9 (nth$ 3 ?nutrientes-menu)))
        ; Proteinas
        (>= (* ?cal 0.2) (* 4 (nth$ 4 ?nutrientes-menu)))
        (<= (* ?cal 0.15) (* 4 (nth$ 4 ?nutrientes-menu)))
        ; Fibras
        (>= 30 (nth$ 5 ?nutrientes-menu))
        (<= 20 (nth$ 5 ?nutrientes-menu))
        ) then (return TRUE) else (return FALSE)
    )
)

(deffunction calcular-calorias-fun(?edad ?sexo ?actividad)
    (if(eq ?sexo m) then
        (if (and (eq ?actividad 0) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2006)
        )
        (if (and (eq ?actividad 1) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2293)
        )
        (if (and (eq ?actividad 2) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2603)
        )
        (if (and (eq ?actividad 0) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 1982)
        )
        (if (and (eq ?actividad 1) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 2268)
        )
        (if (and (eq ?actividad 2) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 2555)
        )
        (if (> ?edad 79) then
            ; Calculado a partir de la media de MJ de actividades físicas 0 y 1, y se le resta 0.1 MJ que desciende por cada 10 años
            (bind ?kcal 2102)
        )
    )
    (if(eq ?sexo f) then
        (if (and (eq ?actividad 0) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 1634)
        )
        (if (and (eq ?actividad 1) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 1863)
        )
        (if (and (eq ?actividad 2) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2102)
        )
        (if (and (eq ?actividad 0) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 1634)
        )
        (if (and (eq ?actividad 1) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 1839)
        )
        (if (and (eq ?actividad 2) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 2078)
        )
        (if (> ?edad 79) then
            ; Calculado a partir de la media de MJ de actividades físicas 0 y 1, y se le resta 0.1 MJ que desciende por cada 10 años
            (bind ?kcal 1708)
        )
    )
    ?kcal
)

(deffunction borrar-plato (?ing)
    (do-for-all-instances ((?plato plato)) (member$ ?plato (send ?ing get-esta_en))
        (send ?plato delete)
    )
)


(deffunction borrar-plato-temporada (?temporada_actual)
    (bind ?temporada_actual (str-cat ?temporada_actual ""))
    ; Por cada ingrediente que no esté en temporada
    (do-for-all-instances ((?ingrediente ingrediente)) (and (not (member$ "siempre" ?ingrediente:disponible_en)) (not (member$ ?temporada_actual ?ingrediente:disponible_en)))
        ; Por cada plato que lleve ese ingrediente
        (do-for-all-instances ((?plato plato)) (member$ ?plato ?ingrediente:esta_en)
            ; Por cada ingrediente que lleva ese plato
            (do-for-all-instances ((?ingrediente2 ingrediente)) (member$ ?plato ?ingrediente2:esta_en)
                (bind ?lista-plato ?ingrediente2:esta_en)
                (bind $?nueva-lista (delete-member$ ?lista-plato ?plato))
                (modify-instance ?ingrediente2 (esta_en ?nueva-lista))
            )
            (send ?plato delete)
        )
    )
)

(deffunction filtrar-plato (?plato ?ingredientes-excluidos)
  (bind ?plato-filtrados (create$))
  (foreach ?plato ?plato
    (bind ?excluir FALSE)
    (foreach ?ingrediente ?ingredientes-excluidos
      (if (member$ ?ingrediente (send ?plato get-formado_por))
        then
        (bind ?excluir TRUE)
        (break)
      )
    )
    (if (not ?excluir)
      then
      (bind ?plato-filtrados (create$ ?plato ?plato-filtrados))
    )
  )
  (return ?plato-filtrados)
)

(deffunction eliminar-repetidos (?lista)
   (bind ?lista-unicos (create$))
   (foreach ?elemento ?lista
      (if (not (member$ ?elemento ?lista-unicos)) then
         (bind ?lista-unicos (create$ ?lista-unicos ?elemento))
      )
   )
   (return ?lista-unicos)
)

;;*******************************
;;* INICIO DEL CREADOR DE MENUS *
;;*******************************

(defmodule MAIN (export ?ALL))

(defrule initial
    (initial-fact)
    =>
    (printout t crlf "*** BIENVENIDOS AL CREADOR DE MENUS *** " crlf crlf)
    (focus PREGUNTAS)
)

;;;***********************
;;;* MÓDULO DE PREGUNTAS *
;;;***********************

(defmodule PREGUNTAS (import MAIN ?ALL) (export ?ALL))

(deftemplate Persona
    (slot edad (type INTEGER) (range 65 120))
    (slot sexo (type STRING) (allowed-values "m" "f"))
    (slot actividad-fisica (type INTEGER) (range 0 2))
    ; Kilocalorias diarias recomendadas
    (slot calorias (type INTEGER))
)


(deftemplate Enfermedades-persona
    (slot colesterol (type INTEGER) (allowed-integers 0 1))
    (slot diabetes (type INTEGER) (allowed-integers 0 1))
    (slot anemia (type INTEGER) (allowed-integers 0 1))
    (slot hipertension (type INTEGER) (allowed-integers 0 1))
    (slot osteoporosis (type INTEGER) (allowed-integers 0 1))
    (slot obesidad (type INTEGER) (allowed-integers 0 1))
)

(deftemplate Alergias-persona
    (slot pescado (type INTEGER) (allowed-integers 0 1))
    (slot carne (type INTEGER) (allowed-integers 0 1))
    (slot lacteos (type INTEGER) (allowed-integers 0 1))
    (slot frutas (type INTEGER) (allowed-integers 0 1))
    (slot cereales (type INTEGER) (allowed-integers 0 1))
    (slot verduras (type INTEGER) (allowed-integers 0 1))
)

(defrule crear-perfil ""
    (declare (salience 1000))
    =>
    (printout t "Introduzca sus datos personales." crlf)
    (bind ?edad (question "Que edad tiene? (Valor entre 65 y 120) "))
    (bind ?edad (integer ?edad))
    (if (or (> ?edad 120) (< ?edad 65) ) then (printout t crlf "La edad introducida no esta entre 65 y 120" crlf)(exit))
    (bind ?sexo (ask-question "Indique su sexo Masculino (m) o Femenino (f): " m f))
    (bind ?act (ask-question "Cuanta actividad física realiza (0=Poca, 1=Normal, 2=Mucha)? " 0 1 2))
    (bind ?calo (calcular-calorias-fun ?edad ?sexo ?act))
    (assert (Persona (edad ?edad) (sexo ?sexo) (actividad-fisica ?act) (calorias ?calo)))
    (printout t crlf "Información personal guardada correctamente." crlf)
)

(defrule preguntar-enfermedades ""
	(declare (salience 999))
	=>
	(printout t crlf "Responde las siguientes preguntas para que el menú tenga en cuenta sus enfermedades con (yes/no) o (y/n)." crlf)
	(bind ?col (yes-or-no-p "¿Tiene el colesterol alto? "))
    (bind ?dia (yes-or-no-p "¿Tiene diabetes? "))
    (bind ?ane (yes-or-no-p "¿Tiene anemia? (deficiencia de hierro) "))
    (bind ?hip (yes-or-no-p "¿Tiene hipertensión arterial? "))
    (bind ?ost (yes-or-no-p "¿Tiene osteoporosis? "))
    (bind ?obe (yes-or-no-p "¿Tiene obesidad? "))
	(assert (Enfermedades-persona (colesterol ?col) (diabetes ?dia) (anemia ?ane) (hipertension ?hip) (osteoporosis ?ost)
			(obesidad ?obe)))
	(printout t crlf "Información de las enfermedades guardada correctamente." crlf)
    (bind ?ingredientes-enfermedades (create$))
    (assert (ing-enf ?ingredientes-enfermedades))
)

(defrule preguntar-alergias ""
	(declare (salience 998))
	=>
	(printout t crlf "Responde las siguientes preguntas para que el menú tenga en cuenta sus alergias." crlf)
	(printout t crlf "Cada pregunta estará relacionada con un grupo de alimentos general. Si en ese grupo existe algún alimento al cual tenga alergia indíquelo con (yes/y), por el contrario indique (no/n)." crlf)
	(bind ?pes (yes-or-no-p "¿Tiene alergia a algún pescado? "))
    (bind ?roj (yes-or-no-p "¿Tiene alergia a alguna carne? "))
    (bind ?lac (yes-or-no-p "¿Tiene alergia a algún lácteo? "))
    (bind ?fru (yes-or-no-p "¿Tiene alergia a alguna fruta? "))
    (bind ?cer (yes-or-no-p "¿Tiene alergia a algún cereal? "))
    (bind ?ver (yes-or-no-p "¿Tiene alergia a alguna verdura o legumbre? "))
	(assert (Alergias-persona (pescado ?pes) (carne ?roj) (lacteos ?lac) (frutas ?fru) (cereales ?cer) (verduras ?ver)))
    (assert (ingredientes-alergico))
	(printout t crlf "Información de las preferencias guardada correctamente." crlf)
)

;;; ***** POR ALERGIAS *****

(defrule alergia-pescado
	(declare (salience 957))
	?pre <- (Alergias-persona (pescado 1))
    =>
    (printout t crlf "Indique con (1) si es alérgico al alimento o con (2) si no tiene problemas." crlf)
    (bind ?i (ask-question "¿Salmón? " 1 2))
    (if (eq ?i 1) then (borrar-plato [salmon]))
    (bind ?i (ask-question "¿Merluza? " 1 2))
    (if (eq ?i 1) then (borrar-plato [merluza]))
    (bind ?i (ask-question "¿Rape? " 1 2))
    (if (eq ?i 1) then (borrar-plato [rape]))
    (bind ?i (ask-question "¿Bacalao? " 1 2))
    (if (eq ?i 1) then (borrar-plato [bacalao]))
    (bind ?i (ask-question "¿Gamba? " 1 2))
    (if (eq ?i 1) then (borrar-plato [gamba]))
    (bind ?i (ask-question "¿Atún? " 1 2))
    (if (eq ?i 1) then (borrar-plato [atun]))
	(modify ?pre (pescado 0))
)

(defrule alergia-carne
	?pre <- (Alergias-persona (carne 1))
    =>
    (printout t crlf "Indique con (1) si es alérgico al alimento o con (2) si no tiene problemas." crlf)
    (bind ?i (ask-question "¿Pollo? " 1 2))
    (if (eq ?i 1) then (borrar-plato [pollo]))
    (bind ?i (ask-question "¿Ternera? " 1 2))
    (if (eq ?i 1) then (borrar-plato [ternera]))
    (bind ?i (ask-question "¿Cerdo? " 1 2))
    (if (eq ?i 1) then (borrar-plato [cerdo]))
	(modify ?pre (carne 0))
)

(defrule alergia-lacteos
	(declare (salience 957))
	?pre <- (Alergias-persona (lacteos 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento o con (2) si no tiene problemas." crlf)
    (bind ?i (ask-question "¿Leche? " 1 2))
    (if (eq ?i 1) then (borrar-plato [leche]))
    (bind ?i (ask-question "¿Queso? " 1 2))
    (if (eq ?i 1) then (borrar-plato [queso]))
	(modify ?pre (lacteos 0))
)

(defrule alergia-frutas
	(declare (salience 957))
	?pre <- (Alergias-persona (frutas 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento o con (2) si no tiene problemas." crlf)
    (bind ?i (ask-question "¿Plátano? " 1 2))
    (if (eq ?i 1) then (borrar-plato [platano]))
    (bind ?i (ask-question "¿Naranja? " 1 2))
    (if (eq ?i 1) then (borrar-plato [naranja]))
    (bind ?i (ask-question "¿Melón? " 1 2))
    (if (eq ?i 1) then (borrar-plato [melon]))
    (bind ?i (ask-question "¿Tomate? " 1 2))
    (if (eq ?i 1) then (borrar-plato [tomate]))
	(modify ?pre (frutas 0))
)

(defrule alergia-cereales
	(declare (salience 957))
	?pre <- (Alergias-persona (cereales 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento o con (2) si no tiene problemas." crlf)
    (bind ?i (ask-question "¿Pasta? " 1 2))
    (if (eq ?i 1) then (borrar-plato [pasta]))
    (bind ?i (ask-question "¿Arroz? " 1 2))
    (if (eq ?i 1) then (borrar-plato [arroz]))
    (bind ?i (ask-question "¿Pan? " 1 2))
    (if (eq ?i 1) then (borrar-plato [pan]))
    (bind ?i (ask-question "¿Harina? " 1 2))
    (if (eq ?i 1) then (borrar-plato [harina]))
	(modify ?pre (cereales 0))
)

(defrule alergia-verduras-legumbres
	(declare (salience 957))
	?pre <- (Alergias-persona (verduras 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento o con (2) si no tiene problemas." crlf)
    (bind ?i (ask-question "¿Zanahoria? " 1 2))
    (if (eq ?i 1) then (borrar-plato [zanahoria]))
    (bind ?i (ask-question "¿Patata? " 1 2))
    (if (eq ?i 1) then (borrar-plato [patata]))
    (bind ?i (ask-question "¿Lechuga? " 1 2))
    (if (eq ?i 1) then (borrar-plato [lechuga]))
    (bind ?i (ask-question "¿Lentejas? " 1 2))
    (if (eq ?i 1) then (borrar-plato [lentejas]))
    (bind ?i (ask-question "¿Garbanzos? " 1 2))
    (if (eq ?i 1) then (borrar-plato [garbanzos]))
	(modify ?pre (verduras 0))
)


;;;****************************

(defrule preguntar-temporada ""
	(declare (salience 917))
	=>
	(printout t crlf "Información adicional para configurar el menú." crlf)
	(bind ?temp (ask-question "¿En qué estación del año estamos? (primavera, verano, otoño o invierno) -- Escriba aquí la estación: " primavera verano otoño invierno))
    (assert (temporada ?temp))
	(borrar-plato-temporada ?temp)
)

(defrule preguntar-preferencias "Pregunta al usuario por sus preferencias respecto a una serie de parejas de ingredientes"
    ?t <- (temporada ?temp)
    =>
    (bind ?ingredientes-preferidos (create$))
    (printout t crlf "Por cada pareja de alimentos, indique con (1) si prefiere el primero, con (2) si prefiere el segundo y con (3) si no tiene preferencia." crlf)

    ; Melón o Sandía
    (if (eq ?temp verano) then
        (bind ?mel-san (ask-question "¿Melón o Sandía? " 1 2 3))
        (if (eq ?mel-san 1) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Melon")))
        (if (eq ?mel-san 2) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Sandia")))
    )

    ; Lentejas o Garbanzos
    (bind ?len-gar (ask-question "¿Lentejas o Garbanzos? " 1 2 3))
    (if (eq ?len-gar 1) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Lentejas")))
    (if (eq ?len-gar 2) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Garbanzos")))

    ; Naranja o Mandarina
    (if (eq ?temp otoño) then
        (bind ?nar-man (ask-question "¿Naranja o Mandarina? " 1 2 3))
        (if (eq ?nar-man 1) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Naranja")))
        (if (eq ?nar-man 2) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Mandarina")))
    )

    ; Cerdo o Ternera
    (bind ?cer-ter (ask-question "¿Cerdo o Ternera? " 1 2 3))
    (if (eq ?cer-ter 1) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Cerdo")))
    (if (eq ?cer-ter 2) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Ternera")))

    ; Rape o Bacalao
    (bind ?rap-bac (ask-question "¿Rape o Bacalao? " 1 2 3))
    (if (eq ?rap-bac 1) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Rape")))
    (if (eq ?rap-bac 2) then (bind ?ingredientes-preferidos (create$ ?ingredientes-preferidos "Bacalao")))

    (assert (ingr-pref ?ingredientes-preferidos))
    (focus FILTRADO)
)


;;;********************************
;;;* MÓDULO DE FILTRADO DE PLATO *
;;;********************************

(defmodule FILTRADO (import MAIN ?ALL) (import PREGUNTAS ?ALL) (import MAIN ?ALL) (export ?ALL))

;;; ***** POR ENFERMEDADES *****

;;; 25-35% grasas, 50-60% carbohidratos, 20-30 gramos fibra, > 15% proteinas
;;; Alimentos de origen vegetal, alto contenido en fibra, integrales y pescado. Evitar grasas saturadas y 5g max de sal/d.
(defrule tiene-colesterol
    (declare (salience 999))
    ?enf <- (Enfermedades-persona (colesterol 1))
    =>
    (bind ?lista (find-all-instances ((?plato plato)) (> (send ?plato get-cantidad_grasas) 30)))
    (foreach ?plato ?lista
    		(bind ?p (nth$ 1 (find-instance ((?pla plato)) (eq ?plato ?pla))))

    		(do-for-all-instances ((?ingrediente ingrediente)) (member$ ?p ?ingrediente:esta_en)
    		    (bind $?lista (send ?ingrediente get-esta_en))
    		    (bind $?nueva-lista (delete-member$ ?lista ?p))
    		    (send ?ingrediente put-esta_en ?nueva-lista)
            )

    		(send ?p delete)
    )
	(modify ?enf (colesterol 0))
)

;;; > 50-60% hidratos de carbono, 30-40 gramos fibra, 12-20% proteinas, > 30% grasas,
;;; Hidratos de carbono sanos, mucha fibra, pescado, quitar azucares, 2.5-3 g  sal/d
(defrule tiene-diabetes
    (declare (salience 998))
	?enf <- (Enfermedades-persona (diabetes 1))
        =>
    	(borrar-plato [azucar])
        (modify ?enf (diabetes 0))

)

;;; Vitamina C mejora absorción hierro
(defrule tiene-anemia
    (declare (salience 997))
	?enf <- (Enfermedades-persona (anemia 1))
    ?ing <- (ing-enf $?ingredientes-enfermedades)
    =>
    (bind ?hierro [hierro])
    (bind ?vitaminaC [vitaminaC])
    (bind ?añadir (create$))
    (do-for-all-instances ((?ingrediente ingrediente)) (or (member$ ?hierro (send ?ingrediente get-contiene)) (member$ ?vitaminaC (send ?ingrediente get-contiene)))
        (bind ?añadir (create$ ?añadir ?ingrediente:nombre)))
    (retract ?ing) ; Retract the existing instance
    (assert (ing-enf $?ingredientes-enfermedades ?añadir)) ; Assert a new instance with the modified slot value
    (modify ?enf (anemia 0))
)

;;; Incluir potasio y calcio
(defrule tiene-hipertension
    (declare (salience 996))
	?enf <- (Enfermedades-persona (hipertension 1))
    ?ing <- (ing-enf $?ingredientes-enfermedades)
    =>
    (bind ?calcio [calcio])
    (bind ?potasio [potasio])
    (bind ?añadir (create$))
    (do-for-all-instances ((?ingrediente ingrediente)) (or (member$ ?calcio (send ?ingrediente get-contiene)) (member$ ?potasio (send ?ingrediente get-contiene)))
        (bind ?añadir (create$ ?añadir ?ingrediente:nombre)))
    (retract ?ing) ; Retract the existing instance
    (assert (ing-enf $?ingredientes-enfermedades ?añadir)) ; Assert a new instance with the modified slot value
    (modify ?enf (hipertension 0))
)

;;; Mejorar ingesta calcio
(defrule tiene-osteoporosis
    (declare (salience 995))
	?enf <- (Enfermedades-persona (osteoporosis 1))
    ?ing <- (ing-enf $?ingredientes-enfermedades)
    =>
    (bind ?calcio [calcio])
    (bind ?añadir (create$))
    (do-for-all-instances ((?ingrediente ingrediente)) (member$ ?calcio (send ?ingrediente get-contiene))
        (bind ?añadir (create$ ?añadir ?ingrediente:nombre)))
    (retract ?ing) ; Retract the existing instance
    (assert (ing-enf $?ingredientes-enfermedades ?añadir)) ; Assert a new instance with the modified slot value
    (modify ?enf (osteoporosis 0))
)

;;;Aumentar ingesta de proteina, fibra y alimentos de origen vegetal
;;;
(defrule tiene-obesidad
	(declare (salience 994))
	?enf <- (Enfermedades-persona (obesidad 1))
    =>
    (bind ?lista (find-all-instances ((?plato plato)) (eq (send ?plato get-cocinado) [frito])))
    (foreach ?plato ?lista
    		(bind ?p (nth$ 1 (find-instance ((?pla plato)) (eq ?plato ?pla))))

    		(do-for-all-instances ((?ingrediente ingrediente)) (member$ ?p ?ingrediente:esta_en)
    		    (bind $?lista (send ?ingrediente get-esta_en))
    		    (bind $?nueva-lista (delete-member$ ?lista ?p))
    		    (send ?ingrediente put-esta_en ?nueva-lista)
            )
    		(send ?p delete)
    )
	(modify ?enf (obesidad 0))
)

(defrule no-mas-filtrado
    (declare (salience 993))
    =>
    (focus ASIGNACION)
)

;;;*****************************



(defmodule ASIGNACION (import MAIN ?ALL) (import PREGUNTAS ?ALL)(import FILTRADO ?ALL)(export ?ALL))

;;;***********************************
;;;* MÓDULO DE ACTUALIZACIÓN DE MENÚ *
;;;***********************************


(deftemplate usados
    (multislot plato)
)

(deftemplate ing-usados
    (multislot ingrediente)
)

(deftemplate Menu-diario
    (slot dia (type INTEGER) (range 1 8))
    (multislot menu)
)

(defrule empieza-asignacion
    (declare (salience 1))
    =>
    (assert (empieza 7))
    (assert (usados))
    (assert (ing-usados))
)

(defrule seleccionar-plato
    ?usado <-(usados (plato $?p))
    ?ing-usado <-(ing-usados (ingrediente $?i))
    ?emp <-(empieza ?d)
    (test (> ?d 0))
    ?pe <- (Persona (calorias ?cal))
    ?ing <- (ingr-pref $?ingredientes-preferidos)
    ?ing2 <- (ing-enf $?ingredientes-enfermedades)
    =>
    (bind ?des-plato (find-all-instances ((?plato plato)) (and (not (member$ ?plato:nombre ?p)) (member$ "desayuno" ?plato:tipoComida))))
    (bind ?pri-plato (find-all-instances ((?plato plato)) (and (not (member$ ?plato:nombre ?p)) (member$ "primero" ?plato:tipoComida))))
    (bind ?seg-plato (find-all-instances ((?plato plato)) (and (not (member$ ?plato ?p)) (member$ "segundo" ?plato:tipoComida))))
    (bind ?pos-plato (find-all-instances ((?plato plato)) (and (not (member$ ?plato:nombre ?p)) (member$ "postre" ?plato:tipoComida))))

    (bind ?ingredientes-a-duplicar (create$ ?ingredientes-preferidos ?ingredientes-enfermedades))
    (bind ?ingredientes-a-duplicar (eliminar-repetidos ?ingredientes-a-duplicar)) ; Eliminamos ingredientes que puedan estar duplicados

    ; Duplicamos los plato cuyos ingredientes son preferidos
    ; Por todos los ingredientes favoritos
    (foreach ?nombre-ingrediente ?ingredientes-a-duplicar
        (bind ?ingrediente (nth$ 1 (find-instance ((?i ingrediente)) (eq ?i:nombre ?nombre-ingrediente))))
        ; Por todos los plato en los que está ese ingrediente
        (do-for-all-instances ((?plato plato)) (member$ ?plato (send ?ingrediente get-esta_en))
            ; Si el plato es de desayuno y se puede escoger
            (if (and (member$ "desayuno" ?plato:tipoComida) (member$ ?plato ?des-plato)) then
                (bind ?des-plato (create$ ?des-plato ?plato)) ; Duplicamos el plato que contiene un ingrediente que le gusta
            )
            ; Si el plato es un primero y se puede escoger
            (if (and (member$ "primero" ?plato:tipoComida) (member$ ?plato ?pri-plato)) then
                (bind ?pri-plato (create$ ?pri-plato ?plato)) ; Duplicamos el plato que contiene un ingrediente que le gusta
            )
            ; Si el plato es un segundo y se puede escoger
            (if (and (member$ "segundo" ?plato:tipoComida) (member$ ?plato ?seg-plato)) then
                (bind ?seg-plato (create$ ?seg-plato ?plato)) ; Duplicamos el plato que contiene un ingrediente que le gusta
            )

            ; Si el plato es de postre y se puede escoger
            (if (and (member$ "postre" ?plato:tipoComida) (member$ ?plato ?pos-plato)) then
                (bind ?pos-plato (create$ ?pos-plato ?plato)) ; Duplicamos el plato que contiene un ingrediente que le gusta
            )
        )
    )

    (bind ?pri-plato-fil (filtrar-plato ?pri-plato ?i))
    (bind ?seg-plato-fil (filtrar-plato ?seg-plato ?i))

    (bind ?posible-menu (generar-combinacion ?des-plato ?pri-plato-fil ?seg-plato-fil ?pos-plato))
    (bind ?a 1)
    (while (and (not (menu-correcto ?posible-menu ?cal)) (< ?a 10000)) do
        (bind ?posible-menu (generar-combinacion ?des-plato ?pri-plato-fil ?seg-plato-fil ?pos-plato))
        (bind ?a (+ ?a 1))
    )
    (while (and (not (menu-correcto ?posible-menu ?cal)) (< ?a 12000)) do
        (bind ?posible-menu (generar-combinacion ?des-plato ?pri-plato ?seg-plato ?pos-plato))
        (bind ?a (+ ?a 1))
    )

    (if (eq ?a 6000) then (printout t crlf "El algoritmo no ha podido encontrarle un menú personalizado." crlf)
        (printout t crlf "Le aconsejamos que visite a un nutricionista para obtener ayuda." crlf)
    else

    (modify ?usado (plato ?posible-menu))
    (modify ?ing-usado (ingrediente (lista-ingredientes-principales ?posible-menu)))

    (assert (Menu-diario (dia ?d) (menu ?posible-menu)))
	(assert (Actualiza ?d))
	(retract ?emp)
    )
)

(defrule actualizar-condiciones
   ?act <- (Actualiza ?d)
   (test (> ?d 0))
   ?p <- (Persona (calorias ?calo))
    =>
     (if(< 1 ?d) then (assert (empieza (- ?d 1))) else (focus RESPUESTA))
    (retract ?act)
)

(defmodule RESPUESTA (import PREGUNTAS ?ALL)(import ASIGNACION ?ALL)(export ?ALL))


(defrule print-menu-dia
    ?dia <- (Menu-diario (dia ?d) (menu $?lista))
    ?pe <- (Persona (calorias ?cal))
    =>
    (printout t crlf "MENU DEL DIA " ?d crlf)

    (bind ?nutrientes (contar-nutrientes ?lista))

    (printout t crlf "Plato del desayuno: " (send (nth$ 1 ?lista) get-nombre) " y " (send (nth$ 2 ?lista) get-nombre) )
    (printout t crlf "Plato de la comida: " (send (nth$ 3 ?lista) get-nombre) " y " (send (nth$ 4 ?lista) get-nombre) )
    (printout t crlf "Postre de la comida: " (send (nth$ 5 ?lista) get-nombre) )
    (printout t crlf "Plato de la cena: " (send (nth$ 6 ?lista) get-nombre)  " y " (send (nth$ 7 ?lista) get-nombre) )
    (printout t crlf "Postre de la cena: " (send (nth$ 8 ?lista) get-nombre) )
    (printout t crlf crlf "Para su edad y nivel de actividad física se recomienda que consuma " ?cal " calorias.")
    (printout t crlf "Hoy ha consumido " (nth$ 1 ?nutrientes) " calorias en su menú.")
    (printout t crlf "De las cuales " (* 4(nth$ 2 ?nutrientes)) " son hidratos de carbono, " (* 9 (nth$ 3 ?nutrientes))
                        " son grasas, " (* 4 (nth$ 4 ?nutrientes)) " son proteinas y " (nth$ 5 ?nutrientes) " gramos de fibra" crlf)

)
