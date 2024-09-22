;;; ---------------------------------------------------------
;;; ontologiav2.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologiav2.ttl
;;; :Date 17/05/2023 22:17:54


(defclass ingrediente
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot contiene
        (type INSTANCE)
        (create-accessor read-write))
    (multislot esta_en
        (type INSTANCE)
        (create-accessor read-write))
    (slot nombre
        (type STRING)
        (create-accessor read-write))
    (multislot disponible_en
        (type STRING)
        (default "siempre")
        (create-accessor read-write)
    )
)


(defclass vitamina
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass mineral
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass plato
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
    (slot cocinado
        (type INSTANCE)
        (create-accessor read-write))
    (multislot formado_por
        (type INSTANCE)
        (create-accessor read-write))
    (multislot tipoComida
        (type STRING)
        (create-accessor read-write))
    (slot contundente
        (type STRING)
        (default "false")
        (create-accessor read-write))
    (slot calorias
        (type INTEGER)
        (create-accessor read-write))
    (slot cantidad_grasas
         (type FLOAT)
         (default 0.0)
        (create-accessor read-write))
    (slot cantidad_hidratos
         (type FLOAT)
         (default 0.0)
        (create-accessor read-write))
    (slot cantidad_proteinas
        (type FLOAT)
        (default 0.0)
        (create-accessor read-write))
    (slot cantidad_fibras
         (type FLOAT)
         (default 0.0)
        (create-accessor read-write))
)


(defclass forma_de_coccion
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)


(definstances instances

    ;;;***********
    ;;; MINERALES
    ;;;***********

    ([hierro] of mineral
        (nombre "Hierro")
    )

    ([potasio] of mineral
        (nombre "Potasio")
    )

    ([calcio] of mineral
        (nombre "Calcio")
    )

    ;;;***********
    ;;; VITAMINAS
    ;;;***********

    ([vitaminaB] of vitamina
        (nombre "VitaminaB")
    )

    ([vitaminaC] of vitamina
        (nombre "VitaminaC")
    )

    ([vitaminaD] of vitamina
        (nombre "VitaminaD")
    )

    ;;;**************
    ;;; INGREDIENTES
    ;;;**************

    ([azucar] of ingrediente
        (esta_en [tartadefrutosrojos] [natillas] [tostadaconmermelada])
        (nombre "Azucar")
    )

	([pollo] of ingrediente
        (contiene [hierro] [potasio] [vitaminaB])
        (esta_en [polloalaplancha] [paellacarne] [croquetas] [pollofrito])
        (nombre "Pollo")
    )

    ([pasta] of ingrediente
        (contiene [vitaminaD] [hierro] [potasio])
        (esta_en [pastaconpesto] [pastabolonesa] [fideua] [sopa])
        (nombre "Pasta")
    )

    ([queso] of ingrediente
        (contiene [vitaminaD] [calcio])
        (esta_en [berenjenasalaparmesana] [pastaconpesto] [pastabolonesa] [risotto])
        (nombre "Queso")
    )

    ([naranja] of ingrediente
        (contiene [vitaminaC] [potasio])
        (esta_en [zumodeNaranja] [naranjaPlato] [naranjaDesayuno])
        (nombre "Naranja")
        (disponible_en "otoño" "primavera" "verano")
    )

    ([salmon] of ingrediente
        (contiene [vitaminaC] [potasio])
        (esta_en [salmonalaplancha])
        (nombre "Salmon")
    )

    ([tomate] of ingrediente
        (contiene [potasio])
        (esta_en [berenjenasalaparmesana] [pastabolonesa] [ensalada] [estofadodecerdo] [sopadetomate] [arrozcubana] [ensaladadetomate] [gazpacho])
        (nombre "Tomate")
    )

    ([lechuga] of ingrediente
        (esta_en [ensalada])
        (nombre "Lechuga")
    )

    ([rape] of ingrediente
        (contiene [vitaminaB])
        (esta_en [rapealaplancha] [rapealhorno])
        (nombre "Rape")
    )

    ([merluza] of ingrediente
        (contiene [vitaminaB])
        (esta_en [merluzaalaplancha])
        (nombre "Merluza")
    )

    ([bacalao] of ingrediente
        (contiene [vitaminaB])
        (esta_en [bacalaoplancha] [croquetasbacalao])
        (nombre "Bacalao")
    )

    ([patata] of ingrediente
        (esta_en [tortilladepatata] [estofadodecerdo] [lentejasguisadas] [garbanzosguisados] [rapealhorno])
        (nombre "Patata")
    )

    ([cerdo] of ingrediente
        (contiene [vitaminaB])
        (esta_en [estofadodecerdo])
        (nombre "Cerdo")
    )

    ([cafe] of ingrediente
        (esta_en [cafeconleche])
        (nombre "Cafe")
    )

    ([leche] of ingrediente
        (contiene [calcio] [vitaminaD])
        (esta_en [cafeconleche] [yogurnaturalPlato] [natillas])
        (nombre "Leche")
    )

    ([pan] of ingrediente
        (contiene [vitaminaB])
        (esta_en [tostadaconaguacate] [tostadaconmermelada] [corquetas] [pollofrito] [croquetassetas] [croquetasbacalao])
        (nombre "Pan")
    )

    ([aguacate] of ingrediente
        (esta_en [tostadaconaguacate])
        (nombre "Aguacate")
    )

    ([manzana] of ingrediente
        (esta_en [manzanaPlato])
        (nombre "Manzana")
    )

    ([sandia] of ingrediente
        (esta_en [sandiaPlato])
        (nombre "Sandia")
        (disponible_en "verano")
    )

    ([melon] of ingrediente
        (contiene [vitaminaC])
        (esta_en [melonPlato])
        (nombre "Melon")
        (disponible_en "verano")
    )

    ([mermelada] of ingrediente
        (esta_en [tostadaconmermelada] [tartadefrutosrojos])
        (nombre "Mermelada")
    )

    ([platano] of ingrediente
        (esta_en [platanoPlato] [arrozcubana])
        (nombre "Platano")
    )

    ([huevo] of ingrediente
        (contiene [hierro] [vitaminaB] [vitaminaC])
        (esta_en [tortilla] [tortilladepatata] [huevoduroconatun] [natillas] [arrozcubana] [croquetas] [croquetassetas] [croquetasbacalao] [pollofrito])
        (nombre "Huevo")
    )

    ([guisante] of ingrediente
        (esta_en [sopadeguisantes])
        (nombre "Guisante")
    )

    ([ternera] of ingrediente
        (contiene [hierro] [vitaminaB])
        (esta_en [pastabolonesa] [bistec] [paellacarne])
        (nombre "Ternera")
    )

    ([lentejas] of ingrediente
        (contiene [hierro] [potasio])
        (esta_en [ensaladadelentejas] [lentejasguisadas])
        (nombre "Lentejas")
    )

    ([garbanzos] of ingrediente
        (contiene [hierro])
        (esta_en [hummus] [garbanzosguisados])
        (nombre "Garbanzos")
    )

    ([gamba] of ingrediente
        (contiene [vitaminaB])
        (esta_en [fideua] [paellapescado] [gambasalaplancha])
        (nombre "Gamba")
    )

    ([berenjena] of ingrediente
        (esta_en [berenjenasalaparmesana])
        (nombre "Berenjena")
        (disponible_en "verano")
    )

    ([atun] of ingrediente
        (contiene [vitaminaD])
        (esta_en [huevoduroconatun] [ensalada] [ensaladadetomate])
        (nombre "Atun")
    )

    ([pera] of ingrediente
        (esta_en [peraPlato])
        (nombre "Pera")
    )

    ([mandarina] of ingrediente
        (esta_en [mandarinaPlato])
        (nombre "Mandarina")
        (disponible_en "otoño" "invierno")
    )

    ([melocoton] of ingrediente
        (esta_en [melocotonPlato])
        (nombre "Melocoton")
        (disponible_en "verano")
    )

    ([harina] of ingrediente
        (esta_en [tartadefrutosrojos])
        (nombre "Harina")
    )

    ([mantequilla] of ingrediente
        (contiene [vitaminaD])
        (esta_en [tartadefrutosrojos])
        (nombre "Mantequilla")
    )

    ([arroz] of ingrediente
        (esta_en [arrozcubana] [paellacarne] [paellapescado] [risotto])
        (nombre "Arroz")
    )

    ([setas] of ingrediente
        (esta_en [risotto] [croquetassetas])
        (nombre "Setas")
        (disponible_en "otoño")
    )

    ([zanahoria] of ingrediente
        (esta_en [lentejasguisadas] [garbanzosguisados] [ensalada] [sopa])
        (nombre "Zanahoria")
    )



    ;;;*******************
    ;;; FORMAS DE COCCION
    ;;;*******************

    ([hervido] of forma_de_coccion
        (nombre "Hervido")
    )

    ([frito] of forma_de_coccion
        (nombre "Frito")
    )

    ([horno] of forma_de_coccion
        (nombre "Horno")
    )

    ([plancha] of forma_de_coccion
        (nombre "Plancha")
    )
    ([sinCoccion] of forma_de_coccion
        (nombre "SinCoccion")
    )

    ;;;********
    ;;; PLATOS
    ;;;********

    ;;; PLATOS DE DESAYUNO

    ([cafeconleche] of plato
        (nombre "Café con leche")
        (formado_por [cafe] [leche])
        (tipoComida "desayuno")
        (cocinado [sinCoccion])
        (calorias 33)
        (cantidad_grasas 1.3)
        (cantidad_hidratos 3)
        (cantidad_proteinas 2.3)
    )

    ([yogurnaturalPlato] of plato
        (nombre "Yogur natural")
        (formado_por [leche])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 107)
        (cantidad_grasas 2.6)
        (cantidad_hidratos 12)
        (cantidad_proteinas 8.9)
    )

    ([tostadaconaguacate] of plato
        (nombre "Tostada con aguacate")
        (formado_por [pan] [aguacate])
        (tipoComida "desayuno")
        (cocinado [sinCoccion])
        (calorias 210)
        (cantidad_grasas 13)
        (cantidad_hidratos 21)
        (cantidad_proteinas 5.5)
        (cantidad_fibras 8)
    )

    ([zumodenaranja] of plato
        (nombre "Zumo natural de naranja")
        (formado_por [naranja])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 112)
        (cantidad_grasas 0.5)
        (cantidad_hidratos 26)
        (cantidad_proteinas 1.7)
        (cantidad_fibras 0.5)
    )

    ([manzanaPlato] of plato
        (nombre "Manzana")
        (formado_por [manzana])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 65)
        (cantidad_grasas 0.2)
        (cantidad_hidratos 17)
        (cantidad_proteinas 0.3)
        (cantidad_fibras 3)
    )

    ([sandiaPlato] of plato
        (nombre "Trozos de sandía")
        (formado_por [sandia])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 46)
        (cantidad_grasas 0.2)
        (cantidad_hidratos 12)
        (cantidad_proteinas 0.9)
        (cantidad_fibras 0.6)
    )

    ([melonPlato] of plato
        (nombre "Trozos de melón")
        (formado_por [melon])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 48)
        (cantidad_grasas 0.2)
        (cantidad_hidratos 11)
        (cantidad_proteinas 1.9)
        (cantidad_fibras 1.5)
    )

    ([tostadaconmermelada] of plato
        (nombre "Tostada con mermelada")
        (formado_por [pan] [mermelada] [azucar])
        (tipoComida "desayuno")
        (cocinado [sinCoccion])
        (calorias 131)
        (cantidad_grasas 1)
        (cantidad_hidratos 28)
        (cantidad_proteinas 2.6)
        (cantidad_fibras 1)
    )

    ([naranjaPlato] of plato
        (nombre "Naranja")
        (formado_por [naranja])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 85)
        (cantidad_grasas 0.2)
        (cantidad_hidratos 21)
        (cantidad_proteinas 1.7)
        (cantidad_fibras 4.3)
    )

    ([platanoPlato] of plato
        (nombre "Plátano")
        (formado_por [platano])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 200)
        (cantidad_grasas 0.7)
        (cantidad_hidratos 51)
        (cantidad_proteinas 2.5)
        (cantidad_fibras 5.9)
    )

    ([tortilla] of plato
        (nombre "Tortilla a la francesa")
        (formado_por [huevo])
        (tipoComida "desayuno" "segundo")
        (cocinado [plancha])
        (calorias 23)
        (cantidad_grasas 1.7)
        (cantidad_hidratos 0.1)
        (cantidad_proteinas 1.6)
    )


    ;;; PLATOS PRINCIPALES


    ([sopadetomate] of plato
        (nombre "Sopa de tomate")
        (formado_por [tomate])
        (tipoComida "primero")
        (cocinado [hervido])
        (calorias 170)
        (cantidad_grasas 1.1)
        (cantidad_hidratos 36)
        (cantidad_proteinas 3.5)
        (cantidad_fibras 2.6)
    )

    ([sopadeguisantes] of plato
        (nombre "Sopa de guisantes")
        (formado_por [guisante])
        (tipoComida "primero")
        (cocinado [hervido])
        (calorias 158)
        (cantidad_grasas 2.8)
        (cantidad_hidratos 26)
        (cantidad_proteinas 8.3)
        (cantidad_fibras 4.9)
    )

    ([pastaconpesto] of plato
        (nombre "Pasta con pesto")
        (formado_por [pasta] [queso])
        (tipoComida "primero")
        (contundente "true")
        (cocinado [hervido])
        (calorias 735)
        (cantidad_grasas 31)
        (cantidad_hidratos 91)
        (cantidad_proteinas 23)
        (cantidad_fibras 5.7)
    )

    ([pastabolonesa] of plato
        (nombre "Pasta boloñesa")
        (formado_por [pasta] [tomate] [queso] [ternera])
        (tipoComida "primero")
        (contundente "true")
        (cocinado [hervido])
        (calorias 333)
        (cantidad_grasas 11)
        (cantidad_hidratos 42)
        (cantidad_proteinas 17.5)
        (cantidad_fibras 5.5)
    )

    ([ensalada] of plato
        (nombre "Ensalada")
        (formado_por [atun] [zanahoria] [lechuga] [tomate])
        (tipoComida "primero")
        (cocinado [sinCoccion])
        (calorias 148)
        (cantidad_grasas 13)
        (cantidad_hidratos 6.4)
        (cantidad_proteinas 3.8)
        (cantidad_fibras 2.8)
    )

    ([ensaladadelentejas] of plato
        (nombre "Ensalada de lentejas")
        (formado_por [lentejas])
        (tipoComida "primero")
        (cocinado [sinCoccion])
        (calorias 255)
        (cantidad_grasas 10)
        (cantidad_hidratos 31)
        (cantidad_proteinas 12)
        (cantidad_fibras 11)
    )

    ([sopa] of plato
        (nombre "Sopa")
        (formado_por [pasta] [zanahoria])
        (tipoComida "primero")
        (cocinado [hervido])
        (calorias 221)
        (cantidad_grasas 11)
        (cantidad_hidratos 25)
        (cantidad_proteinas 6.5)
        (cantidad_fibras 4.5)
    )

    ([ensaladadetomate] of plato
        (nombre "Ensalada de tomate")
        (formado_por [tomate] [atun])
        (tipoComida "primero")
        (cocinado [sinCoccion])
        (calorias 225)
        (cantidad_grasas 22)
        (cantidad_hidratos 7.6)
        (cantidad_proteinas 0.9)
        (cantidad_fibras 1.2)
    )

    ([gazpacho] of plato
        (nombre "Gazpacho")
        (formado_por [tomate])
        (tipoComida "primero")
        (cocinado [sinCoccion])
        (calorias 243)
        (cantidad_grasas 17)
        (cantidad_hidratos 20)
        (cantidad_proteinas 4.1)
        (cantidad_fibras 4.4)
    )

    ([hummus] of plato
        (nombre "Hummus de garbanzos")
        (formado_por [garbanzos])
        (tipoComida "primero")
        (cocinado [sinCoccion])
        (calorias 100)
        (cantidad_grasas 5.8)
        (cantidad_hidratos 8.6)
        (cantidad_proteinas 4.7)
        (cantidad_fibras 3.6)
    )

    ([bistec] of plato
        (nombre "Bistec de ternera")
        (formado_por [ternera])
        (tipoComida "segundo")
        (cocinado [plancha])
        (calorias 307)
        (cantidad_grasas 20.5)
        (cantidad_proteinas 29)
    )

    ([polloalaplancha] of plato
        (nombre "Pollo a la plancha")
        (formado_por [pollo])
        (tipoComida "segundo")
        (cocinado [plancha])
        (calorias 142)
        (cantidad_grasas 3.3)
        (cantidad_proteinas 28.5)
    )

    ([estofadodecerdo] of plato
        (nombre "Estofado de cerdo")
        (formado_por [cerdo] [tomate] [patata])
        (tipoComida "segundo")
        (contundente "true")
        (cocinado [hervido])
        (calorias 604)
        (cantidad_grasas 35)
        (cantidad_hidratos 35)
        (cantidad_proteinas 38)
        (cantidad_fibras 5.4)
    )

    ([salmonalaplancha] of plato
        (nombre "Salmón a la plancha")
        (formado_por [salmon])
        (tipoComida "segundo")
        (cocinado [plancha])
        (calorias 468)
        (cantidad_grasas 28)
        (cantidad_proteinas 50)
    )

    ([merluzaalaplancha] of plato
        (nombre "Merluza a la plancha")
        (formado_por [merluza])
        (tipoComida "segundo")
        (cocinado [plancha])
        (calorias 468)
        (cantidad_grasas 28)
        (cantidad_proteinas 50)
    )

    ([rapealaplancha] of plato
        (nombre "Rape a la plancha")
        (formado_por [rape])
        (tipoComida "segundo")
        (cocinado [plancha])
        (calorias 220)
        (cantidad_grasas 3)
        (cantidad_hidratos 35)
        (cantidad_proteinas 42.7)
        (cantidad_fibras 0)
    )

    ([bacalaoplancha] of plato
        (nombre "Bacalao a la plancha")
        (formado_por [bacalao])
        (tipoComida "segundo")
        (cocinado [plancha])
        (calorias 246)
        (cantidad_grasas 10)
        (cantidad_hidratos 3)
        (cantidad_proteinas 53.3)
        (cantidad_fibras 0)
    )

    ([rapealhorno] of plato
        (nombre "Rape al horno con patatas")
        (formado_por [rape] [patata] )
        (tipoComida "segundo")
        (contundente "true")
        (cocinado [horno])
        (calorias 370)
        (cantidad_grasas 14)
        (cantidad_hidratos 3)
        (cantidad_proteinas 53)
        (cantidad_fibras 1.2)
    )

    ([fideua] of plato
        (nombre "Fideuá de pescado al horno")
        (formado_por [pasta] [gamba])
        (tipoComida "primero" "segundo")
        (contundente "true")
        (cocinado [horno])
        (calorias 538)
        (cantidad_grasas 22)
        (cantidad_hidratos 56)
        (cantidad_proteinas 28)
        (cantidad_fibras 2.4)
    )

    ([berenjenasalaparmesana] of plato
        (nombre "Berenjenas a la parmesana")
        (formado_por [tomate] [berenjena] [queso])
        (tipoComida "primero")
        (contundente "true")
        (cocinado [horno])
        (calorias 302)
        (cantidad_grasas 19)
        (cantidad_hidratos 20)
        (cantidad_proteinas 14)
        (cantidad_fibras 3)
    )

    ([tortilladepatata] of plato
        (nombre "Tortilla de patata")
        (formado_por [patata] [huevo])
        (tipoComida "primero" "segundo")
        (contundente "true")
        (cocinado [frito])
        (calorias 230)
        (cantidad_grasas 13)
        (cantidad_hidratos 20)
        (cantidad_proteinas 7)
        (cantidad_fibras 2.9)
    )

    ([huevoduroconatun] of plato
        (nombre "Huevo duro con atún")
        (formado_por [huevo] [atun])
        (tipoComida "primero")
        (cocinado [hervido])
        (calorias 97)
        (cantidad_grasas 3.6)
        (cantidad_hidratos 0.2)
        (cantidad_proteinas 19.6)
    )

    ([arrozcubana] of plato
        (nombre "Arroz a la cubana")
        (formado_por [arroz] [tomate] [huevo] [platano])
        (tipoComida "primero")
        (contundente "true")
        (cocinado [hervido])
        (calorias 329)
        (cantidad_grasas 9.5)
        (cantidad_hidratos 51.5)
        (cantidad_proteinas 9.5)
        (cantidad_fibras 2.6)
    )

    ([paellacarne] of plato
        (nombre "Paella de carne")
        (formado_por [arroz] [ternera] [pollo])
        (tipoComida "segundo")
        (contundente "true")
        (cocinado [hervido])
        (calorias 442)
        (cantidad_grasas 22)
        (cantidad_hidratos 34)
        (cantidad_proteinas 25)
        (cantidad_fibras 1)
    )

    ([paellapescado] of plato
        (nombre "Paella de marisco")
        (formado_por [arroz] [gamba])
        (tipoComida "segundo")
        (contundente "true")
        (cocinado [hervido])
        (calorias 425)
        (cantidad_grasas 8.8)
        (cantidad_hidratos 62.5)
        (cantidad_proteinas 22.5)
        (cantidad_fibras 2.5)
    )

    ([risotto] of plato
        (nombre "Risotto de setas")
        (formado_por [arroz] [queso] [setas])
        (tipoComida "primero")
        (contundente "true")
        (cocinado [hervido])
        (calorias 413)
        (cantidad_grasas 13)
        (cantidad_hidratos 54)
        (cantidad_proteinas 14)
        (cantidad_fibras 1.5)
    )

    ([gambasalaplancha] of plato
        (nombre "Gambas a la plancha")
        (formado_por [gamba])
        (tipoComida "primero" "segundo")
        (cocinado [plancha])
        (calorias 202)
        (cantidad_grasas 2.8)
        (cantidad_hidratos 2.7)
        (cantidad_proteinas 38)
        (cantidad_fibras 0)
    )

    ([lentejasguisadas] of plato
        (nombre "Lentejas guisadas")
        (formado_por [lentejas] [patata] [zanahoria])
        (tipoComida "segundo")
        (contundente "true")
        (cocinado [hervido])
        (calorias 241)
        (cantidad_grasas 1)
        (cantidad_hidratos 45)
        (cantidad_proteinas 16)
        (cantidad_fibras 16)
    )

    ([garbanzosguisados] of plato
        (nombre "Garbanzos guisados")
        (formado_por [garbanzos] [patata] [zanahoria])
        (tipoComida "segundo")
        (contundente "true")
        (cocinado [hervido])
        (calorias 269)
        (cantidad_grasas 4.2)
        (cantidad_hidratos 45)
        (cantidad_proteinas 15)
        (cantidad_fibras 12)
    )

    ([croquetas] of plato
        (nombre "3 croquetas de pollo")
        (formado_por [pollo] [huevo] [pan])
        (tipoComida "primero" "segundo")
        (cocinado [frito])
        (calorias 462)
        (cantidad_grasas 22.2)
        (cantidad_hidratos 39)
        (cantidad_proteinas 26.2)
        (cantidad_fibras 2.4)
    )

    ([croquetassetas] of plato
        (nombre "3 croquetas de boletus")
        (formado_por [setas] [huevo] [pan])
        (tipoComida "primero" "segundo")
        (cocinado [frito])
        (calorias 320)
        (cantidad_grasas 12)
        (cantidad_hidratos 22.4)
        (cantidad_proteinas 6.4)
        (cantidad_fibras 3.6)
    )

    ([croquetasbacalao] of plato
        (nombre "3 croquetas de balacao")
        (formado_por [bacalao] [huevo] [pan])
        (tipoComida "primero" "segundo")
        (cocinado [frito])
        (calorias 303)
        (cantidad_grasas 15)
        (cantidad_hidratos 25.2)
        (cantidad_proteinas 16.5)
        (cantidad_fibras 1.7)
    )

    ([pollofrito] of plato
        (nombre "Fingers de pollo")
        (formado_por [pollo] [huevo] [pan])
        (tipoComida "segundo")
        (cocinado [frito])
        (calorias 527)
        (cantidad_grasas 29.4)
        (cantidad_hidratos 6.2)
        (cantidad_proteinas 56)
        (cantidad_fibras 0.2)
    )

    ;;; PLATOS DE POSTRE


    ([peraPlato] of plato
        (nombre "Pera")
        (formado_por [pera])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 101)
        (cantidad_grasas 0.3)
        (cantidad_hidratos 27)
        (cantidad_proteinas 0.6)
        (cantidad_fibras 5.5)
    )

    ([mandarinaPlato] of plato
        (nombre "Mandarina")
        (formado_por [mandarina])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 70)
        (cantidad_grasas 0.1)
        (cantidad_hidratos 8.9)
        (cantidad_proteinas 0.6)
        (cantidad_fibras 1.3)
    )

    ([melocotonPlato] of plato
        (nombre "Melocotón")
        (formado_por [melocoton])
        (tipoComida "desayuno" "postre")
        (cocinado [sinCoccion])
        (calorias 65)
        (cantidad_grasas 0.4)
        (cantidad_hidratos 16)
        (cantidad_proteinas 1.4)
        (cantidad_fibras 2.3)
    )

    ([natillas] of plato
        (nombre "Natillas")
        (formado_por [leche] [huevo] [azucar])
        (tipoComida "desayuno" "postre")
        (contundente "true")
        (cocinado [sinCoccion])
        (calorias 344)
        (cantidad_grasas 11.2)
        (cantidad_hidratos 50)
        (cantidad_proteinas 11.2)
    )

    ([tartadefrutosrojos] of plato
        (nombre "Tarta de frutos rojos")
        (formado_por [mermelada] [harina] [mantequilla] [azucar])
        (tipoComida "desayuno" "postre")
        (contundente "true")
        (cocinado [horno])
        (calorias 370)
        (cantidad_grasas 14)
        (cantidad_hidratos 60)
        (cantidad_proteinas 4.1)
        (cantidad_fibras 5.5)
    )
    
)
