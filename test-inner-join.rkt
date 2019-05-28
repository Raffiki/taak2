#!r6rs

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d file constants)
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (Taak2 table) tbl:)
        (prefix (Taak2 schema) schema:)
        (prefix (Taak2 node) node:)
        (prefix (Taak2 database) db:))

;                       naam        afstand   aard  middel  omlooptijd  ontdekkerId
;                                   tot zon  massa  lijn    aardjaar    
(define mercurius (list "Mercurius" 0.3871   0.053   4840   0.241      1))
(define venus     (list "Venus"     0.7233   0.815  12200   0.615     3))
(define aarde     (list "Aarde"     1.0000   1.000  12756   1.000      2))
(define mars      (list "Mars"      1.5237   0.109   6790   1.881       3))
(define jupiter   (list "Jupiter"   5.2028 317.900 142800  11.862      1))
(define saturnus  (list "Saturnus"  9.5388  95.100 119300  29.458       2))
(define uranus    (list "Uranus"   19.1819  14.500  47100  84.013       3))
(define neptunus  (list "Neptunus" 30.0578  17.500  44800 164.793       3))
(define pluto     (list "Pluto"    39.2975   1.000   5000 248.430       2))
(define pluto2     (list "Pluto2"    39.2975   1.000   5000 248.430     4))
(define pluto3     (list "Pluto3"    39.2975   1.000   5000 248.430      2))
(define pluto4     (list "Pluto4"    39.2975   1.000   5000 248.430      4))
(define pluto5     (list "Pluto5"    39.2975   1.000   5000 248.430      1))


(define cassini (list 1 "cassini" ))
(define hubble (list 2 "hubble" ))
(define herschel (list 3 "Herschel" ))
(define asdf (list 4 "asdf" ))


; Maak een schijf aan
(define d (disk:new "MyDisk2"))

; Maak een bestandssysteem aan
(fs:format! d)

; Maak een schema aan (9 bytes voor de naam, 3 bytes voor de middellijn)
(define :naam: 0)
(define :massa: 2)
(define planeten-schema '((string 9)
                          (decimal)
                          (decimal)
                          (natural 3)
                          (decimal)
                          (natural 2)))

(define ontdekker-schema '(( natural 3)
                           (string 9)))

(define zonnestelsel (db:new d "zonnestelsel"))
(define zonnestelsel-planeten-table (db:create-table zonnestelsel "planeten" planeten-schema))
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table mercurius)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table venus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table aarde)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table mars)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table jupiter)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table saturnus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table uranus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table neptunus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table pluto)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table pluto2)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table pluto3)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table pluto4)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table pluto5)

(define ontdekker-table (db:create-table zonnestelsel "ontdekker" ontdekker-schema))
(db:insert-into-table! zonnestelsel ontdekker-table cassini)
(db:insert-into-table! zonnestelsel ontdekker-table hubble)
(db:insert-into-table! zonnestelsel ontdekker-table herschel)
(db:insert-into-table! zonnestelsel ontdekker-table asdf)

(define results (db:select*-from/inner-join zonnestelsel zonnestelsel-planeten-table 5 ontdekker-table 0))

(display "result:")
(newline)
(newline)

(let loop ((res results))
  (unless (null? res)
    (display (car res)) (newline)
    (loop (cdr res))))
