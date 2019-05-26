#!r6rs

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d file constants)
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d db table fixed-size-slots table) tbl:)
        (prefix (a-d db table fixed-size-slots schema) schema:)
        (prefix (a-d db table fixed-size-slots node) node:)
        (prefix (Hoofdstuk11 b-tree+) btree:)
        (prefix (Hoofdstuk11 database+) db:))

;                       naam        afstand   aard  middel  omlooptijd  rotatietijd
;                                   tot zon  massa  lijn    aardjaar    aarddag
(define mercurius (list "Mercurius" 0.3871   0.053   4840   0.241      +58.79))
(define venus     (list "Venus"     0.7233   0.815  12200   0.615     -243.68))
(define aarde     (list "Aarde"     1.0000   1.000  12756   1.000       +1.00))
(define mars      (list "Mars"      1.5237   0.109   6790   1.881       +1.03))
(define jupiter   (list "Jupiter"   5.2028 317.900 142800  11.862       +0.41))
(define saturnus  (list "Saturnus"  9.5388  95.100 119300  29.458       +0.43))
(define uranus    (list "Uranus"   19.1819  14.500  47100  84.013       -0.45))
(define neptunus  (list "Neptunus" 30.0578  17.500  44800 164.793       +0.63))
(define pluto     (list "Pluto"    39.2975   1.000   5000 248.430       +0.26))

; Maak een schijf aan
(define d (disk:new "MyDisk"))

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
                          (decimal)))

; Maak een tabel aan
(define planeten-table (tbl:new d "TblPlaneten" planeten-schema)) 

; Voeg alle planeten toe aan de tabel
(tbl:insert! planeten-table mercurius)
(tbl:insert! planeten-table venus)
(tbl:insert! planeten-table aarde)
(tbl:insert! planeten-table mars)
(tbl:insert! planeten-table jupiter)
(tbl:insert! planeten-table saturnus)
(tbl:insert! planeten-table uranus)
(tbl:insert! planeten-table neptunus)
(tbl:insert! planeten-table pluto)

; Hulpfunctie die een procedure uitvoert voor elk tupel in een tabel
(define (tbl:foreach table proc)
  (tbl:set-current-to-first! table)
  (let loop ()
    (let ((tuple (tbl:peek table)))
      (unless (eq? tuple no-current) ; no-current is gedefinieerd in de (a-d file constants) library
        (proc tuple (tbl:current table)) ; Voer de procedure uit met het tupel en haar record ID als parameters
        (tbl:set-current-to-next! table)
        (loop)))))

; Toon alle tupels in onze tabel
(tbl:foreach planeten-table (lambda (tuple rcid)
                              (display tuple)
                              (newline)))

; Maak een nieuwe index aan en vul deze met de record IDs van de tupels in onze tabel
(define ktyp (schema:type (tbl:schema planeten-table) :naam:))
(define ksiz (schema:size (tbl:schema planeten-table) :naam:))
(define planeten-naam-index (btree:new d "IdxNaamPlaneten" ktyp ksiz))
(tbl:foreach planeten-table (lambda (tuple rcid)
                              (btree:insert! planeten-naam-index (list-ref tuple :naam:) rcid)))

; Definieer een hulpfunctie die een procedure uitvoert voor elk tupel waarnaar een index verwijst
(define (btree:foreach index table proc)
  (btree:set-current-to-first! index)
  (let loop ()
    (let ((key-rcid (btree:peek index)))
      (unless (eq? key-rcid no-current)
        (let ((key (car key-rcid))
              (rcid (cdr key-rcid)))
          (let ((bptr (car rcid))
                (slot (cdr rcid)))
            (let* ((node (node:read (tbl:schema table) bptr))
                   (tuple (node:record node slot)))
              (proc tuple rcid))))
        (btree:set-current-to-next! index)
        (loop)))))

; Toon alle tupels waarnaar onze index verwijst
(newline)
(btree:foreach planeten-naam-index planeten-table (lambda (tuple rcid)
                                                    (display tuple)
                                                    (newline)))

; Maak een aangepast b-tree ADT met een set-current-to-last! procedure (oef. 11.1.a)
; -> Cf. b-tree+.rkt

; Test de set-current-to-last! procedure
(btree:set-current-to-last! planeten-naam-index)
(newline)
(display (btree:peek planeten-naam-index)) (newline)

; Maak een nieuwe database aan met één tabel en index
(define zonnestelsel (db:new d "zonnestelsel"))
(define zonnestelsel-planeten-table (db:create-table zonnestelsel "planeten" planeten-schema))
(define zonnestelsel-planeten-naam-index (db:create-index! zonnestelsel zonnestelsel-planeten-table "planeten-naam" :naam:))
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table mercurius)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table venus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table aarde)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table mars)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table jupiter)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table saturnus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table uranus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table neptunus)
(db:insert-into-table! zonnestelsel zonnestelsel-planeten-table pluto)

; Maak een aangepast database ADT met een select-from/min en een select-from/max procedure (oef. 11.1.b)
; -> Cf. database+.rkt

; Test de select-from/min en select-from/max procedures
(newline)
(display "min naam  : ") (display (db:select-from/min zonnestelsel zonnestelsel-planeten-table :naam:)) (newline)
(display "min massa : ") (display (db:select-from/min zonnestelsel zonnestelsel-planeten-table :massa:)) (newline)
(display "max naam  : ") (display (db:select-from/max zonnestelsel zonnestelsel-planeten-table :naam:)) (newline)
(display "max massa : ") (display (db:select-from/max zonnestelsel zonnestelsel-planeten-table :massa:)) (newline)
