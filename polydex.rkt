#! /usr/bin/racket
#lang racket
;(require racket/require
;  (path-up "Projects/source-libs/tools.rkt"))
(require racket/require
  (path-up "source-libs/func-radix-tree-free.rkt"))

(require bitsyntax)
;[require plot]
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [bv-ref v i] [bit-string-ref v i]]
[define [bv-l v] [bit-string-length v]]

[define [bv-tl v] [sub-bit-string v 0  [/ [bv-l v] 2]]]
[define [bv-tr v] [bit-string-drop v [/ [bv-l v] 2]]]

[define [eblg a] [sub1 [integer-length a]]]
[define [p2 n] [expt 2 n]]
[define [mod2 x] [modulo x 2]]

[define [bv-xor a b]
  [int->bv
    [bv-xor-rec [bit-string->unsigned-integer a #t] [bit-string->unsigned-integer b #t]]
    [bv-l a]]]

[define [bv-xor-rec ai bi]
  [if [and [equal? ai 0] [equal? bi 0]] 0
    [let [[aib [mod2 ai]] [bib [mod2 bi]]]
      [+ [* 2 [bv-xor-rec [/ [- ai aib] 2] [/ [- bi bib] 2]]] [mod2 [+ aib bib]]]]]]

[define [bv-dist v1 v2 sp]
  [for/sum [[i [in-range sp [bv-l v1]]]]
    [mod2 [+ [bv-ref v1 i] [bv-ref v2 i]]]]]

[define [bv-par v] [mod2 [for/sum [[i [in-range 0 [bv-l v]]]] [bv-ref v i]]]]


[define [cerm v lc ll d]
  [if d [display  [format "~a:(~a,~a)\n" [bv->char-str v] lc ll]] [void]]
  [if [equal? lc -1]
    [int->bv 0 [p2 ll]]
    [if [equal? lc ll] v
      [let* [[L [bv-tl v]]
             [Lp [cerm L [+ lc] [sub1 ll]d]]
             [R [bv-tr v]]
             [Rp [cerm R [+ lc] [sub1 ll]d]]
             [D [bv-xor L R]]
             [Sdp [cerm D [sub1 lc] [sub1 ll]d]]
             [M [bv-xor Sdp D]]
             [LM [bv-xor M [bv-xor L Lp]]]
             [RM [bv-xor M [bv-xor R Rp]]]
             [LMdp [cerm LM [sub1 lc] [sub1 ll]d]]
             [RMdp [cerm RM [sub1 lc] [sub1 ll]d]]
             [c1 [bit-string-append Lp [bv-xor Lp [bv-xor Sdp RMdp]]]]
             [c2 [bit-string-append [bv-xor Rp [bv-xor Sdp LMdp]] Rp]]
             [d1 [bv-dist v c1 1]]
             [d2 [bv-dist v c2 1]]
            ]

[if d
[begin
  [displayln "body"]
  [display [format "~a:\n~a:\n~a:\n~a:\n~a:\n~a:\n" [bv->char-str L] [bv->char-str Lp] [bv->char-str R] [bv->char-str Rp] [bv->char-str D] [bv->char-str Sdp]]]
  [display [format "~a:\n~a:\n~a:\n~a:\n~a:\n~a:\n" 0 [bv->char-str M] [bv->char-str LM] [bv->char-str RM] [bv->char-str LMdp] [bv->char-str RMdp]]]
  [display [format "~a:\n~a:\n~a:\n~a:\n~a:\n~a:\n" 0 [bv->char-str c1] 0 [bv->char-str c2] 0 0]]
  [displayln " end body"]
] [void]]
        [if [< d1 d2] c1 c2]

   ]]]]

;[pad [random [expt 2 15]] 16 2]

[define [tc v a b] 
[let [[v [char-str->bv v]]]
  ;[displayln "result ->"]
  ;[displayln [bv->char-str v]] 
  ;[displayln [bv->char-str [cerm v a b #f]]]
[bv->char-str [cerm v a b #f]]
  ]
]


[define l-16-1-4 [list
  "0000000000000000"
  "0000000011111111"
  "0000111100001111"
  "0000111111110000"
  "0011001100110011"
  "0011001111001100"
  "0011110000111100"
  "0011110011000011"
  "0101010101010101"
  "0101010110101010"
  "0101101001011010"
  "0101101010100101"
  "0110011001100110"
  "0110011010011001"
  "0110100101101001"
  "0110100110010110"
  "1001011001101001"
  "1001011010010110"
  "1001100101100110"
  "1001100110011001"
  "1010010101011010"
  "1010010110100101"
  "1010101001010101"
  "1010101010101010"
  "1100001100111100"
  "1100001111000011"
  "1100110000110011"
  "1100110011001100"
  "1111000000001111"
  "1111000011110000"
  "1111111100000000"
  "1111111111111111"]]

;[for* [[i [in-list l-16-1-4]]
;       [j [in-list l-16-1-4]]]
;  [displayln [bv-dist [char-str->bv i] [char-str->bv j] 0]]
;]

[for* [ 
        [j [in-range 3 4]]
        [k [in-range j]]]
[printf "~a ~a \n" j k ]
  
[displayln [remove-duplicates [sort [for/list [ [i [in-range [expt 2 8]]] 
        ;[j [in-range 2]]
        ;[k [in-range [add1 j]]]
          ]
  ;[printf "~a ~a ~a ~a \n" [pad i 8 2] i j k ]
  ;[displayln  [bv->char-str [cerm [char-str->bv [pad i 8 2]] k j #f]]]
  [bv->char-str [cerm [char-str->bv [pad i 8 2]] k j #f]]
 ] string<?]]]
]

#|
;;;xor/dist check;;;
[let* [[tas [pad [random [expt 2 31]] 32 2]]
       [tbs [pad [random [expt 2 31]] 32 2]]
       [txs [bv->char-str [bv-xor [char-str->bv tas] [char-str->bv tbs]]]]]
  [display [format "~a:\n~a:\n~a:\n" tas tbs txs]]
  [display [format "~a:\n" [bv-dist [char-str->bv tas] [char-str->bv tbs]0]]]
  [display [format "~a:\n" [bv-dist [char-str->bv tas] [char-str->bv txs]0]]]
  [display [format "~a:\n" [bv-dist [char-str->bv tbs] [char-str->bv txs]0]]]
  [display [format "~a:\n" [bv-par [bv-xor [char-str->bv tas] [char-str->bv tbs]]]]]
  ]


[let [[v [char-str->bv [pad [random [expt 2 16]] 16 2]]]]
   [displayln [bv->char-str v]]
   [displayln [bv->char-str [sub-bit-string v 0  [/ [bv-l v] 2]]]]
  [displayln [bv->char-str [bit-string-drop v [/ [bv-l v] 2]]]]
  ]
|#



#|

                      001
                   001   001
                001   002   001
             001   003   003   001
          001   004   006   004   001

          ^00   ^01   ^05   ^11   ^15
          ^15   ^14   ^10   ^04   ^00
           01    04    06    04    00 
           15    07    03    01    00

       001   005   010   010   005   001
    001   006   015   020   015   006   001
 001   007   021   035   035   021   007   001

      | < | < |   ..  |
      |  /|   |   ..  |
    |   |   u       ..  |
    |   |   u       ..  |



D = L + R

Sdp = cerm D . .

LM = L Lp M

RM R Rp M

cL Lp - Lp Sdp RMdp
cR Rp Sdp LMdp - Rp

|#




#|
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 2]]]] [tc [pad i 2 2] 1 1]] string<?]]]
4
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 2]]]] [tc [pad i 2 2] 0 1]] string<?]]]
2
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 2]]]] [tc [pad i 2 2] -1 1]] string<?]]]
1

> [length [remove-duplicates [sort [for/list [[i [in-range [p2 4]]]] [tc [pad i 4 2] 2 2]] string<?]]]
16
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 4]]]] [tc [pad i 4 2] 1 2]] string<?]]]
8
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 4]]]] [tc [pad i 4 2] 0 2]] string<?]]]
2
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 4]]]] [tc [pad i 4 2] -1 2]] string<?]]]
1

> [length [remove-duplicates [sort [for/list [[i [in-range [p2 8]]]] [tc [pad i 8 2] 3 3]] string<?]]]
256
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 8]]]] [tc [pad i 8 2] 2 3]] string<?]]]
128
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 8]]]] [tc [pad i 8 2] 1 3]] string<?]]]
16
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 8]]]] [tc [pad i 8 2] 0 3]] string<?]]]
2
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 8]]]] [tc [pad i 8 2] -1 3]] string<?]]]
1

> [length [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] 4 4]] string<?]]]
65536
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] 3 4]] string<?]]]
32768
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] 2 4]] string<?]]]
2048
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] 1 4]] string<?]]]
32
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] 0 4]] string<?]]]
2
> [length [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] -1 4]] string<?]]]
1

> [remove-duplicates [sort [for/list [[i [in-range [p2 16]]]] [tc [pad i 16 2] 1 4]] string<?]]
'("0000000000000000"
  "0000000011111111"
  "0000111100001111"
  "0000111111110000"
  "0011001100110011"
  "0011001111001100"
  "0011110000111100"
  "0011110011000011"
  "0101010101010101"
  "0101010110101010"
  "0101101001011010"
  "0101101010100101"
  "0110011001100110"
  "0110011010011001"
  "0110100101101001"
  "0110100110010110"
  "1001011001101001"
  "1001011010010110"
  "1001100101100110"
  "1001100110011001"
  "1010010101011010"
  "1010010110100101"
  "1010101001010101"
  "1010101010101010"
  "1100001100111100"
  "1100001111000011"
  "1100110000110011"
  "1100110011001100"
  "1111000000001111"
  "1111000011110000"
  "1111111100000000"
  "1111111111111111")
>

> [remove-duplicates [sort [for/list [[i [in-range [p2 8]]]] [tc [pad i 8 2] 1 3]] string<?]]
'("00000000"
  "00001111"
  "00110011"
  "00111100"
  "01010101"
  "01011010"
  "01100110"
  "01101001"
  "10010110"
  "10011001"
  "10100101"
  "10101010"
  "11000011"
  "11001100"
  "11110000"
  "11111111")
>

|#


