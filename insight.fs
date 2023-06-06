
\ -------------------------------------------------------------
\  Small tools not available for default
\ -------------------------------------------------------------

: .x  ( -- ) 0 hex <# # # # # #> type space decimal ;
: .x2 ( -- ) 0 hex <#     # # #> type space decimal ;

: umax ( u1 u2 -- u ) 2dup u> if drop else nip then ;
: umin ( u1 u2 -- u ) 2dup u< if drop else nip then ;

: dump-ascii ( addr len -- )
  ?dup
  if
    1- 4 rshift 1+
    0 do
      cr dup dup .x space space
      16 0 do
        dup c@ .x2 1+
      loop
      space swap
      16 0 do
        dup c@ dup bl 127 within invert if
          drop [char] .
        then
        emit 1+
      loop
      drop
    loop
  then
  drop
;

\ -------------------------------------------------------------
\  Detailed insight into dictionary
\ -------------------------------------------------------------

: insight ( -- )  \ Long listing of everything inside of the dictionary structure
  base @ hex cr

  8 0 do
    up 16 - i 2* + @ \ There is a table of eight dictionary pointers (tab_dict), sorted by name length

    begin
      dup
    while
       ." Addr: "  dup .x
      ."  Link: "  dup @ .x
      ."  Flags: " dup cell+ c@ dup 128 and if ." I " else ." - " then
                                    5 rshift .
      ."  Code: "  dup cell+ count 31 and + aligned .x
      space        dup cell+ count 31 and type
      @ cr
    repeat
    drop

  cr
  loop

  base !
;

\ -------------------------------------------------------------
\  Try to find address as code start in dictionary
\ -------------------------------------------------------------

: name. ( addr -- )  \ If the address is code start of a dictionary word, it gets named.

  8 0 do
    up 16 - i 2* + @ \ There is a table of eight dictionary pointers (tab_dict), sorted by name length

    begin
      dup
    while
      2dup cell+ count 31 and + aligned
      = if ." --> " dup cell+ count 31 and type then
      @
    repeat
    drop

  loop
  drop
;

\ -------------------------------------------------------------
\   Disassembler
\ -------------------------------------------------------------

variable disasm-$    \ Current position for disassembling
variable disasm-cont \ Continue up to this position

: memstamp ( addr -- ) dup .x ." : " c@ .x2 ."   " ; \ Shows a memory location nicely

: disasm-fetch ( -- c ) disasm-$ @ c@ 1 disasm-$ +! ;

: disasm-reg       ( -- ) disasm-fetch ."  r" u. ;
: disasm-imm       ( -- ) disasm-fetch ."  #" .x2 ;
: disasm-addr-name ( -- ) disasm-fetch disasm-fetch 8 lshift or dup ."  " .x name. ;
: disasm-addr-cont ( -- ) disasm-fetch disasm-fetch 8 lshift or disasm-cont @ over umax disasm-cont ! ."  " .x ;

hex
: disasm-inst ( -- )

  disasm-fetch
  case
    00 of ." RST" endof
    01 of ." LD " disasm-reg disasm-imm endof
    02 of ." LD " disasm-reg disasm-reg endof
    03 of ." LDA" disasm-imm endof
    04 of ." LDA" disasm-reg endof
    05 of ." LAP" disasm-reg endof
    06 of ." STA" disasm-reg endof
    07 of ." SAP" disasm-reg endof

    \        ---
    09 of ." INC" disasm-reg endof
    0A of ." DEC" disasm-reg endof
    0B of ." ROL" disasm-reg endof
    0C of ." ROR" disasm-reg endof
    0D of ." AND" disasm-reg endof
    0E of ." OR " disasm-reg endof
    0F of ." XOR" disasm-reg endof

    10 of ." ADD" disasm-reg endof
    11 of ." SUB" disasm-reg endof
    \        ---
    13 of ." CMP" disasm-imm endof
    14 of ." CMP" disasm-reg endof
    15 of ." TST" disasm-reg endof
    16 of ." JPF" disasm-addr-cont endof
    17 of ." JNF" disasm-addr-cont endof

    18 of ." JMP" disasm-addr-name endof
    19 of ." JSR" disasm-addr-name endof
    1A of ." RET" endof
    1B of ." PSH" disasm-reg endof
    1C of ." POP" disasm-reg endof
    1D of ." IN"  endof
    1E of ." PHL" endof
    1F of ." RTS" endof

    20 of ." JLP" disasm-addr-cont endof
    21 of ." RWL" disasm-reg endof
    22 of ." SEC" endof
    23 of ." CLC" endof
    24 of ." AD " disasm-reg endof
    25 of ." SU " disasm-reg endof
    26 of ." OUT" endof
    \

    ." --- "
  endcase
;
decimal

: disasm-step ( -- )
  disasm-$ @                 \ Note current position
  dup memstamp disasm-inst cr \ Disassemble one instruction

  begin \ Write out all disassembled memory locations
    1+ dup disasm-$ @ <>
  while
    dup memstamp cr
  repeat
  drop
;

hex
: seec ( -- ) ( Continues to see )
  base @ hex cr
  0 disasm-cont !

  begin
    disasm-$ @ c@ 1F =                  \ Definitions usually end with RTS
    disasm-$ @ c@ 1A = or                \ Some also end with RET
    disasm-$ @ c@ 18 = or                 \ End with an unconditional jump
    disasm-$ @ disasm-cont @ u< invert and \ Do not stop when there has been a conditional jump further
    disasm-step
  until

  base !
;
decimal

: see ( -- ) \ Takes name of definition and shows its contents
  ' disasm-$ !
  seec
;
