= badIndent
    """ Top-Level forms must not be indented

/ ! m
  @ Monad m
  > FilePath
  > ConduitT Text (Int , Either Text Block) m []
= (blocked-lines fil)
    & bloop 1 NA ~
    / Int > Bool > Block > ConduitT Text (Int , Either Text Block) m []
    = (bloop line-num in-err so-far)
    !=  yield-block
        ? NIL | pure []
        ? fs  | yield (line-num, RIGHT fs)
    !=  continue
        | bloop (inc line-num) NA
    !=  (go-err finishes-block e)
        !! unless in-err | yield (line-num, LEFT e)
        !! bloop (inc line-num) (not finishes-block) NIL
    !<  m-line await
    !=  scrut
        >> m-line
        ? NONE    | NONE
        ? SOME|ln | SOME | fmap line-type | lex-line (fil, line num) ln
    !!  >> scrut
        ? NONE
            !! pure []
        ? (| SOME | LEFT er)
            !! go-err NA er
        ? (| SOME | RIGHT EMP-LINE)
            !! yield-block so-far
            !! continue []
        ? (| SOME | RIGHT | CONTINUED fs)
            !! if null so-far
            !! then go-err NA bad-indent
            !! else continue (so-far <> fs)
        ? (| SOME | RIGHT | NEW-BLOCK fs)
            !! yield-block so-far
            !! continue fs
        ? (| SOME | RIGHT | ONE-LINE w)
            !! yield-block so-far
            !! yield-block ~(0 , WIDE w)
            !! continue []
        ? (| SOME | RIGHT | TOP-MULTI-SHUT w f)
            ;; Let the parser handle it, not our problem.
            !! yield-block (0 , WIDE w)
            !! continue []
