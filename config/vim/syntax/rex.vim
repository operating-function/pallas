syntax region Curl start="{" end="}" contains=Curl oneline

syntax match BadC /}/
syntax match Rune /[$!#%&*+,-.\/:<=>?@\\^`|~]\+/
syntax match Rine /^ *\zs[$!#%&*+,-.\/:<=>?@\\^`|~]\+\ze/
syntax match Word /[_a-zA-Z0-9]\+/
syntax match Nest /[[()\]]/
syntax match Head /^""".*$/
syntax match Line /'''.*$/
syntax match Page /.\zs""".*\ze$/
syntax match Note /;.*$/
syntax match Cord /\zs'[^']*'\ze\($\|[^']\)/
syntax match Tape /\zs"[^"]*"\ze\($\|[^"]\)/
syntax match Long '\%>80v.\+' containedin=ALL
syntax match Evil /\s*$/ containedin=ALL
syntax match Dent /\t/ containedin=ALL

highlight Dent ctermbg=Brown
highlight BadC ctermbg=Red ctermfg=Blue cterm=bold
highlight Long ctermbg=Red ctermfg=Blue cterm=bold
highlight Head ctermfg=Gray cterm=bold
highlight Evil ctermbg=Brown
highlight Rune ctermfg=Yellow
highlight Rine ctermfg=Yellow cterm=bold
highlight Word ctermfg=LightGreen
highlight Nest ctermfg=Magenta
highlight Note ctermfg=Gray
highlight Curl ctermfg=White
highlight Cord ctermfg=White
highlight Tape ctermfg=White
highlight Page ctermfg=White
highlight Line ctermfg=White
