"use strict";(self.webpackChunkpallas=self.webpackChunkpallas||[]).push([[641],{9607:(e,i,r)=>{r.r(i),r.d(i,{assets:()=>c,contentTitle:()=>l,default:()=>o,frontMatter:()=>a,metadata:()=>t,toc:()=>d});var n=r(4848),s=r(8453);const a={},l="Characters and Strings",t={id:"reference/standard-library/characters-strings",title:"Characters and Strings",description:"TODO: rectify ord and chr with REPL formatting; should results format as in the REPL?",source:"@site/../../doc/reference/standard-library/04_characters-strings.md",sourceDirName:"reference/standard-library",slug:"/reference/standard-library/characters-strings",permalink:"/reference/standard-library/characters-strings",draft:!1,unlisted:!1,editUrl:"https://github.com/operating-function/pallas/edit/main/doc/../../doc/reference/standard-library/04_characters-strings.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{},sidebar:"referenceSidebar",previous:{title:"Comparisons",permalink:"/reference/standard-library/comparisons"},next:{title:"Rows",permalink:"/reference/standard-library/rows"}},c={},d=[{value:"Character Functions",id:"character-functions",level:2},{value:"ord",id:"ord",level:3},{value:"chr",id:"chr",level:3},{value:"isDigit",id:"isdigit",level:3},{value:"isHexDigit",id:"ishexdigit",level:3},{value:"isUpper",id:"isupper",level:3},{value:"isLower",id:"islower",level:3},{value:"isAlpha",id:"isalpha",level:3},{value:"isPrint",id:"isprint",level:3},{value:"isAlphaNum",id:"isalphanum",level:3},{value:"toLower",id:"tolower",level:3},{value:"toUpper",id:"toupper",level:3},{value:"Special Characters",id:"special-characters",level:2},{value:"newlineChar",id:"newlinechar",level:3},{value:"tabChar",id:"tabchar",level:3},{value:"spaceChar",id:"spacechar",level:3},{value:"Number to String Conversion",id:"number-to-string-conversion",level:2},{value:"listDigits",id:"listdigits",level:3},{value:"digits",id:"digits",level:3},{value:"String Functions",id:"string-functions",level:2},{value:"strLen",id:"strlen",level:3},{value:"strWeld",id:"strweld",level:3},{value:"strCat",id:"strcat",level:3},{value:"strToList",id:"strtolist",level:3},{value:"strFromList",id:"strfromlist",level:3},{value:"explode",id:"explode",level:3},{value:"implode",id:"implode",level:3},{value:"strToUpper",id:"strtoupper",level:3},{value:"strToLower",id:"strtolower",level:3},{value:"strCapitalize",id:"strcapitalize",level:3},{value:"strIsCapitalized",id:"striscapitalized",level:3},{value:"strMap",id:"strmap",level:3},{value:"String Parsing Functions",id:"string-parsing-functions",level:2},{value:"isDecimalLit",id:"isdecimallit",level:3},{value:"loadDecimal",id:"loaddecimal",level:3},{value:"isHexLit",id:"ishexlit",level:3},{value:"loadHexLit",id:"loadhexlit",level:3},{value:"loadKeyWord",id:"loadkeyword",level:3}];function h(e){const i={admonition:"admonition",code:"code",h1:"h1",h2:"h2",h3:"h3",header:"header",p:"p",pre:"pre",...(0,s.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(i.header,{children:(0,n.jsx)(i.h1,{id:"characters-and-strings",children:"Characters and Strings"})}),"\n",(0,n.jsx)(i.admonition,{title:"TODO",type:"warning",children:(0,n.jsxs)(i.p,{children:["TODO: rectify ",(0,n.jsx)(i.code,{children:"ord"})," and ",(0,n.jsx)(i.code,{children:"chr"})," with REPL formatting; should results format as in the REPL?"]})}),"\n",(0,n.jsx)(i.h2,{id:"character-functions",children:"Character Functions"}),"\n",(0,n.jsx)(i.h3,{id:"ord",children:"ord"}),"\n",(0,n.jsx)(i.p,{children:"Converts a digit character to its numeric value. For non-digit characters, it returns the ASCII value minus 48."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"ord {1}    == 1\nord {A}    == 17\nord {a}    == 49\n"})}),"\n",(0,n.jsx)(i.h3,{id:"chr",children:"chr"}),"\n",(0,n.jsx)(i.p,{children:"Converts a number to its corresponding ASCII character by adding 48."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"chr 48    == {0}\nchr 65    == {A}\nchr 97    == {a}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"isdigit",children:"isDigit"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is a digit (0-9)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isDigit {0}    == 1\nisDigit {9}    == 1\nisDigit {a}    == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"ishexdigit",children:"isHexDigit"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is a hexadecimal digit (0-9, a-f, A-F)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isHexDigit {0}    == 1\nisHexDigit {F}    == 1\nisHexDigit {g}    == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"isupper",children:"isUpper"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is uppercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isUpper {A}    == 1\nisUpper {a}    == 0\nisUpper {0}    == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"islower",children:"isLower"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is lowercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isLower {a}    == 1\nisLower {A}    == 0\nisLower {0}    == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"isalpha",children:"isAlpha"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is alphabetic (a-z, A-Z)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isAlpha {a}    == 1\nisAlpha {Z}    == 1\nisAlpha {0}    == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"isprint",children:"isPrint"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is printable (space through tilde)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isPrint { }     == 1\nisPrint {~}     == 1\nisPrint {a}     == 1\nisPrint 0       == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"isalphanum",children:"isAlphaNum"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a character is alphanumeric (a-z, A-Z, 0-9)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isAlphaNum {a}    == 1\nisAlphaNum {Z}    == 1\nisAlphaNum {0}    == 1\nisAlphaNum {!}    == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"tolower",children:"toLower"}),"\n",(0,n.jsx)(i.p,{children:"Converts a character to lowercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"toLower {A}    == {a}\ntoLower {a}    == {a}\ntoLower {0}    == 48\n"})}),"\n",(0,n.jsx)(i.h3,{id:"toupper",children:"toUpper"}),"\n",(0,n.jsx)(i.p,{children:"Converts a character to uppercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"toUpper {a}    == {A}\ntoUpper {A}    == {A}\ntoUpper {0}    == 48\n"})}),"\n",(0,n.jsx)(i.h2,{id:"special-characters",children:"Special Characters"}),"\n",(0,n.jsx)(i.h3,{id:"newlinechar",children:"newlineChar"}),"\n",(0,n.jsx)(i.p,{children:"The ASCII code for newline (10)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"newlineChar == 10\n"})}),"\n",(0,n.jsx)(i.h3,{id:"tabchar",children:"tabChar"}),"\n",(0,n.jsx)(i.p,{children:"The ASCII code for tab (9)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"tabChar == 9\n"})}),"\n",(0,n.jsx)(i.h3,{id:"spacechar",children:"spaceChar"}),"\n",(0,n.jsx)(i.p,{children:"The ASCII code for space (32)."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"spaceChar == 32\n"})}),"\n",(0,n.jsx)(i.h2,{id:"number-to-string-conversion",children:"Number to String Conversion"}),"\n",(0,n.jsx)(i.h3,{id:"listdigits",children:"listDigits"}),"\n",(0,n.jsx)(i.p,{children:"Converts a number to a list of digit characters."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"listDigits 0     == [48 0]\nlistDigits 123   == [49 [50 [51 0]]]\n"})}),"\n",(0,n.jsx)(i.h3,{id:"digits",children:"digits"}),"\n",(0,n.jsx)(i.p,{children:"Converts a number to a row of digit characters."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"digits 0     == [48]\ndigits 123   == [49 50 51]\n"})}),"\n",(0,n.jsx)(i.h2,{id:"string-functions",children:"String Functions"}),"\n",(0,n.jsx)(i.h3,{id:"strlen",children:"strLen"}),"\n",(0,n.jsx)(i.p,{children:"Returns the length of a string."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strLen {}        == 0\nstrLen {abc}     == 3\nstrLen {hello}   == 5\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strweld",children:"strWeld"}),"\n",(0,n.jsx)(i.p,{children:"Concatenates two strings."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strWeld {hello} { world}    == {hello world}\nstrWeld {} {abc}            == {abc}\nstrWeld {abc} {}            == {abc}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strcat",children:"strCat"}),"\n",(0,n.jsx)(i.p,{children:"Concatenates a row of strings."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strCat [{a} {b} {c}]    == {abc}\nstrCat [{} {abc} {}]    == {abc}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strtolist",children:"strToList"}),"\n",(0,n.jsx)(i.admonition,{title:"TODO",type:"warning",children:(0,n.jsx)(i.p,{children:"TODO: REPL formatting?"})}),"\n",(0,n.jsx)(i.p,{children:"Converts a string to a list of character codes."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strToList {abc}    == [97 [98 [99 0]]]\nstrToList {}       == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strfromlist",children:"strFromList"}),"\n",(0,n.jsx)(i.admonition,{title:"TODO",type:"warning",children:(0,n.jsx)(i.p,{children:"TODO: REPL formatting?"})}),"\n",(0,n.jsx)(i.p,{children:"Converts a list of character codes to a string."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strFromList [97 [98 [99 0]]]    == {abc}\nstrFromList 0                   == {}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"explode",children:"explode"}),"\n",(0,n.jsx)(i.admonition,{title:"TODO",type:"warning",children:(0,n.jsx)(i.p,{children:"TODO: REPL formatting?"})}),"\n",(0,n.jsx)(i.p,{children:"Converts a string to a row of character codes."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"explode {abc}    == [97 98 99]\nexplode {}       == []\n"})}),"\n",(0,n.jsx)(i.h3,{id:"implode",children:"implode"}),"\n",(0,n.jsx)(i.p,{children:"Converts a row of character codes to a string."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"implode [97 98 99]    == {abc}\nimplode []            == {}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strtoupper",children:"strToUpper"}),"\n",(0,n.jsx)(i.p,{children:"Converts a string to uppercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strToUpper {Hello}     == {HELLO}\nstrToUpper {ABC123}    == {ABC123}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strtolower",children:"strToLower"}),"\n",(0,n.jsx)(i.p,{children:"Converts a string to lowercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strToLower {Hello}     == {hello}\nstrToLower {ABC123}    == {abc123}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strcapitalize",children:"strCapitalize"}),"\n",(0,n.jsx)(i.p,{children:"Capitalizes the first character of a string."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strCapitalize {hello}    == {Hello}\nstrCapitalize {HELLO}    == {HELLO}\nstrCapitalize {}         == {}\n"})}),"\n",(0,n.jsx)(i.h3,{id:"striscapitalized",children:"strIsCapitalized"}),"\n",(0,n.jsx)(i.p,{children:"Checks if the first character of a string is uppercase."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strIsCapitalized {Hello}    == 1\nstrIsCapitalized {hello}    == 0\nstrIsCapitalized {}         == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"strmap",children:"strMap"}),"\n",(0,n.jsx)(i.p,{children:"Applies a function to every character in a string."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"strMap toUpper {Hello}      == {HELLO}\nstrMap (add 1) {ABC}        == {BCD}\nstrMap (const {x}) {abc}    == {xxx}\n"})}),"\n",(0,n.jsx)(i.h2,{id:"string-parsing-functions",children:"String Parsing Functions"}),"\n",(0,n.jsx)(i.h3,{id:"isdecimallit",children:"isDecimalLit"}),"\n",(0,n.jsx)(i.p,{children:"Checks if a string is a valid decimal literal."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isDecimalLit {123}      == 1\nisDecimalLit {12_34}    == 1\nisDecimalLit {12.34}    == 0\nisDecimalLit {abc}      == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"loaddecimal",children:"loadDecimal"}),"\n",(0,n.jsx)(i.p,{children:"Parses a decimal literal string into a number."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"loadDecimal {123}      == 123\nloadDecimal {12_34}    == 1234\nloadDecimal {0}        == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"ishexlit",children:"isHexLit"}),"\n",(0,n.jsx)(i.p,{children:'Checks if a string is a valid hexadecimal literal (starting with "0x").'}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"isHexLit {0xff}       == 1\nisHexLit {0xAB_CD}    == 1\nisHexLit {ff}         == 0\nisHexLit {0xGG}       == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"loadhexlit",children:"loadHexLit"}),"\n",(0,n.jsx)(i.p,{children:"Parses a hexadecimal literal string into a number."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"loadHexLit {0xff}     == 255\nloadHexLit {0xA_B}    == 171\nloadHexLit {0x0}      == 0\n"})}),"\n",(0,n.jsx)(i.h3,{id:"loadkeyword",children:"loadKeyWord"}),"\n",(0,n.jsx)(i.p,{children:"Parses a string as either a decimal literal, hexadecimal literal, or returns the string itself."}),"\n",(0,n.jsx)(i.pre,{children:(0,n.jsx)(i.code,{className:"language-sire",children:"loadKeyWord {123}     == 123\nloadKeyWord {0xff}    == 255\nloadKeyWord {abc}     == {abc}\n"})})]})}function o(e={}){const{wrapper:i}={...(0,s.R)(),...e.components};return i?(0,n.jsx)(i,{...e,children:(0,n.jsx)(h,{...e})}):h(e)}},8453:(e,i,r)=>{r.d(i,{R:()=>l,x:()=>t});var n=r(6540);const s={},a=n.createContext(s);function l(e){const i=n.useContext(a);return n.useMemo((function(){return"function"==typeof e?e(i):{...i,...e}}),[i,e])}function t(e){let i;return i=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:l(e.components),n.createElement(a.Provider,{value:i},e.children)}}}]);