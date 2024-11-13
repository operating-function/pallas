"use strict";(self.webpackChunkpallas=self.webpackChunkpallas||[]).push([[537],{7648:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>a,contentTitle:()=>c,default:()=>h,frontMatter:()=>o,metadata:()=>t,toc:()=>l});var r=s(4848),i=s(8453);const o={},c="Runes",t={id:"reference/sire-runes-macros",title:"Runes",description:"Runes are special symbols or characters that serve as syntactic markers for various language constructs and operations. Runes are a key part of flexible syntax system that is used by Sire, known as Rex (R-expressions), which allows for multiple ways to express the same code structure.",source:"@site/../../doc/reference/sire-runes-macros.md",sourceDirName:"reference",slug:"/reference/sire-runes-macros",permalink:"/reference/sire-runes-macros",draft:!1,unlisted:!1,editUrl:"https://github.com/operating-function/pallas/edit/main/doc/../../doc/reference/sire-runes-macros.md",tags:[],version:"current",frontMatter:{},sidebar:"referenceSidebar",next:{title:"Bits (Booleans)",permalink:"/reference/standard-library/bits-booleans"}},a={},l=[{value:"Runes",id:"runes-1",level:3},{value:"Macros",id:"macros",level:3}];function d(e){const n={code:"code",h1:"h1",h3:"h3",header:"header",li:"li",ol:"ol",p:"p",strong:"strong",ul:"ul",...(0,i.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(n.header,{children:(0,r.jsx)(n.h1,{id:"runes",children:"Runes"})}),"\n",(0,r.jsx)(n.p,{children:"Runes are special symbols or characters that serve as syntactic markers for various language constructs and operations. Runes are a key part of flexible syntax system that is used by Sire, known as Rex (R-expressions), which allows for multiple ways to express the same code structure."}),"\n",(0,r.jsx)(n.p,{children:"Runes in Sire serve several purposes:"}),"\n",(0,r.jsxs)(n.ol,{children:["\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.strong,{children:"Function Definition and Application"}),": Runes like ",(0,r.jsx)(n.code,{children:"="})," for defining functions and ",(0,r.jsx)(n.code,{children:"|"})," for function application are fundamental to Sire's syntax."]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.strong,{children:"Control Flow"}),": Runes such as ",(0,r.jsx)(n.code,{children:"?"})," for creating anonymous functions or ",(0,r.jsx)(n.code,{children:"#datacase"})," for pattern matching help control the flow of execution."]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.strong,{children:"Metaprogramming"}),": Some runes, like ",(0,r.jsx)(n.code,{children:"^"})," for expression reordering or ",(0,r.jsx)(n.code,{children:"'"})," for quoting are used in metaprogramming."]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.strong,{children:"Syntactic Sugar"}),": Runes like ",(0,r.jsx)(n.code,{children:":"})," for continuation passing style or ",(0,r.jsx)(n.code,{children:"-"})," for infix function application provide convenient shorthand notations."]}),"\n"]}),"\n",(0,r.jsx)(n.p,{children:"Runes are one or more non-alphanumeric characters and are used at the beginning of expressions or in specific syntactic positions. They allow Sire to have a highly expressive and flexible syntax while maintaining a simple, uniform structure that's easy to parse and manipulate programmatically."}),"\n",(0,r.jsx)(n.h3,{id:"runes-1",children:"Runes"}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"|"}),"     function application (can be omitted in nested forms: ",(0,r.jsx)(n.code,{children:"(| f a)"})," is equal to ",(0,r.jsx)(n.code,{children:"(f a)"}),")"]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"-"}),"     function application "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"="}),"     top-level defintion "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"@"}),"     let-binding "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"&"}),"     anonymous lambda "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"?"}),"     named lambda "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"??"}),"  named and pinned lambda "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:","}),"     tuples "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:":|"}),"   imports "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"####"})," dependency"]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"*"}),"     list of expressions (advanced - this can be used to circumvent the need for indentation)"]}),"\n"]}),"\n",(0,r.jsx)(n.h3,{id:"macros",children:"Macros"}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"[]"}),"    short form for ",(0,r.jsx)(n.code,{children:","})," "]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:"^"}),'      expression reordering ("anonymous where": ',(0,r.jsx)(n.code,{children:"(^ f _ b)(expr)"})," gets rewritten to ",(0,r.jsx)(n.code,{children:"(@ _ expr | f _ b))"})]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.code,{children:":"}),"     continuation passing"]}),"\n"]})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,r.jsx)(n,{...e,children:(0,r.jsx)(d,{...e})}):d(e)}},8453:(e,n,s)=>{s.d(n,{R:()=>c,x:()=>t});var r=s(6540);const i={},o=r.createContext(i);function c(e){const n=r.useContext(o);return r.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function t(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:c(e.components),r.createElement(o.Provider,{value:n},e.children)}}}]);