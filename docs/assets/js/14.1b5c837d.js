(window.webpackJsonp=window.webpackJsonp||[]).push([[14],{213:function(a,t,s){"use strict";s.r(t);var e=s(0),n=Object(e.a)({},(function(){var a=this,t=a.$createElement,s=a._self._c||t;return s("ContentSlotsDistributor",{attrs:{"slot-key":a.$parent.slotKey}},[s("h1",{attrs:{id:"validation"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#validation"}},[a._v("#")]),a._v(" Validation")]),a._v(" "),s("p",[a._v("Caliban provides a little macro called "),s("code",[a._v("gqldoc")]),a._v(" that can check at "),s("strong",[a._v("compile-time")]),a._v(" that a GraphQL query (a "),s("em",[a._v("document")]),a._v(" to be exact) has valid syntax.")]),a._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[a._v("import")]),a._v(" caliban"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v(".")]),a._v("Macros"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v(".")]),a._v("gqldoc\n\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[a._v("val")]),a._v(" query "),s("span",{pre:!0,attrs:{class:"token operator"}},[a._v("=")]),a._v(" gqldoc"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v("(")]),s("span",{pre:!0,attrs:{class:"token triple-quoted-string string"}},[a._v('"""\n  query test {\n    amos: character(name: "Amos Burton") {\n      name\n    }\n  }"""')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v(")")]),a._v("\n")])])]),s("p",[a._v("At "),s("strong",[a._v("runtime")]),a._v(", it is possible to validate a query against your schema by calling the method "),s("code",[a._v("check")]),a._v(" on your API.")]),a._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[a._v("def")]),a._v(" check"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v("(")]),a._v("query"),s("span",{pre:!0,attrs:{class:"token operator"}},[a._v(":")]),a._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[a._v("String")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v(")")]),s("span",{pre:!0,attrs:{class:"token operator"}},[a._v(":")]),a._v(" IO"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v("[")]),a._v("CalibanError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v(",")]),a._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[a._v("Unit")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[a._v("]")]),a._v("\n")])])]),s("p",[a._v("It is also possible to skip validation when executing a query by passing "),s("code",[a._v("skipValidation = true")]),a._v(" when calling "),s("code",[a._v("execute")]),a._v(". This will slightly improve performance.")])])}),[],!1,null,null,null);t.default=n.exports}}]);