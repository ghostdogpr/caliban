(window.webpackJsonp=window.webpackJsonp||[]).push([[11],{211:function(t,a,s){"use strict";s.r(a);var n=s(0),e=Object(n.a)({},(function(){var t=this,a=t.$createElement,s=t._self._c||a;return s("ContentSlotsDistributor",{attrs:{"slot-key":t.$parent.slotKey}},[s("h1",{attrs:{id:"middleware"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#middleware"}},[t._v("#")]),t._v(" Middleware")]),t._v(" "),s("p",[t._v("You might want to perform some actions on every query received. Caliban supports this in 2 different ways:")]),t._v(" "),s("ul",[s("li",[t._v("you can wrap the execution of each query with any arbitrary code")]),t._v(" "),s("li",[t._v("you can analyze the requested fields before execution, and possibly modify or reject the query")])]),t._v(" "),s("h2",{attrs:{id:"wrapping-query-execution"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#wrapping-query-execution"}},[t._v("#")]),t._v(" Wrapping query execution")]),t._v(" "),s("p",[t._v("Once you have a "),s("code",[t._v("GraphQL")]),t._v(" interpreter, you can call "),s("code",[t._v("wrapExecutionWith")]),t._v(" on it. This method takes in a function "),s("code",[t._v("f")]),t._v(" and returns a new "),s("code",[t._v("GraphQL")]),t._v(" interpreter that will wrap the "),s("code",[t._v("execute")]),t._v(" method with this function "),s("code",[t._v("f")]),t._v(".")]),t._v(" "),s("p",[t._v("It is used internally to implement "),s("code",[t._v("mapError")]),t._v(" (customize errors) and "),s("code",[t._v("provide")]),t._v(" (eliminate the environment), but you can use it for other purposes such as adding a general timeout, logging response times, etc.")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// create an interpreter")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" i"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" GraphQLInterpreter"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("MyEnv"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" CalibanError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" graphqQL"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("interpreter\n\n"),s("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// change error type to String")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" i2"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" GraphQLInterpreter"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("MyEnv"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" i"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("mapError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("toString"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n"),s("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// provide the environment")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" i3"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" GraphQLInterpreter"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Any")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" CalibanError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" i"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("provide"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("myEnv"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n"),s("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// add a timeout on every query execution")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" i4"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" GraphQLInterpreter"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("MyEnv "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("with")]),t._v(" Clock"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" CalibanError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v("\n  i"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("wrapExecutionWith"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n    _"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("timeout"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token number"}},[t._v("30")]),t._v(" seconds"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("map"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n      _"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("getOrElse"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("GraphQLResponse"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("NullValue"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" List"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("ExecutionError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"Timeout!"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n    "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),s("h2",{attrs:{id:"query-analyzer"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#query-analyzer"}},[t._v("#")]),t._v(" Query Analyzer")]),t._v(" "),s("p",[t._v("You can also use "),s("code",[t._v("GraphQL#withQueryAnalyzer")]),t._v(" to register a hook function that will be run before query execution. Such function is called a "),s("code",[t._v("QueryAnalyzer")]),t._v(" and looks like this:")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("type")]),t._v(" QueryAnalyzer"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("-")]),t._v("R"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" Field "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v(" ZIO"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("R"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" CalibanError"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" Field"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n")])])]),s("p",[t._v("As an input, you receive the root "),s("code",[t._v("Field")]),t._v(" object that contains the whole query in a convenient format (the fragments are replaced by the actual fields). You can analyze this object and return a "),s("code",[t._v("ZIO[R, CalibanError, Field]")]),t._v(" that allows you to:")]),t._v(" "),s("ul",[s("li",[t._v("modify the query (e.g. add or remove fields)")]),t._v(" "),s("li",[t._v("return an error to prevent execution")]),t._v(" "),s("li",[t._v("run some effect (e.g. write metrics somewhere)")])]),t._v(" "),s("p",[t._v("A typical use case is to limit the number of fields or the depth of the query. Those are already implemented in Caliban and can be used like this:")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" interpreter "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v("\n  maxDepth"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token number"}},[t._v("30")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n    maxFields"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token number"}},[t._v("200")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n      graphQL"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n    "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),s("p",[t._v("You can look at their implementation which is only a few lines long.")]),t._v(" "),s("p",[t._v('You can even use a Query Analyzer to "inject" some data into your ZIO environment and possibly use it later during execution. For example, you can have a '),s("code",[t._v("Context")]),t._v(" as part of your environment:")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" ContextData"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("cost"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("trait")]),t._v(" Context "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("def")]),t._v(" context"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" Ref"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("ContextData"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),s("p",[t._v("Then you can register a Query Analyzer that calculates the cost of every query (in this example, I just used the number of fields) and save it into the context. that way you can support an API that returns the cost associated with each query.")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[t._v("interpreter"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("withQueryAnalyzer "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v(" root "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" cost "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" QueryAnalyzer"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("countFields"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("root"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  ZIO"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("accessM"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("Context"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("context"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("update"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("copy"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("cost "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" cost"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("as"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("root"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])])])}),[],!1,null,null,null);a.default=e.exports}}]);