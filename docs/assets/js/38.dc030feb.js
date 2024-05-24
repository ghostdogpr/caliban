(window.webpackJsonp=window.webpackJsonp||[]).push([[38],{317:function(t,a,s){"use strict";s.r(a);var n=s(14),e=Object(n.a)({},(function(){var t=this,a=t._self._c;return a("ContentSlotsDistributor",{attrs:{"slot-key":t.$parent.slotKey}},[a("h1",{attrs:{id:"code-generation"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#code-generation"}},[t._v("#")]),t._v(" Code generation")]),t._v(" "),a("p",[t._v("If you want a workflow where you first edit a GraphQL schema file, and then generate type-safe server stubs, Caliban has your back.")]),t._v(" "),a("p",[t._v("You'll first need to add the following dependency to your "),a("code",[t._v("project/plugins.sbt")]),t._v(" file:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[t._v("addSbtPlugin"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"com.github.ghostdogpr"')]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("%")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"caliban-codegen-sbt"')]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("%")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"2.6.0"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("You then enable it in your "),a("code",[t._v("build.sbt")]),t._v(" file:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" _root_"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("caliban"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("tools"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("Codegen\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("lazy")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" myproject "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" project\n  "),a("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// enable caliban codegen plugin")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("enablePlugins"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("CalibanPlugin"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("settings"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// add code generation settings")]),t._v("\n    Compile "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("/")]),t._v(" caliban "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("/")]),t._v(" calibanSettings "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("++")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" Seq"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n      calibanSetting"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("file"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"myproject/src/main/graphql/myapi.graphql"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n        "),a("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// important to set this, otherwise you'll get client code")]),t._v("\n        _"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("genType"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Codegen"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("GenType"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("Schema"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n          "),a("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// you can customize the codegen further with this DSL")]),t._v("\n          "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("clientName"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"NameOfApi.scala"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n          "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("packageName"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"myproject.mypackage"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n      "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("You can also generate it manually using the following sbt command:")]),t._v(" "),a("div",{staticClass:"language- extra-class"},[a("pre",{pre:!0,attrs:{class:"language-text"}},[a("code",[t._v("calibanGenSchema schemaPath outputPath [options]\n")])])]),a("p",[t._v("Example:")]),t._v(" "),a("div",{staticClass:"language- extra-class"},[a("pre",{pre:!0,attrs:{class:"language-text"}},[a("code",[t._v("calibanGenSchema project/schema.graphql src/main/MyAPI.scala --addDerives true\n")])])]),a("h2",{attrs:{id:"options"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#options"}},[t._v("#")]),t._v(" Options")]),t._v(" "),a("ul",[a("li",[a("code",[t._v("scalafmtPath")]),t._v(": Specifies the configuration file for Scalafmt. Default: "),a("code",[t._v(".scalafmt.conf")]),t._v(".")]),t._v(" "),a("li",[a("code",[t._v("headers")]),t._v(": Provides request headers when "),a("code",[t._v("schemaPath")]),t._v(" is a URL.")]),t._v(" "),a("li",[a("code",[t._v("packageName")]),t._v(": Overrides the package name derived from the folder of "),a("code",[t._v("outputPath")]),t._v(".")]),t._v(" "),a("li",[a("code",[t._v("effect")]),t._v(": Overrides the default effect ("),a("code",[t._v("zio.UIO")]),t._v(") for wrapping fields in Queries and Mutations.")]),t._v(" "),a("li",[a("code",[t._v("scalarMappings")]),t._v(": Forces a mapping between a GraphQL type and a Scala class (e.g., scalars).")]),t._v(" "),a("li",[a("code",[t._v("imports")]),t._v(": Adds additional imports to the generated code.")]),t._v(" "),a("li",[a("code",[t._v("abstractEffectType")]),t._v(": Indicates that the effect type is abstract. Fields in Queries and Mutations will return "),a("code",[t._v("F[_]")]),t._v(".")]),t._v(" "),a("li",[a("code",[t._v("preserveInputNames")]),t._v(": Disables the default behavior of appending "),a("code",[t._v("Input")]),t._v(" to the type name of input types in the derived schema.")]),t._v(" "),a("li",[a("code",[t._v("addDerives")]),t._v(": Adds "),a("code",[t._v("derives")]),t._v(" clauses for type class instance derivation in Scala 3.")]),t._v(" "),a("li",[a("code",[t._v("envForDerives")]),t._v(": Specifies the type alias for your ZIO Environment when using "),a("code",[t._v("derives")]),t._v(" and a ZIO Environment other than "),a("code",[t._v("Any")]),t._v(".")])]),t._v(" "),a("h2",{attrs:{id:"lazy-evaluation"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#lazy-evaluation"}},[t._v("#")]),t._v(" Lazy evaluation")]),t._v(" "),a("p",[t._v("The main difference between generating code for client usage and for server usage is that on the server you need to account for\ncode which should only be evaluated if the client requests the field!")]),t._v(" "),a("p",[t._v("You can annotate this directly in the graphql schema by creating a "),a("code",[t._v("@lazy")]),t._v(" directive.")]),t._v(" "),a("div",{staticClass:"language-graphql extra-class"},[a("pre",{pre:!0,attrs:{class:"language-graphql"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("directive")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@lazy")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("on")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token constant"}},[t._v("FIELD_DEFINITION")]),t._v("\n")])])]),a("p",[t._v("You can then annotate fields in the graphql schema like this:")]),t._v(" "),a("div",{staticClass:"language-graphql extra-class"},[a("pre",{pre:!0,attrs:{class:"language-graphql"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("directive")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@lazy")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("on")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token constant"}},[t._v("FIELD_DEFINITION")]),t._v("\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("type")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token class-name"}},[t._v("MyType")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("myLazyField")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token scalar"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("!")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@lazy")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("myField")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token scalar"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("!")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),a("p",[t._v("And you'll get a case class which looks something like this:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" MyType"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("myLazyField"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" zio"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("UIO"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" myField"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("When implementing this, "),a("code",[t._v("myLazyField")]),t._v(" will only be evaluated if the client requested it in the query")]),t._v(" "),a("h2",{attrs:{id:"newtype-declaration"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#newtype-declaration"}},[t._v("#")]),t._v(" Newtype declaration")]),t._v(" "),a("p",[t._v("The "),a("code",[t._v("@newtype")]),t._v(" directive in caliban allows you to wrap your GraphQL fields into statically\ntyped IDs for backend. For clients, they can use the GraphQL as before and do not\nhave to adjust their typing, or optionally can generate stronger typed IDs using\nthe directive.")]),t._v(" "),a("p",[t._v("In the following example we want to encapsulate "),a("code",[t._v("id : ID")]),t._v(" as FooId for better type safety, so\nwe use the "),a("code",[t._v("@newtype")]),t._v(" directive on the "),a("code",[t._v("Query")]),t._v(" and Foo object type.\nOn mutation, we are passing an optional field of String. To avoid mixing with other String\ntypes in backend code we decided to create a "),a("code",[t._v("@newtype")]),t._v(" of Bar instead.")]),t._v(" "),a("div",{staticClass:"language-graphql extra-class"},[a("pre",{pre:!0,attrs:{class:"language-graphql"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("directive")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@newtype")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("name")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token scalar"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("on")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token constant"}},[t._v("FIELD_DEFINITION")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("|")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token constant"}},[t._v("ARGUMENT_DEFINITION")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("|")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token constant"}},[t._v("INPUT_FIELD_DEFINITION")]),t._v("\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("type")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token class-name"}},[t._v("Query")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token property-query"}},[t._v("getFoo")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("id")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token scalar"}},[t._v("ID")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("!")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@newtype")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("name")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"FooId"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token class-name"}},[t._v("Foo")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("type")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token class-name"}},[t._v("Mutation")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("updateFoo")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("foo")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token atom-input class-name"}},[t._v("FooInput")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("!")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token class-name"}},[t._v("Foo")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("type")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token class-name"}},[t._v("Foo")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("id")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token scalar"}},[t._v("ID")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("!")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@newtype")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("name")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"FooId"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("...")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("input")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token atom-input class-name"}},[t._v("FooInput")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("maybeBar")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token scalar"}},[t._v("String")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token directive function"}},[t._v("@newtype")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token attr-name"}},[t._v("name")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"Bar"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("...")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])]),a("p",[t._v("With Scalar mapping for ID set to Int, it would give you the following case classes.")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Bar"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("value"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("AnyVal")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" FooId"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("value"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("AnyVal")]),t._v("\n")])])]),a("p",[t._v("The "),a("code",[t._v("extends AnyVal")]),t._v(" ensures type erasure in such a way that we do not have to consider this abstraction\non client side, and hence graphql query remains the same as before applying the directive.\nBut for this to work we need to supply some implicit for our schema to understand the AnyVal\nconversion and mapping of the return values using "),a("code",[t._v("@GQLDirective")]),t._v(" annotation.")]),t._v(" "),a("p",[t._v("With this in place our generate type code should look something like this:")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("object")]),t._v(" Types "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" QueryGetFooArgs"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("id"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" FooId"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" MutationUpdateFooArgs"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("foo"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" FooInput"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  \n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Bar"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("value"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("AnyVal")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("object")]),t._v(" Bar   "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n     "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("implicit")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" schema"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" Schema"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Any")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" Bar"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("    "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" implicitly"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("Schema"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Any")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("contramap"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("value"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n     "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("implicit")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" argBuilder"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" ArgBuilder"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("Bar"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" implicitly"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("ArgBuilder"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("map"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Bar"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" FooId"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("value"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("extends")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("AnyVal")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("object")]),t._v(" FooId "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("implicit")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" schema"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" Schema"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Any")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" FooId"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("    "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" implicitly"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("Schema"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Any")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("contramap"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("value"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("implicit")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" argBuilder"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" ArgBuilder"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("FooId"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" implicitly"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("ArgBuilder"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("map"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("FooId"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Foo"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLDirective")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Directive"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"newtype"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" Map"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"name"')]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("->")]),t._v(" StringValue"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"FooId"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n    id"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" FooId\n    "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("final")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" FooInput"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token annotation punctuation"}},[t._v("@GQLDirective")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Directive"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"newtype"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" Map"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"name"')]),t._v(" "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("->")]),t._v(" StringValue"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token string"}},[t._v('"Bar"')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n    maybeBar"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" scala"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("Option"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("Bar"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n    "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("\n  "),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v("\n")])])])])}),[],!1,null,null,null);a.default=e.exports}}]);