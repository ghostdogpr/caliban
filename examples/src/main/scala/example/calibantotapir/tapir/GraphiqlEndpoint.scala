package example.calibantotapir.tapir

import sttp.tapir.ztapir._
import zio._
import sttp.capabilities.zio.ZioStreams

object GraphiQLEndpoint {
  val graphiql: ZServerEndpoint[Any, ZioStreams] =
    infallibleEndpoint.get
      .in("graphiql")
      .out(htmlBodyUtf8)
      .serverLogic(_ => ZIO.right(html(apiPath = "graphql", uiPath = "graphiql")))

  private def html(apiPath: String, uiPath: String): String =
    s"""
       |<!--
       | *  Copyright (c) 2021 GraphQL Contributors
       | *  All rights reserved.
       | *
       | *  This source code is licensed under the license found in the
       | *  LICENSE file in the root directory of this source tree.
       |-->
       |<!doctype html>
       |<html lang="en">
       |<head>
       |    <title>GraphiQL</title>
       |    <style>
       |        body {
       |            height: 100%;
       |            margin: 0;
       |            width: 100%;
       |            overflow: hidden;
       |        }
       |
       |        #graphiql {
       |            height: 100vh;
       |        }
       |    </style>
       |    <script
       |            crossorigin
       |            src="https://unpkg.com/react@18/umd/react.development.js"
       |    ></script>
       |    <script
       |            crossorigin
       |            src="https://unpkg.com/react-dom@18/umd/react-dom.development.js"
       |    ></script>
       |    <script
       |            src="https://unpkg.com/graphiql/graphiql.min.js"
       |            type="application/javascript"
       |    ></script>
       |    <link rel="stylesheet" href="https://unpkg.com/graphiql/graphiql.min.css"/>
       |    <script
       |            src="https://unpkg.com/@graphiql/plugin-explorer/dist/index.umd.js"
       |            crossorigin
       |    ></script>
       |
       |    <link
       |            rel="stylesheet"
       |            href="https://unpkg.com/@graphiql/plugin-explorer/dist/style.css"
       |    />
       |</head>
       |
       |<body>
       |<div id="graphiql">Loading...</div>
       |<script>
       |    const root = ReactDOM.createRoot(document.getElementById('graphiql'));
       |    const fetcher = GraphiQL.createFetcher({
       |        url: window.location.href.replace("$uiPath", "$apiPath")
       |    });
       |    const explorerPlugin = GraphiQLPluginExplorer.explorerPlugin();
       |    root.render(
       |        React.createElement(GraphiQL, {
       |            fetcher,
       |            defaultEditorToolsVisibility: true,
       |            plugins: [explorerPlugin],
       |        }),
       |    );
       |</script>
       |</body>
       |</html>
       |""".stripMargin
}
