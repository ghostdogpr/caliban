package caliban

import zio.http._

object GraphiQLAdapter {

  def handler(apiPath: Path, uiPath: Path): RequestHandler[Any, Nothing] =
    Handler.fromBody(Body.fromString(html(apiPath.toString, uiPath.toString)))

  /**
   * Creates an HTML string which can be used to serve the GraphiQL UI from CDN.
   *
   * @param apiPath The path at which the API can be introspected.
   * @param uiPath The path at which the GraphiQL UI will be served.
   *
   * @see [[https://github.com/graphql/graphiql/tree/main/examples/graphiql-cdn]]
   */
  def html(apiPath: String, uiPath: String): String =
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
