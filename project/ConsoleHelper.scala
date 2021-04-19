import sbt.Keys._

object ConsoleHelper {
  def prompt: String               = s"${Console.CYAN}>${Console.RESET} "
  def header(text: String): String = s"${Console.GREEN}$text${Console.RESET}"

  def item(text: String): String =
    s"${Console.RED}> ${Console.CYAN}$text${Console.RESET}"

  def welcomeMessage =
    onLoadMessage :=
      raw"""|${header(s"""   ____      _ _ _                 """)}
            |${header(s"""  / ___|__ _| (_) |__   __ _ _ __  """)}
            |${header(s""" | |   / _` | | | '_ \\ / _` | '_ \\ """)}
            |${header(s""" | |__| (_| | | | |_) | (_| | | | |""")}
            |${header(s"""  \\____\\__,_|_|_|_.__/ \\__,_|_| |_|  ${version.value}""")}
            |
            |Useful sbt tasks:
            |${item("~compile")} - Compile all modules with file-watch enabled
            |${item("test")} - Run the unit test suite
            |${item("fmt")} - Run scalafmt on the entire project
            |${item("scripted")} - Run the scripted test suite
            |${item("examples/runMain example.http4s.ExampleApp")} - Start the example server (http4s based)
            |${item("examples/runMain example.akkahttp.ExampleApp")} - Start the example server (akka-http based)
            |${item("benchmarks/jmh:run")} - Run the benchmarks
            |${item("+publishLocal")} - Publish caliban locally""".stripMargin

}
