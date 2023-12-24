import sbt.Keys.*

object ConsoleHelper {
  def prompt: String               = s"${Console.CYAN}>${Console.RESET} "
  def header(text: String): String = s"${Console.GREEN}$text${Console.RESET}"

  def item(text: String): String =
    s"${Console.RED}> ${Console.CYAN}$text${Console.RESET}"

  def welcomeMessage(scala212Version: String, scala213Version: String, scala3Version: String) =
    onLoadMessage := {
      raw"""|${header(s"""   ____      _ _ _                 """)}
            |${header(s"""  / ___|__ _| (_) |__   __ _ _ __  """)}
            |${header(s""" | |   / _` | | | '_ \\ / _` | '_ \\ """)}
            |${header(s""" | |__| (_| | | | |_) | (_| | | | |""")}
            |${header(s"""  \\____\\__,_|_|_|_.__/ \\__,_|_| |_|  ${version.value}""")}
            |
            |Useful sbt tasks:
            |${item("~compile")} - Compile all modules with file-watch enabled
            |${item("+test")} - Run the unit test suite for all modules and Scala versions
            |${item(s"++$scala3Version; rootJVM3/test")} - Run tests for all JVM modules in Scala 3
            |${item("fmt")} - Run scalafmt on the entire project
            |${item("scripted")} - Run the scripted test suite
            |${item("examples/runMain example.quick.ExampleApp")} - Start the example server
            |${item("benchmarks/jmh:run")} - Run the benchmarks
            |${item("+publishLocal")} - Publish caliban locally
            |
            |Changing scala versions:
            |${item(s"++$scala212Version")}
            |${item(s"++$scala213Version")}
            |${item(s"++$scala3Version")}""".stripMargin
    }

}
