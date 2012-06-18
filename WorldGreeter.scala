class WorldGreeter(greeting: String) {
  def greet() = {
    val worldlyGreeting = WorldGreeter.worldify(greeting)
    println(worldlyGreeting)
  }
}

// The WorldlyGreeter companion object
object WorldGreeter {
  def worldify(s: String) = s + ", world!"
}