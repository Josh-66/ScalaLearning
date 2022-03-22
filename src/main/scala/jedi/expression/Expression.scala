package jedi.expression
import jedi.value.Value

trait Expression {
  def execute(env:Environment): Value
}
