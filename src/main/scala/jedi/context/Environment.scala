package jedi.context
import jedi.value.Value
import jedi.expression.Identifier

class Environment extends collection.mutable.HashMap[Identifier, Value]