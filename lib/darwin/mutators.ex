defmodule Darwin.Mutators do
  require Darwin.Mutator.Helpers.DefMutator, as: DefMutator
  alias Darwin.Mutator.Context
  alias Darwin.Mutators.Default

  @doc """
  Mutates Erlang abstract code.
  """
  def mutate(abstract_code, module, mutators \\ Default.mutators()) do
    ctx = Context.new(module: module)
    DefMutator.apply_mutators(mutators, abstract_code, ctx)
  end
end

# # Mutators
# ## Conditionals Boundary
#
# - Active by default
#
# The conditionals boundary mutator replaces the relational operators <, <=, >, >=
# with their boundary counterpart as per the table below.

# ## Math

# ### Unary operators

# ### Binary arithmetic operators

# ## Negate Conditionals

# ## Return Values

# ## Void Method Calls

# ## Constructor Calls

# ## Empty returns

# ## False Returns

# ## Inline Constant

# ## Null returns

# ## Non Void Method Calls

# ## Primitive returns

# ## Remove Conditionals

# ## Remove Increments

# ## True returns

# ## Experimental Argument Propagation
# ## Experimental Big Integer
# ## Experimental Member Variable
# ## Experimental Naked Receiver
# ## Experimental Switch

# ## Negation
# ## Arithmetic Operator Replacement
# ## Arithmetic Operator Deletion
# ## Constant Replacement
# ## Bitwise Operator
# ## Relational Operator Replacement
# ## Unary Operator Insertion
