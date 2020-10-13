defmodule Darwin.Erlang.AbstractCodeTransformer do
  @moduledoc false

  @doc """
  Traverse Erlang abstract code, possibly mutating it.

  TODO: complete description
  """
  def transform(list, acc, fun) when is_list(list) do
    map_reduce_forms(list, acc, fun)
  end

  def transform(form, acc, fun) when not(is_list(form)) do
    handle_form(form, acc, fun)
  end

  defp handle_form(form, acc, fun) do
    case fun.(form, acc) do
      {:halt, new_form, new_acc} ->
        {new_form, new_acc}

      {:cont, new_form, new_acc} ->
        handle_deeper_forms(new_form, new_acc, fun)
    end
  end

  defp handle_deeper_forms({:clauses, _clauses} = form, acc, fun) do
    # Handle clauses as a special case
    case fun.(form, acc) do
      {:halt, new_form, new_acc} ->
        {new_form, new_acc}

      {:cont, new_form, new_acc} ->
        case new_form do
          {:clauses, clauses} ->
            {transformed_clauses, newer_acc} =
              map_reduce_forms(clauses, new_acc, fun)

          new_form = {:clauses, transformed_clauses}
          {new_form, newer_acc}
        end
    end
  end

  defp handle_deeper_forms({_cons, _anno} = form, acc, fun) do
    # The first element of the tuple is a constructor.
    # The second element is an annotation (it contains the line number)
    #
    # It doesn't matter if the function wants to continue because
    # there's nothing deeper than the node we're in
    {_, new_form, new_acc} = fun.(form, acc)
    {new_form, new_acc}
  end

  defp handle_deeper_forms(form, acc, fun)
      when is_tuple(form) and tuple_size(form) >= 3 do
    # We ignore the first two elements of the tuple
    # (the first of them is the constructor and the second is the annotation
    # with the line number)
    # This function is not supposed to change those.
    # If these two elements were to change, you should have done it
    # when calling `fun.(form, acc)`
    [cons, anno | rest] = Tuple.to_list(form)
    # Transform the rightmost elements (which may or may not be forms)
    {new_forms, new_acc} = map_reduce_forms(rest, acc, fun)
    # Build a new form
    new_tuple = List.to_tuple([cons, anno | new_forms])
    # Return the new form and the new accumulator
    {new_tuple, new_acc}
  end

  defp handle_deeper_forms(forms, acc, fun) when is_list(forms) do
    map_reduce_forms(forms, acc, fun)
  end

  defp handle_deeper_forms(neither_a_form_nor_a_list, acc, _fun) do
    # Now, because of the way we are "reducing" the list composed
    # of the rightmost elements of the tuple, we might find ourselves
    # trying to handle things that are neither forms nor lists of forms.
    # According to the erlang spec, forms are always tuples.
    # We simply return those unchanged.
    {neither_a_form_nor_a_list, acc}
  end

  defp map_reduce_forms(forms, acc, fun) when is_list(forms) do
    {reversed_new_forms, newer_acc} =
      Enum.reduce(forms, {[], acc}, fn form, {reversed_forms, current_acc} ->
        {new_form, new_acc} = transform(form, current_acc, fun)
        {[new_form | reversed_forms], new_acc}
      end)

    new_forms = :lists.reverse(reversed_new_forms)
    {new_forms, newer_acc}
  end
end
