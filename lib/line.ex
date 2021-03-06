defmodule Jot.Line do
  @moduledoc false

  use Jot.Record, import: [:line]

  @doc """
  Split a template into lines for parsing.
  """
  def from_template(template) when is_binary(template) do
    template
    |> split()
    |> Enum.reverse()
  end

  defp split(template) do
    parse_indent(template)
  end


  defp parse_indent(content, line \\ line(), next_pos \\ 2, acc \\ [])

  defp parse_indent(<< " "::utf8, t::binary >>, line, next_pos, acc) do
    indent = line(line, :indent) + 1
    line   = line(line, indent: indent)
    parse_indent(t, line, next_pos, acc)
  end

  defp parse_indent("", line, next_pos, acc) do
    parse_content("", line, next_pos, acc)
  end

  defp parse_indent(content, line, next_pos, acc) do
    parse_content(content, line, next_pos, acc)
  end


  defp parse_content(content, line, next_pos, acc)

  defp parse_content(<< "\\\n"::utf8, t::binary >>, line, next_pos, acc) do
    parse_content(t, line, next_pos + 1, acc)
  end

  defp parse_content(<< "\n"::utf8, t::binary >>, line, next_pos, acc) do
    next_line = line(pos: next_pos)
    acc       = add(acc, line)
    parse_indent(t, next_line, next_pos + 1, acc)
  end

  defp parse_content("", line, _next_pos, acc) do
    add(acc, line)
  end

  defp parse_content(<< h::utf8, t::binary >>, line, next_pos, acc) do
    content  = line(line, :content) <> <<h>>
    new_line = line(line, content: content)
    parse_content(t, new_line, next_pos, acc)
  end


  defp add(acc, line) do
    case line(line, :content) do
      "" -> acc
      _  -> [line|acc]
    end
  end
end
