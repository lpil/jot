export function make(patterns) {
  let pattern = "";
  let cursor = patterns;
  while (cursor.tail) {
    if (pattern !== "") pattern += "|";
    pattern += escapeRegExp(cursor.head);
    cursor = cursor.tail;
  }
  return new RegExp(pattern);
}

export function split(splitter, string) {
  const match = string.match(splitter);

  if (!match) return [string, "", ""]; // No delimiter found

  const index = match.index;
  const delimiter = match[0];

  return [
    string.slice(0, index),
    delimiter,
    string.slice(index + delimiter.length),
  ];
}

function escapeRegExp(string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}
