# Changelog

## v2.2.0 - 2025-03-31

- The performance of `parse` and `to_html` has been greatly improved!

## v2.1.0 - 2025-03-24

- Added support for raw blocks.

## v2.0.2 - 2025-03-13

- Added missing label on paragraph record.

## v2.0.1 - 2025-02-11

- Updated for latest stdlib.

## v2.0.0 - 2024-01-08

- Add support for footnotes.

## v1.1.0 - 2024-10-25

- Add support for thematic breaks.
- Fix bug where inline elements had leading whitespace.

## v1.0.2 - 2024-08-26

- Fixed a bug in parsing inline line breaks.

## v1.0.1 - 2024-08-19

- Add Support for hard line breaks via `\\n`.
- Updated Gleam stdlib and fixed deprecation warning.

## v1.0.0 - 2024-05-25

- Support added for images.
- Link text is used as reference label when label is empty.
- Line breaks in links are parsed in accordance with the spec.

## v0.6.0 - 2024-05-22

- Fixed a bug where inline code endings would only be detected at the end of
  the document.

## v0.5.0 - 2024-04-30

- Support added for inline code with backticks.

## v0.4.0 - 2024-04-19

- Support added for `em` and `strong`.

## v0.3.1 - 2024-02-15

- Fixed bug where commas aren't sanitized when creating id attributes.

## v0.3.0 - 2024-02-15

- Support added for arbitrary block attributes.

## v0.2.1 - 2023-12-08

- Fixed a bug where paragraphs containing links could lose some of their
  content.

## v0.2.0 - 2023-12-08

- Support added for id and class block attributes.

## v0.1.0 - 2023-12-07

- Initial release, with support for headings, paragraphs, links (inline and
  reference), and code blocks.
