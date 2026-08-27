# Standard Library Data

The JSON files in this directory are the hand-maintained source data for the generated standard-library reference. Run `npm run stdlib:generate` from `site/` to render the pages under `site/docs/stdlib`.

Each module record can contain:

- `module`, `order`, `source`, and `summary` for module metadata;
- `types` with an inline kind in `name`, plus `description`, `attributes`, `fields`, and `variants`;
- `functions` with `name`, `owner`, `signature`, `stage`, `description`, `parameters`, `returnType`, an optional `returnDescription` for non-void returns, and optional `examples`.

The generated pages are ignored by Git. Keep the JSON records concise for now; structural extraction and stronger consistency checks can be added later without changing the rendered format.
