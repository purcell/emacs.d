ox-hugo implements a Markdown back-end for Org exporter.  The
exported Markdown is compatible with the Hugo static site generator
(https://gohugo.io/).  This exporter also generates the post
front-matter in TOML or YAML.

To start using this exporter, add the below to your Emacs config:

  (with-eval-after-load 'ox
    (require 'ox-hugo))

With the above evaluated, the ox-hugo exporter options will be
available in the Org Export Dispatcher.  The ox-hugo export
commands have bindings beginning with "H" (for Hugo).

Commonly used export commands:

## For one-post-per-subtree flow, where a single Org file can have
   multiple Org subtrees which export to individual Hugo posts:

   - C-c C-e H H  ->  Export the *current* 'valid Hugo post subtree'
                       to a Hugo post in Markdown.

   - C-c C-e H A  ->  Export *all* 'valid Hugo post subtrees' to
                       Hugo posts in Markdown.

## For one-post-per-file flow, where a single Org file exports to
   only *one* Hugo post:

   - C-c C-e H h  ->  Export the Org file to a Hugo post in Markdown.

Do M-x customize-group, and select `org-export-hugo' to see the
available customization options for this package.

See this package's website for more instructions and examples:

  https://ox-hugo.scripter.co
