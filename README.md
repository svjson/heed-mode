
# heed-mode.el

Major mode for editing [Heed](https://www.github.com/svjson/heed) presentations.

## Features
- Basic syntax highlighting
- Rudimentary integration with `heed-cli` for navigating within a presentation

## Key bindings

| Binding | Command                               | Description                                                                          |   |
|---------|---------------------------------------|--------------------------------------------------------------------------------------|---|
| M-n     | `heed-next-slide`                     | Navigate to the next slide in the current presentation                               |   |
| N-p     | `heed-previous-slide`                 | Navigate to the previous slide in the current presentation                           |   |
| C-c k   | `heed-close-presentation-other-files` | Close all files belonging to the current presentation except the current buffer/fil. |   |

## License

© 2025 Sven Johansson. [GPLv3 Licensed](./LICENSE).
