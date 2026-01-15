# Etch - A Gleam TUI Backend Library

[![Package Version](https://img.shields.io/hexpm/v/etch)](https://hex.pm/packages/etch)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/etch/)

Etch is a powerful terminal user interface (TUI) backend library for Gleam, designed to help you build rich, interactive terminal applications with ease. It provides a comprehensive set of tools for managing terminal output, handling events, and styling text, with no third-party dependencies.

This project was inspired by [crossterm](https://github.com/crossterm-rs/crossterm).

## Features

### Terminal Control
- **Raw Mode**: Enter and exit raw mode for direct terminal control
- **Alternative Screen**: Switch between main and alternative screen buffers
- **Window Management**: Get and set terminal dimensions
- **Screen Clearing**: Clear the screen or specific portions with various options
- **Line Wrapping**: Enable or disable line wrapping
- **Scrolling**: Scroll the terminal content up or down

### Cursor Management
- **Positioning**: Move cursor to specific positions or relative offsets
- **Visibility**: Show or hide the cursor
- **Position Saving**: Save and restore cursor positions
- **Styling**: Set cursor style (block, underscore, bar) with blinking options

### Event Handling
- **Keyboard Input**: Capture and parse keyboard events with modifiers
- **Mouse Support**: Handle mouse events (clicks, drags, scrolls)
- **Focus Management**: Detect focus gain/loss events
- **Window Resizing**: Respond to terminal resize events
- **Keyboard Enhancement**: Support for [progressive keyboard enhancement flags](https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement)

### Text Styling
- **Colors**: Full color support including:
  - Standard colors (black, red, green, etc.)
  - Bright variants
  - 256 ANSI colors
  - True RGB colors
- **Attributes**: Text styling with bold, dim, italic, underline, blinking, and inverse
- **Style Management**: Save and apply complex styles (foreground, background, attributes)

### Command System
- **Queue System**: Batch and flush terminal commands efficiently
- **Comprehensive Commands**: Wide range of commands for all terminal operations
- **Immediate Execution**: Execute commands without queueing

## Getting Started

Here's a basic template to get you started with Etch:

```gleam
import etch/command
import etch/terminal
import etch/style
import etch/stdout.{type Queue, execute, flush, queue}

pub fn main() {
  // Use execute to execute `Commands`
  execute([
    command.EnterRaw,
    command.EnterAlternateScreen,
    command.HideCursor,
    command.Clear(terminal.All)
  ])

  // Queue `Commands`
  let q = Queue([
    command.SetForegroundColor(style.Red),
    command.SetBackgroundColor(style.Black),
  ])
  let text = "Styled text"
  let q = queue(q, [command.Println(text)])
  
  // Flush queued `Commands`
  flush(q)
}
```

## Examples

Check out the examples in the `examples/` directory for more advanced usage:

- `hello_world.gleam`: Basic text rendering and centering
- `events.gleam`: Comprehensive event handling
- `snake.gleam`: Simple game implementation
- `styles.gleam`: Advanced text styling techniques

To run the use this command:
```sh
gleam run -m examples/{example_name}
```

## Documentation

For detailed API documentation, see the [documentation](https://hexdocs.pm/etch/) for the Etch package.

## Building and Shipping Your App
Before building your application I suggest you to look at the [benchmarks](https://github.com/bananaofhappiness/etch/blob/main/benchmarks/README.md).

See this awesome article: [Packaging a Gleam app into a single executable](https://www.dhzdhd.dev/blog/gleam-executable). In short, there are multiple ways to build your app into a single executable:

### JavaScript target

#### **Bun**
1. **Build your application**:
   ```sh
   gleam build
   ```
2. **Compile it using bun**
  ```sh
  bun build --compile --outfile=bundle build/dev/javascript/<project_name>/<project_name>.mjs --footer="main();" 
  ```
You will get an executable file in your project's root directory.

#### **Node and Deno**
Refer to the article above for more information.

### Erlang target

#### **Gleescript**
1. **Add gleescript to your project**:
   ```sh
   gleam add --dev gleescript
   ```

2. **Build your application**:
   ```sh
   gleam build
   gleam run -m gleescript
   ```
You will get an executable file in your project's root directory. Users must have Erlang installed in order to run your executable.

#### **Burrito**
1. **Create new [mix](https://elixirschool.com/en/lessons/basics/mix/) project**
  ```sh
  mix new my_app
  ```

2. **Add [MixGleam](https://github.com/gleam-lang/mix_gleam) to your project**:
   See [Installation](https://github.com/gleam-lang/mix_gleam#installation)
 
3. **Add [Burrito](https://github.com/burrito-elixir/burrito)**:
  See [Quick start](https://github.com/burrito-elixir/burrito#quick-start)

4. **Add etch**
  ```elixir
  # in mix.exs
  # ...
    defp deps do
      [
        # ...
        {:etch, "~> 1.2.0"},
        # ...
      ]
    end
  # ...
  ```
5. **Build your application**:
   ```sh
   mix
   ```
You will get an executable file in build directory. Unlike gleescript, burrito does not require Erlang to be installed to run your application.

## Contributing.

Feel free to open issues and make pull requests.

## Authors

* Egor Kurtashkin — project owner and creator.

## License

This project is licensed under MIT License. See LICENSE file for details.
