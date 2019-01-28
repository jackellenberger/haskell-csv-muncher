# Haskell CSV Muncher

Encode metadata about csv cells by sticking it in the first column as semicolon separated values.

# Usage

```bash
# Test
$ doctest munge.hs
# Run
$ runhaskell main.sh < ~/path/to/file.csv
```

# Use case

![example_screenshot.png](/example/example_screenshot.png)

I made my resume in excel for easier programmatic i/o, and I'd like to be able to update my linkedin and website programmatically when I update my spreadsheet. This lets me take data that makes sense in the context of a resume and auto parse it into a context free text file.

So with `runhaskell main.sh < ./example/example_input.csv` [this input](example/example_input.csv) becomes [this marginally nicer output](example/example_output.txt)
