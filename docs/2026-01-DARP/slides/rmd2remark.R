

## Function to convert remark source with YAML headers into HTML+remark.


htmlHeader <- "<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>{{TITLE}}</title>
    <link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css' integrity='sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm' crossorigin='anonymous'>
    <link rel='stylesheet' href='assets/remark.css'>
  </head>
  <body>
    <textarea id='source'>

class: center, middle

# {{TITLE}}

## {{SUBTITLE}}

### {{AUTHOR}}

<h1 onclick='document.documentElement.requestFullscreen();' style='cursor: pointer;'>
<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-arrows-fullscreen' viewBox='0 0 16 16'>
  <path fill-rule='evenodd' d='M5.828 10.172a.5.5 0 0 0-.707 0l-4.096 4.096V11.5a.5.5 0 0 0-1 0v3.975a.5.5 0 0 0 .5.5H4.5a.5.5 0 0 0 0-1H1.732l4.096-4.096a.5.5 0 0 0 0-.707zm4.344 0a.5.5 0 0 1 .707 0l4.096 4.096V11.5a.5.5 0 1 1 1 0v3.975a.5.5 0 0 1-.5.5H11.5a.5.5 0 0 1 0-1h2.768l-4.096-4.096a.5.5 0 0 1 0-.707zm0-4.344a.5.5 0 0 0 .707 0l4.096-4.096V4.5a.5.5 0 1 0 1 0V.525a.5.5 0 0 0-.5-.5H11.5a.5.5 0 0 0 0 1h2.768l-4.096 4.096a.5.5 0 0 0 0 .707zm-4.344 0a.5.5 0 0 1-.707 0L1.025 1.732V4.5a.5.5 0 0 1-1 0V.525a.5.5 0 0 1 .5-.5H4.5a.5.5 0 0 1 0 1H1.732l4.096 4.096a.5.5 0 0 1 0 .707z'/>
</svg>
</h1>

---

"

htmlFooter <- "


    </textarea>
  </body>

  <script 
	  src='assets/remark-latest.min.js'
	  type='text/javascript'></script>

  <script type='text/javascript'>
    var slideshow = remark.create(
	{
	    navigation: {scroll: false,},
	    ratio: '16:9',
	    // ratio: '4:3',
	    countIncrementalSlides: false
	});

    // Function to add copy buttons
    function addCopyButtons() {
      const codeBlocks = document.querySelectorAll('.remark-code');
      codeBlocks.forEach((codeBlock) => {
        const button = document.createElement('button');
        button.className = 'copy-button';
        button.textContent = 'Copy';
    
        button.addEventListener('click', () => {
          button.textContent = '';
          const code = codeBlock.innerText;
          navigator.clipboard.writeText(code).then(() => {
            button.textContent = 'Copied!';
            setTimeout(() => button.textContent = 'Copy', 2000);
          });
        });
    
        codeBlock.style.position = 'relative'; // Ensure button is positioned correctly
        codeBlock.appendChild(button);
      });
    }

    // Run after slide load
    window.onload = addCopyButtons;

    // Setup MathJax; unused if mathjax == false
    MathJax = {
	tex: {
	    inlineMath: [ ['$', '$'], ['\\\\(', '\\\\)'] ],
	    displayMath: [ ['$$', '$$'], ['\\\\[', '\\\\]'] ]
	},
	svg: {
	    fontCache: 'global'
	},
    };

  </script>

  <script type='text/javascript' async
	  src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js'>
  </script>

</html>
"

strsub <- function(x, old, new)
{
    gsub(pattern = old, replacement = new, x, fixed = TRUE)
}


remarkize <- function(file, outfile = gsub(".md$", ".html", file))
{
    stopifnot(outfile != file)
    content <- readLines(file)
    wdelim <- which(content == "---")
    if (wdelim[[1]] != 1 || wdelim[[2]] < 3) stop("unexpected input")
    header <- content[seq(2, wdelim[[2]]-1L)]
    content <- tail(content, -wdelim[[2]])

    vars <- strsplit(header, ":", fixed = TRUE)
    vars <- structure(lapply(vars, tail, -1) |> lapply(trimws) |> sapply(paste, collapse = ": "),
                      names = sapply(vars, head, 1))

    out <- c(htmlHeader |>
                 strsub("{{TITLE}}",    vars[["title"]])     |>
                 strsub("{{SUBTITLE}}", vars[["subtitle"]]) |>
                 strsub("{{AUTHOR}}",   vars[["author"]]),
             content,
             htmlFooter)

    cat(out, file = outfile, sep = "\n")
}
