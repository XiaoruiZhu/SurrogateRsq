[Fixed] Some code lines in examples are commented out. Please never do that. Ideally find toy examples that can be regularly executed and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest{}. Examples in comments in: surr_rsq_rank.Rd
       
[Fixed] Thanks, we see you used $R^2$ in both Title and Description fields. Unfortunately, LaTeX markup is not supported in the DESCRIPTION file,  hence please rewrite as "R-squared".

* using R Under development (unstable) (2022-11-23 r83380)
* using platform: x86_64-pc-linux-gnu (64-bit)

* using R Under development (unstable) (2022-10-11 r83083 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)

* checking CRAN incoming feasibility ... [8s] NOTE

Found the following (possibly) invalid URLs:
  URL: https://CRAN.R-project.org/package=
    From: README.md
    Status: 404
    Message: Not Found
  URL: https://cranlogs.r-pkg.org/badges/grand-total/
    From: README.md
    Status: 404
    Message: Not Found
  URL: https://cranlogs.r-pkg.org/badges/last-month/
    From: README.md
    Status: 404
    Message: Not Found
  URL: https://cranlogs.r-pkg.org/badges/last-week/
    From: README.md
    Status: 404
    Message: Not Found
  URL: https://www.r-pkg.org/badges/version/
    From: README.md
    Status: 404
    Message: Not Found
    
* checking examples ... [88s/88s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
              user system elapsed
surr_rsq_ci 82.403  0.319  82.726
