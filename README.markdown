# Blue water
Blue water is a simple web app prototype.
It's purpose is to test a Caveman 2 framework and taste Common Lisp in web development.

App can parse movie ratings from several sites and show their total average rating as well as rating charts.


## Usage
```lisp
(blue-water.parser:parse "2016-01-01" "2016-03-25" :verbose t)
(blue-water:start :port 8080)
```
## Installation
```
git clone https://github.com/TatriX/blue-water ~/quicklisp/local-projects/
(ql:quickload :blue-water)
```

## Copyright

Licensed under the MIT License.

