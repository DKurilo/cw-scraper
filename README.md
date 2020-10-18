# cw-scraper

This is simple app I wrote during [course](https://www.futurelearn.com/courses/functional-programming-haskell).  
It scraps english WiKi pages using list in Main.hs and returns list of most frequent words.  
It also builds word cloud for each article and stores it in `./generated-images` forlder as PNG image.  

## Build
```
stack build
```

## Install
```
stack install
```

## Run
```
cw-scrapper-exe
```
or
```
stack run
```

## Test
```
stack test
```

**Important!**  
Run and test application from project root directory. It wil use `./fonts/Underdog-Regular.ttf` font and stores images into `./generated-images` directory. It will fail in case it can't find font or directory.

Enjoy as I enjoying the course!  

## Initial README content

This is a skeleton Stack project for the Functional Programming coursework exercise in Glasgow, Nov 2019. For full details, please refer to the course moodle page at
https://moodle.gla.ac.uk/course/view.php?id=978

---
Jeremy.Singer@glasgow.ac.uk
18 Nov 2019
