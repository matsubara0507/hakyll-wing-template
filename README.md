# Hakyll template
Light and beautiful blog style template for hakyll.

Port of [jekyll-wing-template](https://github.com/nikrich/jekyll-wing-template)

![preview](https://github.com/matsubara0507/hakyll-wing-template/fig/screenshot.png)

## Demo
Find a demo [here](http://matsubara0507.github.io/hakyll-wing-template/#)

## How to run it

1. `stack build`
2. `sass -I sass --scss css/main.scss css/main.css`, if you change css files
3. `stack exec -- site build`
4. `stack exec -- site watch` and browse http://locallhost:8000

## Note
If you want to use ghci, run ` stack ghci --no-load HsSyck`
