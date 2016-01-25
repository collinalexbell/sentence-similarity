# semantic-similarity

This library will compute the similarity between two sentences or short corpus's
It is based of [Sentence similarity based on semantic nets and corpus statistics](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?tp=&arnumber=1644735&url=http%3A%2F%2Fieeexplore.ieee.org%2Fiel5%2F69%2F34468%2F01644735.pdf%3Farnumber%3D1644735) by Yuhua Li

## Usage
Include `[semantic-similarity "1.0.0"]` as a dependency in project.clj 

This library has a 1 function api: `(get-sentence-similarity "Sentence one is awesome" "Sentence two is awsome")`
DO NOT pass in any punctuation!


##IMPORTANT

You need to extract the contents of this library's `./resource` folder into your projects `./resource` folder.

If you know a way around this hack (like how to properly include resource files w your library), then please make a pull request showing me how. 
Just search for `slurp` so find the two locations where I use those files


##Big O

This thing is terribly slow. O(N^2) where N=sentence-length is my guess, it may be more. I just hacked it together.

## License

The IDGAF license
