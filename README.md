# semantic-similarity

This library will compute the similarity between two sentences or short corpus's
It is based of [Sentence similarity based on semantic nets and corpus statistics](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?tp=&arnumber=1644735&url=http%3A%2F%2Fieeexplore.ieee.org%2Fiel5%2F69%2F34468%2F01644735.pdf%3Farnumber%3D1644735) by Yuhua Li

## Usage

One function is needed (get-sentence-similarity "Sentence one is awesome" "Sentence two is awsome")
DO NOT pass in any punctuation!

##Big O

This thing is terribly slow. O(N^2) is my guess, it may be more. I just hacked it together.

## License

The IDGAF license
