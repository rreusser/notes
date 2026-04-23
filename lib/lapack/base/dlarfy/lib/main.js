
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarfy = require( './dlarfy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarfy, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarfy;
