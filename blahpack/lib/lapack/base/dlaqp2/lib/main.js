'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqp2 = require( './dlaqp2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqp2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqp2;
