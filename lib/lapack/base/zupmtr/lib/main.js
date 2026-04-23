'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zupmtr = require( './zupmtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zupmtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zupmtr;
