'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dormhr = require( './dormhr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormhr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormhr;
