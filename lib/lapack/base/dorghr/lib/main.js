'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorghr = require( './dorghr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorghr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorghr;
