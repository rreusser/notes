'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlapll = require( './dlapll.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlapll, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlapll;
