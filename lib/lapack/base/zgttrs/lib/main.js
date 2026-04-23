'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgttrs = require( './zgttrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgttrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgttrs;
