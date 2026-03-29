'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlatrs = require( './zlatrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlatrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlatrs;
